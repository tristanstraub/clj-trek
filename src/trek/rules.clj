(ns trek.rules
  (:require [clojure.string :as str]
            [instaparse.core :as insta]))

(defmacro defgrammar [grammar-name defrule]
  `(do
     (def ~grammar-name {})

     (defmacro ~defrule [rule# & [args# & body#]]
       `(do (alter-var-root #'~'~grammar-name
                            assoc ~rule# (fn ~(or args# '[& _#])
                                           ~@body#))
            nil))))

(defn rules [grammar]
  (->> grammar
       (map first)
       (str/join ";\n")))

(defn transforms [grammar]
  (->> grammar
       (map (fn [[rule transform]]
              [(keyword (first (str/split rule #" "))) transform]))
       (into {})))

(defn parser [grammar]
  (insta/parser (rules grammar)))

(defn parse [parser transforms input start]
  (let [parsed (insta/parse parser input :start start)]
    (if (instaparse.core/failure? parsed)
      (throw (ex-info "Could not parse" parsed))
      (insta/transform transforms parsed))))

(comment (do
           (defgrammar basic defrule)

           (defrule "S = A | A B"
             ([a]
              (str "just:a"))
             ([a b]
              (str a ":" b)))

           (defrule "A = \"a\""
             [a]
             (format "(is %s)" a))

           (defrule "B = \"b\""
             [b]
             (format "(is %s)" b))

           (parse (parser basic) (transforms basic) "ab" :S)))
