(ns trek.rules
  #?(:cljs (:require-macros [trek.async-cljs :as async]))
  (:require #?(:clj [clojure.core.async :as a]
               :cljs [cljs.core.async :as a])
            [clojure.string :as str]
            [instaparse.core :as insta]))

(defn rules [grammar]
  (->> grammar
       (map first)
       (str/join ";\n")))

(defn transforms [grammar machine]
  (->> grammar
       (map (fn [[rule transform]]
              [(keyword (first (str/split rule #" "))) (partial transform machine)]))
       (into {})))

(defn parser [grammar]
  (insta/parser (rules grammar)))

(defn parse-lines
  [parser transforms input start]
  (for [line (str/split input #"\n")]
    (let [parsed (insta/parse parser line :start start)]
      (if (instaparse.core/failure? parsed)
        (throw (ex-info "Could not parse" parsed))
        (insta/transform transforms parsed)))))

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
