(ns trek.rules
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [instaparse.core :as insta]
            [clojure.walk :as walk]))

(defmacro defrule [rule args & body]
  `(defn ~(with-meta (symbol (str "rule-" (first (str/split rule #" "))))
            {:rule rule})
     ~rule
     ~args ~@body))

(defn ns-rules [ns]
  (->> (ns-publics ns)
       (filter (fn [[k v]] (:rule (meta v))))
       (into {})))

(defn ns-grammar [ns]
  (->> (ns-rules ns)
       vals
       (map meta)
       (map :rule)
       (str/join "\n")))

(defn ns-transforms [ns]
  (->> (ns-rules ns)
       (map (fn [[k v]] [(keyword (second (re-find #"^rule-(.*)$" (str k)))) v]))
       (into {})))

(defn ns-parser [ns]
  (insta/parser (ns-grammar ns)))

(defn ns-parse [parser transforms input start]
  (->> (insta/parse parser input :start start)
       (insta/transform transforms)))

(defrule "S = A B"
  [a b]
  (str a ":" b))

(defrule "A = \"a\""
  [a]
  (format "(is %s)" a))

(defrule "B = \"b\""
  [b]
  (format "(is %s)" b))
