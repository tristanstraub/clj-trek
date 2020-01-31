(ns trek.util)

(defn parse-int
  [v]
  #?(:clj (Integer/parseInt v)
     :cljs (js/parseInt v)))
