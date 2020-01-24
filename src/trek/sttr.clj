(ns trek.sttr
  (:require [clojure.java.io :as io]))

(defmacro txt
  []
  (slurp (io/resource "sttr1.txt")))
