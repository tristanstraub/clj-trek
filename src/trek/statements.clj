(ns trek.statements
  (:require [trek.machine :as machine]))

(defn print [machine args]
  (emitter/print machine args))
