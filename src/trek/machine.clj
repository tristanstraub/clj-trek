(ns trek.machine)

(defmulti emit (fn [machine statement-type & args] [(:type machine) statement-type]))
(defmulti evaluate (fn [machine statement] [(:type machine) (:type statement)]))
