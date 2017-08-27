(ns trek.machine)

(defmulti emit (fn [machine statement-type args] [(:type machine) statement-type]))
(defmulti evaluate (fn [machine statement] [(:type machine) (:type statement)]))
(defmulti load (fn [machine program] [(:type machine) (:type program)]))
(defmulti step (fn [machine] (:type machine)))
