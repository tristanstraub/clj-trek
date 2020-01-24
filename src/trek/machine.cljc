(ns trek.machine)

(defmulti emit (fn [machine statement-type args]
                 {:pre [(keyword? statement-type)]}
                 [(:type machine) statement-type]))
(defmulti emitted (fn [machine statement] [(:type machine) (:type statement)]))
(defmulti evaluate (fn [machine statement] [(:type machine) (:type statement)]))
(defmulti load-program (fn [machine program] [(:type machine) (:type program)]))
(defmulti step (fn [machine] (:type machine)))
(defmulti formatter (fn [machine formatter] [(:type machine) (:type formatter)]))
