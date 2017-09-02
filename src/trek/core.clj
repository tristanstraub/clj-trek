(ns trek.core
  (:require [clojure.java.io :as io]
            [trek.grammar :as grammar]
            [trek.interpreter :as interpreter]
            [trek.rules :as rules]
            [clojure.string :as str]
            [trek.machine :as machine]))

(defonce history (atom nil))
(defonce machine (atom nil))

(defn line
  ([]
   (line (:ptr @machine)))
  ([n]
   (get-in @machine [:source n])))

(def *debug* nil)

(defn print-next []
  (when *debug*
    (clojure.pprint/pprint [:env (:env @machine)
                            :stack (:stack @machine)
                            :ptr (:ptr @machine)
                            "Next:" (line)]))
  nil)

(defn parse*
  ([content]
   (parse* content :program))
  ([content start]
   (grammar/parse (grammar/parser) (interpreter/interpreter) content start)))

(defn load-program* [machine content]
  (let [program (grammar/parse (grammar/parser) machine content :program)]

    (-> machine
        (assoc :source (-> content
                           (str/split #"\n")
                           (->> (map (fn [line] [(Integer/parseInt (re-find #"^[0-9]+" line)) line]))
                                (into {}))))
        (machine/load-program program))))

(defn load-program [machine file]
  (load-program* machine (slurp file)))

(defn start*
  [content]
  (let [new-machine (load-program* (interpreter/interpreter) content)]
    (reset! machine new-machine)))

(defn start!
  ([]
   (start! (io/resource "sttr1.txt")))
  ([file]
   (start* (slurp file))
   (print-next)))

(defn env
  ([]
   (:env @machine))
  ([k]
   (-> @machine
       (machine/evaluate (parse* k :expression))
       interpreter/last-value)))

(defn reload!
  ([]
   (reload! (io/resource "sttr1.txt")))
  ([file]
   (swap! machine assoc :program (:program (load-file @machine file)))
   (swap! history (fn [history]
                    (->> history
                         (map #(assoc % :program (:program (load-file @machine file)))))))

   nil))

(defn step!
  ([]
   (step! 1))
  ([n]
   (doseq [i (range n)]
     (swap! machine machine/step)
     (swap! history conj @machine)
     (print-next))))

(defn until!
  [line-number]
  (while (not= (:ptr @machine) line-number)
    (step!)))

(defn continue!
  []
  (while true
    (step!)))

(defn back!
  []
  (reset! machine (peek @history))
  (swap! history pop)
  (print-next))

(defn retry!
  []
  (back!)
  (step!))

(comment (grammar/parse (grammar/parser)
                        (interpreter/interpreter)
                        (slurp (io/resource "sttr1.txt"))))
