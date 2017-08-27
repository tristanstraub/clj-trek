(ns trek.core
  (:require [clojure.java.io :as io]
            [trek.grammar :as grammar]
            [trek.interpreter :as interpreter]
            [trek.rules :refer [ns-parse ns-parser ns-transforms]]
            [clojure.string :as str]
            [trek.machine :as machine]))

(comment (let [machine (interpreter/interpreter)]
           (->> (grammar/parse machine (slurp (io/resource "sttr1.txt")))
                (machine/load machine)
                (machine/step)
                )))

;; (defn run [{:keys [program env instruction-ptr] :as machine}]
;;   (loop [env              {:instruction-ptr (apply min (keys program))}]
;;       (when-let [instruction-ptr (:instruction-ptr env)]
;;         (let [{:keys [eval]} (get program instruction-ptr)
;;               new-env        ]
;;           (recur (move-to-next-instruction program (step machine)))))))

;; (comment (load-program machine (slurp (io/resource "sttr1.txt"))))

;; ;;--


;; (defn parse-1
;;   [s]
;;   (let [parser     (ns-parser *ns*)
;;         transforms (ns-transforms *ns*)]
;;     (ns-parse parser transforms s :S)))

;; (defn move-to-next-instruction
;;   [program env]
;;   (cond-> env
;;     (not (:next-instruction-ptr env)) (update :instruction-ptr
;;                                               (fn [instruction-ptr]
;;                                                 (->> (keys program)
;;                                                      (filter #(< instruction-ptr %))
;;                                                      sort
;;                                                      first)))
;;     (:next-instruction-ptr env) (-> (assoc :instruction-ptr (:next-instruction-ptr env)
;;                                            :next-instruction-ptr nil))))
