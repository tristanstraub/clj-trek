(ns trek.core
  (:gen-class))



(defn get-instruction-line [{:keys [program instruction-ptr] :as machine}]
  (get-in program [:lines instruction-ptr]))



(defn step [machine]
  (evaluate-command machine (get-instruction-line machine)))

(defn run [{:keys [program env instruction-ptr] :as machine}]
  (loop [env              {:instruction-ptr (apply min (keys program))}]
      (when-let [instruction-ptr (:instruction-ptr env)]
        (let [{:keys [eval]} (get program instruction-ptr)
              new-env        ]
          (recur (move-to-next-instruction program (step machine)))))))

(comment (load-program machine (slurp (io/resource "sttr1.txt"))))

;;--


(defn pop-subroutine
  [env]
  env)

(defn parse
  []
  (let [parser     (ns-parser *ns*)
        transforms (ns-transforms *ns*)]
    (->> (str/split (slurp (io/resource "STTR1.txt")) #"\n")
         (take 100)
         (map #(ns-parse parser transforms % :S)))))

(defn parse-1
  [s]
  (let [parser     (ns-parser *ns*)
        transforms (ns-transforms *ns*)]
    (ns-parse parser transforms s :S)))

(defn move-to-next-instruction
  [program env]
  (cond-> env
    (not (:next-instruction-ptr env)) (update :instruction-ptr
                                              (fn [instruction-ptr]
                                                (->> (keys program)
                                                     (filter #(< instruction-ptr %))
                                                     sort
                                                     first)))
    (:next-instruction-ptr env) (-> (assoc :instruction-ptr (:next-instruction-ptr env)
                                           :next-instruction-ptr nil))))
