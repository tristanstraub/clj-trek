(ns trek.demo
  (:require [clojure.core.match :as c]
            [clojure.java.io :as io]
            [instaparse.core :as insta]))

(defn make-step
  [lines]
  (fn [state]
    (let [line-number (or (:line-number state)
                          (ffirst (sort-by first lines)))
          [line-number statement] (get lines line-number)]
      (-> (statement state)
          (as-> state
              (assoc state :line-number (or (:goto state) (->> (sort-by first lines)
                                                               (filter (comp #(< line-number %) first))
                                                               ffirst))))
          (dissoc :goto)))))

(let [step (-> (insta/parser (slurp (io/file "resources/slides/basic.ebnf")))
               (insta/parse (slurp (io/file "resources/slides/example.bas")))
               (->> (insta/transform {:statements  (fn [& statements]
                                                     (let [lines (->> (group-by first statements)
                                                                      (reduce-kv #(assoc %1 %2 (first %3)) {}))]
                                                       (make-step lines)))

                                      :statement   (fn [line-number statement]
                                                     [line-number statement])

                                      :line-number (fn [line-number]
                                                     (Integer/parseInt line-number))

                                      :expression (fn
                                                    ([number]
                                                     (fn [state]
                                                       (number state)))
                                                    ([identifier number]
                                                     (fn [state]
                                                       (assoc state :result (+ (:result (identifier state))
                                                                               (:result (number state)))))))

                                      :number (fn [value]
                                                (fn [state]
                                                  (assoc state :result (Integer/parseInt value))))

                                      :identifier (fn [name]
                                                    (fn
                                                      ([]
                                                       name)
                                                      ([state]
                                                       (assoc state :result (get state name)))))

                                      :assignment (fn
                                                    [identifier expression]
                                                    (fn [state]
                                                      (let [state (expression state)]
                                                        (assoc state (identifier) (:result state)))))

                                      :string (fn [value]
                                                (fn [state]
                                                  (assoc state :result value)))

                                      :print (fn [& args]
                                               (fn [state]
                                                 (reduce (fn [state value]
                                                           (let [next-state (value state)]
                                                             (update next-state :output conj (:result next-state))))
                                                         state
                                                         args)))

                                      :end (fn []
                                             (fn [state]
                                               (assoc state :stop true)))

                                      :goto (fn [line-number]
                                              (fn [state]
                                                (assoc state :goto line-number)))

                                      :if (fn [a op b then]
                                            (assert (= op "="))
                                            (fn [state]
                                              (if (= (:result (a state))
                                                     (:result (b state)))
                                                (then state)
                                                state)))})))]
  (loop [state {:output []}]
    (let [state (step state)]
      (if-not (:stop state)
        (recur state)
        state))))
