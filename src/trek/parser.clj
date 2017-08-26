(ns trek.parser
  (:require [trek
             [rules :refer [ns-parse ns-parser ns-transforms]]]
            [clojure.string :as str]
            [trek.statements :as statements]))

(defn generate-program
  [emitter listing]
  (let [parser     (ns-parser *ns*)
        transforms (ns-transforms *ns*)]
    (->> (str/split listing #"\n")
                        (reduce (fn [program line]
                                  (let [command-builder (ns-parse parser transforms line :S)]
                                    (assert command-builder)

                                    (command-builder program)))
                                {}))))
