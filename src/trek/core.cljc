(ns trek.core
  #?@
   (:clj
    [(:require
      [clojure.java.io :as io]
      [clojure.string :as str]
      [trek.async :as async]
      [trek.grammar :as grammar]
      [trek.interpreter :as interpreter]
      [trek.machine :as machine]
      [trek.util :refer [parse-int]])]
    :cljs
    [(:require
      [cljs-http.client :as http]
      [clojure.string :as str]
      [cognitect.transit :as transit]
      [trek.interpreter :as interpreter]
      [trek.machine :as machine]
      [trek.util :refer [parse-int]])
     (:require-macros [trek.async-cljs :as async])]))

(defn load-program [machine content program]
  (async/go?
   (-> machine
       (assoc :source (-> content
                          (str/split #"\n")
                          (->> (map (fn [line] [(parse-int (re-find #"^[0-9]+" line)) line]))
                               (into {}))))
       (machine/load-program program))))

(defn fetch-program
  []
  #?(:clj (let [txt     (slurp (io/resource "public/sttr1.txt"))
                program (grammar/parse (interpreter/interpreter) txt :S)]
            {:txt     txt
             :program program})

     :cljs (let [txt     (async/<? (http/get "sttr1.txt" {}))
                 program (->> (async/<? (http/get "sttr.transit+json" {}))
                              :body
                              (transit/read (transit/reader :json)))]
             {:txt     txt
              :program program})))

(defn run
  []
  (let [{:keys [txt program]} (fetch-program)]
    (async/go?
     (let [machine (async/<? (load-program (interpreter/interpreter) txt program))]
       (loop [machine machine]
         (recur (async/<? (machine/step machine))))))))

#?(:clj
   (defn -main [& _]
     (let [e (async/<?? (run))]
       (when (ex-data e)
         (throw e)))))
