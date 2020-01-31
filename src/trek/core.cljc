(ns trek.core
  #?(:cljs (:require-macros [trek.sttr :as sttr]
                            [trek.async-cljs :as async]
                            [cljs.core.async :as a]
                            [clojure.java.io :as io]))
  (:require  #?@(:clj [[clojure.core.async :as a]]
                 :cljs [[cljs.core.async :as a]
                        [cljs-http.client :as http]])
             #?@(:clj [[trek.sttr :as sttr]
                       [trek.async :as async]])
             #?(:clj [clojure.java.io :as io])
             [trek.grammar :as grammar]
             [trek.interpreter :as interpreter]
             [trek.rules :as rules]
             [clojure.string :as str]
             [trek.machine :as machine]
             [cognitect.transit :as transit]))

(defonce history (atom nil))
(defonce machine (atom nil))

(defonce source-code
  (sttr/txt))

(defn parse-int
  [v]
  #?(:clj (Integer/parseInt v)
     :cljs (js/parseInt v)))

(defn line
  ([]
   (line (:ptr @machine)))
  ([n]
   (get-in @machine [:source n])))

(def ^:dynamic *debug* nil)

(defn print-next []
  (when *debug*
    (#?(:clj clojure.pprint/pprint :cljs println)
     [:env (:env @machine)
      :stack (:stack @machine)
      :ptr (:ptr @machine)
      "Next:" (line)]))
  nil)

(defn load-program [machine content program]
  (async/go?
   (-> machine
       (assoc :source (-> content
                          (str/split #"\n")
                          (->> (map (fn [line] [(parse-int (re-find #"^[0-9]+" line)) line]))
                               (into {}))))
       (machine/load-program program))))

(defn start*
  [messages content program]
  (async/go?
   (a/>! messages :loading)
   (let [new-machine (async/<? (load-program (interpreter/interpreter) content program))]
     (reset! machine new-machine)
     (a/>! messages :loaded))))

#?(:clj
   (defn reload!
     ([]
      (reload! (sttr/txt)))
     ([file]
      (swap! machine assoc :program (:program (load-file file)))
      (swap! history (fn [history]
                       (->> history
                            (map #(assoc % :program (:program (load-file @machine file)))))))

      nil)))

(defn step!
  ([]
   (step! 1))
  ([n]
   (async/go?
    (doseq [i (range n)]
      (swap! history conj @machine)

      (reset! machine (async/<? (machine/step @machine)))

      (print-next)))))

(defn until!
  [line-number]
  (async/go?
   (while (not= (:ptr @machine) line-number)
     (async/<? (step!)))))

(defn continue!
  [messages]
  (async/go?
   (while true
     (let [e (async/<? (step!))]
       (when (ex-data e)
         (println e))))))

(defn back!
  []
  (reset! machine (peek @history))
  (swap! history pop)
  (print-next))

#?(:clj
   (defn retry!
     []
     (back!)
     (async/<?? (step!))))

(defn run
  []
  (let [messages (a/chan)]
    (-> (async/go?
         (let [txt #?(:cljs (async/<? (http/get "sttr1.txt" {}))
                      :clj (slurp (io/resource "public/sttr1.txt")))
               program #?(:cljs (let [response (async/<? (http/get "sttr.transit+json" {}))]
                                  (transit/read (transit/reader :json)
                                                (:body response)))
                          :clj (grammar/parse (interpreter/interpreter)
                                              (slurp (io/resource "public/sttr1.txt"))
                                              :S))]
           (async/<? (start* messages txt program)))
         #?(:clj (async/<? (continue! messages))
            :cljs  (async/<? (continue! messages))))
        #?(:clj async/<??))
    messages))

(defn -main [& argv]
  (run))


(comment
  (def mm (interpreter/interpreter))
  (def pp (grammar/parser mm))

  (async/go? (time
              (do
                (println :start)
                (let [e (async/<? (grammar/parse pp  mm (sttr/txt) :S))]
                  (println :stop)
                  (if (instance? js/Error e)
                    (println e))))))


  )
