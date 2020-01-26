(ns trek.core
  #?(:cljs (:require-macros [trek.sttr :as sttr]
                            [cljs.core.async]
                            [trek.async-cljs :as async]
                            [cljs.core.async :as a]))
  (:require  #?(:clj [clojure.core.async :as a]
                :cljs [cljs.core.async :as a])
             #?(:clj [trek.sttr :as sttr])
             #?(:clj [trek.async :as async])
             [trek.grammar :as grammar]
             [trek.interpreter :as interpreter]
             [trek.rules :as rules]
             [clojure.string :as str]
             [trek.machine :as machine]))

(defonce history (atom nil))
(defonce machine (atom nil))

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

(defn parse*
  ([machine content]
   (parse* content :program))
  ([machine content start]
   (grammar/parse (grammar/parser machine) (interpreter/interpreter) content start)))

(defn load-program* [machine content]
  (async/go?
   (let [program (async/<? (grammar/parse (grammar/parser machine) machine content :S))]
     (-> machine
         (assoc :source (-> content
                            (str/split #"\n")
                            (->> (map (fn [line] [(parse-int (re-find #"^[0-9]+" line)) line]))
                                 (into {}))))
         (machine/load-program program)))))

(defn load-program [machine content]
  (load-program* machine content))

(defn start*
  [messages content]
  (async/go?
   (a/>! messages :loading)
   (let [new-machine (async/<? (load-program* (interpreter/interpreter) content))]
     (reset! machine new-machine)
     (a/>! messages :loaded))))

(defn env
  ([]
   (:env @machine))
  ([k]
   (-> @machine
       (machine/evaluate (parse* @machine k :expression))
       interpreter/last-value)))

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
    (-> (async/go? (async/<? (start* messages (sttr/txt)))
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
