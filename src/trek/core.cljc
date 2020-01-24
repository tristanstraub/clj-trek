(ns trek.core
  #?(:cljs (:require-macros [trek.sttr :as sttr]
                            [cljs.core.async :as a]))
  (:require  #?(:clj [clojure.core.async :as a]
                :cljs [cljs.core.async :as a])
             #?(:clj [trek.sttr :as sttr])
            [trek.grammar :as grammar]
            [trek.interpreter :as interpreter]
            [trek.rules :as rules]
            [clojure.string :as str]
            [trek.machine :as machine]
            [trek.async :as async]))

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
  ([content]
   (parse* content :program))
  ([content start]
   (grammar/parse (grammar/parser) (interpreter/interpreter) content start)))

(defn load-program* [machine content]
  (let [program (grammar/parse (grammar/parser) machine content :program)]

    (-> machine
        (assoc :source (-> content
                           (str/split #"\n")
                           (->> (map (fn [line] [(parse-int (re-find #"^[0-9]+" line)) line]))
                                (into {}))))
        (machine/load-program program))))

(defn load-program [machine content]
  (load-program* machine content))

(defn start*
  [content]
  (let [new-machine (load-program* (interpreter/interpreter) content)]
    (reset! machine new-machine)))

(defn start!
  ([]
   (start! (sttr/txt)))
  ([content]
   (start* content)
   (print-next)))

(defn env
  ([]
   (:env @machine))
  ([k]
   (-> @machine
       (machine/evaluate (parse* k :expression))
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
  (while (not= (:ptr @machine) line-number)
    (async/<? (step!))))

(defn continue!
  []
  (async/go?
   (while true
     (async/<? (step!)))))

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

(defn -main [& argv]
  (start!)
  #?(:clj (async/<?? (continue!))
     :cljs  (continue!)))

(comment (grammar/parse (grammar/parser)
                        (interpreter/interpreter)
                        (slurp (sttr/txt))))
