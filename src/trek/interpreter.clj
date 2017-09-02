(ns trek.interpreter
  (:require [clojure.core.async :as a]
            [clojure.spec.alpha :as s]
            [trek.machine :as machine]
            [clojure.spec.test.alpha :as stest]))

(def ^:dynamic *line-number* nil)

(defn interpreter []
  {:type    :interpreter
   :program nil
   :env     nil
   :ptr     nil
   :stack   nil})

(defn next-line [machine]
  (let [lines (get-in machine [:program :lines])]
    (->> (keys lines)
         (filter #(< (:ptr machine) %))
         sort
         first)))

(defmethod machine/step :interpreter
  [machine]
  (let [{:keys [lines]} (:program machine)
        line            (get lines (:ptr machine))]
    (assert line)
    (binding [*line-number* (:ptr machine)]
      (as->
          (machine/evaluate machine line)
          machine
          (cond-> machine
            (:goto machine)
            (-> (assoc :ptr (:goto machine))
                (dissoc :goto))

            (not (:goto machine))
            (assoc :ptr (next-line machine))

            true
            (as-> machine
                (loop [tasks (:async machine) machine machine]
                  (if-let [task (first tasks)]
                    (recur (rest tasks) (a/<!! (task machine)))
                    (dissoc machine :async)))))))))

(defn goto [machine n]
  (-> machine
      (assoc :goto n)))

(defn gosub [machine line-number]
  (let [lines (get-in machine [:program :lines])]
    (-> machine
        (assoc :goto line-number)
        (update :stack conj (next-line machine)))))

(defn return [machine]
  (-> machine
      (assoc :goto (peek (:stack machine)))
      (update :stack (comp seq pop))))

(defmethod machine/load-program [:interpreter :program]
  [machine program]
  (-> machine
      (assoc :program program)
      (assoc :ptr (->> (keys (:lines program))
                       sort
                       first))))

(defmethod machine/emitted :default
  [machine# statement#]
  statement#)

;; -- default

(defmacro defemit [statement-type args & body]
  `(defmethod machine/emit [:interpreter ~statement-type]
     [machine# statement-type# args#]
     (let [~args `[~machine# ~@args#]]
       ~@body)))

(defmacro defeval [statement-type args & body]
  `(do (defmethod machine/emit [:interpreter ~statement-type]
         [machine# statement-type# args#]
         {:type ~statement-type
          :args args#})

       (defmethod machine/emitted [:interpreter ~statement-type]
         [machine# statement#]
         (:args statement#))

       (defmethod machine/evaluate [:interpreter ~statement-type]
         [machine# statement#]
         (let [~args `[~machine# ~@(:args statement#)]]
           ~@body))))

(defeval :default
  [& _]
  (throw (Exception. "Unexecutable!")))

;; -- statement --

(defeval :nop
  [machine & _]
  machine)

(defemit :program
  [machine statements]
  {:type :program
   :lines (reduce (fn [program statement]
                    (let [emitted               (machine/emitted machine statement)
                          [line-number content] emitted]
                      (assoc program line-number content)))
                  {}
                  statements)})



(defeval :statement
  [machine line-number content]
  (if content
    (doall (machine/evaluate machine content))
    machine))

;; -- PRINT --

(defeval :print
  [machine args]
  (apply println (map #(machine/evaluate machine %) args))
  machine)

;; -- GOTO --

(defeval :goto
  [machine line-number]
  (goto machine line-number))

;; -- GOSUB --

(defeval :gosub
  [machine line-number]
  (gosub machine line-number))

;; -- RETURN --

(defeval :return
  [machine _]
  (return machine))

;; -- IMAGE --

(defeval :image
  [machine args]
  {:type :image :args args})

;; -- values --

(defeval :value
  [machine value]
  value)

;; -- for

(defeval :for
  [machine identifier lower upper]
  (let [lower-value (machine/evaluate machine lower)
        upper-value (machine/evaluate machine upper)
        identifier  (first (machine/emitted machine identifier))]
    (assert *line-number*)
    (-> machine
        (update :for assoc identifier {:bounds [lower-value upper-value]
                                       :line-number *line-number*})
        (update :env assoc identifier lower-value))))

(defeval :next
  [machine identifier]
  (let [identifier (first (machine/emitted machine identifier))]
    (-> machine
        (update-in [:env identifier] inc)
        (cond->
            (< (get-in machine [:for identifier :line-number])
               (second (get-in machine [:for identifier :bounds])))
            (goto (get-in machine [:env identifier :line-number]))))))

;; -- identifier

(defeval :identifier
  [machine identifier]
  (let [value (get-in machine [:env identifier])]
    (assert value)
    value))

;; -- binary operator

(defeval :binary-operator
  [machine left op right]
  (throw (Exception. "not implemented")))

;; -- INPUT

(defmacro go? [& body]
  `(a/go (try ~@body
              (catch Throwable t#
                t#))))

(defmacro <? [& args]
  `(let [value# (a/<! ~@args)]
     (when (instance? Throwable value#)
       (throw value#))
     value#))

(defn async [machine f]
  (update machine :async conj f))

(defmacro with-chan [[cname chan] & body]
  `(try (let [~cname ~chan
              result# ~@body]
          result#)
        (finally
          (a/close! ~cname))))

(defeval :input
  [machine inputs]
  (async machine (fn [machine]
                   (go?
                    (update machine :env assoc (first (machine/emitted machine (first inputs))) (read-line))))))

;; -- IF

(defeval :if
  [machine expression then]
  (if (machine/evaluate machine expression)
    (machine/evaluate machine then)
    machine))

;; -- bool op

(defeval :compare
  [machine a vs b]
  (let [a (machine/evaluate machine a)
        b (machine/evaluate machine b)]
    (case vs
      ;;  #'<>' | '>' | '<=' | '<' | '=' | '>='"
      "<>" (not= a b)
      ">" (> a b)
      "<=" (<= a b)
      "<" (< a b)
      "=" (= a b)
      ">=" (>= a b))))

(defeval :bool-op
  [machine a vs b]
  (let [a (machine/evaluate machine a)
        b (machine/evaluate machine b)]
    (println :boolvs vs)))

(defeval :assign
  [machine id value]
  (update machine :env assoc
          (first (machine/emitted machine id))
          (machine/evaluate machine value)))

(defeval :array-ref
  [machine identifier dimensions]
  (let [value (get-in machine `[:env identifier ~@dimensions])]
    (assert value)
    value))

(defn new-array
  [value dimensions]
  (if (seq dimensions)
    (into [] (for [i (range (first dimensions))]
               (new-array value (rest dimensions))))
    value))

;; (defmulti evaluate (fn [machine statement] [(:type machine) (:type statement)]))
;; (defmethod evaluate :default
;;   [machine statement]
;;   any?)

;; (defmethod evaluate [:interpreter :rem]
;;   [machine statement]
;;   (s/cat :machine ::machine
;;          :statement ::nop-statement))

;; (s/def ::machine any?)
;; (s/def ::nop-statement any?)

;; (s/fdef machine/evaluate
;;         :args (s/multi-spec evaluate (fn [machine statement] [(:type machine) (:type statement)])))

;; (stest/instrument `machine/evaluate)

(defeval :dim
  [machine array-defs]
  (reduce (fn [machine array-def]
            (let [[[name dimensions]] (machine/emitted machine array-def)]
              (assoc-in machine [:env name] (new-array dimensions))))
          machine
          array-defs))
