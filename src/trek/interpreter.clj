(ns trek.interpreter
  (:require clj-time.core
            clj-time.local
            [clojure.core.async :as a]
            [trek.machine :as machine])
  (:import java.lang.Math))

(def ^:dynamic *line-number* nil)

(defn interpreter []
  {:type    :interpreter
   :program nil
   :env     nil
   :ptr     nil
   :stack   nil})

(defn last-value
  ([machine]
   (:last-value machine))
  ([machine v]
   (assoc machine :last-value v)))

(defn next-line [machine]
  (let [lines (get-in machine [:program :lines])]
    (->> (keys lines)
         (filter #(< (:ptr machine) %))
         sort
         first)))

(defmethod machine/evaluate :default
  [machine statement]
  (last-value machine statement))

(defmethod machine/emitted :default
  [machine statement]
  statement)

(defmethod machine/step :interpreter
  [machine]
  (let [{:keys [lines]} (:program machine)
        line            (get lines (:ptr machine))]
    (assert line (:ptr machine))
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
         (let [~args `[~machine# ~@(:args statement#)]
               result# (do ~@body)]
           (assert (= :interpreter (:type result#)))
           result#))))

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
  (apply println (map #(last-value (machine/evaluate machine %)) args))
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
  ;; TODO this is not used correctly
  (last-value machine args))

;; -- values --

(defeval :value
  [machine value]
  (machine/evaluate machine value))

;; -- for

(defeval :for
  [machine identifier lower upper]
  (let [lower-value (last-value (machine/evaluate machine lower))
        upper-value (last-value (machine/evaluate machine upper))
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
    (last-value machine value)))

;; -- binary operator

(defn apply-op [left op right]
  (case op
    "*" (* left right)
    "+" (+ left right)))

(defeval :binary-operator
  [machine left op right]
  (let [left  (last-value (machine/evaluate machine left))
        right (last-value (machine/evaluate machine right))]
    (last-value machine (apply-op left op right))))

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

(defn get-input [machine inputs]
  (go?
   (update machine :env assoc (first (machine/emitted machine (first inputs))) (read-line))))

(defeval :input
  [machine inputs]
  (async machine #(get-input % inputs)))

;; -- IF

(defeval :if
  [machine expression then]
  (if (last-value (machine/evaluate machine expression))
    (machine/evaluate machine then)
    machine))

;; -- bool op

(defeval :compare
  [machine a vs b]
  (let [a (last-value (machine/evaluate machine a))
        b (last-value (machine/evaluate machine b))]
    (last-value machine
                (case vs
                  ;;  #'<>' | '>' | '<=' | '<' | '=' | '>='"
                  "<>" (not= a b)
                  ">" (> a b)
                  "<=" (<= a b)
                  "<" (< a b)
                  "=" (= a b)
                  ">=" (>= a b)))))

(defeval :bool-op
  [machine a vs b]
  (let [a (last-value (machine/evaluate machine a))
        b (last-value (machine/evaluate machine b))])
  (throw (ex-info "Not implemented" {:args [a vs b]})))



(defeval :assign
  [machine id value]
  (let [key   (first (machine/emitted machine id))
        m2    (machine/evaluate machine value)
        value (last-value m2)]
    (-> m2
        (assoc-in [:env key] value)
        (last-value value))))

(defeval :array-ref
  [machine identifier dimensions]
  (let [value (get-in machine `[:env identifier ~@dimensions])]
    (assert value)
    (last-value machine value)))

(defn new-array
  [value dimensions]
  (if (seq dimensions)
    (into [] (for [i (range (first dimensions))]
               (new-array value (rest dimensions))))
    value))

(defn value-type-of [name]
  (case (last name)
    \$ ""
    0))

(defeval :dim
  [machine array-defs]
  (reduce (fn [machine array-def]
            (let [[name dimensions] (machine/emitted machine array-def)
                  name              (first (machine/emitted machine name))
                  dimensions        (mapv #(first (machine/emitted machine %)) dimensions)]
              (assoc-in machine [:env name] (new-array (value-type-of name) dimensions))))
          machine
          array-defs))

;; -- Calls

(defn eval-fn
  [machine fn-name arg]
  (let [f (get-in machine [:env fn-name])]
    (assert f)
    (f machine (last-value (machine/evaluate machine arg)))))

(defeval :call
  [machine fn-name arg]
  (let [arg (last-value (machine/evaluate machine arg))]
    (case fn-name
      "INT" (last-value machine (Math/floor arg))
      "RND" (last-value machine (* (Math/random) arg))
      "TIM" (last-value machine (case arg
                                  0 (clj-time.core/minute (clj-time.local/local-now))
                                  1 (clj-time.core/hour (clj-time.local/local-now))))
      (eval-fn machine fn-name arg))))

;; -- def

(defeval :def
  [machine fn-name fn-arg expression]
  (assoc-in machine [:env (first (machine/emitted machine fn-name))]
            (fn [machine arg]
              (machine/evaluate (assoc-in machine [:env fn-arg] arg)
                                expression))))

(defeval :zero-array
  [machine identifier]
  (let [array-name (first (machine/emitted machine identifier))]
    (update-in machine [:env array-name] #(do
                                            (if %
                                              (mapv (constantly (value-type-of array-name)) %)
                                              (new-array (value-type-of array-name) [0]))))))
