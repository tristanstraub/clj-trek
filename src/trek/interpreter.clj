(ns trek.interpreter
  (:require [clojure.core.async :as a]
            [clj-time.core :as time.core]
            [clj-time.local :as time.local]
            [clojure.string :as str]
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

(defn goto-of [machine expression line-numbers]
  (let [i (last-value (machine/evaluate machine expression))]
    (-> machine
        (assoc :goto (nth line-numbers (dec i))))))

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

(defeval :goto-of
  [machine expression line-numbers]
  (goto-of machine expression line-numbers))

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
    "+" (+ left right)
    "-" (- left right)))

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

(defn variable-type [name]
  (case (last name)
    \$ :string
    :number))

(defn convert-input [id value]
  (case (variable-type id)
    :string value
    :number (Integer/parseInt value)
    (throw (ex-info "Not an integer" {:value value :id id}))))

(defn get-input [machine inputs]
  (go?
   (let [id (first (machine/emitted machine (first inputs)))]
     (assoc-in machine [:env id] (convert-input id (read-line))))))

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
  [machine a op b]
  (let [a (last-value (machine/evaluate machine a))
        b (last-value (machine/evaluate machine b))]
    (case op
      :or (last-value machine (or a b)))))



(defn value-type-of [name]
  (case (last name)
    \$ ""
    0))

(defn grow-array-to-index
  [array index empty-value]
  (let [grown (cond-> array
                (not (vector? array))
                vec)
        grown (cond-> grown
                (and grown (<= (count grown) index))
                (into (repeat (inc (- index (count grown))) empty-value)))]
    grown))

(defn assign-array
  [array dimensions value empty-value]
  (if (seq (rest dimensions))
    (let [grown (grow-array-to-index array (first dimensions) [])]
      (update grown (first dimensions)
              #(assign-array %
                             (rest dimensions)
                             value
                             empty-value)))
    (let [grown (grow-array-to-index array (first dimensions) empty-value)]
      (assoc grown (first dimensions) value))))

(defn read-array
  [array dimensions empty-value]
  (if (seq (rest dimensions))
    (let [grown (grow-array-to-index array (first dimensions) [])]
      (read-array (get grown (first dimensions))
                  (rest dimensions)
                  empty-value))
    (let [grown (grow-array-to-index array (first dimensions) empty-value)]
      (get grown (first dimensions)))))

(defn grow-string-to-index [base index]
  (str base (apply str (repeat (- (inc index) (count base))
                               " "))))

(defn read-from-string
  ;; TODO check if second dimension in string is inclusive
  [array dimensions]
  (let [grown (grow-string-to-index array (inc (second dimensions)))]
    (subs grown (first dimensions) (inc (second dimensions)))))

(defn assign
  [machine name dimensions value]
  (cond-> machine
    ;; is array[not String]
    (and (seq dimensions) (not= :string (variable-type name)))
    (update-in [:env name] assign-array dimensions value (value-type-of name))

    ;; is array[String]
    (and (seq dimensions) (= :string (variable-type name)))
    (assoc-in [:env name]
              (let [s-value (grow-string-to-index (get-in machine `[:env ~name])
                                                  (+ (first dimensions)
                                                     (count value)))]
                (str (subs s-value 0 (first dimensions))
                     value
                     (subs s-value (+ (first dimensions)
                                      (count value))))))

    ;; is not array
    (not (seq dimensions))
    (assoc-in [:env name] value)

    true
    (last-value value)))

(defn identifier? [id]
  (= (:type id) :identifier))

(defn array-ref? [id]
  (= (:type id) :array-ref))

(defn eval-array-ref
  [machine id]
  (let [[name dimensions] (machine/emitted machine id)]
    [(first (machine/emitted machine name))
     (mapv #(dec (int (last-value (machine/evaluate machine %)))) dimensions)]))

(defeval :assign
  [machine id value]
  (assert (or (identifier? id)
              (array-ref? id)))
  (let [[name dimensions] (cond (identifier? id)
                                (machine/emitted machine id)

                                (array-ref? id)
                                (eval-array-ref machine id)

                                :else
                                (assert false))

        m2                (machine/evaluate machine value)
        value             (last-value m2)]

    (assert (string? name))
    (assign m2 name dimensions value)))

(defeval :array-ref
  [machine identifier dimensions]
  (let [[identifier] (cond (identifier? identifier)
                           (machine/emitted machine identifier)

                           (array-ref? identifier)
                           (eval-array-ref machine identifier)

                           :else
                           (assert false))
        value      (case (variable-type identifier)
                     :number (read-array (get-in machine [:env identifier])
                                         (mapv #(dec (int (last-value (machine/evaluate machine %)))) dimensions)
                                         (value-type-of identifier))
                     :string (read-from-string (get-in machine [:env identifier])
                                          (mapv #(dec (int (last-value (machine/evaluate machine %)))) dimensions))
                     (throw (ex-data "Unrecognized array ref" {:identifier identifier :dimensions dimensions})))]
    (assert value)
    (last-value machine value)))

(defn new-array
  [value dimensions]
  (if (seq dimensions)
    (into [] (for [i (range (first dimensions))]
               (new-array value (rest dimensions))))
    value))



(defeval :dim
  [machine array-defs]
  (reduce (fn [machine array-def]
            (let [[name dimensions] (machine/emitted machine array-def)
                  name              (first (machine/emitted machine name))
                  dimensions        (mapv #(first (machine/emitted machine %)) dimensions)]
              (assoc-in machine [:env name] (case (variable-type name)
                                              :string (do
                                                        (assert (= 1 (count dimensions)))
                                                        (apply str (repeat (first dimensions) " ")))
                                              :number (new-array (value-type-of name) dimensions)
                                              (throw (Exception. "Unknown array type"))))))
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
                                  0 (time.core/minute (time.local/local-now))
                                  1 (time.core/hour (time.local/local-now))))
      (eval-fn machine fn-name arg))))

;; -- def

(defeval :def
  [machine fn-name fn-arg expression]
  (assoc-in machine [:env (first (machine/emitted machine fn-name))]
            (fn [machine arg]
              (machine/evaluate (assoc-in machine [:env fn-arg] arg)
                                expression))))

(defn zero-array
  [array empty-value]
  (cond (empty? array)          (or array [])
        (vector? (first array)) (mapv #(zero-array % empty-value) array)
        :else                   (vec (repeat (count array) empty-value))))

(defeval :zero-array
  [machine identifier]
  (let [array-name (first (machine/emitted machine identifier))]
    (update-in machine [:env array-name] #(zero-array % (value-type-of array-name)))))

(defeval :print-using
  [machine [line-number expressions]]
  (let [image   (get-in machine [:program :lines line-number])
        values  (mapv #(last-value (machine/evaluate machine %)) expressions)
        machine (machine/format (assoc machine :unformatted (into [] (reverse values))) image)]
    (println (last-value machine))
    (assert (not (seq (:unformatted machine))))
    machine))

(defeval :format-type [type])
(defeval :formatter [number format-list])
(defeval :formatter-list [number format-list])
(defeval :format-repeat [number format-list])

(defmethod machine/format [:interpreter :value]
  [machine formatter]
  (last-value machine (first (machine/emitted machine formatter))))

(defmethod machine/format [:interpreter :format-type]
  [machine formatter]
  (let [[format-type] (machine/emitted machine formatter)

        format-count  (if (< 1 (count format-type))
                        (Integer/parseInt (apply str (butlast format-type)))
                        1)
        format-type   (last (str/lower-case format-type))

        value         (-> machine :unformatted peek)

        machine       (case format-type
                        \x (last-value machine (apply str (repeat format-count (str " "))))

                        \a (last-value (update machine :unformatted pop)
                                       (format (str "%" format-count "s") value))

                        \d (last-value (update machine :unformatted pop)
                                       (format (str "%" format-count "d") (int value))))]
    machine))

(defn format-list [machine formatter]
  (let [[machine output]
        (reduce (fn [[machine output] formatter]
                  (let [machine (machine/format machine formatter)]
                    [machine (conj output (last-value machine))]))
                [machine []]
                (machine/emitted machine formatter))]
    (last-value machine (apply str output))))

(defmethod machine/format [:interpreter :formatter-list]
  [machine formatter]
  (format-list machine formatter))

(defmethod machine/format [:interpreter :format-repeat]
  [machine formatter]
  (let [[n body]  (machine/emitted machine formatter)
        formatter (machine/emitted machine body)]
    (let [[machine output] (reduce (fn [[machine output] i]
                                     (let [machine (format-list machine formatter)]
                                       [machine (conj output (last-value machine))]))
                                   [machine []]
                                   (range n))]
      (last-value machine (apply str output)))))
