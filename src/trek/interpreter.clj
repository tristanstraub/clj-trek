(ns trek.interpreter
  (:require [trek.machine :as machine]))

(defn interpreter []
  {:type    :interpreter
   :program nil
   :env     nil
   :ptr     nil
   :stack   nil
   :output  []})

(defn next-line [machine]
  (let [lines (get-in machine [:program :lines])]
    (->> (keys lines)
         (filter #(< (:ptr machine) %))
         sort
         first)))

(defmethod machine/step :interpreter
  [machine]
  {:pre [(-> machine :program :lines)]}
  (let [lines (get-in machine [:program :lines])]
    (as-> (machine/evaluate machine (get lines (:ptr machine)))
        machine
        (cond-> machine
            (:goto machine)
            (-> (assoc :ptr (:goto machine))
                (dissoc :goto))

            (not (:goto machine))
            (assoc :ptr (next-line machine))))))

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
      (update :stack pop)))

(defmethod machine/load [:interpreter :program]
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
     (let [~args `[~machine# ~statement-type# ~@args#]]
       (assoc (do ~@body) :type ~statement-type))))

(defmacro defeval [statement-type args & body]
  `(defmethod machine/evaluate [:interpreter ~statement-type]
     ~args
     ~@body))

(defemit :default
  [& _]
  (throw (Exception. "Unexecutable!")))

(defeval :default
  [& _]
  (throw (Exception. "Unexecutable!")))

;; -- statement --

(defemit :program
  [machine _ statements]
  {:lines (reduce (fn [program {:keys [line-number content]}]
                    (assoc program line-number content))
                  {}
                  statements)})

(defemit :statement
  [machine _ line-number content]
  {:line-number line-number
   :content     content})

(defeval :statement
  [machine {:keys [content] :as s}]
  (when content
    (machine/evaluate machine content)))

;; -- PRINT --

(defemit :print
  [machine _ args]
  {:type :print
   :args args})

(defeval :print
  [machine {:keys [args]}]

  (update machine :output
          (fn [coll]
            (-> (apply conj coll (map #(machine/evaluate machine %) args))
                (conj :newline)))))

;; -- GOTO --

(defemit :goto
  [machine _ line-number]
  {:type        :goto
   :line-number line-number})

(defeval :goto
  [machine {:keys [line-number]}]
  (goto machine line-number))

;; -- GOSUB --

(defemit :gosub
  [machine _ line-number]
  {:type        :gosub
   :line-number line-number})

(defeval :gosub
  [machine {:keys [line-number]}]
  (gosub machine line-number))

;; -- RETURN --

(defemit :return
  [machine _ _]
  {:type :return})

(defeval :return
  [machine _]
  (return machine))

;; -- IMAGE --

(defemit :image
  [machine _ args]
  {:type :image :args args})

;; -- values --

(defemit :value
  [machine _ value]
  {:type :value
   :value value})

(defeval :value
  [machine {:keys [value]}]
  value)
