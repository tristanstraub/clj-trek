(ns trek.interpreter
  (:require [trek.machine :as machine]))

(defn interpreter []
  {:type    :interpreter
   :program nil
   :env     nil
   :ptr     nil
   :stack   nil
   :output  []})

(defmethod machine/step :interpreter
  [machine]
  {:pre [(-> machine :program :lines)]}
  (let [lines (get-in machine [:program :lines])]
    (-> machine
        (machine/evaluate (get lines (:ptr machine)))
        (assoc :ptr (->> (keys lines)
                         (filter #(< (:ptr machine) %))
                         sort
                         first)))))
        ;; (cond->
        ;;     (:ptr machine) (cond->
        ;;                        (:goto machine) (assoc :ptr (:goto machine))
        ;;                        (not (:goto machine))
        ;;                        )
        ;;     ;; (and (:ptr machine)
        ;;     ;;      (get (:program machine) (:ptr machine)))


        ;; ;;     ;; true
        ;; ;;     ;; (as-> machine
        ;; ;;     ;;     (println :machine machine)
        ;; ;;     ;;   machine)
        ;; ;;     )
        ;; )

(defn goto [machine n]
  (-> machine
      (assoc :goto n)))

(defn gosub [machine n]
  (-> machine
      (assoc :goto n)
      (update :stack conj n)))

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
            (apply conj coll (map #(machine/evaluate machine %) args)))))

;; -- GOTO --

(defemit :goto
  [machine _ line-number]
  {:type        :goto
   :line-number line-number})

(defeval :goto
  [machine {:keys [line-number]}]
  (goto line-number))

;; -- GOSUB --

(defemit :gosub
  [machine _ line-number]
  {:type        :gosub
   :line-number line-number})

(defeval :gosub
  [machine {:keys [n] :as gosub}]
  (gosub machine n))

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
