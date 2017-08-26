(ns trek.interpreter
  (:require [trek.machine :as machine]
            [trek.statement :refer [evaluate-statement]]
            [trek.emitter :refer [emit]]))

(defn interpreter []
  {:type    :interpreter
   :program nil
   :env     nil
   :ptr     nil
   :stack   nil})

;; (defn load-program [machine listing]
;;   (assoc machine :program (parser/generate-program emitter listing)))

(defn goto [machine n]
  (-> machine
      (assoc :goto n)))

(defn gosub [machine n]
  (-> machine
      (assoc :goto n)
      (update :stack conj n)))

(defn return [machine]
  (-> machine
      (assoc :ptr (peek (:stack machine)))
      (update :stack pop)))

;; -- EMITTER --

;; -- default

(defmacro defemit [statement-type args & body]
  `(let [statement-type# ~statement-type]
     (defmethod machine/emit [:interpreter statement-type#]
       ~args
       (assoc (do ~@body) :type statement-type#))))

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

(defemit :statement
  [machine _ line-number content]
  {:line-number line-number
   :content     content})

(defeval :statement
  [machine {:keys [content]}]
  (machine/evaluate machine content))

;; -- PRINT --

(defemit :print
  [machine _ args]
  {:type :print
   :args args})

(defemit :print
  [machine {:keys [args]}]
  (apply println (map #(machine/evaluate machine %) args))
  machine)

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
  (machine/gosub machine n))

;; -- RETURN --

(defemit :return
  [machine _]
  {:type :return})

(defeval :return
  [machine _]
  (machine/return machine))

;; -- IMAGE --

(defemit :image
  [machine _ args]
  {:type :image :args args})
