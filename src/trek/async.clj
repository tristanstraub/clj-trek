(ns trek.async
  (:require [clojure.core.async :as a]))

(defmacro go? [& body]
  `(a/go (try ~@body
              (catch Throwable t#
                t#))))

(defmacro <? [& args]
  `(let [value# (a/<! ~@args)]
     (when (instance? Throwable value#)
       (throw value#))
     value#))

(defmacro <?? [& args]
  `(let [value# (a/<!! ~@args)]
     (when (instance? Throwable value#)
       (throw value#))
     value#))
