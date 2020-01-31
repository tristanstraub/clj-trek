(ns trek.async)

(defmacro go? [& body]
  `(clojure.core.async/go
     (try ~@body
          (catch Throwable t#
            t#))))

(defmacro <? [& args]
  `(let [value# (clojure.core.async/<! ~@args)]
     (when (instance? Throwable value#)
       (throw value#))
     value#))

(defmacro <?? [& args]
  `(let [value# (clojure.core.async/<!! ~@args)]
     (when (instance? Throwable value#)
       (throw value#))
     value#))
