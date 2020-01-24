(ns trek.async)

(defmacro go? [& body]
  `(clojure.core.async/go
     (try ~@body
          (catch #?(:clj Throwable :cljs js/Error) t#
            t#))))

(defmacro <? [& args]
  `(let [value# (clojure.core.async/<! ~@args)]
     (when (instance? #?(:clj Throwable :cljs js/Error) value#)
       (throw value#))
     value#))

#?(:clj
   (defmacro <?? [& args]
     `(let [value# (clojure.core.async/<!! ~@args)]
        (when (instance? #?(:clj Throwable :cljs js/Error) value#)
          (throw value#))
        value#)))
