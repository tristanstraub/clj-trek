(ns trek.async-cljs)

(defmacro go? [& body]
  `(cljs.core.async/go
     (try ~@body
          (catch js/Error t#
            (.warn js/console t#)
            t#))))

(defmacro <? [& args]
  `(let [value# (cljs.core.async/<! ~@args)]
     (when (instance? js/Error value#)
       (throw value#))
     value#))
