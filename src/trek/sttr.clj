(ns trek.sttr
  (:require [clojure.java.io :as io]
            [trek.grammar :as grammar]
            [trek.interpreter :as interpreter]
            [cognitect.transit :as transit])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]))

(defmacro txt
  []
  (slurp (io/resource "public/sttr1.txt")))

(defn spit-program
  []
  (with-open [out (io/output-stream (io/file "resources/public/sttr.transit+json"))]
    (let [parsed (grammar/parse (interpreter/interpreter) (slurp (io/resource "public/sttr1.txt")) :S)
          writer (transit/writer out :json)]
      (transit/write writer parsed))))

(defn load-program
  []
  (= (grammar/parse (interpreter/interpreter) (slurp (io/resource "sttr1.txt")) :S)
     (with-open [in (io/input-stream (io/file "resources/public/sttr.transit+json"))]
       (let [reader (transit/reader in :json)]
         (transit/read reader)))))
