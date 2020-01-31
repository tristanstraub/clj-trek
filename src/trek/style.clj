(ns trek.style
  (:require [garden-mower.core]
            [garden.core]
            [garden.selectors]
            [clojure.java.io :as io]))

(defonce tailwind-css
  (delay (garden-mower.core/parse (slurp (io/resource "public/tailwind-utilities.css")))))

(defn attributes
  [& selectors]
  (apply garden-mower.core/attributes @tailwind-css selectors))

(garden.selectors/defselector pre)

(defmacro css
  []
  (garden.core/css
   {:pretty-print? false}
   [[:body (attributes :.bg-gray-700)]
    [:.container (attributes :.mx-auto :.bg-green-400 :.mt-2)]
    [:input (attributes :.border :.border-blue-900 :.border-3 :.bg-gray-400 :.mx-2 :.p-1 :.rounded)]
    [:.terminal
     (attributes :.p-2 :.border :.border-4 :.border-white :.mt-12 :.overflow-x-hidden)
     [(pre (garden.selectors/nth-child "2n")) (attributes :.bg-green-500)]]
    [:.tab-button (attributes :.mr-2 :.mt-1 :.text-lg)]]))
