(ns trek.client
  (:require [rum.core :as rum]
            [goog.dom :as dom]
            [trek.core]
            trek.interpreter))

(defonce input
  (atom nil))

(rum/defc top
  []
  [:div "top"
   [:input {:on-change (fn [e]
                         (.preventDefault e)
                         (reset! input (.. e -target -value)))}]
   [:button {:on-click (fn [e]
                         (.preventDefault e)
                         (trek.interpreter/send-input (or @input "")))}
    "Send"]])

(rum/mount (top) (dom/getElement "app"))

(trek.core/-main)
