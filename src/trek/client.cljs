(ns trek.client
  (:require [rum.core :as rum]
            [goog.dom :as dom]))

(rum/defc top
  []
  [:div "top"])

(rum/mount (top) (dom/getElement "app"))
