(ns trek.client
  (:require-macros [cljs.core.async]
                   [trek.style]
                   [trek.async-cljs :as async])
  (:require [rum.core :as rum]
            [goog.dom :as dom]
            [trek.core]
            trek.interpreter
            [cljs.core.async :as a]
            [clojure.string :as str]))

(defonce input
  (atom nil))

(rum/defc terminal
  < rum/reactive
  {:after-render (fn [state]
                   (.scrollTo js/window 0 (.-scrollHeight js/document.body))
                   state)}
  [terminal]
  [:div.terminal
   (for [[line-number line] (map vector (range) (rum/react terminal))]
     [:pre {:key line-number}
      (str line "\n")])
   [:input {:value        (or (rum/react input) "")
            :on-change    (fn [e]
                            (.preventDefault e)
                            (reset! input (.. e -target -value)))
            :on-key-press (fn [e]
                            (when (= (.. e -key) "Enter")
                              (trek.interpreter/send-input (or @input ""))
                              (reset! input "")))}]])

(def style
  (trek.style/css))

(rum/defc source-code
  [txt]
  [:div.terminal
   (for [[line-number line] (map vector (range) (str/split txt #"\n"))]
     [:pre {:key line-number} (str line "\n")])])

(rum/defc nav
  [page]
  [:div.fixed.top-0.left-0.h-10.border-blue-600.border.bg-blue-500.w-full.shadow-xl.border-b-2.flex
   [:ul.flex.mx-auto.my-1.text-gray-900
    [:li.mr-6 [:a.hover:text-blue-700.cursor-pointer
               {:on-click (fn [e]
                            (.preventDefault e)
                            (reset! page :terminal))
                :class    (when (= @page :terminal)
                            "text-white")}
               "Terminal"]]
    [:li.mr-6 "|"]
    [:li [:a.hover:text-blue-700.cursor-pointer
          {:on-click (fn [e]
                       (.preventDefault e)
                       (reset! page :source-code))
           :class    (when (= @page :source-code)
                       "text-white")}
          "Source code"]]]])

(rum/defc top
  < rum/reactive
  [state]
  [:span
   [:style style]
   (nav (rum/cursor-in state [:page]))
   [:div.container
    (case (rum/react (rum/cursor-in state [:page]))
      :source-code (source-code (rum/react (rum/cursor-in state [:txt])))
      :terminal    (terminal trek.interpreter/terminal))]])

(defonce state
  (atom {:page    :terminal
         :message "waiting"
         :txt     ""}))

(defn main
  []
  (a/go
    (try
      (let [code (async/<? (trek.core/fetch-program))]
        (swap! state assoc :txt (:txt code))
        (async/<? (trek.core/run code)))
      (catch js/Error e
        (.error js/console e))))

  (rum/mount (top state) (dom/getElement "app")))

(main)
