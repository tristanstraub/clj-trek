(ns trek.client
  (:require-macros [cljs.core.async]
                   [trek.style])
  (:require [rum.core :as rum]
            [goog.dom :as dom]
            [trek.core]
            trek.interpreter
            [trek.async-cljs :as async]
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
  []
  [:div.terminal
   (for [[line-number line] (map vector (range) (str/split trek.core/source-code #"\n"))]
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
      :source-code (source-code)
      :terminal    (terminal trek.interpreter/terminal))]])

(defonce state
  (atom {:page    :terminal
         :message "waiting"}))

(defonce animation
  (js/requestAnimationFrame
   (fn []
     (async/go?
      (let [messages (trek.core/run)]
        (loop []

          (try (when-let [message (async/<? messages)]
                 (println :message message)
                 (js/requestAnimationFrame (fn []
                                             (swap! state assoc :message (str message)))))
               (catch js/Error e
                 (.error js/console e)))

          (recur)))))))

(rum/mount (top state) (dom/getElement "app"))
