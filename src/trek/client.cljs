(ns trek.client
  (:require-macros [cljs.core.async])
  (:require [rum.core :as rum]
            [goog.dom :as dom]
            [trek.core]
            trek.interpreter
            [trek.async-cljs :as async]
            [cljs.core.async :as a]))

(defonce input
  (atom nil))

(rum/defc terminal
  < rum/reactive
  {:after-render (fn [state]
                   (let [comp                 (:rum/react-component state)
                         dom-node             (js/ReactDOM.findDOMNode comp)]

                     (.log js/console dom-node)

                     (set! (.-scrollTop dom-node) (.-scrollHeight dom-node))

                     state))}
  [terminal]
  [:pre {:style {:height "800px" :overflow "scroll" :font-size "34px"}}
   (for [line (rum/react terminal)]
     (str line "\n"))])

(rum/defc top
  < rum/reactive
  [state]
  [:div "top"
   [:div (str (rum/react (rum/cursor-in state [:message])))]
   [:input {:on-change (fn [e]
                         (.preventDefault e)
                         (reset! input (.. e -target -value)))}]
   [:button {:on-click (fn [e]
                         (.preventDefault e)
                         (trek.interpreter/send-input (or @input "")))}
    "Send"]
   (terminal trek.interpreter/terminal)])

(defonce state
  (atom {:message "waiting"}))

(js/requestAnimationFrame
 (fn []
   (async/go?
    (let [messages (trek.core/run)]
      (loop []
        (when-let [message (async/<? messages)]
          (println :message message)
          (js/requestAnimationFrame (fn []
                                      (swap! state assoc :message (str message))))
          (recur)))))))

(rum/mount (top state) (dom/getElement "app"))
