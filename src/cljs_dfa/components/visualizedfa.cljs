(ns cljs-dfa.components.visualizedfa
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [reagent.dom :as r]
   [reagent.core :as rc]
   [cljs-dfa.interpreter.index :refer [interpret]]
   [cljs-dfa.interpreter.fa :refer [regex->nfa+epsilon]]
   [cljs-dfa.db :refer [regex]]))

(defrecord Node [x y oldx oldy number])
(defrecord Arrow [fromNode toNode label])


(defn- draw [state])

(defn- build-state [delta]
  (map println delta))

(defn init []
  {:nodes [] :arrows []})



(defn- update-state [state])

(defn canvas []
  (rc/create-class
   {:component-did-mount
    (fn [component]
      (let [node (r/dom-node component)
            width (.-clientWidth node)
            height (.-clientHeight node)]
        (q/sketch
         :host node
         :draw draw
         :setup init
         :update update-state
         :size [width height]
         :middleware [m/fun-mode])))
    :render
    (fn [] [:div.with-canvas])}))


