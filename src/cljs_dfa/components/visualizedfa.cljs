(ns cljs-dfa.components.visualizedfa
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [reagent.dom :as r]
   [reagent.core :as rc]
   [cljs-dfa.interpreter.index :refer [interpret]]
   [cljs-dfa.interpreter.fa :refer [regex->nfa+epsilon]]
   [cljs-dfa.db :refer [regex]]))

(defrecord Node [x y oldx oldy label])
(defrecord Arrow [fromNode toNode label])

(defn- draw [state])

(defn- points-in-circle [n]
  (map (fn [k]
         (let [tk (/ (* 2 3.1419 k) n)]
           [(Math/cos tk) (Math/sin tk)]))
       (range 0 n)))

(defn- nodes-from-labels [node-labels]
  (map (fn [thingy]
         (let [point (first thingy)
               label (last thingy)]
           (Node. (point 0) (point 1) (point 0) (point 1) label)))
       (map list (points-in-circle (count node-labels)) node-labels)))

(defn build-state [delta]
  (if (seq? delta)
    {:arrows (map #(Arrow. (% :from) (% :to) (% :character)) delta)
     :nodes (->> delta
                 (reduce #(conj %1 (%2 :from) (%2 :to)) #{})
                 (map #(% 0))
                 nodes-from-labels)}
    (build-state (seq [delta]))))

(def delta (-> "" interpret regex->nfa+epsilon))
(build-state delta)

(defn init []
  {:nodes [] :arrows []})

(defn- key-pressed [_ event]
  (if (= (event :key) :Enter)
    (build-state (-> @regex interpret regex->nfa+epsilon))))

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
         :key-pressed key-pressed
         :draw draw
         :setup init
         :update update-state
         :size [width height]
         :middleware [m/fun-mode])))
    :render
    (fn [] [:div.with-canvas])}))


