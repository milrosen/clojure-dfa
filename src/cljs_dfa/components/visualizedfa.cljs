(ns cljs-dfa.components.visualizedfa
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [reagent.dom :as r]
   [reagent.core :as rc]
   [cljs-dfa.interpreter.index :refer [interpret]]
   [cljs-dfa.interpreter.fa :refer [regex->nfa+epsilon]]
   [cljs-dfa.db :refer [regex]]))

(def DAMPEN-FORCE 100) ;; heigher => more damp
(def DAMPEN-VEL 0.7)   ;; lower => more damp
(def REPEL-SCALE 0.1)
(def ATTRACT-SCALE 1.5)
;; prevents two nodes from generating infinite force when too close
(def DISTANCE-CLAMPING 0.00)

(defn- dist
  ([x1 y1 x2 y2]
   (let [dx (- x1 x2)
         dy (- y1 y2)]
     (Math/sqrt (+ (* dx dx) (* dy dy)))))
  ([[x1 y1] [x2 y2]]
   (dist x1 y1 x2 y2)))

(defn Node [x y label]
  {label [x y 0 0]})
(defrecord Arrow [from to label])

(defn- points-in-circle [n]
  (map (fn [k]
         (let [tk (/ (* 2 3.1419 k) n)]
           [(/ (Math/cos tk) 2) (/ (Math/sin tk) 2)]))
       (range 0 n)))

(defn- nodes-from-labels [node-labels]
  (map (fn [thingy]
         (let [point (first thingy)
               label (last thingy)]
           (Node (point 0) (point 1) label)))
       (map list (points-in-circle (count node-labels)) node-labels)))

(defn build-state [delta]
  (if (seq? delta)
    {:arrows (map #(Arrow. (% :from) (% :to) (% :character)) delta)
     :nodes (->> delta
                 (reduce #(conj %1 (%2 :from) (%2 :to)) #{})
                 (map #(% 0))
                 nodes-from-labels)
     :middles ()}
    (build-state (seq [delta]))))

(defn- normalize [x y]
  [(/ x (dist x y 0 0)) (/ y (dist x y 0 0))])

(defn- diff [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(defn- normalize-diff [[x1 y1] [x2 y2]]
  (normalize (- x1 x2) (- y1 y2)))

(defn- divide-point [[x y] s]
  [(/ x s) (/ y s)])

(defn- sum-points [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn- equals-points [[x1 y1] [x2 y2]]
  (and (= x1 x2) (= y1 y2)))

(defn- multiply-point [[x y] s]
  [(* x s) (* y s)])

(defn floor [x n]
  (if (> x n) x n))

(defn dist-square [[x1 y1] [x2 y2]]
  (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2))))

(declare draw-arrow)

(defn- repelling-force [nodes]
  (let [points (vals (reduce merge nodes))
        count (count points)]
    (map
     (fn [point]
       (->> (repeat point)
            (map
             #(divide-point (normalize-diff %2 %1) (floor (dist-square %1 %2) DISTANCE-CLAMPING))
             (remove #(equals-points point %) points))
            (reduce sum-points)))
     points)))

(defn- attraction-force [nodes arrows]
  (let [forces (reduce #(merge-with sum-points %1 %2)
                       (map (fn [arrow]
                              (let [nodes (reduce merge nodes)
                                    p1 (nodes ((:from arrow) 0))
                                    p2 (nodes ((:to arrow) 0))]
                                {((:from arrow) 0) (diff p2 p1) ((:to arrow) 0) (diff p1 p2)})) arrows))]
    (map #(get forces (first (keys %))) nodes)))

(defn- apply-force [node force]
  (let [[fx fy] (divide-point force DAMPEN-FORCE)
        [x y vx vy] (first (vals node))
        label (first (keys node))]
    {label [(+ x (* vx)) (+ y vy) (* DAMPEN-VEL (+ fx vx)) (* DAMPEN-VEL (+ fy vy))]}))

(defn- update-state [state]
  (let [nodes (:nodes state)
        arrows (:arrows state)
        forces (map sum-points
                    (repeat [0 0])
                    (map #(multiply-point % REPEL-SCALE) (repelling-force nodes))
                    ;;(map #(multiply-point % ATTRACT-SCALE) (attraction-force nodes arrows))
                    )]
    (assoc state :nodes (map apply-force nodes forces))))



(defn setup
  "Initialize the quil framework."
  []
  (q/frame-rate 25)
  (build-state (-> @regex interpret regex->nfa+epsilon)))

(defn- key-pressed [state event]
  (if (= (event :key) :Enter)
    (build-state (-> @regex interpret regex->nfa+epsilon))
    state))

(defn- scale [x]
  (* (/ (min (q/height) (q/width)) 2) x))

(defn- draw-node [node]
  (let [point (first (vals node))
        x (scale (point 0))
        y (scale (point 1))]
    (q/fill 255)
    (q/ellipse x y 40 40)
    (q/fill 0 0 0)
    (q/ellipse x y 35 35)
    (when (= (first (keys node)) :accept)
      (q/fill 255)
      (q/ellipse x y 8 8))))

(defn- draw-arrow [[x1 y1] [x2 y2] label]
  (q/stroke 255)
  ;; (q/text label (* 1.3 (scale (/ (+ x1 x2) 2.0))) (* 1.3 (scale (/ (+ y1 y2) 2.0))))
  (q/line (scale x1) (scale y1) (scale x2) (scale y2)))

(defn- draw [state]
  (q/background 0 15 28)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (do
    (doseq [nodes (:nodes state)]
      (draw-node nodes))

    (doseq [arrow (:arrows state)]
      (let [nodes (reduce merge (:nodes state))
            p1 (nodes ((:from arrow) 0))
            p2 (nodes ((:to arrow) 0))]
        (draw-arrow p1 p2 (:label arrow))))))

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
         :setup setup
         :update update-state
         :size [width height]
         :middleware [m/fun-mode])))
    :render
    (fn [] [:div.with-canvas])}))