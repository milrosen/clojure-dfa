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
(def ATTRACT-SCALE 1)
;; prevents two nodes from generating infinite force when too close
(def DISTANCE-CLAMPING 0.00)

(defn- dist
  ([x1 y1 x2 y2]
   (let [dx (- x1 x2)
         dy (- y1 y2)]
     (Math/sqrt (+ (* dx dx) (* dy dy)))))
  ([[x1 y1] [x2 y2]]
   (dist x1 y1 x2 y2)))

(defn Node [[x y]] [x y 0 0])
(defrecord Arrow [from to character])

(defn- points-in-circle [n]
  (map (fn [k]
         (let [tk (/ (* 2 3.1419 k) n)]
           [(/ (Math/cos tk) 2) (/ (Math/sin tk) 2)]))
       (range 0 n)))

(defn build-state [delta]
  (if (seq? delta)
    {:arrows (map #(Arrow. (% :from) (% :to) (% :character)) delta)
     :nodes (->> delta
                 (reduce #(conj %1 (%2 :from) (%2 :to)) #{})
                 (count)
                 (points-in-circle)
                 (map Node))
     :labels [[:start 0] [:accept 1]] }
    (build-state (seq [delta]))
    ))

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

(defn- repelling-force [nodes]
    (map
     (fn [node]
       (->> (repeat node)
            (map
             #(divide-point (normalize-diff %2 %1) (floor (dist-square %1 %2) DISTANCE-CLAMPING))
             (remove #(equals-points node %) nodes))
            (reduce sum-points)))
     nodes))

(defn- tprn [x]
  (println x)
  x)

(defn- attraction-force [nodes arrows]
  ;; (do (println nodes arrows))
  (->> arrows
    (map #( let [n1 (:from %) 
                 n2 (:to %) 
                 dir (diff (nth nodes n1) (nth nodes n2))]
              (-> (apply vector (take (count nodes) (repeat [0 0])))
                (assoc n2 dir)
                (assoc n1 (multiply-point dir -1.0)))))
    (reduce #(map (partial apply sum-points) (map vector %1 %2)) ,,,)))

(defn- apply-force [node force]
  (let [[fx fy] (divide-point force DAMPEN-FORCE)
        [x y vx vy] node]
    [(+ x (* vx)) (+ y vy) (* DAMPEN-VEL (+ fx vx)) (* DAMPEN-VEL (+ fy vy))]))

(defn- update-state [state]
  (let [nodes (:nodes state)
        arrows (:arrows state)
        forces (map sum-points
                    (map #(multiply-point % REPEL-SCALE) (repelling-force nodes))
                    (map #(multiply-point % ATTRACT-SCALE) (attraction-force nodes arrows))
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

(defn- mouse-pressed [state event]
  ;; (print (apply min-key (partial dist [(q/mouse-x) (q/mouse-y)]) (map first (:nodes state))))
  state)

(defn get-state []
  (q/state-atom))

(defn- scale [x]
  (* (/ (min (q/height) (q/width)) 2) x))

(defn- draw-node [[x y _ _]]
  (let [x (scale x)
        y (scale y)]
    (q/fill 255)
    (q/ellipse x y 40 40)
    (q/fill 0 0 0)
    (q/ellipse x y 35 35)))

(defn- draw-label [nodes [label n]]
  (let [[x y] (map scale (nth nodes n))]
   (case label
    :accept
      (do (q/fill 0 100 0)
      (q/ellipse x y 20 20))
    :start 
      (do (q/fill 100 0 0)
      (q/ellipse x y 20 20)))))
    

(defn- draw-arrow [arrow nodes]
  ;; (do (println arrow nodes))
  (if (= (:from arrow) (:to arrow))
    ()
  (let [[x1 y1] (map scale (nth nodes (:from arrow)))
        [x2 y2] (map scale (nth nodes (:to arrow)))]
    (q/stroke 255)
    (q/line x1 y1 x2 y2)
    (q/text (:character arrow) (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))
    (q/stroke 0))))

(defn- draw [state]
  (q/background 0 15 28)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (do
    (doseq [nodes (:nodes state)]
      (draw-node nodes))

    (doseq [labels (:labels state)]
      (draw-label (:nodes state) labels))

    (doseq [arrow (:arrows state)]
      (draw-arrow arrow (:nodes state)))))

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
         :mouse-pressed mouse-pressed
         :size [width height]
         :middleware [m/fun-mode])))
    :render
    (fn [] [:div.with-canvas])}))