(ns cljs-dfa.interpreter.fa)

(defn- nnodes [n i]
  (swap! i #(+ n %))
  (vec (map (fn [a] [a]) (range (- @i (dec n)) (inc @i)))))

(defn- combine-delta [a b]
  (into [] (flatten (concat a b))))

(defn regex->nfa+epsilon
  ([q0 f tree i]
   (case (tree 0)
     :Character {#{q0 (tree 1)} f}
     :Concat (let [[q1] (nnodes 1 i)]
               (merge-with combine-delta
                           (regex->nfa+epsilon q0 q1 (tree 1) i)
                           (regex->nfa+epsilon q1 f (tree 2) i)))
     :Plus (merge-with combine-delta
                       (regex->nfa+epsilon q0 f (tree 1) i)
                       (regex->nfa+epsilon q0 f (tree 2) i))
     :Star (merge-with combine-delta
                       {#{q0 "ε"} f}
                       (regex->nfa+epsilon q0 q0 (tree 1) i))
     :Group (regex->nfa+epsilon q0 f (tree 1) i)))
  ([tree]
   (let [i (atom 0)]
     (regex->nfa+epsilon [0] [:accept] tree i))))

(def tree (cljs-dfa.interpreter.index/interpret "(a+b)*ab(a+b)*"))
(map println (regex->nfa+epsilon tree))