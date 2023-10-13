(ns cljs-dfa.interpreter.fa)

(defn- nnodes [n i]
  (swap! i #(+ n %))
  (vec (map (fn [a] [a]) (range (- @i (dec n)) (inc @i)))))

(defn- combine-delta [a b]
  (flatten (vector a b)))

(defn regex->nfa+epsilon
  ([q0 f tree i]
   (case (tree 0)
     :Character {:from q0 :character (tree 1) :to f}
     :Concat (let [[q1] (nnodes 1 i)]
               (combine-delta
                (regex->nfa+epsilon q0 q1 (tree 1) i)
                (regex->nfa+epsilon q1 f (tree 2) i)))
     :Plus (combine-delta
            (regex->nfa+epsilon q0 f (tree 1) i)
            (regex->nfa+epsilon q0 f (tree 2) i))
     :Star (combine-delta
            {:from q0 :character "ε" :to f}
            (regex->nfa+epsilon q0 q0 (tree 1) i))
     :Group (regex->nfa+epsilon q0 f (tree 1) i)))
  ([tree]
   (let [i (atom 0)]
     (if-not tree
       (regex->nfa+epsilon [0] [0] [:Character "ε"] i)
       (regex->nfa+epsilon [0] [:accept] tree i)))))

(def tree (cljs-dfa.interpreter.index/interpret ""))
(map println (regex->nfa+epsilon tree))
