(ns cljs-dfa.interpreter.fa)


(defn regex->delta
  ([q f tree i]
   (case (tree 0)
     :Character {#{q (tree 1)} f}
     :Concat (let [qn (swap! i #(+ 1 %))]
               (merge-with into (regex->delta q [qn] (tree 1) i)
                           (regex->delta [qn] f (tree 2) i)))
     :Plus (let [qn (swap! i #(+ 4 %))]
             (merge-with into {#{q "ε"} [(- qn 3) (- qn 2)]}
                         (regex->delta [(- qn 3)] [(- qn 1)] (tree 1) i)
                         (regex->delta [(- qn 2)] [qn] (tree 2) i)
                         {#{[(- qn 1)] "ε"} f}
                         {#{[qn] "ε"} f}))
     :Star (merge-with into {#{q "ε"} f}
                       (regex->delta q q (tree 1) i))
     :Group (regex->delta q f (tree 1) i)))
  ([tree]
   (let [i (atom 0)]
     (regex->delta [0] [:accept] tree i))))

(def test-tree (cljs-dfa.interpreter.index/interpret "a(ab)*((ab)+b)+a"))
((regex->delta test-tree) #{"b" [4]})
(map println (regex->delta test-tree))