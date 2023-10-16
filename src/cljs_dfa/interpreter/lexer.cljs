(ns cljs-dfa.interpreter.lexer)

(defn- operator [char]
  (case char
    "(" :OpenParenthesis
    ")" :CloseParenthesis
    "+" :Plus
    "*" :Star
    nil))

(defn- explicit-concats [tokens]
  (reduce
   #(let [f (:type (first %2)) l (:type (last %2))]
      (if (and (#{:Character :CloseParenthesis :Star} f)
               (#{:Character :OpenParenthesis} l))
        (conj %1 {:type :Concat} (last %2))
        (conj %1 (last %2))))
   [(first tokens)]
   (map list tokens (rest tokens))))


(defn tokenize [raw]
  (->> raw seq
       (remove #{"\f" "\n" " " "\t" "\r"})
       (map #(if-let [op (operator %)]
               {:type op}
               {:type :Character :val %}))
       explicit-concats))
