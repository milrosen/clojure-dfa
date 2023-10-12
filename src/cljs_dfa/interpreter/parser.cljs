(ns cljs-dfa.interpreter.parser)

(declare expression)

(defn- character-expr [tokens]
  (let [token (first tokens)
        char (:val token)]
    {:tree [:Character char]
     :tokens (rest tokens)}))

(defn- group-expr [tokens]
  (let [op (:type (first tokens))]
    (when (= op :OpenParenthesis)
      (let [expr (expression (rest tokens))
            op (:type (first (:tokens expr)))]
        (if (= op :CloseParenthesis)
          {:tree [:Group (:tree expr)]
           :tokens (rest (:tokens expr))}
          (println ""))))))

(defn- primary-expr [tokens]
  (let [type (:type (first tokens))]
    (let [primary
          (if (= type :Character)
            (character-expr tokens)
            (group-expr tokens))]
      (if (= (:type (first (:tokens primary))) :Star)
        {:tree [:Star (:tree primary)] :tokens (rest (:tokens primary))}
        primary))))

(defn- concat-expr [tokens]
  (let [primary (primary-expr tokens)
        remaining-tokens (:tokens primary)
        op (:type (first remaining-tokens))]
    (if (= :Concat op)
      (let [concat (concat-expr (rest remaining-tokens))]
        {:tree [:Concat (:tree primary) (:tree concat)]
         :tokens (:tokens concat)})
      primary)))

(defn- plus-expr [tokens]
  (let [concat (concat-expr tokens)
        remaining-tokens (:tokens concat)
        op (:type (first remaining-tokens))]
    (if (= :Plus op)
      (let [plus (plus-expr (rest remaining-tokens))]
        {:tree [:Plus (:tree concat) (:tree plus)]
         :tokens (:tokens plus)})
      concat)))


(defn- expression [tokens]
  (plus-expr tokens))

(defn parse [tokens]
  (:tree (expression tokens)))