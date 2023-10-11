(ns cljs-dfa.interpreter.index
  (:require
   [cljs-dfa.interpreter.lexer :refer [tokenize]]
   [cljs-dfa.interpreter.parser :refer [parse]]))


(defn interpret [str]
  (-> str tokenize parse))