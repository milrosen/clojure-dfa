(ns cljs-dfa.components.input (:require
                               ["@mui/material" :as m]
                               [cljs-dfa.db :as db]))

(defn input []
  [:> m/TextField {:variant "filled" :label "enter your regex here!!" :sx {:width "100%"}
                   :onChange #(reset! db/regex (->  % .-target .-value))
                   :value @db/regex}])