(ns cljs-dfa.components.input (:require
                               ["@mui/material" :as m]
                               [reagent.core :as r]
                               [cljs-dfa.db :as db]))

(def value (r/atom ""))

(defn input []
  [:> m/TextField {:variant "filled" :label "enter your regex here!!" :sx {:width "100%"}
                   :onChange #(reset! value (->  % .-target .-value))
                   :value @value
                   :on-key-press #(when (= 13 (.-charCode %)) (reset! db/regex @value))}])