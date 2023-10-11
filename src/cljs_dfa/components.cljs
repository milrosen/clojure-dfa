(ns cljs-dfa.components (:require
                         ["@mui/material" :as m]
                         [reagent.core :as r]
                         [cljs-dfa.interpreter.index :refer [interpret]]))


(def regex (r/atom "(a+b)*(ab)(a+b)*"))

(defn input []
  [:> m/TextField {:variant "filled" :label "enter your regex here!!" :sx {:width "100%"}
                   :onChange #(-> (reset! regex (-> % .-target .-value))
                                  interpret)
                   :value @regex}])