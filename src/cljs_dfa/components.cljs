(ns cljs-dfa.components (:require
                         ["@mui/material" :as mui]))

(defn input []
  [:> mui/Button {:variant "text"} "hello from component"])