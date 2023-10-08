(ns cljs-dfa.core
  (:require
   [reagent.core :as r]
   [reagent.dom :as d]
   [cljs-dfa.components :as c]))

;; -------------------------
;; Views

(defn home-page []
  [:div
   [:h2 "Reagent"]
   [c/input]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
