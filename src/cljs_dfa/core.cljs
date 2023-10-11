(ns cljs-dfa.core
  (:require ["@mui/material/styles" :as m]
            ["@mui/material" :as mui :refer [Box]]
            [cljs-dfa.components :as c]
            [reagent.dom :as d]))



;; -------------------------
;; Views
(def base-theme (m/createTheme (clj->js {:palette #js {:mode "dark"
                                                       :primary #js{:main "#2e7d32x"}
                                                       :background #js{:default "#02203c"}}})))

(defn home-page []
  [:> m/ThemeProvider {:theme base-theme}
   [:> mui/CssBaseline]
   [:> Box {:sx {:height "100vh"
                 :display "grid"
                 :grid-template-rows ".1fr [top] 1fr 1fr [bottom]"
                 :grid-template-columns "[start] 3fr 1fr [end]"
                 :gap "1rem"
                 :padding "1rem"}}
    [:> Box {:sx {:grid-column "start / end"}}
     [:h1 {:style {:margin 0}} "Header"]]
    [:> Box {:sx {:grid-row "top / bottom"
                  :background "#001528"
                  :border-radius ".2rem"}}]
    [:> Box {:sx {:grid-row "top / bottom"
                  :border-radius ".2rem"
                  :background "#001528"}}
     [c/input]]]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))

