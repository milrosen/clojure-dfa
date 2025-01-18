(ns user
  (:require
   [shadow.cljs.devtools.api :as shadow]
   [ring.middleware.resource :refer [wrap-resource]]))
  
(print "h")
(defn cljs []
  (shadow/repl :app))
