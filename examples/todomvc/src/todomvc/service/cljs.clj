(ns todomvc.service.cljs
  (:require [bounce.figwheel :as cljs]))

(def cljs-config
  {:source-paths ["ui-src" "../../src"]
   :target-path "target/cljs/"

   :web-context-path "/js"

   :figwheel {:client {:on-jsload "reagent.core/force-update-all"}}

   :dev {:main 'todomvc.ui.app
         :optimizations :none
         :pretty-print? true}

   :build {:optimizations :advanced
           :pretty-print? false
           :classpath-prefix "js"}})

(defn make-cljs-compiler []
  (cljs/start-cljs-compiler! cljs-config))

(defn build-cljs! []
  (cljs/build-cljs! cljs-config))
