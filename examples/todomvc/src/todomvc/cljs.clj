(ns todomvc.cljs
  (:require [cljs.build.api :as cljs]))

(defn -main []
  (cljs/watch (cljs/inputs "src" "../../src")
              {:output-to "target/app.js"
               :output-dir "target/deps"
               :main 'todomvc.ui.app
               :optimizations :none
               :pretty-print? true}))
