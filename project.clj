(defproject jarohen/oak "0.0.1-SNAPSHOT"
  :description "A ClojureScript library to structure single-page apps - taking inspiration from the Elm Architecture"
  :url "https://github.com/jarohen/oak"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [com.cemerick/url "0.1.1"]
                 [reagent "0.6.2"]]

  :profiles {:dev {:dependencies [[org.clojure/core.async "0.3.442"]
                                  [cljs-http "0.1.43"]
                                  [bidi "2.1.1"]]}})
