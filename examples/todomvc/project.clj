(defproject todomvc ""

  :description "An example app for the Oak library"
  :url "https://github.com/jarohen/oak"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.reader "0.10.0"]
                 [jarohen/embed-nrepl "0.1.7"]

                 [jarohen/bounce "0.0.1-alpha1"]

                 [ring/ring-core "1.4.0"]
                 [jarohen/bounce.aleph "0.0.1-alpha1"]
                 [aleph "0.4.1-beta1"]
                 [jarohen/ringless "0.0.1-alpha1"]
                 [bidi "2.0.3"]
                 [hiccup "1.0.5"]
                 [ring-middleware-format "0.7.0" :exclusions [ring]]

                 [jarohen/bounce.figwheel "0.0.1-alpha1"]

                 [org.clojure/tools.logging "0.3.1"]
                 [org.slf4j/slf4j-api "1.7.18"]
                 [org.apache.logging.log4j/log4j-slf4j-impl "2.5"]
                 [org.apache.logging.log4j/log4j-api "2.5"]
                 [org.apache.logging.log4j/log4j-core "2.5"]

                 ;; for Log4j2 JSON support
                 [com.fasterxml.jackson.core/jackson-core "2.6.3"]
                 [com.fasterxml.jackson.core/jackson-databind "2.6.3"]
                 [com.fasterxml.jackson.core/jackson-annotations "2.6.3"]]

  :exclusions [org.clojure/clojure org.clojure/clojurescript]

  :profiles {:dev {:dependencies [[org.clojure/clojurescript "1.7.228"]
                                  [lein-figwheel "0.5.0-6"]
                                  [com.cemerick/piggieback "0.2.1"]
                                  [reagent "0.5.1"]
                                  [cljs-http "0.1.39"]]}}

  :source-paths ["src" "common-src" "../../src"]

  :auto-clean false

  :uberjar-name "todomvc-standalone.jar"

  :filespecs [{:type :path, :path "target/cljs/build/mains"}
              {:type :path, :path "target/less/build"}]

  :aliases {"dev" ["run" "-m" "todomvc.service.main"]
            "build" ["do"
                     "clean"
                     ["run" "-m" "todomvc.service.main/build!"]
                     "uberjar"]})
