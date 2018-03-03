(defproject jarohen/oak.todomvc ""

  :description "An example app for the Oak library"
  :url "https://github.com/jarohen/oak"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [ring/ring-core "1.6.3"]
                 [aleph "0.4.4"]

                 [me.shenfeng/mustache "1.1"]

                 [cider/cider-nrepl "0.16.0"]

                 [jarohen/nomad "0.9.0-20180227.165804-1"]
                 [jarohen/bounce "0.0.1-20180303.180015-9"]]

  :exclusions [org.clojure/clojure org.clojure/clojurescript]

  :source-paths ["src" "../../src"]

  :profiles {:dev {:dependencies [[org.clojure/clojurescript "1.9.946"]
                                  [javax.xml.bind/jaxb-api "2.3.0"] ; jdk9
                                  [org.clojure/core.async "0.4.474"]
                                  [com.cemerick/url "0.1.1"]
                                  [bidi "2.1.3"]
                                  [cljs-http "0.1.44"]
                                  [reagent "0.8.0-alpha2" :exclusions [cljsjs/react]]
                                  [cljsjs/react "16.2.0-3"]
                                  [cljsjs/react-dom "16.2.0-3"]
                                  [cljsjs/react-dom-server "16.2.0-3"]]}}

  :aliases {"dev" ["run" "-m" "todomvc.app.main"]})
