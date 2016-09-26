(ns todomvc.service.style
  (:require [clojure.java.io :as io]
            [schema.core :as sc]
            [ringless.core :as less]))

(sc/def config :- less/BuildConfig
  {:web-prefix "/static/style"

   :dev {:less-classpath-prefix "less"
         :less-filename "main.less"}

   :build {:lessc-optsv ["--autoprefix=>10%" "--clean-css=--s1 --advanced --compatibility=ie8"]
           :build-path "target/less/build"
           :classpath-prefix "todomvc/style"
           :build-filename "main.css"}})

(defn build-style! []
  (less/build-less! config))
