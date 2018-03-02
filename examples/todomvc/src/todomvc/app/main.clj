(ns todomvc.app.main
  (:require [oak.ssr :as ssr]
            [clojure.tools.nrepl.server :as nrepl]
            [cljs.build.api :as cljs]
            [bidi.bidi :as bidi]
            [bidi.ring :as br]
            [bounce.system :as b]
            [aleph.http.server :as http]
            [clojure.java.io :as io]
            [me.shenfeng.mustache :refer [deftemplate]]
            [ring.util.response :as resp]
            [ring.middleware.file :refer [wrap-file]]
            [ring.middleware.resource :refer [wrap-resource]]))

;;; https://github.com/clojure-emacs/cider-nrepl/issues/447
(defn nrepl-handler []
  (require 'cider.nrepl)
  (ns-resolve 'cider.nrepl 'cider-nrepl-handler))

(def cljs-opts
  {:output-to "target/cljs/s/js/app.js"
   :output-dir "target/cljs/s/js/deps"
   :asset-path "/s/js/deps"
   :main 'todomvc.ui.app
   :optimizations :none
   :pretty-print? true})

(b/defcomponent *nashorn* #{} []
  (ssr/mk-engine cljs-opts))

(b/defcomponent *cljs* #{} []
  (-> (future
        (cljs/watch (cljs/inputs "src" "../../src") cljs-opts))
      (b/with-stop (future-cancel *cljs*))))

(deftemplate index-tpl
  (slurp (io/resource "public/index.html")))

(def handler
  (some-fn (-> (br/make-handler ["" {"/" :root}]
                                {:root (fn [req]
                                         (resp/response (index-tpl {:app (ssr/emit-str {:engine *nashorn*, :emit-str-call '(todomvc.ui.app/emit-str)})})))})
               (wrap-file "target/cljs")
               (wrap-resource "public"))

           (constantly (resp/not-found "Not Found"))))

(b/defcomponent *server* #{} []
  (-> (http/start-server #'handler {:port 3000})
      (b/with-stop (.close *server*))))

(defn -main []
  (nrepl/start-server :port 7888 :handler (nrepl-handler))
  (b/start! #{#'*server* #'*cljs* #'*nashorn*}))
