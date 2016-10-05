(ns todomvc.service.handler
  (:require [todomvc.service.style :as style]
            [todomvc.common.routes :as routes]
            [bidi.bidi :as bidi]
            [bidi.ring :as br]
            [bounce.core :as bc]
            [bounce.figwheel :as cljs]
            [hiccup.page :refer [html5 include-css include-js]]
            [ringless.core :as less]
            [ring.middleware.format :refer [wrap-restful-format]]
            [ring.util.response :refer [response content-type]]))

;; This is all in one NS for now, but you'll likely want to split it
;; out when your webapp grows!

(defn page-handler [req]
  (-> (response
       (html5
        [:head
         [:title "todomvc - CLJS Single Page Web Application"]

         (include-css "//cdnjs.cloudflare.com/ajax/libs/normalize/3.0.3/normalize.min.css")
         (less/include-style style/config)]

        [:body
         [:div#app]
         (include-js (cljs/path-for-js (bc/ask :cljs-compiler)))
         [:script "todomvc.ui.app.main();"]]))

      (content-type "text/html")))

(defn api-handlers []
  {:api (fn [{:keys [body-params] :as req}]
          (case (:action body-params)
            :echo (response body-params)
            ;; fill this out with other actions...

            nil))})

(defn make-handler []
  (some-fn (-> (br/make-handler routes/api-routes (api-handlers))
               (wrap-restful-format :formats [:transit-json :edn :json-kw]))

           (br/make-handler routes/app-routes (constantly page-handler))

           (br/make-handler (cljs/bidi-routes (bc/ask :cljs-compiler)))
           (less/style-handler style/config)

           (constantly {:status 404, :body "Not found."})))
