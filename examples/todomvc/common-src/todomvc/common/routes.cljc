(ns todomvc.common.routes)

(def api-routes
  ["/api" {:post :api}])

(def app-routes
  ["" {"/" :home}])
