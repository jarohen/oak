(ns todomvc.ui.app
  (:require [reagent.core :as r]
            [oak.core :as o]
            [cljs.core.async :as a]
            [cljs-http.client :as http]
            [clojure.string :as s])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

(defn app-container []
  (js/document.getElementById "app"))

(defn page-root [{:keys [app db] :as ctx}]
  [:div {:style {:margin-left "1em"}}
   [:h3 "Followers"]
   (if (:loading? app)
     [:p "loading..."]

     [:ul
      (doall
       (for [{:keys [user-id username]} (->> (:followers db)
                                             (sort-by (comp s/lower-case :username)))]
         [:li {:key user-id}
          [:a {:href (str "/users/" user-id)}
           "@" username]]))])])

(defn <load-followers! [user-id]
  (go
    (a/<! (a/timeout 1000))
    {:success true
     :body {:followers #{{:user-id "foo"
                          :username "foo"}
                         {:user-id "bar"
                          :username "bar"}}}}))

(defmulti handle-event o/dispatch-by-type)

(defmethod handle-event :app-loaded [state ev]
  (-> state
      (o/update-app assoc :loading? true)
      (o/with-cmds (fn []
                     (go
                       (let [{:keys [success body]} (a/<! (<load-followers! "me"))]
                         (if success
                           (o/ev :followers-loaded {:resp body})
                           (o/ev :failed-loading-followers {:resp body}))))))))

(defmethod handle-event :followers-loaded [state {:keys [resp]}]
  (-> state
      (o/update-app assoc :loading? false)
      (o/update-db merge resp)))

(defonce !state
  (r/atom {:app {}
           :db {}}))

(defn root-ctx []
  (o/->ctx {:handle-event handle-event
            :!state !state}))

(defn render-page! []
  (r/render-component [(fn []
                         [page-root (root-ctx)])]
                      (app-container)))

(defn ^:export main []
  (render-page!)
  (go
    (let [ctx (a/<! (o/send! (root-ctx) (o/ev :app-loaded)))]
      (js/console.log (r/render-to-string [page-root ctx])))))
