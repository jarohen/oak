(ns todomvc.ui.app
  (:require [reagent.core :as r]
            [oak.core :as o]
            [cljs.core.async :as a])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

(defn app-container []
  (js/document.getElementById "app"))

(defn page-root [{:keys [app db] :as ctx}]
  [:div
   "Hello world!"
   [:p (pr-str app db)]
   [:button {:on-click #(o/send! ctx :inc)}
    "Inc!"]])

(defn handle-event [state {:keys [:oak/event-type] :as ev}]
  (-> (o/update-app state update :count (case event-type
                                          :inc inc
                                          :dec dec))
      (o/with-cmds (fn []
                     (go
                       (case (rand-int 3)
                         0 (o/ev :inc)
                         1 (o/ev :dec)
                         2 nil))))))

(defonce !state
  (r/atom {:app {}
           :db {}}))

(defn render-page! []
  (r/render-component [(fn []
                         [page-root (o/->ctx {:handle-event handle-event
                                              :!state !state})])]
                      (app-container)))

(defn ^:export main []
  (render-page! ))
