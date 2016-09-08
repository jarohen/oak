(ns oak.core)

(defn start! [{:keys [$el app db view handle]}]
  )

(defn update-app [{:keys [app db] :as ctx} f & args]
  (apply update ctx :app f args))

(defn update-db [{:keys [app db] :as ctx} f & args]
  (apply update ctx :db f args))

(defn update-in-ctx [{:keys [app] :as ctx} app-path f & args]
  (let [{new-app :app, :keys [db]} (f (apply narrow ctx app-path))]
    {:app (assoc-in app app-path new-app)
     :db db}))

(defn get-in-ctx [{:keys [app db] :as ctx} path]
  (update ctx :app get-in path))

(defn ev
  ([ev-type]
   (ev ev-type {}))

  ([ev-type ev]
   (merge ev {:oak/type ev-type})))

(defn wrap-ev [el ev-type opts]
  (fn [event]
    (oak/ev ev-type (f event))))

(defn h [el]
  )

;; ---- APP ----

(defn counter-view [{:keys [app db]}]
  (o/h
   [:div
    [:p
     (:count app)]
    [:p
     [:button {:on-click (fn [e]
                           (oak/ev :inc-clicked))}
      "Click me!"]]]))

(defn counter-handle [ctx ev]
  (case (:oak/type ev)
    :inc-clicked (o/update-app ctx :count inc)))

(defn handle [ctx ev]
  (case (:oak/type ev)
    :counter (update-in-ctx ctx [:counters idx] counter-handle (:oak/inner ev))))

(defn view [ctx]
  (o/h
   [:div
    (for [idx (range 2)]
      (-> (o/h [sub-view (o/get-in-ctx ctx [:counters idx])])
          (o/wrap-ev :counter {:idx idx})))]))

(o/start! {:$el (js/document.getElementById "app")
           :app {:count 0}
           :db {}
           :view view
           :handle-ev handle})
