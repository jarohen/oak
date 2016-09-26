(ns oak.core)

(defn update-app [{:keys [app db] :as ctx} f & args]
  (apply update ctx :app f args))

(defn update-db [{:keys [app db] :as ctx} f & args]
  (apply update ctx :db f args))

(defn update-in-state [{:keys [app db]} ks f & args]
  (let [{new-app :app, :keys [db]} (f {:app (get-in app ks)
                                       :db db})]
    {:app (assoc-in app ks new-app)
     :db db}))

(defprotocol IContext
  (-send! [_ ev])
  (-nest [_ ev])
  (-narrow [_ ks]))

(defrecord Context [app db]
  IContext
  (-send! [{:keys [::!state ::handle-event ::ev-stack]} ev]
    (swap! !state (fn [{:keys [app db]}]
                    ;; TODO split out handle-event result into next val + side effects
                    (handle-event {:app app, :db db}
                                  (reduce (fn [sub-event ev]
                                            (merge ev {:oak/sub-event sub-event}))
                                          ev
                                          ev-stack)))))

  (-nest [{:keys [::ev-stack]} ev]
    (-> ctx
        (update ::ev-stack (cons ev-stack ev))))

  (-narrow [ctx ks]
    (update ctx :app get-in ks)))

(defn send!
  ([ctx ev-type]
   (send! ctx ev-type {}))
  ([ctx ev-type ev-opts]
   (-send! ctx (merge ev-opts {:oak/event-type ev-type}))))

(defn nest
  ([ctx ev-type]
   (nest ctx ev-type {}))
  ([ctx ev-type ev-opts]
   (-nest ctx (merge ev-opts {:oak/event-type ev-type}))))

(defn narrow [ctx & ks]
  (-narrow ctx ks))

(defn ->ctx [{:keys [handle-event !state]}]
  (let [{:keys [app db]} @!state]
    (map->Context {::handle-event handle-event
                   ::!state state
                   :app app
                   :db db
                   ::ev-stack (list)})))
