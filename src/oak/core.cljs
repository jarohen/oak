(ns oak.core
  (:require [cljs.core.async :as a])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defn update-app [{:keys [app db] :as state} f & args]
  (apply update state :app f args))

(defn update-db [{:keys [app db] :as state} f & args]
  (apply update state :db f args))

(defn update-in-state [{:keys [app db]} ks f & args]
  (let [{new-app :app, :keys [db]} (f {:app (get-in app ks)
                                       :db db})]
    {:app (assoc-in app ks new-app)
     :db db}))

(defn ev
  ([ev-type]
   (ev ev-type {}))
  ([ev-type ev-opts]
   (merge ev-opts {:oak/event-type ev-type})))

(defn with-cmds [state & cmds]
  (vary-meta state update ::cmds into cmds))

(defprotocol IContext
  (-send! [_ ev])
  (-nest [_ ev])
  (-narrow [_ ks])
  (snapshot [_]))

(defrecord Context []
  IContext
  (-send! [{:keys [::!state ::handle-event ::ev-stack] :as ctx} ev]
    (let [new-state (swap! !state (fn [state]
                                    (handle-event (-> state
                                                      (vary-meta assoc ::cmds #{}))
                                                  (reduce (fn [sub-event ev]
                                                            (merge ev {:oak/sub-event sub-event}))
                                                          ev
                                                          ev-stack))))]
      (go
        (doseq [cmd (::cmds (meta new-state))]
          (when-let [<ch (cmd)]
            (loop []
              (when-let [ev (a/<! <ch)]
                (a/<! (-send! ctx ev))
                (recur)))))

        ctx)))

  (-nest [{:keys [::ev-stack] :as ctx} ev]
    (-> ctx
        (update ::ev-stack (cons ev-stack ev))))

  (-narrow [ctx ks]
    (update ctx :app get-in ks))

  (snapshot [{:keys [::!state] :as ctx}]
    (merge ctx
           (select-keys @!state [:app :db]))))

(defn send! [ctx ev]
  (-send! ctx ev))

(defn nest [ctx ev]
  (-nest ctx ev))

(defn narrow [ctx & ks]
  (-narrow ctx ks))

(defn dispatch-by-type [state {:keys [oak/event-type] :as ev}]
  event-type)

(defn ->ctx [{:keys [handle-event !state]}]
  (let [{:keys [app db]} @!state]
    (map->Context {::handle-event handle-event
                   ::!state !state
                   :app app
                   :db db
                   ::ev-stack (list)})))
