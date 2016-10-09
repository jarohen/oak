(ns oak.core
  (:require [cljs.core.async :as a])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defn update-app [{:keys [app db] :as state} f & args]
  (apply update state :app f args))

(defn update-db [{:keys [app db] :as state} f & args]
  (apply update state :db f args))

(defn update-in-state [{:keys [app db]} ks f & args]
  (let [{new-app :app, :keys [db]} (apply f
                                          {:app (get-in app ks)
                                           :db db}
                                          args)]
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
  (send! [_ ev])
  (nest [_ ev])
  (-narrow [_ ks]))

(defrecord Context [app db]
  IContext
  (send! [{:keys [::!state ::handle-event ::ev-stack] :as ctx} ev]
    (let [new-state (swap! !state (fn [state]
                                    (handle-event (-> state
                                                      (vary-meta assoc ::cmds #{}))
                                                  (reduce (fn [sub-event ev]
                                                            (merge ev {:oak/sub-event sub-event}))
                                                          ev
                                                          ev-stack))))]
      (reduce (fn [<ctx cmd]
                (go
                  (let [ctx (a/<! <ctx)]
                    (if-let [<ch (when cmd (cmd))]
                      (loop [ctx ctx]
                        (if-let [ev (a/<! <ch)]
                          (recur (a/<! (send! ctx ev)))
                          ctx))

                      ctx))))
              (go
                (merge ctx (select-keys new-state [:app :db])))

              (::cmds (meta new-state)))))

  (nest [{:keys [::ev-stack] :as ctx} ev]
    (-> ctx
        (update ::ev-stack #(cons ev %))))

  (-narrow [ctx ks]
    (update ctx :app get-in ks)))

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
