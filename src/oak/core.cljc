(ns oak.core
  (:require #?(:clj [clojure.core.async :as a :refer [go go-loop]]
               :cljs [cljs.core.async :as a]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]])))

(defn update-app [{:keys [app db] :as state} f & args]
  (apply update state :app f args))

(defn update-db [{:keys [app db] :as state} f & args]
  (apply update state :db f args))

(defn with-focus [{:keys [app db stack]} ks f & args]
  (let [{new-app :app,
         [{new-outer-app :outer-app} & more-stack] :stack,
         :keys [db]} (apply f
                            {:app (get-in app ks)
                             :db db
                             :stack (cons {:outer-app app, :ks ks} stack)}
                            args)]

    {:app (assoc-in new-outer-app ks new-app)
     :db db
     :stack more-stack}))

(defn with-unfocus [{:keys [app db stack]} f & args]
  (let [[{:keys [outer-app ks]} & more-stack] stack
        {new-app :app, :keys [db]} (apply f
                                          {:app (assoc-in outer-app ks app)
                                           :db db
                                           :stack more-stack}
                                          args)]
    {:app (get-in new-app ks)
     :db db
     :stack (cons {:outer-app new-app
                   :ks ks}
                  more-stack)}))

(comment
  (-> {:app {:a {:b 1}}, :db {:the-db :is-cool}, :stack ()}
      (with-focus [:a] (fn [state]
                         (-> state
                             (with-focus [:c] (fn [state]
                                                (-> state
                                                    (update-app assoc :d 0)
                                                    (with-unfocus update-app assoc :e 2))))
                             (update-app assoc :d 2)
                             (update-db assoc :the-db :wins))))))

(defn ev
  ([ev-type]
   (ev ev-type {}))
  ([ev-type ev-opts]
   (merge ev-opts {:oak/event-type ev-type})))

(defn with-cmds [state & cmds]
  (vary-meta state update ::cmds into cmds))

(defprotocol IContext
  (send! [_ ev])
  (wrap-ev [_ ev])
  (-focus [_ ks]))

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

  (wrap-ev [{:keys [::ev-stack] :as ctx} ev]
    (-> ctx
        (update ::ev-stack #(cons ev %))))

  (-focus [ctx ks]
    (update ctx :app get-in ks)))

(defn focus [ctx & ks]
  (-focus ctx ks))

(defn dispatch-by-type [state {:keys [oak/event-type] :as ev}]
  event-type)

(defn ->ctx [{:keys [handle-event !state]}]
  (let [{:keys [app db]} @!state]
    (map->Context {::handle-event handle-event
                   ::!state !state
                   :app app
                   :db db
                   ::ev-stack (list)})))
