(ns oak.core)

(defn update-app [{:keys [app] :as ctx} f & args]
  (apply update ctx :app f args))

(defn with-focus [{:keys [app] :as ctx} ks f & args]
  (let [{new-app :app, :as new-ctx} (apply f
                                           (-> ctx
                                               (merge {:app (get-in app ks)})
                                               (vary-meta assoc ::stack (cons {:outer-app app, :ks ks} (::stack (meta ctx)))))
                                           args)

        [{new-outer-app :outer-app} & more-stack] (::stack (meta new-ctx))]


    (-> (merge new-ctx
               (select-keys ctx [::handle-ev ::ev-stack ::swap-ctx!])
               {:app (assoc-in new-outer-app ks new-app)})
        (vary-meta assoc ::stack more-stack))))

(defn with-unfocus [{:keys [app] :as ctx} f & args]
  (let [[{:keys [outer-app ks]} & more-stack] (::stack (meta ctx))
        {new-app :app, :as new-ctx} (apply f
                                           (-> ctx
                                               (merge {:app (assoc-in outer-app ks app)})
                                               (vary-meta assoc ::stack more-stack))
                                           args)]
    (-> (merge new-ctx
               (select-keys ctx [::handle-ev ::ev-stack ::swap-ctx!])
               {:app (get-in new-app ks)})
        (vary-meta assoc ::stack (cons {:outer-app new-app
                                        :ks ks}
                                       more-stack)))))

(comment
  (-> {:app {:a {:b 1}}
       :foo :bar}
      (with-focus [:a] (fn [new-ctx]
                         (-> new-ctx
                             (update-app update :b inc)
                             (with-unfocus update-app assoc :c :bell))))))

(defn ev
  ([ev-type]
   (ev ev-type {}))
  ([ev-type ev-opts]
   (merge ev-opts {:oak/event-type ev-type})))

(defn with-cmd [ctx cmd]
  (-> ctx
      (vary-meta update ::cmds (fnil conj []) cmd)))

(defprotocol IContext
  (send! [_ ev]))

(defn handle-cmds! [ctx]
  (doseq [cmd (::cmds (meta ctx))]
    (cmd (fn [ev]
           (send! ctx ev)))))

(defn wrap-send [ctx ev]
  (-> ctx
      (vary-meta update ::ev-stack #(cons ev %))))

(defn- nest-ev [ev ev-stack]
  (reduce (fn [sub-event ev]
            (merge ev {:oak/sub-event sub-event}))
          ev
          ev-stack))

(defn focus [ctx & ks]
  (update ctx :app get-in ks))

(defrecord Context [app]
  IContext
  (send! [ctx {:keys [oak/root-ev?] :as ev}]
    (let [{:keys [::handle-ev ::ev-stack ::swap-ctx!]} (meta ctx)]
      (doto (swap-ctx! (fn [ctx]
                         (-> (handle-ev (-> ctx
                                            (vary-meta assoc ::cmds []))
                                        (-> ev
                                            (cond-> (not root-ev?) (nest-ev ev-stack))))
                             (vary-meta merge (select-keys (meta ctx) [::handle-ev ::ev-stack ::swap-ctx!])))))
        handle-cmds!))))

(defn ->ctx [initial-state {:keys [handle-ev swap-ctx!]}]
  (-> (map->Context initial-state)
      (with-meta (merge (meta initial-state)
                        {::handle-ev handle-ev
                         ::swap-ctx! swap-ctx!
                         ::ev-stack (list)}))))


(defn dispatch-by-type [state {:keys [oak/event-type] :as ev}]
  event-type)

(defn fmap-cmd [f cmd]
  (fn [cb]
    (cmd (fn [ev]
           (cb (f ev))))))
