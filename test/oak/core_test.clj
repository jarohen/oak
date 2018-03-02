(ns oak.core-test
  (:require [oak.core :as oak]
            [clojure.test :as t]))

(defn fake-event [state ev]
  (-> state
      (oak/update-local update ::evs (fnil conj []) ev)))

(defn new-ctx []
  {:oak/!app (atom {})
   :oak/!db (atom {})})

(t/deftest test-on-handlers
  (let [{:keys [oak/!app] :as ctx} (merge (new-ctx)
                                          {:oak/event-handlers {::fake-event fake-event}})
        fire! (-> (#'oak/transform-el [:div {:oak/on {:click [::fake-event {:foo :bar}]}}] ctx)
                  (get-in [1 :on-click]))]
    (fire! {})
    (t/is (= (::evs @!app)
             [{:oak/event-type ::fake-event
               :foo :bar}]))))

(t/deftest focuses-component
  (let [ctx (doto (new-ctx)
              (-> :oak/!app (swap! assoc-in [::child-focus :foo] "bar")))
        [component & params] (#'oak/transform-el (-> [(oak/->component child [arg]
                                                        [:div (oak/*local* :foo) arg])
                                                      "foo-arg"]

                                                     (oak/focus ::child-focus))
                                                 ctx)]

    (t/is (= (apply (:reagent-render component) params) [:div "bar" "foo-arg"]))))

(t/deftest sends-with-focus
  (let [{:keys [oak/!app] :as ctx} (merge (new-ctx)
                                          {:oak/event-handlers {::fake-event fake-event}})]
    (#'oak/send! ctx [::fake-event {:bar :baz}])
    (#'oak/send! (merge ctx {:oak/focus [::focus]}) [::fake-event {:focused :here}])

    (t/is (= @!app
             {::evs [{:bar :baz, :oak/event-type ::fake-event}],
              ::focus {::evs [{:focused :here, :oak/event-type ::fake-event}]}}))))

(t/deftest handles-cmds
  (let [!cb (atom nil)
        {:keys [oak/!app] :as ctx} (merge (new-ctx)
                                          {:oak/event-handlers {::event-with-cmd (fn [state _]
                                                                                   (-> state (oak/update-local merge {:have-cmd? true})
                                                                                       (oak/with-cmd [::fake-cmd])))
                                                                ::fake-event fake-event}
                                           :oak/cmd-handlers {::fake-cmd (fn [_ cb]
                                                                           (reset! !cb cb))}})]

    (t/testing "simple event"
      (#'oak/send! ctx [::event-with-cmd {:cmd [::fake-cmd]}])

      (t/is (= @!app {:have-cmd? true}))
      (@!cb [::fake-event {:root :event}])
      (t/is (= @!app {:have-cmd? true, ::evs [{:root :event, :oak/event-type ::fake-event}]})))

    (t/testing "focused event"
      (#'oak/send! (merge ctx {:oak/focus [::focus]}) [::event-with-cmd])
      (t/is (= (::focus @!app) {:have-cmd? true}))

      (@!cb [::fake-event {:focused :event}])
      (t/is (= @!app {:have-cmd? true
                      ::evs [{:root :event, :oak/event-type ::fake-event}]
                      ::focus {:have-cmd? true
                               ::evs [{:focused :event, :oak/event-type ::fake-event}]}})))))

(t/deftest listeners
  (let [{:keys [oak/!app] :as ctx} (-> (merge (new-ctx)
                                              {:oak/focus [:parent-focus]
                                               :oak/event-handlers {::child-clicked (fn [state {:keys [msg]}]
                                                                                      (-> state
                                                                                          (oak/update-local assoc :my-message msg)
                                                                                          (oak/notify [:child/notify-hello {:msg msg}])))

                                                                    [::child-event :child/notify-hello] (fn [state {:keys [child-id msg]}]
                                                                                                          (-> state
                                                                                                              (oak/update-local assoc :child-notify {:child-id child-id, :msg msg})))}}))
        [component & params] (#'oak/transform-el (-> [(oak/->component child []
                                                        [:div {:oak/on {:click [::child-clicked {:msg "Hello world!"}]}}])]
                                                     (oak/focus :child :jimmy)
                                                     (oak/listen [::child-event {:child-id :jimmy}]))
                                                 ctx)
        on-click (-> (apply (:reagent-render component) params)
                     (get-in [1 :on-click]))
        {:keys [oak/app]} (on-click {})]

    (t/is (= app {:parent-focus {:child {:jimmy {:my-message "Hello world!"}}
                                 :child-notify {:child-id :jimmy, :msg "Hello world!"}}}))))

(t/deftest transients
  (let [component (oak/->component transients []
                    ^:oak/transient [{:keys [counter]} {:counter 0}]
                    [:div counter])
        {:keys [oak/!app] :as ctx} (-> (new-ctx) (assoc :oak/focus [:down-one]))
        {:keys [component-will-mount component-will-unmount reagent-render]} (component ctx)]

    (t/is (= {:counter 0} (get-in (component-will-mount) [:oak/app :down-one])))

    (swap! !app update :down-one assoc :foo :bar)

    (t/is (= (reagent-render) [:div 0]))

    (t/is (= {:foo :bar} (get-in (component-will-unmount) [:oak/app :down-one])))))

(t/deftest lifecycles
  (let [component (oak/->component lifecycles []
                    ^:oak/transient []
                    ^:oak/lifecycle {:component-will-mount [::fake-event {:event :will-mount}]
                                     :component-did-mount [::fake-event {:event :did-mount}]}
                    [:div "mounted"])

        {:keys [oak/!app] :as ctx} (merge (new-ctx)
                                          {:oak/focus [:down-one]
                                           :oak/event-handlers {::fake-event fake-event}})

        {:keys [component-will-mount component-did-mount reagent-render]} (component ctx)]

    (t/is (= [{:event :will-mount, :oak/lifecycle-args nil, :oak/event-type ::fake-event}]
             (get-in (component-will-mount) [:oak/app :down-one ::evs])))

    (t/is (= (get-in (component-did-mount) [:oak/app :down-one ::evs])
             [{:event :will-mount, :oak/lifecycle-args nil, :oak/event-type ::fake-event}
              {:event :did-mount, :oak/lifecycle-args nil, :oak/event-type ::fake-event}]))))
