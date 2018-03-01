(ns oak.core-test
  (:require [oak.core :as oak]
            [clojure.test :as t]))

(defmacro def-test-ev [ev-type]
  `(defmethod oak/handle ~ev-type [state# ev#]
     (-> state# (oak/update-local update ::evs (fnil conj []) ev#))))

(def-test-ev ::fake-event)

(defn new-ctx []
  {:oak/!app (atom {})
   :oak/!db (atom {})})

(t/deftest test-on-handlers
  (let [{:keys [oak/!app] :as ctx} (new-ctx)
        fire! (-> (#'oak/transform-el [:div {:oak/on {:click [::fake-event {:foo :bar}]}}] ctx)
                  (get-in [1 :on-click]))]
    (fire! {})
    (t/is (= (::evs @!app)
             [{:oak/event-type ::fake-event
               :foo :bar}]))))

(defmacro ->component {:style/indent 2} [sym params & body]
  (apply #'oak/->component sym params body))

(t/deftest focuses-component
  (let [ctx (doto (new-ctx)
              (-> :oak/!app (swap! assoc-in [::child-focus :foo] "bar")))
        [component & params] (#'oak/transform-el (-> [(->component child [arg]
                                                        [:div (oak/*local* :foo) arg])
                                                      "foo-arg"]

                                                     (oak/focus ::child-focus))
                                                 ctx)]

    (t/is (= (apply (:reagent-render component) params) [:div "bar" "foo-arg"]))))

(t/deftest sends-with-focus
  (let [{:keys [oak/!app] :as ctx} (new-ctx)]
    (#'oak/send! ctx [::fake-event {:bar :baz}])
    (#'oak/send! (merge ctx {:oak/focus [::focus]}) [::fake-event {:focused :here}])

    (t/is (= @!app
             {::evs [{:bar :baz, :oak/event-type ::fake-event}],
              ::focus {::evs [{:focused :here, :oak/event-type ::fake-event}]}}))))

(defmethod oak/handle ::event-with-cmd [state {:keys [cmd]}]
  (-> state (oak/update-local merge {:have-cmd? true}) (oak/with-cmd cmd)))

(t/deftest handles-cmds
  (let [{:keys [oak/!app] :as ctx} (new-ctx)
        !cb (atom nil)
        cmd (fn [cb]
              (reset! !cb cb))]

    (t/testing "simple event"
      (#'oak/send! ctx [::event-with-cmd {:cmd cmd}])

      (t/is (= @!app {:have-cmd? true}))
      (@!cb [::fake-event {:root :event}])
      (t/is (= @!app {:have-cmd? true, ::evs [{:root :event, :oak/event-type ::fake-event}]})))

    (t/testing "focused event"
      (#'oak/send! (merge ctx {:oak/focus [::focus]}) [::event-with-cmd {:cmd cmd}])
      (t/is (= (::focus @!app) {:have-cmd? true}))

      (@!cb [::fake-event {:focused :event}])
      (t/is (= @!app {:have-cmd? true
                      ::evs [{:root :event, :oak/event-type ::fake-event}]
                      ::focus {:have-cmd? true
                               ::evs [{:focused :event, :oak/event-type ::fake-event}]}})))))

(defmethod oak/handle [::child-event :child/notify-hello] [state {:keys [child-id msg]}]
  (-> state
      (oak/update-local assoc :child-notify {:child-id child-id, :msg msg})))

(defmethod oak/handle ::child-clicked [state {:keys [msg]}]
  (-> state
      (oak/update-local assoc :my-message msg)
      (oak/notify [:child/notify-hello {:msg msg}])))

(t/deftest listeners
  (let [{:keys [oak/!app] :as ctx} (-> (new-ctx) (assoc :oak/focus [:parent-focus]))
        [component & params] (#'oak/transform-el (-> [(->component child []
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
  (let [component (->component transients []
                    ^:oak/transient [{:keys [counter]} {:counter 0}]
                    [:div counter])
        {:keys [oak/!app] :as ctx} (-> (new-ctx) (assoc :oak/focus [:down-one]))
        {:keys [component-will-mount component-will-unmount reagent-render]} (component ctx)]

    (t/is (= (get-in (component-will-mount) [0 :down-one]) {:counter 0}))

    (swap! !app update :down-one assoc :foo :bar)

    (t/is (= (reagent-render) [:div 0]))

    (t/is (= (get-in (component-will-unmount) [0 :down-one]) {:foo :bar}))))

(t/deftest lifecycles)
(let [component (->component lifecycles []
                  ^:oak/transient []
                  ^:oak/lifecycle {:component-will-mount [::fake-event {:event :will-mount}]
                                   :component-did-mount [::fake-event {:event :did-mount}]}
                  [:div "mounted"])

      {:keys [oak/!app] :as ctx} (-> (new-ctx) (assoc :oak/focus [:down-one]))
      {:keys [component-will-mount component-did-mount reagent-render]} (component ctx)]

  (t/is (= (get-in (doto (component-will-mount) prn) [0 :oak/app :down-one ::evs])
           [{:event :will-mount, :oak/lifecycle-args nil, :oak/event-type :oak.core-test/fake-event}]))

  (t/is (= (get-in (component-did-mount) [0 :oak/app :down-one ::evs])
           [{:event :will-mount, :oak/lifecycle-args nil, :oak/event-type :oak.core-test/fake-event}
            {:event :did-mount, :oak/lifecycle-args nil, :oak/event-type :oak.core-test/fake-event}])))
