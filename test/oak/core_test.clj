(ns oak.core-test
  (:require [oak.core :as oak]
            [clojure.test :as t]))

(defmacro def-test-ev [ev-type]
  `(defmethod oak/handle ~ev-type [state# ev#]
     (-> state# (update-in [:oak/app ::evs] (fnil conj []) ev#))))

(def-test-ev ::click-event)

(defn new-ctx []
  {:oak/!app (atom {})
   :oak/!db (atom {})})

(t/deftest test-on-handlers
  (let [{:keys [oak/!app] :as ctx} (new-ctx)
        fire! (-> (#'oak/transform-el [:div {:oak/on {:click [::click-event {:foo :bar}]}}] ctx)
                  (get-in [1 :on-click]))]
    (fire! {})
    (t/is (= (::evs @!app)
             [{:oak/event-type :oak.core-test/click-event
               :foo :bar}]))))

(defn fake-component [render]
  (-> (fn [ctx]
        (fn [& params]
          (#'oak/reagent-class {:ctx ctx
                                :render render})))
      (vary-meta assoc :oak/component? true)))

(t/deftest focuses-component
  (let [ctx (doto (new-ctx)
              (-> :oak/!app (swap! assoc-in [::child-focus :foo] "bar")))
        [component & params] (#'oak/transform-el (-> [(fake-component (fn [arg]
                                                                        [:div (oak/*app* :foo) arg]))]

                                                     (oak/focus ::child-focus))
                                                 ctx)
        render (-> (apply component params) :reagent-render)]

    (t/is (= (render "foo-arg") [:div "bar" "foo-arg"]))))

(t/deftest sends-with-focus
  (let [{:keys [oak/!app] :as ctx} (new-ctx)]
    (#'oak/send! ctx [::click-event {:bar :baz}])
    (#'oak/send! (merge ctx {:oak/focus [::focus]}) [::click-event {:focused :here}])

    (t/is (= @!app)
          {::evs [{:bar :baz, :oak/event-type ::click-event}],
           ::focus {::evs [{:focused :here, :oak/event-type ::click-event}]}})))

(defmethod oak/handle ::event-with-cmd [state {:keys [cmd]}]
  (-> state (oak/update-app merge {:have-cmd? true}) (oak/with-cmd cmd)))

(t/deftest handles-cmds
  (let [{:keys [oak/!app] :as ctx} (new-ctx)
        !cb (atom nil)
        cmd (fn [cb]
              (reset! !cb cb))]

    (t/testing "simple event"
      (#'oak/send! ctx [::event-with-cmd {:cmd cmd}])

      (t/is (= @!app {:have-cmd? true}))
      (@!cb [::click-event {:root :event}])
      (t/is (= @!app {:have-cmd? true, ::evs [{:root :event, :oak/event-type ::click-event}]})))

    (t/testing "focused event"
      (#'oak/send! (merge ctx {:oak/focus [::focus]}) [::event-with-cmd {:cmd cmd}])
      (t/is (= (::focus @!app) {:have-cmd? true}))

      (@!cb [::click-event {:focused :event}])
      (t/is (= @!app {:have-cmd? true
                      ::evs [{:root :event, :oak/event-type ::click-event}]
                      ::focus {:have-cmd? true
                               ::evs [{:focused :event, :oak/event-type ::click-event}]}})))))

;; TODO: listen, transient state
