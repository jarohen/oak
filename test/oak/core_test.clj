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

(oak/defc focused-child-component [arg]
  [:div (oak/*app* :foo)])

(t/deftest focuses-component
  (let [ctx (doto (new-ctx)
              (-> :oak/!app (swap! assoc-in [::child-focus :foo] "bar")))
        [component & params] (#'oak/transform-el (-> [focused-child-component :foo-arg]
                                                     (oak/focus ::child-focus))
                                                 ctx)
        render (-> (apply component params) :reagent-render)]
    (t/is (= (second (render :foo-arg)) "bar"))))

(t/deftest sends-with-focus
  (let [{:keys [oak/!app] :as ctx} (new-ctx)]
    (#'oak/send! ctx [::click-event {:bar :baz}])
    (#'oak/send! (merge ctx {:oak/focus [::focus]}) [::click-event {:focused :here}])

    (t/is (= @!app)
          {::evs [{:bar :baz, :oak/event-type ::click-event}],
           ::focus {::evs [{:focused :here, :oak/event-type ::click-event}]}})))
