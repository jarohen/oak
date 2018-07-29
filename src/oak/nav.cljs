(ns oak.nav
  (:require [oak.core :as oak]
            [cemerick.url :as curl]
            [cljs.reader :as edn])

  (:import [goog.events BrowserEvent]))

(defprotocol Router
  (parse [_ url-path])
  (unparse [_ handler route-params]))

(defn- browser-location [router]
  (let [{:keys [query path]} (curl/url (str js/location.pathname js/location.search))]
    (merge (parse router path)
           {:query-params query
            :history-state (some-> js/history.state edn/read-string)})))

(defn location->href [{:keys [handler route-params query-params anchor] :as location} {:keys [::router]}]
  (str (merge (curl/url (str js/document.origin (unparse router handler route-params)))
              {:query query-params
               :anchor anchor})))

(defn- nav-cmd [{:keys [::update-location! location] :as cmd} cb]
  (update-location! (pr-str (:history-state location))
                    nil
                    (location->href location {::router (get-in cmd [:oak/db ::router])}))
  (cb [::location-changed {:new-location location}]))

(defmethod oak/cmd! ::push-location [{:keys [location] :as cmd} cb]
  (nav-cmd (merge cmd {::update-location! #(js/history.pushState %1 %2 %3)}) cb))

(defmethod oak/cmd! ::replace-location [{:keys [location] :as cmd} cb]
  (nav-cmd (merge cmd {::update-location! #(js/history.replaceState %1 %2 %3)}) cb))

(defn intercept-click-event? [event]
  (let [event (BrowserEvent. event)]
    (and (.isMouseActionButton event)
         ;; open link in new tab
         (not (.-platformModifierKey event))
         ;; open link in new window
         (not (.-shiftKey event))
         ;; save link as
         (not (.-altKey event)))))

(defmethod oak/handle ::link-clicked [state {:keys [location oak/react-ev]}]
  (if (intercept-click-event? react-ev)
    (do
      (.preventDefault react-ev)
      (-> state
          (oak/with-cmd [::push-location {:location location}])))

    state))

(defn link [location]
  {:href (location->href location {::router (oak/*db* ::router)})
   :on-click ^{:oak/prevent-default? false} [::link-clicked {:location location}]})

(defmulti handle-mount (fn [state ev] (get-in ev [:location :handler])))
(defmethod handle-mount :default [state {:keys [location]}] state)

(defmulti handle-change (fn [state ev] (get-in ev [:new-location :handler])))
(defmethod handle-change :default [state {:keys [old-location new-location]}] state)

(defmulti handle-unmount (fn [state ev] (get-in ev [:location :handler])))
(defmethod handle-unmount :default [state {:keys [location]}] state)

(defn ks= [ks & ms]
  (apply = (->> ms
                (into [] (map (fn [m]
                                (-> m
                                    (select-keys ks)
                                    (->> (into {} (remove (comp (some-fn nil? #{{} []}) val)))))))))))

(defmethod oak/handle ::location-changed [state {:keys [new-location]}]
  (let [old-location (get-in state [:oak/db ::location])
        remount? (not (ks= [:handler :route-params]
                           old-location new-location))

        change? (and (not remount?)
                     (not (ks= [:handler :route-params :query-params :history-state]
                               old-location new-location)))]

    (-> state
        (assoc-in [:oak/db ::location] new-location)
        (cond-> change? (handle-change {:old-location old-location
                                        :new-location new-location})

                (and remount? old-location) (handle-unmount {:location old-location})

                remount? (handle-mount {:location new-location})))))

(defmethod oak/handle ::nav-initialised [state {:keys [location ::router]}]
  (-> state
      (assoc-in [:oak/db ::router] router)
      (oak/handle [::location-changed {:new-location location}])))

(defmethod oak/cmd! ::init-nav [{:keys [::router]} cb]
  (.addEventListener js/window "popstate"
                     (fn [_]
                       (cb [::location-changed {:new-location (browser-location router)}])))

  (cb [::nav-initialised {:location (browser-location router)
                          ::router router}]))
