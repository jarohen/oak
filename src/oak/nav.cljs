(ns oak.nav
  (:require [oak.core :as o]
            [cemerick.url :as curl]
            [clojure.reader :as edn])

  (:import [goog.events BrowserEvent]))

(defprotocol Router
  (parse [_ url-path])
  (unparse [_ handler route-params]))

(defn- browser-location [router]
  (let [{:keys [query path]} (curl/url (str js/location.pathname js/location.search))]
    (merge (parse router path)
           {:query-params query
            :history-state (some-> js/history.state edn/read-string)})))

(defn location [ctx]
  (get-in ctx [::nav :location]))

(defn location->href [ctx {:keys [handler route-params query-params] :as location}]
  (str (merge (curl/url (str js/document.origin (unparse (get-in ctx [::nav :router]) handler route-params)))
              {:query query-params})))

(defn init [ctx {:keys [router ev]}]
  (let [initial-location (browser-location router)]
    (-> ctx
        (assoc ::nav {:router router
                      :ev ev})
        (o/with-cmd (fn [cb]
                      (cb (merge ev {:new-location initial-location}))

                      (.addEventListener js/window
                                         "popstate"
                                         (fn [_]
                                           (cb (merge ev {:new-location (browser-location router)})))))))))

(defn nav-cmd [ctx method {:keys [history-state] :as new-location}]
  (fn [cb]
    (let [old-location (location ctx)
          update-location! (case method
                             :push #(js/history.pushState %1 %2 %3)
                             :replace #(js/history.replaceState %1 %2 %3))]
      (update-location! (pr-str history-state) nil (location->href ctx new-location))

      (cb (merge (get-in ctx [::nav :ev])
                 {:new-location new-location})))))

(defn push-cmd [ctx location]
  (nav-cmd ctx :push location))

(defn replace-cmd [ctx location]
  (nav-cmd ctx :replace location))

(defn intercept-click-event? [event]
  (let [event (BrowserEvent. event)]
    (and (.isMouseActionButton event)
         ;; open link in new tab
         (not (.-platformModifierKey event))
         ;; open link in new window
         (not (.-shiftKey event))
         ;; save link as
         (not (.-altKey event)))))

(defn link [ctx location]
  {:href (location->href ctx location)
   :on-click (fn [ev]
               (when (intercept-click-event? ev)
                 (.preventDefault ev)
                 ((push-cmd ctx location) (fn [ev] (o/send! ctx ev)))))})

(defmulti handle-mount (fn [ctx ev] (get-in ev [:location :handler])))
(defmethod handle-mount :default [ctx {:keys [location]}] ctx)

(defmulti handle-change (fn [ctx ev] (get-in ev [:new-location :handler])))
(defmethod handle-change :default [ctx {:keys [old-location new-location]}] ctx)

(defmulti handle-unmount (fn [ctx ev] (get-in ev [:location :handler])))
(defmethod handle-unmount :default [ctx {:keys [location]}] ctx)

(defn ks= [ks & ms]
  (apply = (->> ms
                (into [] (map (fn [m]
                                (-> m
                                    (select-keys ks)
                                    (->> (into {} (remove (comp (some-fn nil? #{{} []}) val)))))))))))

(defn handle-nav [ctx {:keys [new-location]}]
  (let [old-location (location ctx)
        remount? (not (ks= [:handler :route-params]
                           old-location new-location))
        change? (not (ks= [:handler :route-params :query-params :history-state]
                          old-location new-location))]
    (-> ctx
        (assoc-in [::nav :location] new-location)
        (cond-> (and change? (not remount?)) (handle-change {:old-location old-location
                                                             :new-location new-location}))
        (cond-> (and remount? old-location) (handle-unmount {:location old-location}))
        (cond-> remount? (handle-mount {:location new-location})))))
