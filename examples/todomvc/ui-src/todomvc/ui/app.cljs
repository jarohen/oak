(ns todomvc.ui.app
  (:require [reagent.core :as r]
            [oak.core :as o]
            [cljs.core.async :as a]
            [cljs-http.client :as http]
            [clojure.string :as s]
            [goog.events.KeyCodes :as kc])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

(defn done? [{:keys [status] :as todo}]
  (= status :done))

(defn toggle-all-component [{:keys [app db] :as ctx}]
  [:span
   [:input.toggle-all {:type "checkbox"
                       :checked (every? done? (vals (:todos db)))
                       :on-change #(o/send! ctx (o/ev ::toggle-all-done))}]
   [:label {:for "toggle-all"}
    "Mark all as complete"]])

(defn handle-toggle-all-event [{:keys [db] :as state} _]
  (o/update-db state update :todos (fn [todos]
                                     (let [new-status (if (every? done? (vals todos))
                                                        :active
                                                        :done)]
                                       (into {}
                                             (map (fn [[todo-id todo]]
                                                    [todo-id (assoc todo :status new-status)]))
                                             todos)))))

(defn new-todo [{{:keys [new-todo-label]} :app,
                 :as ctx}]
  [:input.new-todo {:value new-todo-label
                    :on-change #(o/send! ctx (o/ev ::new-todo-changed {:new-todo-label (-> % .-target .-value)}))
                    :on-key-down (fn [e]
                                   (when (= kc/ENTER (.-which e))
                                     (o/send! ctx (o/ev ::new-todo-submitted))))
                    :placeholder "What needs to be done?"}])

(defmulti handle-new-todo-event o/dispatch-by-type)

(defmethod handle-new-todo-event ::new-todo-changed [state {:keys [new-todo-label]}]
  (o/update-app state assoc :new-todo-label new-todo-label))

(defmethod handle-new-todo-event ::new-todo-submitted [{{:keys [new-todo-label]} :app, :as state} _]
  (let [{:keys [todo-id] :as todo} {:todo-id (random-uuid)
                                    :label new-todo-label
                                    :status :active}]
    (-> state
        (o/update-db assoc-in [:todos todo-id] todo)
        (o/update-app dissoc :new-todo-label))))

(defn todo-item [{:keys [app db] :as ctx} {:keys [todo-id]}]
  (let [{:keys [todo-id label status] :as todo} (get-in db [:todos todo-id])]
    [:li {:class (when (done? todo)
                   "completed")}
     [:input.toggle {:type "checkbox"
                     :checked (done? todo)
                     :on-change #(o/send! ctx (o/ev ::todo-toggled {:todo-id todo-id}))}]
     [:label label]
     [:button.destroy {:on-click #(o/send! ctx (o/ev ::todo-deleted {:todo-id todo-id}))}]]))


(defn todo-list [{{:keys [todo-filter]} :app, :keys [db], :as ctx}]
  [:ul.todo-list
   (doall
    (for [{:keys [todo-id]} (->> (vals (:todos db))
                                 (filter (comp (case todo-filter
                                                 :all #{:done :active}
                                                 :active #{:active}
                                                 :completed #{:done})
                                               :status))
                                 (sort-by (comp s/lower-case :label)))]
      ^{:key (str todo-id)}
      [todo-item (-> ctx
                     (o/narrow ::todo-items todo-id))
       {:todo-id todo-id}]))])

(def initial-todo-list-state
  {:todo-filter :all})

(defmulti handle-todo-list-event o/dispatch-by-type)

(defmethod handle-todo-list-event ::todo-toggled [state {:keys [todo-id]}]
  (-> state
      (o/update-db update-in [:todos todo-id :status] {:active :done, :done :active})))

(defmethod handle-todo-list-event ::todo-deleted [state {:keys [todo-id]}]
  (-> state
      (o/update-db update :todos dissoc todo-id)))

(defmethod handle-todo-list-event ::clear-completed [state _]
  (-> state
      (o/update-db update :todos (fn [todos]
                                   (into {}
                                         (remove (comp done? val))
                                         todos)))))

(defmethod handle-todo-list-event ::filter-updated [state {:keys [new-filter]}]
  (-> state
      (o/update-app assoc :todo-filter new-filter)))

(defn todo-count [{:keys [db]}]
  (let [items-left (count (remove done? (vals (:todos db))))]
    [:span.todo-count
     (str items-left
          " "
          (if (= 1 items-left)
            "item"
            "items")
          " left")]))

(defn todo-filters [{{:keys [todo-filter]} :app, :as ctx}]
  (let [update-filter! (fn [new-filter]
                         #(o/send! ctx (o/ev ::filter-updated {:new-filter new-filter})))
        link-class (fn [todo-filter-option]
                     (when (= todo-filter todo-filter-option)
                       "selected"))]
    [:ul.filters
     [:li
      [:a {:href "#",
           :on-click (update-filter! :all)
           :class (link-class :all)}
       "All"]]
     [:li
      [:a {:href "#",
           :on-click (update-filter! :active)
           :class (link-class :active)}
       "Active"]]
     [:li
      [:a {:href "#",
           :on-click (update-filter! :completed)
           :class (link-class :completed)}
       "Completed"]]]))

(defn todo-clear [{:keys [db] :as ctx}]
  [:button.clear-completed {:on-click #(o/send! ctx (o/ev ::clear-completed))
                            :style {:display (if (seq (filter (comp #{:done} :status) (vals (:todos db))))
                                               "inline"
                                               "none")}}
   "Clear completed"])

(defn page-root [{:keys [app db] :as ctx}]
  [:div
   [:section.todoapp
    [:header#header
     [:h1 "todos"]
     [new-todo (-> ctx
                   (o/nest (o/ev ::new-todo))
                   (o/narrow ::new-todo))]]

    [:section.main
     [toggle-all-component (-> ctx
                               (o/nest (o/ev ::toggle-all)))]

     [todo-list (-> ctx
                    (o/nest (o/ev ::todo-list))
                    (o/narrow ::todo-list))]]

    [:footer.footer
     [todo-count ctx]
     [todo-filters (-> ctx (o/narrow ::todo-list) (o/nest (o/ev ::todo-list)))]
     [todo-clear (-> ctx (o/nest (o/ev ::todo-clear)))]]]

   [:footer.info
    [:p "Double-click to edit a todo"]
    [:p "Credits: "
     [:a {:href "https://twitter.com/jarohen"} "James Henderson"]]
    [:p "Part of " [:a {:href "http://todomvc.com"} "TodoMVC"]]]])

(defmulti handle-event o/dispatch-by-type)

(defmethod handle-event ::new-todo [state {:keys [oak/sub-event]}]
  (o/update-in-state state [::new-todo] handle-new-todo-event sub-event))

(defmethod handle-event ::todo-list [state {:keys [oak/sub-event]}]
  (o/update-in-state state [::todo-list] handle-todo-list-event sub-event))

(defmethod handle-event ::toggle-all [state {:keys [oak/sub-event]}]
  (handle-toggle-all-event state sub-event))

(defonce !state
  (r/atom {:app {::todo-list initial-todo-list-state}
           :db {}}))

(defn root-ctx []
  (o/->ctx {:handle-event handle-event
            :!state !state}))

(defn render-page! []
  (r/render-component [(fn []
                         [page-root (root-ctx)])]
                      (js/document.getElementById "app")))

(defn ^:export main []
  (render-page!))
