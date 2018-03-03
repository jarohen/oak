 (ns todomvc.ui.app
  (:require [oak.core :as oak :include-macros true]
            [clojure.string :as s]))

(enable-console-print!)

(defn done? [{:keys [status] :as todo}]
  (= status :done))

(defmethod oak/handle ::toggle-all [state _]
  (-> state
      (oak/update-db update :todos (fn [todos]
                                     (into {}
                                           (map (let [new-status (if (every? done? (vals todos))
                                                                   :active
                                                                   :done)]
                                                  (fn [[todo-id todo]]
                                                    [todo-id (assoc todo :status new-status)])))
                                           todos)))))

(oak/defc toggle-all-component []
  [:span
   [:input.toggle-all {:type "checkbox"
                       :checked (oak/*db* (comp #(every? done? %) vals :todos))
                       :oak/on {:change [::toggle-all]}}]
   [:label {:for "toggle-all"}
    "Mark all as complete"]])

(defmethod oak/handle ::new-todo-submitted [state {:keys [new-todo-label]}]
  (let [todo-id (random-uuid)]
    (-> state
        (oak/update-db assoc-in [:todos todo-id] {:todo-id todo-id
                                                  :label new-todo-label
                                                  :status :active})
        (oak/update-local assoc :new-todo-label ""))))

(oak/defc new-todo []
  ^:oak/transient [{:keys [new-todo-label]} {:new-todo-label ""}]

  [:form {:oak/on {:submit [::new-todo-submitted {:new-todo-label new-todo-label}]}}
   [:input.new-todo {:oak/bind [:new-todo-label]
                     :placeholder "What needs to be done?"}]])

(defmethod oak/handle ::start-editing-todo [state {:keys [todo-id]}]
  (-> state
      (oak/update-local merge {:editing? true
                               :new-label (get-in state [:oak/db :todos todo-id :label])})))

(defmethod oak/handle ::stop-editing-todo [state {:keys [todo-id]}]
  (-> state
      (oak/update-local (constantly {:editing? false
                                     :new-label nil}))
      (oak/update-db assoc-in [:todos todo-id :label] (get-in state [:oak/local :new-label]))))

(oak/defc todo-item [{:keys [todo-id]}]
  ^:oak/transient [{:keys [editing? new-label]} {:editing? false}]

  (let [{:keys [todo-id label status] :as todo} (oak/*db* [:todos todo-id])]
    [:li {:class #{(cond
                     editing? "editing"
                     (done? todo) "completed")}}
     (if editing?
       [:form {:oak/on {:submit [::stop-editing-todo {:todo-id todo-id}]}}
        [:input.edit {:oak/bind [:new-label]
                      :oak/on {:blur [::stop-editing-todo {:todo-id todo-id}]}
                      :auto-focus true}]]

       [:div.view
        [:input.toggle {:type :checkbox
                        :checked (done? todo)
                        :oak/on {:change [::todo-toggled {:todo-id todo-id}]}}]
        [:label {:oak/on {:double-click [::start-editing-todo {:todo-id todo-id}]}}
         label
         (when editing?
           " [editing]")]
        [:button.destroy {:oak/on {:click [::todo-deleted {:todo-id todo-id}]}}]])]))

(defmethod oak/handle ::todo-toggled [state {:keys [todo-id]}]
  (-> state
      (oak/update-db update-in [:todos todo-id :status] {:active :done, :done :active})))

(defmethod oak/handle ::todo-deleted [state {:keys [todo-id]}]
  (-> state
      (oak/update-db update :todos dissoc todo-id)))

(oak/defc todo-list [{:keys [todo-filter]}]
  [:ul.todo-list
   (doall
    (for [{:keys [todo-id]} (->> (vals (oak/*db* [:todos]))
                                 (filter (comp (case todo-filter
                                                 :all #{:done :active}
                                                 :active #{:active}
                                                 :completed #{:done}
                                                 #{:done :active})
                                               :status))
                                 (sort-by (comp s/lower-case :label)))]
      ^{:key (str todo-id)
        :oak/focus [:items todo-id]}
      [todo-item {:todo-id todo-id}]))])

(defmethod oak/handle ::filter-updated [state {:keys [new-filter]}]
  (-> state
      (oak/update-local assoc :todo-filter new-filter)))

(oak/defc todo-count []
  (let [items-left (oak/*db* (comp count #(remove done? %) vals :todos))]
    [:span.todo-count
     (str items-left " " (case items-left 1 "item" "items") " left")]))

(oak/defc todo-filters []
  (let [link-class (fn [todo-filter-option]
                     (when (= (oak/*local* :todo-filter) todo-filter-option)
                       "selected"))]
    [:ul.filters
     [:li
      [:a {:class (link-class :all)
           :oak/on {:click [::filter-updated {:new-filter :all}]}}
       "All"]]
     [:li
      [:a {:class (link-class :active)
           :oak/on {:click [::filter-updated {:new-filter :active}]}}
       "Active"]]
     [:li
      [:a {:class (link-class :completed)
           :oak/on {:click [::filter-updated {:new-filter :completed}]}}
       "Completed"]]]))

(defmethod oak/handle ::clear-completed [state _]
  (-> state
      (oak/update-db update :todos (fn [todos]
                                     (into {}
                                           (remove (comp done? val))
                                           todos)))))

(oak/defc todo-clear []
  [:button.clear-completed {:oak/on {:click [::clear-completed]}
                            :style {:display (if (seq (filter (comp #{:done} :status) (vals (oak/*db* [:todos]))))
                                               "inline"
                                               "none")}}
   "Clear completed"])

(defmethod oak/handle ::page-will-mount [state _]
  (-> state
      (oak/update-db (constantly {:todos {:foo {:todo-id :foo
                                                :status :active
                                                :label "Foo"}
                                          :bar {:todo-id :bar
                                                :status :active
                                                :label "Bar"}}}))))

(oak/defc ^:export page-root []
  ^:oak/transient [{:keys [todo-filter]} {:todo-filter :all}]
  ^:oak/lifecycle {:component-will-mount [::page-will-mount]}

  [:div
   [:section.todoapp
    [:header#header
     [:h1 "todos"]
     [new-todo]]

    [:section.main
     [toggle-all-component]
     [todo-list {:todo-filter todo-filter}]]

    [:footer.footer
     [todo-count]
     [todo-filters]
     [todo-clear]]]

   [:footer.info
    [:p "Double-click to edit a todo"]
    [:p "Part of " [:a {:href "http://todomvc.com"} "TodoMVC"]]]])
