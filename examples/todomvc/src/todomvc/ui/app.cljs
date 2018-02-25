 (ns todomvc.ui.app
  (:require [oak.core :as oak :include-macros true]
            [clojure.string :as s]))

(enable-console-print!)

(defn done? [{:keys [status] :as todo}]
  (= status :done))

(defmethod oak/handle ::toggle-all [state _]
  (-> state
      (update-in [:oak/db :todos] (fn [todos]
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
                       :oak/on {:change [::toggle-all]}
                       }]
   [:label {:for "toggle-all"}
    "Mark all as complete"]])

(defmethod oak/handle ::new-todo-submitted [state {:keys [new-todo-label]}]
  (let [todo-id (random-uuid)]
    (-> state
        (assoc-in [:oak/db :todos todo-id] {:todo-id todo-id
                                            :label new-todo-label
                                            :status :active})
        (update :oak/app assoc :new-todo-label ""))))

(oak/defc new-todo []
  #_(oak/with-transient-state [{:keys [new-todo-label]} {:new-todo-label ""}])
  (let [{:keys [new-todo-label]} (oak/*app*)]
    [:form {:oak/on {:submit [::new-todo-submitted {:new-todo-label new-todo-label}]}}
     [:input.new-todo {:oak/bind [:new-todo-label]
                       :placeholder "What needs to be done?"}]]))

(defmethod oak/handle ::start-editing-todo [state {:keys [todo-id]}]
  (-> state
      (update :oak/app merge {:editing? true
                              :new-label (get-in state [:oak/db :todos todo-id :label])})))

(defmethod oak/handle ::stop-editing-todo [state {:keys [todo-id]}]
  (-> state
      (assoc :oak/app {:editing? false})
      (assoc-in [:oak/db :todos todo-id :label] (get-in state [:oak/app :new-label]))))

(oak/defc todo-item [{:keys [todo-id]}]
  #_(oak/with-transient-state [{:keys [editing? new-label]} {:editing? false}])
  (let [{:keys [editing? new-label]} (oak/*app*)
        {:keys [todo-id label status] :as todo} (oak/*db* [:todos todo-id])]
    [:li {:oak/class #{(cond
                         editing? :editing
                         (done? todo) :completed)}}
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
      (update-in [:oak/db :todos todo-id :status] {:active :done, :done :active})))

(defmethod oak/handle ::todo-deleted [state {:keys [todo-id]}]
  (-> state
      (update-in [:oak/db :todos] dissoc todo-id)))

(oak/defc todo-list [{:keys [todo-filter]}]
  [:ul.todo-list
   (doall
    (for [{:keys [todo-id]} (->> (vals (oak/*db* [:todos]))
                                 (filter (comp (case todo-filter
                                                 :all #{:done :active}
                                                 :active #{:active}
                                                 :completed #{:done})
                                               :status))
                                 (sort-by (comp s/lower-case :label)))]
      ^{:key (str todo-id)
        :oak/focus [:items todo-id]}
      [todo-item {:todo-id todo-id}]))])

(defmethod oak/handle ::filter-updated [state {:keys [new-filter]}]
  (-> state
      (assoc-in [:oak/app :todo-filter] new-filter)))

(oak/defc todo-count []
  (let [items-left (count (remove done? (vals (oak/*db* [:todos]))))]
    [:span.todo-count
     (str items-left
          " "
          (if (= 1 items-left)
            "item"
            "items")
          " left")]))

(oak/defc todo-filters []
  (let [link-class (fn [todo-filter-option]
                     (when (= (oak/*app* [:todo-filter]) todo-filter-option)
                       :selected))]
    [:ul.filters
     [:li
      [:a {:oak/class (link-class :all)
           :oak/on {:click [::filter-updated {:new-filter :all}]}}
       "All"]]
     [:li
      [:a {:oak/class (link-class :active)
           :oak/on {:click [::filter-updated {:new-filter :active}]}}
       "Active"]]
     [:li
      [:a {:oak/class (link-class :completed)
           :oak/on {:click [::filter-updated {:new-filter :completed}]}}
       "Completed"]]]))

(defmethod oak/handle ::clear-completed [state _]
  (-> state
      (update-in [:oak/db :todos] (fn [todos]
                                    (into {}
                                          (remove (comp done? val))
                                          todos)))))

(oak/defc todo-clear []
  [:button.clear-completed {:oak/on {:click [::clear-completed]}
                            :style {:display (if (seq (filter (comp #{:done} :status) (vals (oak/*db* [:todos]))))
                                               "inline"
                                               "none")}}
   "Clear completed"])

(oak/defc page-root []
  #_(oak/with-transient-state [_ {:todo-filter :all}])
  (let [todo-filter (or (oak/*app* [:todo-filter]) :all)]
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
      [:p "Part of " [:a {:href "http://todomvc.com"} "TodoMVC"]]]]))

(defn ^:export main []
  (oak/render! {:$el (js/document.getElementById "app")
                :state {:oak/db {:todos {:foo {:todo-id :foo
                                               :status :active
                                               :label "Foo"}
                                         :bar {:todo-id :bar
                                               :status :active
                                               :label "Bar"}}}}
                :component [page-root]}))
