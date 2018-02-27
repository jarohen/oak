(ns oak.core
  (:require [clojure.string :as s]
            #?(:cljs [reagent.core :as r :include-macros true]))
  #?(:cljs (:require-macros [oak.core])))

(def ^:dynamic *app* nil)
(def ^:dynamic *db* nil)

(defmulti handle
  (fn [state ev]
    (:oak/event-type ev)))

(defmethod handle :default [state {:keys [oak/event-type] :as ev}]
  #?(:cljs (js/console.warn "No handler for event-type" event-type))
  state)

(defn with-cmd [state cmd]
  (-> state
      (vary-meta update :oak/cmds (fnil conj []) {:oak/cmd cmd
                                                  :oak/ctx (:oak/ctx state)})))

(declare send!)

(defn- handle-cmds! [state ctx]
  (doseq [{:oak/keys [cmd ctx], :or {ctx ctx}} (:oak/cmds (meta state))]
    (cmd (fn [ev]
           (when ev
             (send! ctx ev))))))

(defn fmap-cmd [f cmd]
  (fn [cb]
    (cmd (fn [ev]
           (cb (f ev))))))

(defn update-app [state f & args]
  (apply update state :oak/app f args))

(defn update-db [state f & args]
  (apply update state :oak/db f args))

(defn- send! [{:oak/keys [!app !db focus] :as ctx} [event-type event-args]]
  (let [{:oak/keys [app db] :as state} (handle {:oak/app (get-in @!app focus), :oak/db @!db, :oak/ctx ctx}
                                               (merge (or event-args {})
                                                      {:oak/event-type event-type}))]
    (swap! !app (if (seq focus) #(assoc-in %1 focus %2) #(merge %1 %2)) app)
    (reset! !db db)
    (doto state (handle-cmds! ctx))))

(def ^:private ->reagent-ev
  (-> (fn [ev]
        (keyword (str "on-" (name ev))))
      memoize))

(defn- with-classes [{:keys [oak/classes] :as attrs}]
  (-> (merge attrs
             (when classes
               {:class (s/join " " (into #{} (comp (keep identity) (map name)) classes))}))
      (dissoc :oak/classes)))

(defn- with-handlers [attrs ctx]
  (-> (merge attrs
             (->> (:oak/on attrs)
                  (into {} (keep (fn [[dom-ev ev]]
                                   [(->reagent-ev dom-ev) (fn [e]
                                                            #?(:cljs (.preventDefault e))
                                                            (send! ctx ev))])))))

      (dissoc :oak/on)))

(defn- with-binds [{:keys [oak/bind on-change] :as attrs} {:oak/keys [focus] :as ctx}]
  (-> (merge attrs
             (when bind
               (let [[value-k ev->value] (case (:type attrs)
                                           ;; TODO more types - checkbox + radio?
                                           [:value #?(:clj :oak/ev-value
                                                      :cljs #(.. % -target -value))])]
                 {value-k (*app* bind)
                  :on-change (fn [e]
                               #?(:cljs (.preventDefault e))
                               (swap! (:oak/!app ctx) assoc-in ((fnil into []) focus bind) (ev->value e))

                               (when on-change
                                 (on-change e)))})))

      (dissoc :oak/bind)))

(defn focus [el & focus]
  (-> el
      (vary-meta assoc :oak/focus (vec focus))))

(defn- transform-el [el ctx]
  (letfn [(transform-el* [el]
            (cond
              (vector? el) (-> (let [[tag & [maybe-attrs & more :as params]] el]
                                 (or (when (keyword? tag)
                                       (when (map? maybe-attrs)
                                         (into [tag (-> maybe-attrs
                                                        (with-handlers ctx)
                                                        with-classes
                                                        (with-binds ctx))]
                                               (mapv transform-el* more))))

                                     (when (and (fn? tag)
                                                (:oak/component? (meta tag)))
                                       (into [(tag (-> ctx
                                                       (update :oak/focus (fnil into []) (:oak/focus (meta el)))))]
                                             params))

                                     (into [] (map transform-el*) el)))
                               (with-meta (meta el)))
              (seq? el) (map transform-el* el)

              :else el))]

    (transform-el* el)))

(defn- tracker [!atom focus]
  (fn [& path-or-fn]
    (let [lookup (comp (cond
                         (fn? (first path-or-fn)) (first path-or-fn)
                         (vector? (first path-or-fn)) #(get-in % (first path-or-fn))
                         :else #(get-in % path-or-fn))

                       #(get-in % focus)
                       deref)]
      #?(:clj (lookup !atom)
         :cljs @(r/track lookup !atom)))))

(defn- reagent-class [{{:oak/keys [!app !db focus] :as ctx} :ctx, :keys [display-name render]}]
  (-> (merge (when display-name {:display-name display-name})
             {:reagent-render (fn [& params]
                                (binding [*app* (tracker !app focus)
                                          *db* (tracker !db [])]
                                  (-> (apply render params)
                                      (transform-el ctx))))})
      #?(:cljs r/create-class)))

(defn- ratom [initial-val]
  #?(:clj (atom initial-val)
     :cljs (r/atom initial-val)))

(defn mount! [{{:keys [oak/app oak/db] :as state} :state, :keys [$el component]}]
  (let [[component-f & params] component
        ctx {:oak/!app (ratom app)
             :oak/!db (ratom db)}]

    (handle-cmds! state ctx)

    (-> (into [(reagent-class {:ctx ctx
                               :render (fn [& params]
                                         (into [component-f ctx] params))})]
              params)

        #?(:cljs (r/render-component $el)))))

#?(:clj
   (defmacro defc [sym params & body]
     `(def ~sym
        (-> (fn [ctx#]
              (fn ~sym [~@params]
                (#'reagent-class {:ctx ctx#
                                  :display-name (str ~(str *ns*) "/" ~(name sym))
                                  :render (fn [~@params]
                                            ~@body)})))
            (with-meta {:oak/component? true})))))
