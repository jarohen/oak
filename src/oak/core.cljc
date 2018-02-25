(ns oak.core
  (:require [clojure.string :as s]
            #?(:cljs [reagent.core :as r :include-macros true]))
  #?(:cljs (:require-macros [oak.core])))

(def ^:private ^:dynamic *ctx* nil)
(def ^:dynamic *app* nil)
(def ^:dynamic *db* nil)

(defmulti handle
  (fn [state ev]
    (:oak/event-type ev)))

(defmethod handle :default [state {:keys [oak/event-type] :as ev}]
  #?(:cljs (js/console.warn "No handler for event-type" event-type))
  state)

#_(defn with-cmd [ctx cmd]
  (-> ctx
      (vary-meta update ::cmds (fnil conj []) cmd)))

#_(defn- handle-cmds! [ctx]
  (doseq [cmd (::cmds (meta ctx))]
    (cmd (fn [ev]
           (when ev
             (send! ctx ev))))))

#_(defn fmap-cmd [f cmd]
    (fn [cb]
      (cmd (fn [ev]
             (cb (f ev))))))

(defn send! [{:oak/keys [!app !db focus]} [event-type event-args]]
  (let [{:oak/keys [app db] :as res} (handle {:oak/app (get-in @!app focus), :oak/db @!db}
                                             (merge (or event-args {})
                                                    {:oak/event-type event-type}))]
    (swap! !app (if (seq focus) #(assoc-in %1 focus %2) #(merge %1 %2)) app)
    (reset! !db db)))

(def ->reagent-ev
  (-> (fn [ev]
        (keyword (str "on-" (name ev))))
      memoize))

(defn with-classes [{:keys [oak/classes] :as attrs}]
  (-> (merge attrs
             (when classes
               {:class (s/join " " (into #{} (comp (keep identity) (map name)) classes))}))
      (dissoc :oak/classes)))

(defn with-handlers [attrs ctx]
  (-> (merge attrs
             (->> (:oak/on attrs)
                  (into {} (keep (fn [[dom-ev ev]]
                                   [(->reagent-ev dom-ev) (fn [e]
                                                            (.preventDefault e)
                                                            (send! ctx ev))])))))

      (dissoc :oak/on)))

(defn with-binds [{:keys [oak/bind on-change] :as attrs} {:oak/keys [focus] :as ctx}]
  (-> (merge attrs
             (when bind
               (case (:type attrs)
                 ;; TODO more types - checkbox + radio?
                 {:value (*app* bind)
                  :on-change (fn [e]
                               (.preventDefault e)
                               (swap! (:oak/!app ctx) assoc-in ((fnil into []) focus bind) (.. e -target -value))

                               (when on-change
                                 (on-change e)))})))

      (dissoc :oak/bind)))

(defn with-focus [ctx focus]
  (cond-> ctx
    focus (update :oak/focus (fnil into []) focus)))

(defn- transform-el [el ctx]
  (letfn [(transform-el* [el]
            (cond
              (vector? el) (-> (let [[tag maybe-attrs & more] el]
                                 (or (when (keyword? tag)
                                       (when (map? maybe-attrs)
                                         (into [tag (-> maybe-attrs
                                                        (with-handlers ctx)
                                                        with-classes
                                                        (with-binds ctx))]
                                               (mapv transform-el* more))))

                                     (when (and (fn? tag)
                                                (:oak/component? (meta tag)))
                                       (into [tag (-> ctx (with-focus (:oak/focus (meta el))))]
                                             (when (or maybe-attrs (seq more))
                                               (cons maybe-attrs more))))

                                     (into [] (map transform-el*) el)))
                               (with-meta (meta el)))
              (seq? el) (map transform-el* el)

              :else el))]

    (transform-el* el)))

(defn- tracker [!atom focus]
  (fn [path-or-fn]
    (let [lookup (comp (if (fn? path-or-fn)
                         path-or-fn
                         #(get-in % path-or-fn))
                       #(get-in % focus)
                       deref)]
      #?(:clj (lookup !atom)
         :cljs @(r/track lookup !atom)))))

(defn- reagent-class [{:keys [ctx display-name render]}]
  (-> (merge (when display-name {:display-name display-name})
             {:reagent-render (fn [{:oak/keys [!app !db focus] :as ctx} & params]
                                (binding [*app* (tracker !app focus)
                                          *db* (tracker !db [])
                                          *ctx* ctx]
                                  (-> (apply render ctx params)
                                      (transform-el ctx))))})
      #?(:cljs r/create-class)))

(defn- ratom [initial-val]
  #?(:clj (atom initial-val)
     :cljs (r/atom initial-val)))

(defn render! [{{:keys [oak/app oak/db] :as state} :state, :keys [$el component]}]
  (let [[component-f & params] component
        ctx {:oak/!app (ratom app)
             :oak/!db (ratom db)}]
    (-> (into [(reagent-class {:ctx ctx
                               :render (fn [ctx & params]
                                         (into [component-f ctx] params))})
               ctx]
              params)

        #?(:cljs (r/render-component $el)))))

#?(:clj
   (defmacro defc [sym params & body]
     `(def ~sym
        (-> (fn ~sym [ctx# ~@params]
              (reagent-class {:ctx ctx#
                              :display-name (str ~(str *ns*) "/" ~(name sym))
                              :render (fn [_# ~@params]
                                        ~@body)}))
            (with-meta {:oak/component? true})))))
