(ns oak.core
  (:require [clojure.string :as s]
            #?@(:cljs [[reagent.core :as r :include-macros true]
                       [cljs.reader :as edn]]))
  #?(:cljs (:require-macros [oak.core])))

(def ^:dynamic *local* nil)
(def ^:dynamic *db* nil)

(defmulti cmd!
  (fn [cmd cb]
    (:oak/cmd-type cmd)))

(defmulti handle
  (fn [state ev]
    (:oak/event-type ev)))

(defmethod handle :default [state {:keys [oak/event-type] :as ev}]
  #?(:cljs (js/console.warn "No handler for event-type" event-type))
  state)

(defn with-cmd [state [cmd-type cmd-args]]
  (-> state
      (update :oak/cmds (fnil conj []) {:oak/cmd-type cmd-type
                                        :oak/cmd-args cmd-args
                                        :oak/ctx (:oak/ctx state)})))

(declare send!)

(defn- handle-cmds! [cmds]
  (doseq [{:oak/keys [cmd-type cmd-args ctx]} cmds]
    (let [{:keys [oak/cmd-handlers]} ctx]
      ;; if cmd-handlers is provided, and there's no cmd-handler for this type,
      ;; it's a no-op
      (when-let [cmd! (if cmd-handlers
                        (get cmd-handlers cmd-type)
                        cmd!)]
        (cmd! (merge cmd-args {:oak/cmd-type cmd-type})
              (fn [ev]
                (when ev
                  (send! ctx ev))))))))

(defn fmap-cmd [f cmd]
  (fn [cb]
    (cmd (fn [ev]
           (cb (f ev))))))

(defn- apply% [f args]
  #(apply f % args))

(defn- get-focus [state]
  (get-in state [:oak/ctx :oak/focus]))

(defn get-local [state]
  (get-in state (into [:oak/app] (get-focus state))))

(defn assoc-local [state val]
  (assoc-in state (into [:oak/app] (get-focus state)) val))

(defn update-local [state f & args]
  (assoc-in state (into [:oak/app] (get-focus state)) (apply f (get-local state) args)))

(defn- handle* [{:oak/keys [app db ctx] :as state} [event-type event-args]]
  (if-let [handle (if-let [event-handlers (:oak/event-handlers ctx)]
                    (get event-handlers event-type)
                    handle)]
    (handle #:oak{:app app, :db db, :ctx ctx}
            (merge (or event-args {})
                   {:oak/event-type event-type}))

    state))

(defn- send! [{:oak/keys [!app !db focus] :as ctx} [event-type event-args]]
  (let [{:oak/keys [app db cmds] :as state} (handle* #:oak{:app @!app, :db @!db, :ctx ctx} [event-type event-args])]

    (reset! !app app)
    (reset! !db db)
    (handle-cmds! cmds)
    {:oak/app app, :oak/db db}))

(defn notify [state [event-type event-args :as ev]]
  (if-let [{listener-ctx :oak/ctx, [listener-event-type listener-event-args] :oak/listener-ev} (get-in state [:oak/ctx :oak/listener])]
    (let [{listener-focus :oak/focus} listener-ctx]
      (handle* (merge state #:oak{:ctx listener-ctx})
               [[listener-event-type event-type] (merge event-args listener-event-args)]))

    state))

(def ^:private ->reagent-ev
  (-> (fn [ev]
        (keyword (str "on-" (name ev))))
      memoize))

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
                 {value-k (*local* bind)
                  :on-change (fn [e]
                               #?(:cljs (.preventDefault e))
                               (swap! (:oak/!app ctx) assoc-in ((fnil into []) focus bind) (ev->value e))

                               (when on-change
                                 (on-change e)))})))

      (dissoc :oak/bind)))

(defn focus [el & focus]
  (-> el
      (vary-meta assoc :oak/focus (vec focus))))

(defn listen [el [event-type event-args :as ev]]
  (-> el
      (vary-meta assoc :oak/listener-ev ev)))

(defn- transform-el [el {:keys [oak/focus] :as ctx}]
  (letfn [(transform-el* [el]
            (cond
              (vector? el) (-> (let [[tag & [maybe-attrs & more :as params]] el]
                                 (or (when (keyword? tag)
                                       (when (map? maybe-attrs)
                                         (into [tag (-> maybe-attrs
                                                        (with-handlers ctx)
                                                        (with-binds ctx))]
                                               (mapv transform-el* more))))

                                     (when (and (fn? tag)
                                                (:oak/component? (meta tag)))
                                       (let [{new-focus :oak/focus, listener-ev :oak/listener-ev} (meta el)]
                                         (into [(tag (-> ctx
                                                         (cond-> new-focus (update :oak/focus (fnil into []) new-focus))
                                                         (cond-> listener-ev (assoc :oak/listener {:oak/listener-ev listener-ev
                                                                                                   :oak/ctx ctx}))))]
                                               params)))

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

(defn- reagent-class [{{:oak/keys [!app !db focus] :as ctx} :oak/ctx, :keys [display-name oak/transients oak/lifecycles oak/render] :as args}]
  (let [lifecycle-fns (->> lifecycles
                           (into {} (map (fn [[lifecycle [event-type event-args]]]
                                           [lifecycle #(send! ctx [event-type (merge event-args {:oak/lifecycle-args %&})])]))))

        update-in-focus (fn [m f & args]
                          (if focus
                            (apply update-in m focus f args)
                            (apply f m args)))]

    (-> (merge lifecycle-fns
               (when display-name {:display-name display-name})
               (when transients
                 {:component-will-mount (comp second
                                              (juxt (fn [& _]
                                                      (swap! !app update-in-focus merge (->> transients
                                                                                             (into {} (map (fn [[k v]]
                                                                                                             [k (v)]))))))
                                                    (or (:component-will-mount lifecycle-fns)
                                                        (fn [& _] {:oak/app @!app, :oak/db @!db}))))

                  :component-will-unmount (comp second
                                                (juxt (fn [& _]
                                                        (swap! !app update-in-focus #(apply dissoc % (keys transients))))
                                                      (or (:component-will-unmount lifecycle-fns)
                                                          (fn [& _] {:oak/app @!app, :oak/db @!db}))))})

               {:reagent-render (fn [& params]
                                  (binding [*local* (tracker !app focus)
                                            *db* (tracker !db [])]
                                    (-> (apply render params)
                                        (transform-el ctx))))})
        #?(:cljs r/create-class))))

(defn- ratom [initial-val]
  #?(:clj (atom initial-val)
     :cljs (r/atom initial-val)))

(defn- build-root [{:oak/keys [component event-handlers cmd-handlers app db] :or {app {}, db {}}}]
  (let [[component-f & params] component
        ctx {:oak/!app (ratom app)
             :oak/!db (ratom db)
             :oak/cmd-handlers cmd-handlers
             :oak/event-handlers event-handlers}]

    (-> (into [(reagent-class {:oak/ctx ctx
                               :oak/render (fn [& params]
                                             (into [(component-f ctx)] params))})]
              params)

        (with-meta {:oak/ctx ctx}))))

(defn ^:export mount! [$el {:oak/keys [component event-handlers cmd-handlers] :as opts}]
  (-> (build-root opts)
      #?(:cljs (r/render-component $el))))

#?(:cljs
   (defn- ^:export js->clj* [obj]
     (js->clj obj :keywordize-keys true)))

#?(:cljs
   (defn- ^:export edn->clj* [obj]
     (edn/read-string obj)))

(defn- parse-body-forms [body]
  (loop [[form & more-forms :as body] body
         res {}]
    (if form
      (let [m (meta form)]
        (cond
          (:oak/transient m) (recur more-forms {:oak/transients form})
          (:oak/lifecycle m) (recur more-forms {:oak/lifecycles form})
          :else (merge res {:body body})))

      (throw (ex-info "Missing component" {})))))

(defn ->component* {:style/indent :defn} [sym params body]
  (let [{:keys [oak/transients oak/lifecycles body]} (parse-body-forms body)]
    `(let [transients# ~(some->> (second transients)
                                 (into {} (map (fn [[k v]]
                                                 `[~k (fn [] ~v)]))))
           render# (fn ~sym [~@params]
                     ~@(if transients
                         `[(let [~(first transients) (*local*)]
                             ~@body)]
                         body))]
       (-> (fn [ctx#]
             (#'reagent-class {:oak/ctx ctx#
                               :oak/transients transients#
                               :oak/lifecycles ~lifecycles
                               :display-name ~(str (str *ns*) "/" (name sym))
                               :oak/render render#}))
           memoize
           (with-meta {:oak/component? true})))))

#?(:clj
   (defmacro ->component {:style/indent :defn}
     [sym params & body]
     (->component* sym params body)))

#?(:clj
   (defmacro defc [sym params & body]
     `(def ~sym
        ~(->component* sym params body))))

#?(:clj
   (defn app-js [{:oak/keys [app db html component script-src]}]
     (let [[component-f & args] component]
       (s/join "\n" [(format "<div>%s</div>" (or html ""))
                     "<script>window.oak_root = document.scripts[document.scripts.length - 1].previousSibling.previousSibling;</script>"
                     (format "<script src=\"%s\" type=\"text/javascript\"></script>" script-src)
                     (format "<script>oak.core.mount_BANG_(oak_root, oak.core.js__GT_clj_STAR_({%s}))</script>"
                             (s/join ", " [(format "\"oak/component\": [%s]"
                                                   (s/join ", " (into [(format "%s.%s" (munge (namespace component-f)) (munge (name component-f)))]
                                                                      (map pr-str args))))
                                           (format "\"oak/app\": oak.core.edn__GT_clj_STAR_(%s)" (pr-str (pr-str (or app {}))))
                                           (format "\"oak/db\": oak.core.edn__GT_clj_STAR_(%s)" (pr-str (pr-str (or db {}))))]))]))))
