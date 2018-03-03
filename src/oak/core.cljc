(ns oak.core
  (:require [clojure.string :as s]
            #?@(:cljs [[reagent.core :as r :include-macros true]]))
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

(defn assoc-local [state val]
  (assoc state :oak/local (-> val
                              (vary-meta assoc :oak/transient-keys (:oak/transient-keys (meta (:oak/local state)))))))

(defn update-local [state f & args]
  (assoc-local state (apply f (:oak/local state) args)))

(defn update-db [state f & args]
  (apply update state :oak/db f args))

(defn- assoc-in-focus [app focus local]
  (if (seq focus)
    (assoc-in app focus local)
    (merge app local)))

(defn- handle* [{:oak/keys [app local db ctx] :as state} [event-type event-args]]
  (if-let [handle (if-let [event-handlers (:oak/event-handlers ctx)]
                    (get event-handlers event-type)
                    handle)]
    (handle #:oak{:app app, :local local, :db db, :ctx ctx}
            (merge (or event-args {})
                   {:oak/event-type event-type}))

    state))

(defn- send! [{:oak/keys [!app !db focus] :as ctx} [event-type event-args]]
  (let [app @!app
        {:oak/keys [app local db cmds] :as state} (handle* #:oak{:app app, :local (get-in app focus), :db @!db, :ctx ctx} [event-type event-args])
        app (assoc-in-focus app focus local)]

    (reset! !app app)
    (reset! !db db)
    (handle-cmds! cmds)
    {:oak/app app, :oak/db db}))

(defn notify [state [event-type event-args :as ev]]
  (let [{:oak/keys [app db local ctx]} state]
    (if-let [{listener-ctx :oak/ctx, [listener-event-type listener-event-args] :oak/listener-ev} (:oak/listener ctx)]
      (let [{child-focus :oak/focus} ctx
            {listener-focus :oak/focus} listener-ctx
            app (assoc-in-focus app child-focus local)

            {:oak/keys [local] :as state} (handle* (merge state #:oak{:app app, :local (get-in app listener-focus), :ctx listener-ctx})
                                                   [[listener-event-type event-type] (merge event-args listener-event-args)])

            app (assoc-in-focus app listener-focus local)]

        (merge state #:oak{:app app
                           :local (get-in app child-focus)}))

      state)))

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

(defn- reagent-class [{{:oak/keys [!app !db focus] :as ctx} :oak/ctx, :keys [display-name oak/->transients oak/lifecycles oak/render] :as args}]
  (let [lifecycle-fns (->> lifecycles
                           (into {} (map (fn [[lifecycle [event-type event-args]]]
                                           [lifecycle #(send! ctx [event-type (merge event-args {:oak/lifecycle-args %&})])]))))]

    (-> (merge lifecycle-fns
               (when display-name {:display-name display-name})
               (when ->transients
                 {:component-will-mount (comp second
                                              (juxt (fn [& _]
                                                      (let [transients (->transients)]
                                                        (swap! !app (fn [app]
                                                                      (-> app
                                                                          (assoc-in-focus focus (-> (merge (get-in app focus) transients)
                                                                                                    (with-meta {:oak/transient-keys (set (keys transients))}))))))))
                                                    (or (:component-will-mount lifecycle-fns)
                                                        (fn [& _] {:oak/app @!app, :oak/db @!db}))))

                  :component-will-unmount (comp second
                                                (juxt (fn [& _]
                                                        (swap! !app (fn [app]
                                                                      (let [local (get-in app focus)]
                                                                        (-> app
                                                                            (assoc-in-focus focus (apply dissoc local (:oak/transient-keys (meta local)))))))))
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

(defn- build-root [{:oak/keys [component event-handlers cmd-handlers]}]
  (let [[component-f & params] component
        ctx {:oak/!app (ratom {})
             :oak/!db (ratom {})
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

#?(:clj
   (defmacro ->component {:style/indent :defn}
     [sym params & body]
     (let [{:keys [oak/transients oak/lifecycles body]} (parse-body-forms body)]
       `(let [->transients# ~(when transients
                               `(fn []
                                  ~(second transients)))
              render# (fn ~sym [~@params]
                        ~@(if transients
                            `[(let [~(first transients) (*local*)]
                                ~@body)]
                            body))]
          (-> (fn [ctx#]
                (#'reagent-class {:oak/ctx ctx#
                                  :oak/->transients ->transients#
                                  :oak/lifecycles ~lifecycles
                                  :display-name ~(str (str *ns*) "/" (name sym))
                                  :oak/render render#}))
              memoize
              (with-meta {:oak/component? true}))))))

#?(:clj
   (defmacro defc [sym params & body]
     `(def ~sym
        (->component ~sym ~params ~@body))))

#?(:clj
   (defn app-js [{:oak/keys [html component script-src]}]
     (let [[component-f & args] component]
       (s/join "\n" [(format "<div>%s</div>" (or html ""))
                     "<script>window.oak_root = document.scripts[document.scripts.length - 1].previousSibling.previousSibling;</script>"
                     (format "<script src=\"%s\" type=\"text/javascript\"></script>" script-src)
                     (format "<script>oak.core.mount_BANG_(oak_root, oak.core.js__GT_clj_STAR_({\"oak/component\": [%s]}))</script>"
                             (s/join ", " (into [(format "%s.%s" (munge (namespace component-f)) (munge (name component-f)))]
                                                (map pr-str args))))]))))
