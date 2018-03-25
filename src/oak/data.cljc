(ns oak.data
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [clojure.walk :as w])
  #?(:cljs (:require-macros [oak.data])))

(def !entities
  (atom {}))

(s/def ::selects (s/coll-of keyword? :kind set?))
(s/def ::props (s/map-of keyword? any?))
(s/def ::facet keyword?)
(s/def ::facets (s/coll-of ::facet :kind set?))
(s/def ::key keyword?)
(s/def ::joins (s/map-of keyword? (s/or :many (s/tuple keyword?)
                                        :one keyword?)))

(s/def ::cardinality #{:many :one})

(defn transform-entity-spec [entity-spec]
  (-> (s/conform (s/keys :opt [::selects ::props ::facets ::key ::joins])
                 entity-spec)
      (update ::joins (fn [joins]
                        (->> joins
                             (into {} (map (fn [[k [cardinality join-opt]]]
                                             (let [entity (case cardinality
                                                            :many (first join-opt)
                                                            :one join-opt)]
                                               [k {::entity entity
                                                   ::cardinality cardinality}])))))))))

#?(:clj
   (defmacro defentity [entity entity-spec]
     `(do
        (swap! !entities assoc ~entity ~(transform-entity-spec entity-spec))
        ~entity)))

(defmulti fetch-entity
  (fn [{::keys [from]} opts]
    from))

(defn ^:dynamic *fetch-entity* [query opts]
  (fetch-entity query opts))

(s/def ::query-param
  (s/or ::props (s/map-of keyword? any?)
        ::facet keyword?
        ::sub-query (s/spec ::sub-query)))

(s/def ::sub-query
  (s/cat ::from keyword?
         ::query-params (s/* ::query-param)))

(s/def ::query
  (s/cat ::from keyword?
         ::select-spec (s/spec (s/? (s/cat ::select-key keyword?
                                           ::select-vals (s/+ any?))))
         ::query-params (s/* ::query-param)))

(def facet-presence-kw
  (-> (fn [facet]
        (keyword (namespace facet) (str (name facet) "?")))
      memoize))

(defn- parse-query [query]
  (let [conformed-query (s/conform ::query query)]
    (if-not (= conformed-query ::s/invalid)
      (letfn [(parse-query* [{::keys [from select-spec query-params]}]
                (reduce (fn [acc [qp-type qp]]
                          (case qp-type
                            ::sub-query (update acc ::sub-queries (fnil conj []) (parse-query* qp))
                            ::facet (update acc ::facets (fnil conj #{}) qp)
                            ::props (update acc ::props (fnil merge {}) qp)))

                        {::from from
                         ::select-spec select-spec}

                        query-params))]
        (parse-query* conformed-query))

      (throw (ex-info "Invalid query" {:query query
                                       :explain-data (s/explain-data ::query query)})))))

(defn apply-query [{::keys [from select-spec facets props]}]
  (let [keep? (apply every-pred
                     (if-let [{::keys [select-key select-vals]} select-spec]
                       (comp (set select-vals) select-key)
                       identity)

                     (for [[prop values] props
                           :when (some? values)]
                       (cond
                         (boolean? values) #(= (get % prop) values)
                         (set? values) #(contains? values (get % prop))
                         :else identity)))

        deselected-facets (set/difference (get-in @!entities [from ::facets]) facets)]

    (fn [instance]
      (when (keep? instance)
        (apply dissoc instance deselected-facets)))))

(defn by-key [res f]
  (if (map? res)
    res
    (-> (reduce (fn [res el]
                  (let [k (f el)]
                    (assoc! res k (merge (get res k) el))))
                (transient {})
                res)
        persistent!)))

(defn fetch [query query-opts]
  (loop [[{::keys [from props facets select-spec sub-queries] :as entity-query} & more-queries] [(parse-query query)]
         res {}]
    (if entity-query
      (let [entity (get @!entities from)
            query-res (-> (*fetch-entity* (merge #::{:from from,
                                                     :props props,
                                                     :facets (into #{}
                                                                   (keep (fn [facet]
                                                                           (when (contains? facets facet)
                                                                             (facet-presence-kw facet))))
                                                                   (::facets entity))}
                                                 (when select-spec
                                                   {::select (mapv select-spec [:select-key :select-vals])}))

                                          query-opts)
                          (by-key (::key entity))
                          (->> (into {} (let [transform (apply-query entity-query)]
                                          (keep (fn [[k v]]
                                                  (when-let [transformed-v (transform v)]
                                                    [k transformed-v])))))))]

        (recur (concat (when (seq query-res)
                         (for [{::keys [from] :as sub-query} sub-queries]
                           (let [{join-entity ::entity, ::keys [cardinality]} (get-in entity [::joins from])
                                 {join-entity-key ::key} (get @!entities join-entity)]
                             (merge sub-query
                                    {::from join-entity
                                     ::select (case cardinality
                                                :many [(::key entity) (into #{} (keys query-res))]
                                                :one [join-entity-key (into #{} (keep join-entity-key) (vals query-res))])}))))
                       more-queries)

               (-> res
                   (update from (fn [existing-entities]
                                  (merge-with merge existing-entities query-res))))))

      res)))

(defn q [db query]
  (letfn [(q* [{::keys [from select-spec sub-queries] :as parsed-query}]
            (let [{entity-key ::key, :as entity} (get @!entities from)]
              (for [instance (->> (vals (get db from))
                                  (into [] (keep (apply-query parsed-query))))]

                (reduce (fn [instance {::keys [from] :as sub-query}]
                          (let [{::keys [cardinality entity]} (get-in entity [::joins from])
                                {sub-entity-key ::key} (get @!entities entity)]
                            (assoc instance from (-> (q* (merge sub-query
                                                             {::from entity
                                                              ::select-spec (case cardinality
                                                                              :many {::select-key entity-key
                                                                                     ::select-vals [(get instance entity-key)]}
                                                                              :one {::select-key sub-entity-key
                                                                                    ::select-vals [(get instance sub-entity-key)]})}))
                                                     (cond-> (= cardinality :one) first)))))
                        instance
                        sub-queries))))]
    (q* (parse-query query))))

(comment
  (let [query #_`(:comment [] (:blog #_(:comments))) `(:blog [:blog-id "foo-blog"]
                                                             {:published? true}
                                                             (:comments))

        db (fetch query {})]

    (q db query)))
;; ----- COMMANDS -----

(def ^:private !commands
  (atom {}))

(defmacro defcmd [op cmd-spec [& bindings] & body]
  `(let [op# ~op]
     (swap! !commands assoc op# {::cmd-spec ~cmd-spec
                                 ::run-cmd! (fn [~@bindings]
                                              ~@body)})
     op#))

(s/def ::op keyword?)

(s/def ::command
  (s/cat ::op ::op
         ::data any?))

(defn cmd! [cmd cmd-opts]
  (let [{::keys [op data]} (let [conformed (s/conform ::command cmd)]
                             (or (when-not (= ::s/invalid conformed)
                                   conformed)
                                 (throw (ex-info "Invalid command passed to `cmd!`"
                                                 {:explain (s/explain-data ::command cmd)}))))

        {::keys [cmd-spec run-cmd!]} (or (get @!commands op)
                                         (throw (ex-info "Unknown `cmd` op" {::error :bad-cmd
                                                                             ::cmd-error :unknown-op
                                                                             ::op op})))

        conformed-data (when-let [conformed (s/conform cmd-spec data)]
                         (or (when-not (= ::s/invalid conformed)
                               conformed)
                             (throw (ex-info "Error conforming command data"
                                             {::error :bad-cmd
                                              ::cmd-error :error-conforming-data
                                              :op op
                                              :data data
                                              :explain (s/explain-data cmd-spec data)}))))]

    (try
      (run-cmd! conformed-data cmd-opts)

      (catch Exception e
        (throw (if (::error (ex-data e))
                 e
                 (ex-info "Error running command"
                          {::error :unknown-cmd-error
                           ::cmd-error :error-running-cmd
                           :op op
                           :data data}
                          e)))))))

(defn apply-tempids [{::keys [tempids]} query]
  (->> query
       (w/postwalk-replace tempids)))

;; ----------------------

(comment
  (defentity :blog
    #::{:key :blog-id
        :facets #{:content}
        :props {:published? boolean?}
        :joins {:comments [:comment]}})

  (defmethod fetch-entity :blog [{::keys [select props facets joins] :as query} {}]
    [{:blog-id "foo-blog"
      :title "Foo Blog"
      :published? true
      :content "Welcome to my blog!"}])

  (defentity :comment
    #::{:key :comment-key
        :selects #{:blog-id}
        :facets #{:content}
        :joins {:blog :blog}})

  (defmethod fetch-entity :comment [{::keys [select facets joins]} {}]
    [{:comment-key {:blog-id "foo-blog", :comment-id "foo-comment-1"}
      :blog-id "foo-blog"
      :content "Nice blog!"}]))
