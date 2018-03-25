(ns oak.data
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.walk :as w])
  #?(:cljs (:require-macros [oak.data])))

;; ----- QUERIES -----

(def !entities (atom {}))

(defn- ^:dynamic *entity-spec* [entity]
  (get @!entities entity))

(def keyword-ish?
  (s/conformer (fn [o]
                 (cond
                   (keyword? o) o
                   (string? o) (-> o (cond-> (str/starts-with? o ":") (subs 1)) keyword)
                   :else ::s/invalid))))

(s/def ::key keyword-ish?)
(s/def ::selects (s/coll-of keyword-ish? :kind set? :into #{}))
(s/def ::wheres (s/coll-of keyword-ish? :kind set? :into #{}))
(s/def ::fields (s/coll-of keyword-ish? :kind set? :into #{}))

(s/def ::cardinality #{:many :one})
(s/def ::joins (s/map-of keyword-ish?
                         (s/or :many (s/tuple keyword-ish?)
                               :one keyword-ish?)))

(s/def ::entity-spec (s/keys :req [::key] :opt [::selects ::wheres ::fields ::joins]))

(defn transform-entity-spec [entity-spec]
  (doto (s/assert ::entity-spec entity-spec) prn)

  (-> (s/conform ::entity-spec entity-spec)
      (select-keys [::key ::selects ::wheres ::fields ::joins])
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
  (s/or ::where (s/map-of keyword-ish? any?)
        ::field keyword-ish?
        ::sub-query ::sub-query))

(s/def ::sub-query
  (s/spec (s/cat ::from keyword-ish?
                 ::query-params (s/* ::query-param))))

(s/def ::query
  (s/cat ::from keyword-ish?
         ::select (s/spec (s/? (s/cat ::select-key keyword-ish?
                                      ::select-vals (s/+ any?))))
         ::query-params (s/* ::query-param)))

(def field-presence-kw
  (-> (fn [field]
        (keyword (namespace field) (str (name field) "?")))
      memoize))

(defn- parse-query [query]
  (let [conformed-query (let [conformed (s/conform ::query query)]
                          (or (when (= ::s/invalid conformed)
                                (throw (ex-info "Invalid query" {::error :bad-query
                                                                 ::query-error :malformed-query
                                                                 :query query
                                                                 :explain-data (s/explain-data ::query query)})))
                              conformed))]
    (letfn [(parse-query* [query]
              (reduce (fn [acc [qp-type qp]]
                        (case qp-type
                          ::sub-query (update acc ::sub-queries (fnil conj []) (parse-query* qp))
                          ::field (update acc ::fields (fnil conj #{}) qp)
                          ::where (update acc ::where (fnil merge {}) qp)))

                      (select-keys query [::from ::select])

                      (::query-params query)))]
      (parse-query* conformed-query))))

(defn apply-query [{::keys [from select fields where]}]
  (let [keep? (apply every-pred
                     (if-let [{::keys [select-key select-vals]} select]
                       (comp (set select-vals) select-key)
                       identity)

                     (for [[field values] where
                           :when (some? values)]
                       (cond
                         (boolean? values) #(= (get % field) values)
                         (set? values) #(contains? values (get % field))
                         :else identity)))

        deselected-fields (set/difference (::fields (*entity-spec* from)) fields)]

    (fn [instance]
      (when (keep? instance)
        (apply dissoc instance deselected-fields)))))

(defn fetch [db query query-opts]
  (loop [[{::keys [from where fields select sub-queries] :as entity-query} & more-queries] [(parse-query query)]
         db db]
    (if-not entity-query
      db

      (let [entity (*entity-spec* from)
            query-res (-> (*fetch-entity* (merge #::{:from from, :where where,
                                                     :fields (into #{}
                                                                   (keep (fn [field]
                                                                           (when (contains? fields field)
                                                                             (field-presence-kw field))))
                                                                   (::fields entity))}
                                                 (when select
                                                   {::select (mapv select [:select-key :select-vals])}))

                                          query-opts)

                          (->> (into {} (let [transform (apply-query entity-query)]
                                          (keep (fn [[k v]]
                                                  (when-let [transformed-v (transform v)]
                                                    [k transformed-v])))))))]

        (recur (concat (when (seq query-res)
                         (for [{::keys [from] :as sub-query} sub-queries]
                           (let [{join-entity ::entity, ::keys [cardinality]} (get-in entity [::joins from])
                                 {join-entity-key ::key} (*entity-spec* join-entity)]
                             (merge sub-query
                                    {::from join-entity
                                     ::select (case cardinality
                                                :many [(::key entity) (into #{} (keys query-res))]
                                                :one [join-entity-key (into #{} (keep join-entity-key) (vals query-res))])}))))
                       more-queries)

               (-> db
                   (update from (fn [existing-entities]
                                  (merge-with merge existing-entities query-res)))))))))

(defn q [db query]
  (letfn [(q* [{::keys [from sub-queries] :as parsed-query}]
            (let [{entity-key ::key, :as entity} (*entity-spec* from)]
              (for [instance (->> (vals (get db from))
                                  (into [] (keep (apply-query parsed-query))))]

                (reduce (fn [instance {::keys [from] :as sub-query}]
                          (let [{::keys [cardinality entity]} (get-in entity [::joins from])
                                {sub-entity-key ::key} (*entity-spec* entity)]
                            (assoc instance from (-> (q* (merge sub-query
                                                             {::from entity
                                                              ::select (case cardinality
                                                                              :many {::select-key entity-key
                                                                                     ::select-vals [(get instance entity-key)]}
                                                                              :one {::select-key sub-entity-key
                                                                                    ::select-vals [(get instance sub-entity-key)]})}))
                                                     (cond-> (= cardinality :one) first)))))
                        instance
                        sub-queries))))]
    (q* (parse-query query))))

(def q1 (comp first q))

;; ----- COMMANDS -----

(def ^:private !commands
  (atom {}))

(defmacro defcmd [op cmd-spec [& bindings] & body]
  `(let [op# ~op]
     (swap! !commands assoc op# {::cmd-spec ~cmd-spec
                                 ::run-cmd! (fn [~@bindings]
                                              ~@body)})
     op#))

(s/def ::op keyword-ish?)

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

        conformed-data (if cmd-spec
                         (let [conformed (s/conform cmd-spec data)]
                           (or (when-not (= ::s/invalid conformed)
                                 conformed)
                               (throw (ex-info "Error conforming command data"
                                               {::error :bad-cmd
                                                ::cmd-error :error-conforming-data
                                                :op op
                                                :data data
                                                :explain (s/explain-data cmd-spec data)}))))
                         data)]

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
        :fields #{:content}
        :wheres #{:published?}
        :joins {:comments [:comment]}})

  (defmethod fetch-entity :blog [{::keys [select wheres fields joins] :as query} {}]
    (->> [{:blog-id "foo-blog"
           :title "Foo Blog"
           :published? true
           :content "Welcome to my blog!"}]
         (into {} (map (juxt :blog-id identity)))))

  (defentity :comment
    #::{:key :comment-key
        :selects #{:blog-id}
        :fields #{:content}
        :joins {:blog :blog}})

  (defmethod fetch-entity :comment [{::keys [select fields joins]} {}]
    (->> [{:comment-key {:blog-id "foo-blog", :comment-id "foo-comment-1"}
           :blog-id "foo-blog"
           :content "Nice blog!"}]
         (into {} (map (juxt :comment-key identity)))))

  (let [query (or `(:comment [:blog-id "foo-blog"]
                             {::limit 4}
                             (:blog (:comments)))
                  `(:blog [:blog-id "foo-blog"]
                          {:published? nil}
                          (:comments)))

        db (fetch {} query {})]

    (q1 db query)))
