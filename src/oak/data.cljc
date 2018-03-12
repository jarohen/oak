(ns oak.data
  (:require [clojure.spec.alpha :as s])
  #?(:cljs (:require-macros [oak.data])))

(def !entities
  (atom {}))

(s/def ::selects (s/coll-of keyword? :kind set?))
(s/def ::props (s/map-of keyword? any?))
(s/def ::facets set?)
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
                                               [k {:entity entity
                                                   ::cardinality cardinality}])))))))))

#?(:clj
   (defmacro defentity [entity entity-spec]
     `(do
        (swap! !entities assoc ~entity ~(transform-entity-spec entity-spec))
        ~entity)))

(defmulti fetch-entity
  (fn [{::keys [from]} opts]
    from))

(s/def ::query-param
  (s/or ::props (s/map-of keyword? any?)
        :facet keyword?
        :sub-query (s/spec (s/& ::sub-query))))

(s/def ::sub-query
  (s/cat ::from keyword?
         ::query-params (s/* ::query-param)))

(s/def ::query
  (s/cat ::from keyword?
         :select-spec (s/spec (s/? (s/cat :select-key keyword?
                                          :select-vals (s/+ any?))))
         ::query-params (s/* ::query-param)))

(def facet-presence-kw
  (-> (fn [facet]
        (keyword (namespace facet) (str (name facet) "?")))
      memoize))

(defn by-key [res f]
  (if (map? res)
    res
    (-> (reduce (fn [res el]
                  (let [k (f el)]
                    (assoc! res k (merge (get res k) el))))
                (transient {})
                res)
        persistent!)))

(defn- parse-query [query]
  (let [{:keys [select-spec] ::keys [from query-params]} (let [conformed-query (s/conform ::query query)]
                                                           (if-not (= conformed-query ::s/invalid)
                                                             conformed-query
                                                             (throw (ex-info "Invalid query" {:query query
                                                                                              :explain-data (s/explain-data ::query query)}))))]
    (reduce (fn [acc [qp-type qp]]
              (case qp-type
                :sub-query (update acc :sub-queries (fnil conj []) qp)
                :facet (update acc ::facets (fnil conj #{}) qp)
                ::props (update acc ::props (fnil merge {}) qp)))

            {::from from
             :select-spec select-spec}

            query-params)))

(defn fetch* [parsed-query query-opts]
  (loop [[{::keys [from props facets], :keys [select-spec sub-queries] :as entity-query} & more-queries] [parsed-query]
         res {}]
    (if entity-query
      (let [entity (get @!entities from)
            query-res (-> (fetch-entity (merge #::{:from from,
                                                   :props props,
                                                   :facets (into #{}
                                                                 (keep (fn [facet]
                                                                         (when (contains? facets facet)
                                                                           (facet-presence-kw facet))))
                                                                 (::facets entity))}
                                               (when select-spec
                                                 {::select (mapv select-spec [:select-key :select-vals])}))

                                        query-opts)
                          (by-key (::key entity)))]

        (recur (concat (when (seq query-res)
                         (for [{::keys [from] :as sub-query} sub-queries]
                           (let [{join-entity :entity, ::keys [cardinality]} (get-in entity [::joins from])
                                 {join-entity-key ::key} (get @!entities join-entity)]
                             (merge sub-query
                                    {::from join-entity
                                     ::select (case cardinality
                                                :many [(::key entity) (into #{} (keys query-res))]
                                                :one [join-entity-key (into #{} (keep join-entity-key) (vals query-res))])}))))
                       more-queries)
               (cond
                 (map? query-res) (update res from (fn [existing-entities]
                                                     (merge-with merge existing-entities query-res))))))

      res)))

(defn fetch [query query-opts]
  (fetch* (parse-query query) query-opts))

(defn query-db* [db {::keys [from], :keys [select-spec sub-queries] :as parsed-query}]
  (let [{entity-key ::key, :as entity} (get @!entities from)]
    (for [instance (-> (vals (get db from))
                       (cond->> select-spec (filter (let [{:keys [select-key select-vals]} select-spec]
                                                      (comp (set select-vals) select-key)))))]
      (reduce (fn [instance {::keys [from] :as sub-query}]
                (let [{::keys [cardinality], :keys [entity]} (get-in entity [::joins from])
                      {sub-entity-key ::key} (get @!entities entity)]
                  (assoc instance from (query-db* db
                                                  (merge sub-query
                                                         {::from entity
                                                          :select-spec (case cardinality
                                                                         :many {:select-key entity-key
                                                                                :select-vals [(get instance entity-key)]}
                                                                         :one {:select-key sub-entity-key
                                                                               :select-vals [(get instance sub-entity-key)]})})))))
              instance
              sub-queries))))

(defn query-db [db query]
  (query-db* db (parse-query query)))

(comment
  (let [query #_`(:comment [] (:blog)) `(:blog [:james/blog-id "foo-blog"]
                                               {:foo :bar}
                                               (:comments))

        parsed-query (parse-query query)
        db (fetch* parsed-query {})]

    (query-db* db parsed-query)))


;; ----------------------

(comment
  (s/def :james/blog-id string?)

  (s/def :james/comment-id string?)
  (s/def :james/comment-key (s/keys :req-un [:james/blog-id :james/comment-id]))

  (defentity :blog
    #::{:key :james/blog-id
        :facets #{:content}
        :props {:published? boolean?}
        :joins {:comments [:comment]}})

  (defmethod fetch-entity :blog [{::keys [select props facets joins] :as query} {}]
    [{:james/blog-id "foo-blog"
      :title "Foo Blog"
      :content "Welcome to my blog!"}])

  (defentity :comment
    #::{:key :james/comment-key
        :selects #{:james/blog-id}
        :facets #{:content}
        :joins {:blog :blog}})

  (defmethod fetch-entity :comment [{::keys [select facets joins]} {}]
    [{:james/comment-key #:james{:blog-id "foo-blog", :comment-id "foo-comment-1"}
      :james/blog-id "foo-blog"
      :content "Nice blog!"}]))
