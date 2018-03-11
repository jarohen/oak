(ns oak.data
  (:require [clojure.spec.alpha :as s]))

(def !entities
  (atom {}))

(s/def ::selects (s/map-of keyword? any?))
(s/def ::props map?)
(s/def ::facets set?)
(s/def ::key (s/tuple keyword? any?))
(s/def ::joins (s/map-of keyword? (s/or :many (s/tuple keyword?)
                                        :one keyword?)))

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
                                                   :cardinality cardinality}])))))))))

(defmacro defentity [entity entity-spec]
  `(do
     (swap! !entities assoc ~entity ~(transform-entity-spec entity-spec))
     ~entity))

(defmulti query-entity
  (fn [{::keys [from]} opts]
    from))

(s/def :james/blog-id string?)

(s/def :james/comment-id string?)
(s/def :james/comment-key (s/keys :req-un [:james/blog-id :james/comment-id]))

(defentity :blog
  #::{:key [:blog-id :james/blog-id]
      :facets #{:content}
      :props {:published? boolean?}
      :joins {:comments [:comment]}})

(defmethod query-entity :blog [{::keys [select props facets joins] :as query} {}]
  {"foo-blog" {:title "Foo Blog"
               :content "Welcome to my blog!"
               :comments #{{:blog-id "foo-blog", :comment-id "foo-comment-1"}}}})

(defentity :comment
  #::{:key [:comment-key :james/comment-key]
      :selects {:blog-id :james/blog-id}
      :facets #{:content}
      :joins {:blog-id :blog}})

(defmethod query-entity :comment [{::keys [select facets joins]} {}]
  {{:blog-id "foo-blog", :comment-id "foo-comment-1"} {:content "Nice blog!"
                                                       :blog-id "foo-blog"}})

(s/def ::query
  (s/cat ::from keyword?
         :select-spec (s/? (s/spec (s/cat :select-key keyword?
                                          :select-vals (s/* any?))))
         :opts (s/? (s/keys :opt [::props ::facets]))
         :join-specs (s/* (s/spec ::query))))

(def facet-presence-kw
  (-> (fn [facet]
        (keyword (namespace facet) (str (name facet) "?")))
      memoize))

(defn query [query query-opts]
  (loop [[{::keys [from], :keys [select-spec opts join-specs] :as entity-query} & more-queries] [(s/conform ::query query)]
         res {}]
    (if entity-query
      (let [{::keys [props facets]} opts
            entity (get @!entities from)
            query-res (query-entity (merge #::{:from from,
                                               :props props,
                                               :facets (into #{}
                                                             (keep (fn [facet]
                                                                     (when (contains? facets facet)
                                                                       (facet-presence-kw facet))))
                                                             (::facets entity))}
                                           (when select-spec
                                             {::select (mapv select-spec [:select-key :select-vals])}))

                                    query-opts)]
        (recur (concat (for [join-spec join-specs]
                         )
                       more-queries)
               (cond
                 (map? query-res) (update res from (fn [existing-entities]
                                                     (merge-with merge existing-entities query-res))))))

      res)))

(query `(:blog [:blog-id "foo-blog"]
               #::{:props {}, :facets #{}}
               (:comment)) {})
