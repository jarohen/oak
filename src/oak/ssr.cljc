(ns oak.ssr
  (:require #?@(:clj [[clojure.java.io :as io]
                      [clojure.tools.reader.edn :as edn]])
            [clojure.string :as s]
            #?@(:cljs [[oak.core :refer [build-root]]
                       [reagent.dom.server :as rdm]
                       [cljs.reader :as edn]]))
  #?(:clj (:import [javax.script Invocable ScriptEngine ScriptEngineManager])))

#?(:clj
   (do
     (defn- eval-str
       ([engine s] (.eval engine s))
       ([engine s bindings] (.eval engine s bindings)))

     (defn- eval-readable [engine res]
       (.eval engine (io/reader res)))

     (defn mk-engine [{:keys [output-dir output-to main optimizations]}]
       (let [engine (-> (->> (. (ScriptEngineManager.) (getEngineFactories))
                             (into {} (map (juxt #(.getEngineName %) identity))))
                        (get "Oracle Nashorn")
                        (.getScriptEngine (into-array String ["-pcc" "--language=es6"])))]
         (doto engine
           (eval-str "var global = this;"))

         (if (= optimizations :none)
           (doto engine
             (eval-readable (io/file output-dir "goog/base.js"))
             (eval-readable (io/file output-dir "goog/deps.js"))
             (eval-readable (io/file output-dir "cljs_deps.js"))
             (eval-str (format "goog.global.CLOSURE_IMPORT_SCRIPT = function(path) {load('%s/goog/' + path);}" output-dir))
             (eval-str "goog.require('oak.core')")
             (eval-str "goog.require('oak.ssr')")
             (eval-str (format "goog.require('%s')" (munge main))))

           (throw (ex-info "not implemented yet" {:optimizations optimizations})))))))

#?(:cljs
   (defn- ^:export from-nashorn [obj]
     #?(:clj obj
        :cljs (or (when (and (exists? js/Java) (exists? js/Java.type))
                    (cond
                      (instance? (js/Java.type "java.util.List") obj)
                      (into [] (map (fn [k] (from-nashorn (aget obj k)))) (range (alength obj)))

                      (instance? (js/Java.type "java.util.Map") obj)
                      (into {} (map (fn [k] [(keyword k) (from-nashorn (aget obj k))])) (js-keys obj))))

                  (cond
                    (= "array" (type obj)) (vec obj))

                  (js->clj obj :keywordize-keys true)))))

#?(:clj
   (defn emit-str [^Invocable engine {:oak/keys [component script-src app db event-handlers cmd-handlers timeout-ms], :or {timeout-ms 2000}}]
     (let [[component-f & args] component
           component-name (format "%s.%s"
                                  (munge (namespace component-f))
                                  (munge (name component-f)))
           !job (when engine
                  (future
                    (try
                      (let [oak-ssr (eval-str engine "oak.ssr")
                            component-f-obj (eval-str engine component-name)
                            emit-str-opts (.invokeMethod engine oak-ssr "from_nashorn"
                                                         (object-array [{"oak/component" (into [component-f-obj] (map pr-str args))
                                                                         "oak/db" (pr-str db)
                                                                         "oak/app" (pr-str app)}]))]
                        (-> engine
                            (.invokeMethod oak-ssr "emit_str" (object-array [emit-str-opts]))))
                      (catch Exception e
                        {:oak.ssr/error e}))))]

       (try
         (let [{:strs [html app db] :as res} (when !job
                                               (into {} (deref !job timeout-ms nil)))]
           (merge (when html
                    {:oak/html html
                     :oak/app (edn/read-string app)
                     :oak/db (edn/read-string db)})
                  (select-keys res [:oak.ssr/error])))

         (finally
           (when !job
             (future-cancel !job))))))

   :cljs
   (defn ^:export emit-str [{:oak/keys [component event-handlers cmd-handlers] :as opts}]
     (let [root (build-root (-> opts
                                (update :oak/db #(some-> % edn/read-string))
                                (update :oak/app #(some-> % edn/read-string))))
           ctx (:oak/ctx (meta root))
           html (-> root
                    #?(:clj (as-> [{:keys [reagent-render]} & args] (let [[{render :reagent-render} & args] (apply reagent-render args)]
                                                                      (apply render args)))

                       :cljs rdm/render-to-string))]

       (-> {:app (pr-str @(:oak/!app ctx))
            :db (pr-str @(:oak/!db ctx))
            :html html}
           #?(:cljs clj->js)))))
