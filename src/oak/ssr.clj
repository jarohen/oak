(ns oak.ssr
  (:require [clojure.java.io :as io]
            [clojure.string :as s])
  (:import [javax.script Invocable ScriptEngine ScriptEngineManager]))

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
        (eval-str (format "goog.require('%s')" (munge main))))

      (throw (ex-info "not implemented yet" {:optimizations optimizations})))))

(defn emit-str [{:oak/keys [^Invocable engine component script-src app db event-handlers cmd-handlers timeout-ms], :or {timeout-ms 2000}}]
  (let [[component-f & args] component
        component-name (format "%s.%s"
                               (munge (namespace component-f))
                               (munge (name component-f)))
        !job (when engine
               (future
                 (let [oak (eval-str engine "oak.core")
                       component-f-obj (eval-str engine component-name)
                       emit-str-opts (.invokeMethod engine oak "from_nashorn" (object-array [{"oak/component" (into [component-f-obj] (map pr-str args))}]))]
                   (-> engine
                       (.invokeMethod oak "emit_str" (object-array [emit-str-opts]))))))]

    (try
      (let [{:strs [html app db]} (when !job
                                    (into {} (deref !job timeout-ms nil)))]
        (s/join "\n" [(format "<div>%s</div>" (or html ""))
                      "<script>window.oak_root = document.scripts[document.scripts.length - 1].previousSibling.previousSibling;</script>"
                      (format "<script src=\"%s\" type=\"text/javascript\"></script>" script-src)
                      (format "<script>oak.core.mount_BANG_(oak_root, oak.core.from_nashorn({\"oak/component\": [%s]}))</script>"
                              (s/join ", " (into [component-name] (map pr-str args))))]))

      (finally
        (when !job
          (future-cancel !job))))))
