(ns oak.ssr
  (:require [clojure.java.io :as io])
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

(defn emit-str [^Invocable engine {:oak/keys [component app db event-handlers cmd-handlers timeout-ms], :or {timeout-ms 2000}}]
  (let [oak (eval-str engine "oak.core")
        [component-f & args] component
        component-f-obj (eval-str engine (format "%s.%s"
                                                 (munge (namespace component-f))
                                                 (munge (name component-f))))

        !job (future
               (-> engine
                   (.invokeMethod oak "ssr" (into-array [{"component" (into [component-f-obj] (map pr-str args))}]))))]
    (try
      (let [{:strs [html app db], :keys [timeout?]} (into {} (deref !job timeout-ms {:timeout? true}))]
        (if-not timeout?
          html
          nil)))))
