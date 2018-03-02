(ns oak.ssr
  (:require [clojure.java.io :as io])
  (:import [javax.script ScriptEngine ScriptEngineManager]))

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
        (eval-str (format "goog.require('%s')" (munge main))))

      (throw (ex-info "not implemented yet" {:optimizations optimizations})))))

(comment
  (def foo-engine
    (mk-engine {:output-dir "examples/todomvc/target/deps"
                :main 'todomvc.ui.app
                :optimizations :none})))

(defn emit-str [{[f & args] :emit-str-call, :keys [engine]}]
  (-> engine
      (eval-str (format "%s.%s()" (munge (namespace f)) (munge (name f))))
      (.get "html")))

(comment
  (-> foo-engine
      (eval-str "todomvc.ui.app.emit_str()")
      (.get "html")))
