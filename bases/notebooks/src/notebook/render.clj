(ns notebook.render
  "Render notebooks to HTML documentation site."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [scicloj.clay.v2.api :as clay]))

(def notebook-sources
  "Source paths for notebooks to render."
  ["bases/notebooks/src/quickstart.clj"
   "bases/notebooks/src/warmup.clj"
   "bases/notebooks/src/benchmark_problem_detection.clj"
   "bases/notebooks/src/non_parametric_analysis.clj"
   "bases/notebooks/src/parametric_analysis.clj"
   "bases/notebooks/src/tail_analysis.clj"
   "bases/notebooks/src/analysis_and_view_options.clj"
   "bases/notebooks/src/in_situ.clj"
   "bases/notebooks/src/domain_complexity_analysis.clj"
   "bases/notebooks/src/domain_comparison_analysis.clj"])

(def quarto-config-source
  "Source path for Quarto configuration."
  "bases/notebooks/src/_quarto.yml")

(defn- copy-quarto-config!
  "Copy _quarto.yml to docs directory for Quarto site rendering."
  [target-path]
  (let [source (io/file quarto-config-source)
        target (io/file target-path "_quarto.yml")]
    (io/copy source target)))

(defn render-site!
  "Render all notebooks to HTML in the docs directory."
  ([]
   (render-site! {}))
  ([opts]
   (let [target-path (get opts :base-target-path "docs")]
     (copy-quarto-config! target-path)
     (clay/make!
      (merge
       {:source-path notebook-sources
        :base-target-path target-path
        :clean-up-target-dir false
        :format [:quarto :html]
        :show false
        :run-quarto false}
       opts)))))

(defn render-site-cli!
  "Entry point for -X execution. Renders site and exits."
  [opts]
  (try
    (render-site! opts)
    (catch Throwable t
      (binding [*out* *err*]
        (println "Notebook generation failed")
        (.printStackTrace t))
      (System/exit 1))
    (finally
      (shutdown-agents)))
  (System/exit 0))

(defn -main
  "Entry point for rendering notebooks.
  Accepts optional EDN map of options to merge with defaults."
  [& args]
  (try
    (let [opts (when (seq args)
                 (edn/read-string (first args)))]
      (render-site! (or opts {})))
    (catch Throwable t
      (binding [*out* *err*]
        (println "Notebook generation failed")
        (.printStackTrace t)
        (System/exit 1)))
    (finally
      (shutdown-agents)))
  (System/exit 0))
