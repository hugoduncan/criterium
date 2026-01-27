(ns criterium.notebook.render
  "Render notebooks to HTML documentation site."
  (:require
   [clojure.edn :as edn]
   [scicloj.clay.v2.api :as clay]))

(def notebook-sources
  "Source paths for notebooks to render."
  ["bases/notebooks/src/criterium/index.clj"
   "bases/notebooks/src/quickstart.clj"
   "bases/notebooks/src/warmup.clj"
   "bases/notebooks/src/in_situ.clj"
   "bases/notebooks/src/criterium/basic_usage_notebook.clj"
   "bases/notebooks/src/criterium/bench_options_notebook.clj"
   "bases/notebooks/src/criterium/arg_gen_notebook.clj"
   "bases/notebooks/src/criterium/sampled_fn_notebook.clj"
   "bases/notebooks/src/criterium/instrument_fn_notebook.clj"
   "bases/notebooks/src/criterium/allocation_tracking_notebook.clj"
   "bases/notebooks/src/criterium/call_tracing_notebook.clj"
   "bases/notebooks/src/criterium/allocation_bench_notebook.clj"
   "bases/notebooks/src/criterium/domain_bench_notebook.clj"
   "bases/notebooks/src/criterium/analyse_domain_notebook.clj"
   "bases/notebooks/src/criterium/domain_builder_notebook.clj"
   "bases/notebooks/src/criterium/kde_analysis_notebook.clj"
   "bases/notebooks/src/criterium/distribution_fitting_notebook.clj"
   "bases/notebooks/src/criterium/tail_analysis_notebook.clj"])

(defn render-site!
  "Render all notebooks to HTML in the docs directory."
  ([]
   (render-site! {}))
  ([opts]
   (clay/make!
    (merge
     {:source-path notebook-sources
      :base-target-path "docs"
      :clean-up-target-dir false
      :format [:quarto :html]
      :show false
      :run-quarto false}
     opts))))

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
