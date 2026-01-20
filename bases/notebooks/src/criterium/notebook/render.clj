(ns criterium.notebook.render
  "Render notebooks to HTML documentation site."
  (:require
   [scicloj.clay.v2.api :as clay]))

(def notebook-sources
  "Source paths for notebooks to render."
  ["bases/notebooks/src/criterium/index.clj"
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
      :format [:html]
      :show false
      :run-quarto false}
     opts))))

(defn -main
  "Entry point for rendering notebooks."
  [& _args]
  (try
    (render-site!)
    (catch Throwable t
      (binding [*out* *err*]
        (println "Notebook generation failed")
        (.printStackTrace t)
        (System/exit 1)))
    (finally
      (shutdown-agents)))
  (System/exit 0))
