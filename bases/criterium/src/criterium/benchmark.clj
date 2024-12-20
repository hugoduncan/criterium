(ns criterium.benchmark
  "Namespace for composing and executing benchmarks from declarative specifications.
   Provides functionality to construct benchmark functions from analysis and view
   configurations."
  (:require
   [criterium.util.helpers :as util]))

(defn- resolve-analyse-fn
  "Resolves a single analysis function specification.
   If x is a sequence, treats first element as function and rest as args.
   Otherwise treats x as a function name to resolve.
   Returns a function of one argument (the sampled data)."
  [x]
  (let [options {:default-ns 'criterium.analyse}]
    (if (sequential? x)
      (apply (util/maybe-var-get (first x) options) (rest x))
      ((util/maybe-var-get x options)))))

(defn- resolve-view-fn
  "Resolves a single view function specification.
   If x is a sequence, treats first element as function and rest as args.
   Otherwise treats x as a function name to resolve.
   Returns a function of one argument (the analysis result)."
  [x]
  (let [options {:default-ns 'criterium.view}]
    (if (sequential? x)
      (apply (util/maybe-var-get (first x) options) (rest x))
      ((util/maybe-var-get x options)))))

(defn- resolve-analyse-fns
  "Resolves all analysis functions from a specification vector."
  [spec]
  (mapv resolve-analyse-fn spec))

(defn- resolve-view-fns
  "Resolves all view functions from a specification vector."
  [spec]
  (mapv resolve-view-fn spec))

(defn- compose
  "Composes a benchmark function from analysis and view functions.
   Returns a function that when given sampled data will:
   1. Apply all analysis functions in sequence
   2. Pass the analysis result to each view function
   3. Return the analysis result"
  [{:keys [analyse view]}]
  (when-not (or (nil? analyse) (sequential? analyse))
    (throw
     (ex-info "analyse must be a sequence of functions" {:analyse analyse})))
  (when-not (or (nil? view) (sequential? view))
    (throw
     (ex-info "view must be a sequence of functions" {:view view})))
  (let [analysis-fn (reduce comp (reverse analyse))]
    (fn [sampled]
      (let [result (analysis-fn sampled)]
        (run! #(% result) view)
        result))))

(defn ->benchmark
  "Compose a benchmark based on a declarative map.

  The :analyse and :view keywords take a vector of function specs. A
  function spec is either a keyword/symbol to resolve a function, or a
  vector with a keyword/symbol first element followed by arguments.

  Analysis functions:
  - Are composed in sequence from last to first
  - Each takes a sampled map as input
  - Each returns a modified sampled map

  View functions:
  - Are called in order with the final analysis result
  - Are expected to produce side effects (printing, plotting etc.)
  - Return values are ignored

  Example spec:
  {:analyse [:stats [:quantiles {:quantiles [0.025 0.975]}]]
   :view [:text-table]}

  Returns a function that takes a sampled map and returns analysis results."
  [benchmark-spec]
  (-> benchmark-spec
      (update :analyse resolve-analyse-fns)
      (update :view resolve-view-fns)
      compose))
