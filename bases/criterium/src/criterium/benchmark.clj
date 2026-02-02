(ns criterium.benchmark
  "Namespace for composing and executing benchmarks from declarative specs.
   Provides functionality to construct benchmark functions from analysis and
   view configurations."
  (:require
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have have?]]
   [criterium.view :as view]))

(def ^:dynamic *time-analyse*
  "When true, print elapsed time for each analysis function."
  false)

(defn- format-elapsed-ms
  "Format elapsed time in milliseconds to 4 significant figures."
  ^String [^double elapsed-ns]
  (let [elapsed-ms (/ elapsed-ns 1e6)]
    (cond
      (>= elapsed-ms 1000.0) (format "%.1f" elapsed-ms)
      (>= elapsed-ms 100.0) (format "%.2f" elapsed-ms)
      (>= elapsed-ms 10.0) (format "%.3f" elapsed-ms)
      (>= elapsed-ms 1.0) (format "%.4f" elapsed-ms)
      (>= elapsed-ms 0.1) (format "%.4f" elapsed-ms)
      (>= elapsed-ms 0.01) (format "%.5f" elapsed-ms)
      :else (format "%.6f" elapsed-ms))))

(defn wrap-with-timing
  "Wrap an analysis function with timing instrumentation.
   When *time-analyse* is truthy, prints the spec and elapsed time to *out*.
   Returns a function that takes data-map and returns the analysis result."
  [spec f]
  (fn [data-map]
    (if *time-analyse*
      (let [start (System/nanoTime)
            result (f data-map)
            elapsed (- (System/nanoTime) start)]
        (println (format-elapsed-ms elapsed) "ms" (pr-str spec))
        result)
      (f data-map))))

(defn- resolve-analyse-fn
  "Resolves a single analysis function specification and wraps with timing.
   If x is a sequence, treats first element as function and rest as args.
   Otherwise treats x as a function name to resolve.
   Returns a function of one argument (the sampled data) wrapped with timing."
  [x]
  (let [options {:default-ns 'criterium.analyse}
        f (if (sequential? x)
            (apply (util/maybe-var-get (first x) options) (rest x))
            ((util/maybe-var-get x options)))]
    (wrap-with-timing x f)))

(defn- resolve-view-fn
  "Resolves a single view function specification.
   If x is a sequence, treats first element as function and rest as args.
   Otherwise treats x as a function name to resolve.
   Returns a function of one argument (the analysis result)."
  [x]
  (let [options {:default-ns 'criterium.view}]
    (have
     fn?
     (if (sequential? x)
       (apply (util/maybe-var-get (first x) options) (rest x))
       ((util/maybe-var-get x options)))
     {:x x})))

(defn- resolve-analyse-fns
  "Resolves all analysis functions from a specification vector."
  [spec]
  (mapv resolve-analyse-fn spec))

(defn- resolve-view-fns
  "Resolves all view functions from a specification vector."
  [spec]
  (mapv resolve-view-fn spec))

(defn ->analyse
  "Creates a composite analysis function from a sequence of analysis specs.

  Each spec is either a keyword/symbol to resolve a function, or a vector
  with a keyword/symbol first element followed by arguments.

  Analysis functions:
  - Are composed in sequence from last to first
  - Each takes a sampled map as input
  - Each returns a modified sampled map

  Example specs: [:stats
                  [:quantiles {:quantiles [0.025 0.975]}]]

  Returns a function that takes sampled data and returns analysis results."
  [analyse-plan]
  (when-not (or (nil? analyse-plan) (sequential? analyse-plan))
    (throw
     (ex-info "analyse must be a sequence of specs" {:analyse analyse-plan})))
  (let [fns (resolve-analyse-fns analyse-plan)]
    (reduce comp (reverse fns))))

(defn ->view
  "Creates a composite view function from a sequence of view specs.

  Each spec is either a keyword/symbol to resolve a function, or a vector
  with a keyword/symbol first element followed by arguments.

  View functions:
  - Are called in order with the analysis result
  - Are expected to produce side effects (printing, plotting etc.)
  - Return values are ignored

  Example specs: [:text-table]

  Returns a function that takes analysis results and handles viewing."
  [view-plan]
  (when-not (or (nil? view-plan) (sequential? view-plan))
    (throw
     (ex-info "view must be a sequence of specs" {:view view-plan})))
  (let [fns (resolve-view-fns view-plan)]
    (fn [viewer result]
      {:pre [(have? keyword? viewer)]}
      (run! #(% viewer result) fns)
      (or (view/flush-viewer viewer) result))))

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
  [bench-plan]
  (let [analyse-fn (->analyse (:analyse bench-plan))
        view-fn    (->view (:view bench-plan))]
    (fn [sampled]
      (-> sampled
          analyse-fn
          view-fn))))
