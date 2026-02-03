(ns criterium.domain
  "Abstraction for working with multiple related benchmark runs.

  A domain is an immutable collection of benchmark runs indexed by coordinates.
  It enables analysis of performance behaviour across a parameter space rather
  than at a single point.

  Supports:
  - Scaling behaviour analysis (e.g., O(N)) with varying argument values
  - Identifying parts of argument space with different metric behaviours
  - Comparing different implementation behaviours
  - Tracking behaviour over time (in-memory session)

  Example domain structure:
  {:type :criterium/domain
   :runs [{:coord {:n 100} :data <bench-result>}
          {:coord {:n 1000} :data <bench-result>}
          {:coord {:n 100 :impl :foo} :data <bench-result>}]}

  See also:
  - criterium.domain.analysis for extract, compare-by, group-by-axis,
    fit-complexity
  - criterium.domain.builder for domain-builder and input generators"
  (:require
   [criterium.domain-plans :as domain-plans]
   [criterium.domain.analysis :as analysis]
   [criterium.domain.builder :as builder]
   [criterium.domain.types :as types]
   [criterium.measured :as measured]))

;;; Re-export from types

(def domain
  "Create a domain from runs. Returns empty domain when called with no args."
  types/domain)

(def add-run
  "Add a benchmark result to a domain with the given coordinates."
  types/add-run)

(def remove-run
  "Remove a run from a domain by coordinates."
  types/remove-run)

(def runs
  "Return runs from a domain, optionally filtered by partial coordinate."
  types/runs)

(def coords
  "Return all coordinates from a domain as a vector."
  types/coords)

(def axes
  "Infer dimension keys from all map coordinates in a domain."
  types/axes)

(def implementations
  "Returns the implementation keys for a domain."
  types/implementations)

(def impl-axis
  "Returns the impl-axis key for a domain, or nil if not set."
  types/impl-axis)

(def select
  "Filter domain to runs matching a partial coordinate."
  types/select)

;;; Domain Expression Macro

(defn- transform-impl-expr
  "Transform an implementation expression into a function.
     (fn [{:keys [syms]}] (measured/expr body options)).
  Type hints on axis binding symbols transfer to the destructured keys.
  Options are passed through to measured/expr (e.g., :warmup-args-fn)."
  [expr axis-syms options]
  (let [keys-with-hints (mapv (fn [sym]
                                (if-let [hint (-> sym meta :tag)]
                                  (with-meta (symbol (name sym)) {:tag hint})
                                  (symbol (name sym))))
                              axis-syms)]
    (if options
      `(fn [{:keys ~keys-with-hints}]
         (measured/expr ~expr ~options))
      `(fn [{:keys ~keys-with-hints}]
         (measured/expr ~expr)))))

(defmacro domain-expr
  "Create a domain specification map from a concise expression syntax.

  Returns a map with :axes and :implementations suitable for use with
  domain-builder: (apply domain-builder (mapcat identity (domain-expr ...)))

  Syntax:
    ;; Single expression - uses :default impl key
    (domain-expr [n (log-range 10 1000 5)] (sort (random-seq n)))

    ;; Multiple implementations via map
    (domain-expr [n (log-range 10 1000 5)]
      {:sort (sort (random-seq n))
       :sort-by (sort-by identity (random-seq n))})

    ;; Multiple axes
    (domain-expr [n (log-range 10 1000 5)
                  m (linear-range 1 10 3)]
      (matrix-mult (random-matrix n m)))

    ;; With options (e.g., shared warmup-args-fn for JIT warmup)
    (domain-expr [n (log-range 10 1000 5)]
      {:sort (sort (random-seq n))
       :sort-by (sort-by identity (random-seq n))}
      {:warmup-args-fn (fn [] [(vec (shuffle (range 5000)))])})

  The binding vector defines axes as [sym range-expr ...] pairs.
  Axis symbols are available in implementation expressions.

  Each implementation becomes a function (fn [axis-map] measured) that
  receives axis values and returns a Measured for that coordinate.
  Type hints on axis bindings transfer to the destructured keys.

  Options:
    :warmup-args-fn - Function returning arguments for warmup phase.
                      Applied to all implementations for consistent JIT warmup."
  ([bindings body]
   `(domain-expr ~bindings ~body nil))
  ([bindings body options]
   (let [axis-pairs (partition 2 bindings)
         axis-syms (mapv first axis-pairs)
         axes-map (into {}
                        (map (fn [[sym range-expr]]
                               [(keyword sym) range-expr]))
                        axis-pairs)
         impls (if (map? body) body {:default body})
         impl-entries (map (fn [[impl-key expr]]
                             [impl-key
                              (transform-impl-expr expr axis-syms options)])
                           impls)]
     `{:axes ~axes-map
       :implementations ~(into {} impl-entries)})))

;;; High-level API

(defn bench
  "Run benchmarks across a domain and analyze the results.

  Convenience wrapper combining domain-builder and analyse-domain.

  Arguments:
    domain-spec - Map with :axes and :implementations (as returned
                  by domain-expr)

  Options:
    :domain-plan   - Analysis plan (default: domain-plans/extract-metrics)
    :viewer        - Output format (:print, :pprint, :kindly, :portal, :none).
                     Overrides any :viewer in the domain-plan.
    :reporter      - Progress reporter (default: dot-reporter, nil for silent)
    :bench-options - Options passed to bench-measured (e.g., :limit-time-s,
                     :metric-ids). Note: :warmup-args-fn is not supported here;
                     use domain-expr options instead.
    :time-axis     - Axis key for time estimation (default: first axis)

  Returns the analysis data-map (same as analyse-domain).

  Example:
    (bench (domain-expr [n (log-range 100 10000 5)]
                        {:sort (sort (random-seq n))
                         :sort-by (sort-by identity (random-seq n))})
           :domain-plan domain-plans/implementation-comparison)"
  [domain-spec & {:keys [domain-plan viewer reporter bench-options time-axis]
                  :or   {domain-plan domain-plans/extract-metrics}}]
  (let [builder-opts (cond-> {}
                       (some? reporter) (assoc :reporter reporter)
                       (contains?
                        #{nil false}
                        reporter)       (assoc :reporter nil)
                       bench-options    (assoc :bench-options bench-options)
                       time-axis        (assoc :time-axis time-axis))
        domain       (apply builder/domain-builder
                            (:axes domain-spec)
                            (:implementations domain-spec)
                            (mapcat identity builder-opts))
        domain-plan  (cond-> domain-plan
                       viewer (analysis/options->domain-plan :viewer viewer))]
    (analysis/analyse-domain domain-plan domain)))
