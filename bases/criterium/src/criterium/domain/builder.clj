(ns criterium.domain.builder
  "Domain builder for automated benchmark collection across parameter spaces.

  Provides functions for building domains by running benchmarks with varying
  inputs, plus input sequence generators for scaling analysis."
  (:require
   [criterium.bench :as bench]
   [criterium.domain :as domain]
   [criterium.domain.analysis :as analysis]
   [criterium.measured :as measured]
   [criterium.util.invariant :refer [have]]))

;;; Input Sequence Generators
;;
;; These functions generate sequences of values useful for scaling analysis.
;; They are designed to work with domain coordinates, producing input sizes
;; that reveal algorithmic complexity patterns.

(defn powers-of-2
  "Generate a sequence of powers of 2.
  Returns (2^from 2^(from+1) ... 2^to) inclusive.

  Useful for testing algorithms where input size doubling reveals
  O(n), O(n log n), O(n²) patterns clearly.

  Examples:
  (powers-of-2 0 4)  ;=> (1 2 4 8 16)
  (powers-of-2 4 8)  ;=> (16 32 64 128 256)"
  [^long from ^long to]
  (map (fn [^long n] (bit-shift-left 1 n)) (range from (inc to))))

(defn powers-of
  "Generate a sequence of powers of base.
  Returns (base^from base^(from+1) ... base^to) inclusive.

  Useful for testing with specific base increments.

  Examples:
  (powers-of 10 1 4)  ;=> (10 100 1000 10000)
  (powers-of 3 0 4)   ;=> (1 3 9 27 81)"
  [base ^long from ^long to]
  (mapv #(long (Math/pow base %)) (range from (inc to))))

(defn log-range
  "Generate a logarithmically-spaced sequence of n values from start to end.
  Values are rounded to integers.

  Produces values where the ratio between successive elements is constant,
  useful for covering a wide range of input sizes efficiently.

  Examples:
  (log-range 1 1000 4)  ;=> (1 10 100 1000)
  (log-range 10 10000 5) ;=> (10 56 316 1778 10000)"
  [start end ^long n]
  (if (= n 1)
    (list (long start))
    (let [log-start (Math/log start)
          log-end   (Math/log end)
          step      (/ (- log-end log-start) (dec n))]
      (mapv
       (fn [^long n] (long (Math/round (Math/exp (+ log-start (* n step))))))
       (range n)))))

(defn linear-range
  "Generate a linearly-spaced sequence of n values from start to end.
  Values are rounded to integers.

  Produces evenly-spaced values, useful for detecting linear scaling
  or when uniform sampling across a range is needed.

  Examples:
  (linear-range 100 500 5)  ;=> (100 200 300 400 500)
  (linear-range 0 1000 5)   ;=> (0 250 500 750 1000)"
  [^long start ^long end ^long n]
  (if (= n 1)
    (list (long start))
    (let [step (double  (/ (- end start) (unchecked-dec n)))]
      (mapv
       (fn [^long n] (long (Math/round (double (+ start (* n step))))))
       (range n)))))

(defn- invert-n-log-n
  "Solve x*ln(x) = y for x using Newton-Raphson iteration.
  f(x) = x*ln(x) - y, f'(x) = ln(x) + 1
  x_{n+1} = x_n - f(x_n)/f'(x_n)"
  ^double [^double y ^double initial-guess]
  (let [max-iterations 50
        tolerance      1e-10]
    (loop [x initial-guess
           i 0]
      (if (>= i max-iterations)
        x
        (let [ln-x (Math/log x)
              fx   (- (* x ln-x) y)
              fpx  (+ ln-x 1.0)]
          (if (< (Math/abs fx) tolerance)
            x
            (recur (- x (/ fx fpx)) (inc i))))))))

(defn n-log-n-range
  "Generate n values from start to end spaced along an n*log(n) curve.
  Values are rounded to integers.

  Useful for testing algorithms with O(n log n) complexity where you want
  denser sampling at smaller sizes and sparser sampling at larger sizes.

  Start must be >= e^-1 (approximately 0.368) where x*ln(x) has its minimum.

  Examples:
  (n-log-n-range 10 10000 5)  ;=> sequence of 5 values from 10 to 10000"
  [start end ^long n]
  (have #(>= (double %) (/ 1.0 Math/E)) start {:msg "start must be >= e^-1"})
  (if (= n 1)
    [(long start)]
    (let [f               (fn ^double [^double x] (* x (Math/log x)))
          ^double y-start (f start)
          ^double y-end   (f end)
          y-step          (/ (- y-end y-start) (dec n))]
      (mapv (fn [^long i]
              (let [y (+ y-start (* i y-step))]
                (if (zero? i)
                  (long start)
                  (if (= i (dec n))
                    (long end)
                    (long (Math/round (invert-n-log-n y start)))))))
            (range n)))))

;;; Domain Builder

(def ^:private default-initial-limit-time-s 10)

(defn- simplified-impl-map?
  "Check if implementations map uses simplified form (values are Measured instances).
  In simplified form, each value is a Measured directly rather than a map with
  :measured and :args-builder keys."
  [implementations]
  (and (map? implementations)
       (seq implementations)
       (measured/measured? (val (first implementations)))))

(defn- normalize-implementations
  "Normalize simplified implementation map to full form.
  Converts {:impl-key measured} to {:impl-key {:measured measured :args-builder ...}}
  where args-builder returns the measured's own args-fn for any coordinate."
  [implementations]
  (into {}
        (map (fn [[impl-key m]]
               [impl-key {:measured m
                          :args-builder (constantly (:args-fn m))}]))
        implementations))

(defn- cartesian-product
  "Return cartesian product of axis values as sequence of maps.
  axes is a map of axis-key to sequence of values."
  [axes]
  (if (empty? axes)
    [{}]
    (let [[k vs] (first axes)
          rest-product (cartesian-product (dissoc axes k))]
      (for [v vs
            m rest-product]
        (assoc m k v)))))

(defn- predict-time-ns
  "Predict execution time for x using fitted model coefficients."
  ^double [model ^double x]
  (let [{:keys [coefficients]}
        model
        {:keys [^double a ^double b ^double c]}
        coefficients
        model-def (get
                   analysis/default-complexity-models
                   (:id model))]
    (if-let [transforms (:transforms model-def)]
      ;; Composite model: y = a*f1(x) + b*f2(x) + c
      (let [[t1 t2] transforms]
        (+ (* a (double (t1 x))) (* b (double (t2 x))) c))
      ;; Simple model: y = a*f(x) + b
      (let [transform (:transform model-def)]
        (+ (* a (double (transform x))) b)))))

(defn- estimate-limit-time-s
  "Estimate limit-time-s for next run based on collected data.
  Uses projected time for time-limited runs, total benchmark time otherwise.
  Returns initial-limit-time-s if insufficient data for estimation."
  [impl-runs time-axis next-coord initial-limit-time-s]
  (if (< (count impl-runs) 2)
    initial-limit-time-s
    (let [;; Build extract-like data for fit-complexity
          ;; Use projected time for limited runs, total benchmark time otherwise
          extract-data
          (mapv
           (fn [{:keys [coord data]}]
             (let [time-ns (if-let [projected
                                    (get-in
                                     data
                                     [:samples :time-limit :projected-time-ns])]
                             projected
                             (get-in data [:samples :total-benchmark-time-ns]))]
               [coord time-ns]))
           impl-runs)
          extract    {:type    :criterium/domain-extract
                      :metrics {:benchmark-time
                                {:metric [:samples :total-benchmark-time-ns]
                                 :data   extract-data}}}
          regression (analysis/fit-complexity extract time-axis)
          best-model (first
                      (filter
                       #(= (:id %)
                           (get-in
                            regression
                            [:regressions :benchmark-time :best-fit]))
                       (get-in
                        regression
                        [:regressions :benchmark-time :models])))
          next-x     (get next-coord time-axis)]
      (if (and best-model next-x (> (double (:r-squared best-model)) 0.5))
        (let [predicted-ns (predict-time-ns best-model next-x)
              ;; Convert ns to seconds with margin
              predicted-s  (* 2.5 (/ predicted-ns 1e9))]
          (max predicted-s (double initial-limit-time-s)))
        initial-limit-time-s))))

;;; Reporter Protocol

(defprotocol DomainBuilderReporter
  "Protocol for reporting domain-builder progress."
  (report-start [reporter impl-key total-runs]
    "Called when starting to benchmark an implementation.")
  (report-run [reporter impl-key coord run-index]
    "Called after completing a benchmark run.")
  (report-end [reporter impl-key]
    "Called when finished benchmarking an implementation."))

(defrecord DotReporter []
  DomainBuilderReporter
  (report-start [_ impl-key total-runs]
                (print (str (name impl-key) " (" total-runs " runs): "))
                (flush))
  (report-run [_ _impl-key _coord _run-index]
              (print ".")
              (flush))
  (report-end [_ _impl-key]
              (println)))

(defn dot-reporter
  "Create a dot reporter that prints progress dots."
  []
  (->DotReporter))

(defn domain-builder
  "Build a domain by running benchmarks across a parameter space.

  Simplified form (no axes, just implementations):
    (domain-builder {:sort (measured/expr (sort coll))
                     :sort-by (measured/expr (sort-by identity coll))})

  Full form with axes:
    (domain-builder {:n (log-range 10 10000 5)}
                    {:sort {:measured (measured/expr (sort coll))
                            :args-builder (fn [{:keys [n]}] (fn [] [(vec (range n))]))}})

  In the full form, implementations is a map of impl-key to impl-spec where
  each impl-spec contains:
    :measured     - A Measured created with example args (establishes type hints)
    :args-builder - (fn [axis-map] (fn [] [args...])) returns zero-arg fn
                    producing argument vector for the given coordinates

  Options:
    :initial-limit-time-s - Time limit for runs before estimation kicks in
                            (default: 10 seconds)
    :time-axis           - Axis key for complexity modeling (default: first axis)
    :reporter            - Progress reporter (default: dot-reporter, nil for silent)
    :bench-options       - Additional options passed to bench-measured

  Benchmarks run in order: all coordinates for first implementation,
  then all for second, etc. Within each implementation, coordinates are
  sorted by :time-axis ascending (smallest values first) to enable
  adaptive time estimation.

  When a benchmark hits its time limit and the projected time differs from
  the limit by more than 5%, the benchmark is re-run with the projected time.

  Returns a domain with runs indexed by {:impl impl-key ...axis-coords...}.
  When multiple implementations are provided, the domain's :impl-axis
  key is set to :impl, enabling automatic grouping in analysis functions."
  [first-arg & args]
  ;; Detect simplified form: single map of impl-key -> Measured
  ;; In simplified form, first-arg is the implementations map, args are options
  ;; In full form, first-arg is axes, (first args) is implementations, rest are options
  (let [[axes implementations options]
        (if (simplified-impl-map? first-arg)
          [{} (normalize-implementations first-arg) (apply hash-map args)]
          [first-arg (first args) (apply hash-map (rest args))])
        {:keys [initial-limit-time-s time-axis reporter bench-options]
         :or   {initial-limit-time-s default-initial-limit-time-s}}
        options
        time-axis      (or time-axis (first (keys axes)))
        reporter       (if (contains? #{nil false} reporter)
                         nil
                         (or reporter (dot-reporter)))
        coords         (cartesian-product axes)
        ;; Sort by time-axis ascending
        sorted-coords  (sort-by #(get % time-axis) coords)
        total-runs     (count sorted-coords)
        impl-keys      (vec (keys implementations))
        ;; Create initial domain with :impl-axis when multiple impls
        initial-domain (if (> (count implementations) 1)
                         (domain/domain {:impl-axis       :impl
                                         :implementations impl-keys})
                         (domain/domain {:implementations impl-keys}))
        ;; Helper to run a single benchmark
        run-bench      (fn [coord-measured limit-time-s]
                         (let [bench-plan (bench/options->bench-plan
                                           (merge bench-options
                                                  {:limit-time-s limit-time-s
                                                   :viewer       :none}))]
                           (bench/bench-measured bench-plan coord-measured)
                           (:data (bench/last-bench))))
        ;; Helper to check if re-run is needed (>5% difference)
        needs-rerun?   (fn [bench-result limit-time-s]
                         (when-let [time-limit (get-in
                                                bench-result
                                                [:samples :time-limit])]
                           (let [projected-s (/
                                              (double
                                               (:projected-time-ns time-limit))
                                              1e9)
                                 diff-pct    (Math/abs
                                              (/ (- projected-s
                                                    (double limit-time-s))
                                                 (double limit-time-s)))]
                             (> diff-pct 0.05))))]

    (reduce
     (fn [domain [impl-key impl-spec]]
       (let [{:keys [measured args-builder]} impl-spec]
         (when reporter
           (report-start reporter impl-key total-runs))

         (let [result
               (reduce
                (fn [{:keys [domain impl-runs]} [idx coord]]
                  (let [;; Estimate time limit based on previous runs
                        limit-time-s   (estimate-limit-time-s
                                        impl-runs time-axis coord
                                        initial-limit-time-s)
                        ;; Create args-fn for this coordinate
                        args-fn        (args-builder coord)
                        ;; Create measured with new args-fn
                        coord-measured (measured/with-args-fn measured args-fn)
                        ;; Run benchmark
                        bench-result   (run-bench coord-measured limit-time-s)
                        ;; Re-run if time-limited and projected differs by >5%
                        bench-result   (if (needs-rerun? bench-result limit-time-s)
                                         (let [new-limit-s
                                               (* 1.1
                                                  (/
                                                   (double
                                                    (get-in
                                                     bench-result
                                                     [:samples :time-limit :projected-time-ns]))
                                                   1e9))]
                                           (run-bench coord-measured new-limit-s))
                                         bench-result)
                        ;; Build full coordinate with impl
                        full-coord     (assoc coord :impl impl-key)
                        ;; Accumulate into domain
                        new-domain     (domain/add-run domain full-coord bench-result)
                        ;; Track impl-specific runs for time estimation
                        new-impl-runs  (conj impl-runs {:coord coord
                                                        :data  bench-result})]

                    (when reporter
                      (report-run reporter impl-key coord idx))

                    {:domain    new-domain
                     :impl-runs new-impl-runs}))
                {:domain domain :impl-runs []}
                (map-indexed vector sorted-coords))]

           (when reporter
             (report-end reporter impl-key))

           (:domain result))))
     initial-domain
     implementations)))
