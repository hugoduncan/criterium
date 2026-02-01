(ns in-situ
  "Measuring function performance during normal execution."
  (:require
   [criterium.bench :as bench]
   [criterium.collector-configs :as collector-configs]
   [criterium.instrument-fn :as inst-fn]
   [criterium.sampled-fn :as sampled-fn]
   [criterium.sampler :as sampler]
   [criterium.util.t-digest :as t-digest]
   [scicloj.kindly.v4.kind :as kind]))

;; # In-Situ Measurement
;;
;; Sometimes you need to measure performance during normal execution rather
;; than in isolated benchmarks. This chapter covers two approaches:
;;
;; - **instrument-fn**: Stores raw samples for detailed analysis
;; - **sampled-fn**: Uses t-digest for constant-memory production monitoring
;;
;; Both create instrumented function wrappers that collect timing data every
;; time they're called.

;; ## When to Use In-Situ Measurement
;;
;; Use in-situ measurement when you want to:
;;
;; - Measure real-world usage patterns with actual call frequencies
;; - Profile functions during test execution
;; - Monitor production performance without isolated benchmarks
;; - Capture timing across varied inputs from natural program flow
;;
;; Use `bench` when you want:
;;
;; - Controlled conditions with JIT warmup
;; - Statistical rigor with many samples
;; - Comparison of specific inputs or implementations

;; ## instrument-fn: Raw Samples for Testing
;;
;; `instrument-fn` wraps a function and stores every timing sample in a vector.
;; This gives you raw data for detailed statistical analysis.

(defn process-request
  "Simulate processing a request."
  [request]
  (Thread/sleep (long (+ 1 (rand-int 5))))
  {:status 200 :body (str "Processed: " (:path request))})

;; Create an instrumented version:

(def instrumented-process
  (inst-fn/instrument-fn process-request collector-configs/default-collector-config))

;; The instrumented function behaves identically to the original:

(instrumented-process {:path "/api/users"})

;; ### Collecting Samples During Tests
;;
;; A typical workflow: reset samples, run your test scenario, then analyze:

(do
  ;; Clear any previous samples
  (sampler/reset-samples! instrumented-process)

  ;; Simulate test traffic
  (doseq [path ["/api/users" "/api/orders" "/api/products"]]
    (dotimes [_ 10]
      (instrumented-process {:path path})))

  ;; Check sample count
  {:num-samples (:num-samples (sampler/samples-map instrumented-process))})

;; ### Analyzing Collected Samples
;;
;; The samples-map output feeds directly into criterium's analysis pipeline:

(do
  (sampler/reset-samples! instrumented-process)
  (dotimes [_ 50] (instrumented-process {:path "/test"}))

  (let [samples (sampler/samples-map instrumented-process)]
    (->> {:samples samples}
         (bench/analyze
          [:transform-log
           [:quantiles {:quantiles [0.5 0.9 0.99]}]
           :outliers
           [:stats {}]])
         (bench/view
          [[:stats {:metric-ids [:elapsed-time]}]
           [:quantiles {:metric-ids [:elapsed-time]}]]
          :print))
    nil))

;; The analysis pipeline works identically to `bench` results. You get the
;; same statistical summaries, outlier detection, and confidence intervals.

;; ## sampled-fn: Constant Memory for Production
;;
;; `sampled-fn` uses t-digest data structures that maintain constant memory
;; regardless of sample count. Trade-off: you get quantile estimates, not
;; raw samples.

(def sampled-process
  (sampled-fn/sample-fn process-request collector-configs/default-collector-config))

;; Same interface as instrument-fn:

(do
  (sampler/reset-samples! sampled-process)
  (dotimes [_ 1000] (sampled-process {:path "/test"}))

  (let [samples (sampler/samples-map sampled-process)
        digest  (get-in samples [:metric->digest [:elapsed-time]])]
    {:sample-count (t-digest/sample-count digest)
     :median-ms    (/ (t-digest/quantile digest 0.5) 1e6)
     :p99-ms       (/ (t-digest/quantile digest 0.99) 1e6)}))

;; ### Analysis Pipeline with sampled-fn
;;
;; The same `bench/analyze` and `bench/view` pipeline works with digest-based
;; samples:

(do
  (sampler/reset-samples! sampled-process)
  (dotimes [_ 100] (sampled-process {:path "/test"}))

  (let [samples (sampler/samples-map sampled-process)]
    (->> {:samples samples}
         (bench/analyze
          [:transform-log
           [:quantiles {:quantiles [0.5 0.9 0.99]}]
           [:stats {}]])
         (bench/view
          [[:stats {:metric-ids [:elapsed-time]}]
           [:quantiles {:metric-ids [:elapsed-time]}]]
          :print))
    nil))

;; ## Thread Safety
;;
;; Both `instrument-fn` and `sampled-fn` create thread-safe function objects.
;; Each instrumented function maintains its own internal state that can be
;; safely called from multiple threads:

(let [inst-f (inst-fn/instrument-fn
              (fn [x] (Thread/sleep 1) (* x x))
              collector-configs/default-collector-config)]
  (sampler/reset-samples! inst-f)

  ;; Safe: each future calls the same instrumented function
  (let [futures (mapv #(future (inst-f %)) (range 20))]
    (run! deref futures))

  {:num-samples (:num-samples (sampler/samples-map inst-f))})

;; ### The with-redefs Pattern (Not Thread-Safe)
;;
;; The existing notebooks show `with-redefs` for temporarily replacing a
;; function. This captures calls from existing code without modification:
;;
;; ```clojure
;; (with-redefs [my-fn (sampled-fn/sample-fn my-fn config)]
;;   (run-my-tests)
;;   (sampler/samples-map my-fn))
;; ```
;;
;; **Warning**: `with-redefs` is NOT thread-safe. All threads see the
;; redefined var during the block. Use it only for:
;;
;; - Single-threaded test scenarios
;; - Quick exploration in the REPL
;; - Situations where thread isolation doesn't matter
;;
;; For production or multi-threaded code, use the instrumented function
;; directly (as shown above).

;; ## Choosing Between Approaches
;;
;; The choice depends on your constraints and analysis needs:

(kind/table
 {:column-names ["Factor" "instrument-fn" "sampled-fn"]
  :row-vectors
  [["Memory" "Grows with samples" "Constant (~10KB)"]
   ["Raw samples" "Yes" "No (quantiles only)"]
   ["Best for" "Testing, bounded runs" "Production, unbounded"]
   ["Histogram" "From actual samples" "From digest estimates"]
   ["Sample limit" "Memory-bound" "Unlimited"]]})

;; ### Decision Guide
;;
;; **Use instrument-fn when:**
;; - Running tests with bounded sample counts
;; - You need raw sample values for custom analysis
;; - You want histograms from actual measurements
;; - Memory growth is acceptable (thousands of samples)
;;
;; **Use sampled-fn when:**
;; - Monitoring production systems
;; - Sample count is unbounded or very large
;; - You only need quantile estimates (p50, p99, etc.)
;; - Memory must stay constant

;; ## Further Reading
;;
;; For additional details, see:
;;
;; - [Analysis and View Options](./analysis_and_view_options.html) -
;;   Configuring analysis steps and output views
