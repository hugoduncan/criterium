(ns criterium.sampled-fn-notebook
  "Memory-efficient function sampling with t-digest aggregation."
  (:require
   [criterium.bench :as bench]
   [criterium.collector-configs :as collector-configs]
   [criterium.sampled-fn :as sampled-fn]
   [criterium.sampler :as sampler]
   [criterium.util.t-digest :as t-digest]
   [scicloj.kindly.v4.kind :as kind]))

;; # Function Sampling with sampled-fn
;;
;; The `sampled-fn` namespace provides memory-efficient function sampling
;; using t-digest data structures. Unlike `instrument-fn` which stores all
;; raw samples, `sample-fn` aggregates timing data into compact digests
;; suitable for long-running or high-volume sampling.

;; ## When to Use sample-fn vs instrument-fn
;;
;; Choose based on your sampling needs:
;;
;; **sample-fn (this notebook):**
;; - High-volume sampling (thousands+ calls)
;; - Long-running processes
;; - Production monitoring
;; - Memory-constrained environments
;; - When you only need quantile estimates
;;
;; **instrument-fn:**
;; - Detailed sample analysis
;; - Short measurement periods
;; - When you need raw sample values
;; - Histogram generation from actual samples

;; ## Creating a Sampled Function
;;
;; The `sample-fn` function wraps any function with timing collection
;; using t-digest for memory-efficient aggregation:

(defn slow-computation
  "A computation with variable execution time."
  [n]
  (Thread/sleep (+ 1 (long (rand-int 5))))
  (* (long n) (long n)))

(def sampled-slow
  "Sampled version of slow-computation."
  (sampled-fn/sample-fn slow-computation
                        collector-configs/default-collector-config))

;; The sampled function behaves exactly like the original:

{:original-result (slow-computation 5)
 :sampled-result (sampled-slow 5)}

;; ## The with-redefs Pattern
;;
;; A common pattern is to temporarily replace a function with its sampled
;; version using `with-redefs`. This captures timing from all existing
;; call sites without modifying calling code:

(defn process-item
  "Process a single item."
  [^long x]
  (Thread/sleep 1)
  (* x 2))

(defn batch-process
  "Process items using process-item."
  [items]
  (mapv process-item items))

(with-redefs [process-item (sampled-fn/sample-fn
                            process-item
                            collector-configs/default-collector-config)]
  ;; All calls to process-item are now sampled
  (batch-process (range 20))

  ;; Retrieve the digest data
  (let [samples (sampler/samples-map process-item)
        digest (get-in samples [:metric->digest [:elapsed-time]])]
    {:sample-count (t-digest/sample-count digest)
     :median-ns (t-digest/quantile digest 0.5)
     :p99-ns (t-digest/quantile digest 0.99)}))

;; ## Understanding the Sample Map
;;
;; The `samples-map` function returns a digest-based structure:
;; - `:metric->digest` - Map of metric paths to t-digest objects
;; - `:metrics-defs` - Definitions of collected metrics
;; - `:type` - Always `:criterium/digest` for sample-fn

(do
  (sampler/reset-samples! sampled-slow)
  (dotimes [_ 100] (sampled-slow 1))
  (let [samples (sampler/samples-map sampled-slow)]
    {:type (:type samples)
     :metric-keys (keys (:metric->digest samples))
     :digest-class (class (get-in samples [:metric->digest [:elapsed-time]]))}))

;; ## Working with T-Digests
;;
;; T-digests provide memory-efficient quantile estimation. Access
;; statistics directly from the digest:

(do
  (sampler/reset-samples! sampled-slow)
  (dotimes [_ 500] (sampled-slow 1))
  (let [digest (get-in (sampler/samples-map sampled-slow)
                       [:metric->digest [:elapsed-time]])]
    {:sample-count (t-digest/sample-count digest)
     :min-ns       (t-digest/minimum digest)
     :max-ns       (t-digest/maximum digest)
     :median-ns    (t-digest/quantile digest 0.5)
     :p90-ns       (t-digest/quantile digest 0.9)
     :p99-ns       (t-digest/quantile digest 0.99)}))

;; ## Analyzing Samples with bench/analyze
;;
;; For comprehensive analysis, use criterium's analysis pipeline.
;; The sample map from `sample-fn` is compatible with `bench/analyze`
;; and `bench/view`:

;; The analysis pipeline includes:
;; - `:transform-log` - Log transform for timing data
;; - `:quantiles` - Percentile calculations
;; - `:outliers` - Outlier detection
;; - `:stats` - Mean, variance, confidence intervals
;; - `:histogram` - Distribution visualization

(with-redefs [slow-computation (sampled-fn/sample-fn
                                slow-computation
                                collector-configs/default-collector-config)]
  ;; Collect samples
  (dotimes [_ 100] (slow-computation 1))

  ;; Run analysis pipeline
  (let [samples (sampler/samples-map slow-computation)]
    (->> {:samples samples}
         (bench/analyze
          [:transform-log
           [:quantiles {:quantiles [0.5 0.9 0.99]}]
           [:quantiles {:quantiles  [0.5 0.9 0.95 0.99]
                        :samples-id :log-samples
                        :id         :log-quantiles}]
           :outliers
           [:outliers {:samples-id   :log-samples
                       :quantiles-id :log-quantiles
                       :id           :log-outliers}]
           [:stats {}]
           [:stats {:samples-id  :log-samples
                    :outliers-id :log-outliers
                    :id          :log-stats}]
           :histogram])
         (bench/view
          [[:stats {:stats-id :log-stats}]
           :histogram]
          :print))
    nil))

;; ## Resetting Samples
;;
;; Use `reset-samples!` to clear the t-digest and start fresh:

(do
  (sampler/reset-samples! sampled-slow)
  (dotimes [_ 50] (sampled-slow 1))
  (let [before (t-digest/sample-count
                (get-in (sampler/samples-map sampled-slow)
                        [:metric->digest [:elapsed-time]]))]
    (sampler/reset-samples! sampled-slow)
    (let [after (t-digest/sample-count
                 (get-in (sampler/samples-map sampled-slow)
                         [:metric->digest [:elapsed-time]]))]
      {:before-reset before
       :after-reset  after})))

;; ## Memory Efficiency
;;
;; T-digests maintain constant memory regardless of sample count.
;; The sample count tracks how many samples have been added:

;; ## Callable and Runnable Interfaces
;;
;; Sampled functions implement `Callable` and `Runnable` for use
;; with executor services:

(let [sampled-f (sampled-fn/sample-fn
                 (fn [] (Thread/sleep 1) :done)
                 collector-configs/default-collector-config)]
  (sampler/reset-samples! sampled-f)

  ;; Run concurrently
  (let [futures (doall (repeatedly 10 #(future (sampled-f))))]
    (run! deref futures))

  {:sample-count (t-digest/sample-count
                  (get-in (sampler/samples-map sampled-f)
                          [:metric->digest [:elapsed-time]]))})

;; ## Practical Example: Database Query Monitoring
;;
;; Simulate monitoring database query performance in production:

(defn query-database
  "Simulate a database query with variable latency."
  [query-type]
  (Thread/sleep (long (case query-type
                        :simple    2
                        :join      5
                        :aggregate 10
                        5)))
  {:rows (rand-int 100)})

(with-redefs [query-database (sampled-fn/sample-fn
                              query-database
                              collector-configs/default-collector-config)]
  ;; Simulate mixed query workload
  (dotimes [_ 50] (query-database :simple))
  (dotimes [_ 30] (query-database :join))
  (dotimes [_ 20] (query-database :aggregate))

  (let [samples (sampler/samples-map query-database)
        digest  (get-in samples [:metric->digest [:elapsed-time]])]
    {:total-queries (t-digest/sample-count digest)
     :median-ms     (/ (t-digest/quantile digest 0.5) 1e6)
     :p95-ms        (/ (t-digest/quantile digest 0.95) 1e6)
     :p99-ms        (/ (t-digest/quantile digest 0.99) 1e6)}))

;; ## Comparison: sample-fn vs instrument-fn
;;
;; Key differences at a glance:

(kind/table
 {:column-names [:aspect :sample-fn :instrument-fn]
  :row-vectors
  [["Data structure" "t-digest" "Vector of samples"]
   ["Memory growth" "Constant (logarithmic)" "Linear with samples"]
   ["Raw samples" "No" "Yes"]
   ["Quantile access" "Direct from digest" "Computed from samples"]
   ["Best for" "High-volume monitoring" "Detailed analysis"]
   ["Sample map key" ":metric->digest" ":metric->values"]]})

;; ## Thread Safety Note
;;
;; The `with-redefs` pattern is NOT thread-safe: all threads see the
;; redefined var during the block. Use `sample-fn` directly (like
;; `sampled-slow` above) for thread-safe instrumentation where each
;; sampled function maintains independent state.

(comment
  ;; Quick test
  (sampled-slow 42)

  ;; Check current samples
  (sampler/samples-map sampled-slow)

  ;; Reset and collect fresh
  (sampler/reset-samples! sampled-slow)
  (dotimes [_ 100] (sampled-slow 1))
  (t-digest/quantile
   (get-in (sampler/samples-map sampled-slow)
           [:metric->digest [:elapsed-time]])
   0.5))
