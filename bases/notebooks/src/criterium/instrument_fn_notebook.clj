(ns criterium.instrument-fn-notebook
  "Instrument functions for continuous performance sampling."
  (:require
   [criterium.bench :as bench]
   [criterium.collector-configs :as collector-configs]
   [criterium.instrument-fn :as inst-fn]
   [criterium.sampler :as sampler]
   [scicloj.clay.v2.api :as clay]
   [scicloj.kindly.v4.kind :as kind]))

;; # Function Instrumentation with criterium
;;
;; You can instrument functions to collect performance data during normal
;; execution. Unlike `bench`, which runs expressions in isolation, instrumented
;; functions collect timing data every time they are called in your application
;; or in tests.

;; ## When to Use Instrumentation
;;
;; Use `instrument-fn` when you want to:
;; - Measure real-world usage patterns
;; - Collect samples over time during normal operation
;; - Profile functions without isolated benchmarking
;; - Track performance variations in production-like conditions

;; ## Creating an Instrumented Function
;;
;; The `instrument-fn` function wraps any function with timing collection:

(defn slow
  "A computation with variable execution time."
  [n]
  (Thread/sleep (long (+ 1 (rand-int 5))))
  (* n n))

(def instrumented-slow
  "Instrumented version of slow-computation."
  (inst-fn/instrument-fn slow collector-configs/default-collector-config))

;; The instrumented function behaves exactly like the original:

{:original-result     (slow 5)
 :instrumented-result (instrumented-slow 5)}

;; ## The Sampler Protocol
;;
;; Instrumented functions implement the `Sampler` protocol with two operations:
;; - `samples-map` - Retrieve collected samples
;; - `reset-samples!` - Clear all collected samples
;;
;; These are available in `criterium.sampler`:

(do
  ;; Reset any previous samples
  (sampler/reset-samples! instrumented-slow)

  ;; Execute several times
  (dotimes [_ 10]
    (instrumented-slow 42))

  ;; Retrieve collected samples
  (sampler/samples-map instrumented-slow))

;; ## Understanding the Sample Map
;;
;; The `samples-map` function returns a metrics sample map containing:
;; - `:metric->values` - Map of metric IDs to sample vectors
;; - `:num-samples` - Count of samples collected
;; - `:eval-count` - Total evaluations (same as num-samples for instrumentation)
;; - `:batch-size` - Always 1 for instrumented functions
;; - `:metrics-defs` - Definitions of collected metrics

(do
  (sampler/reset-samples! instrumented-slow)
  (dotimes [_ 5] (instrumented-slow 1))
  (let [samples (sampler/samples-map instrumented-slow)]
    {:num-samples   (:num-samples samples)
     :eval-count    (:eval-count samples)
     :batch-size    (:batch-size samples)
     :metric-keys   (keys (:metric->values samples))
     :elapsed-times (get (:metric->values samples) [:elapsed-time])}))

;; ## Analyzing Collected Samples
;;
;; Collected samples can be analyzed using criterium's analysis pipeline.
;; The sample map format is compatible with `bench/analyze` and `bench/view`:

(do
  (sampler/reset-samples! instrumented-slow)

  ;; Collect more samples for meaningful statistics
  (dotimes [_ 50]
    (instrumented-slow 10))

  ;; Analyze the samples
  (let [samples (sampler/samples-map instrumented-slow)]
    (->> {:samples samples}
         (bench/analyze
          [:transform-log
           [:quantiles {:quantiles [0.5 0.9 0.99]}]
           :outliers
           [:stats {}]
           [:stats {:samples-id :log-samples :id :log-stats}]
           :histogram])
         (bench/view
          [[:stats {:metric-ids [:elapsed-time]}]
           [:quantiles {:metric-ids [:elapsed-time]}]
           :histogram]
          :print))
    nil))

;; ## Multiple Arguments and Return Values
;;
;; Instrumented functions preserve argument passing and return values:

(defn process-data
  "Process data with multiple arguments."
  [data multiplier offset]
  (Thread/sleep 1)
  (+ (* data multiplier) offset))

(def instrumented-process
  "Instrumented version of process-data."
  (inst-fn/instrument-fn process-data collector-configs/default-collector-config))

(do
  (sampler/reset-samples! instrumented-process)

  ;; Call with various arguments
  (let [results (for [n (range 5)]
                  (instrumented-process n 2 10))]

    {:results     (vec results)
     :num-samples (:num-samples (sampler/samples-map instrumented-process))}))

;; ## Resetting Samples
;;
;; Use `reset-samples!` to clear collected data between measurement periods:

(do
  (sampler/reset-samples! instrumented-slow)
  (dotimes [_ 10] (instrumented-slow 1))
  (let [before-reset (:num-samples (sampler/samples-map instrumented-slow))]

    ;; Reset clears all samples
    (sampler/reset-samples! instrumented-slow)
    (let [after-reset (:num-samples (sampler/samples-map instrumented-slow))]

      ;; Collect new samples
      (dotimes [_ 5] (instrumented-slow 1))
      (let [after-new (:num-samples (sampler/samples-map instrumented-slow))]
        {:before-reset before-reset
         :after-reset  after-reset
         :after-new    after-new}))))

;; ## Callable and Runnable Interfaces
;;
;; Instrumented functions implement `Callable` and `Runnable` interfaces,
;; making them suitable for executor services and concurrent use:

(do
  (let [inst-f (inst-fn/instrument-fn
                (fn [] (Thread/sleep 1) :done)
                collector-configs/default-collector-config)]
    (sampler/reset-samples! inst-f)

    ;; Run concurrently
    (let [futures (doall (repeatedly 10 #(future (inst-f))))]
      (run! deref futures))

    {:num-samples (:num-samples (sampler/samples-map inst-f))}))

;; ## Comparing to with-redefs Approach
;;
;; The sampled-fn examples use `with-redefs` to temporarily replace functions.
;; `instrument-fn` creates a separate instrumented version, which has
;; different tradeoffs:
;;
;; **with-redefs approach:**
;; - Instruments existing call sites automatically
;; - Temporary instrumentation during a block
;; - Thread-unsafe: affects all threads
;;
;; **instrument-fn approach:**
;; - Creates a new function object
;; - Requires explicit use of the instrumented version
;; - Thread-safe: each instrumented function maintains its own state
;; - Samples persist until explicitly reset

;; ## Practical Example: HTTP Handler Simulation
;;
;; Instrument a simulated HTTP handler to track request timing:

(defn handle-request
  "Simulate handling an HTTP request."
  [request]
  (Thread/sleep (long (+ 5 (rand-int 10))))
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (str "Processed: " (:path request))})

(def instrumented-handler
  "Instrumented HTTP handler."
  (inst-fn/instrument-fn handle-request collector-configs/default-collector-config))

(do
  (sampler/reset-samples! instrumented-handler)

  ;; Simulate requests
  (doseq [path ["/api/users" "/api/products" "/api/orders"]]
    (dotimes [_ 20]
      (instrumented-handler {:path path :method :get})))

  ;; Analyze the response time distribution
  (let [samples (sampler/samples-map instrumented-handler)]
    (->> {:samples samples}
         (bench/analyze
          [:transform-log
           [:quantiles {:quantiles [0.5 0.9 0.95 0.99]}]
           :outliers
           [:stats {}]])
         (bench/view
          [[:stats {:metric-ids [:elapsed-time]}]
           [:quantiles {:metric-ids [:elapsed-time]}]]
          :print))))
