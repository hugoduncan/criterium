(ns criterium.basic-usage-notebook
  "Introduction to benchmarking with criterium."

  (:require
   [criterium.bench :as bench]
   [criterium.notebook.helpers :refer [bench-display]]
   [criterium.util.helpers :as util]
   [scicloj.kindly.v4.kind :as kind]))

;; # Basic Benchmarking with criterium
;;
;; Criterium provides statistically rigorous benchmarking that accounts for
;; JVM warmup, garbage collection, and measurement overhead.

;; ## The bench Macro
;;
;; The `bench` macro is the primary entry point for benchmarking expressions.
;; It handles:
;; - JIT compiler warmup
;; - Statistical sampling
;; - Outlier detection
;; - Result formatting

;; ### Simple Expression Benchmarking
;;
;; Start by benchmarking a simple arithmetic expression:

^:kindly/hide-code
(bench-display (bench/bench (+ 1 1)))

;; The output includes:
;; - Mean execution time with confidence interval
;; - Standard deviation
;; - Outlier detection
;; - Sample counts and execution time

;; ### String Operations
;;
;; String operations are common benchmarking targets:

^:kindly/hide-code
(bench-display (bench/bench (str "hello" " " "world")))

;; ### Collection Operations
;;
;; Measure common collection operations:

^:kindly/hide-code
(bench-display (bench/bench (vec (range 100))))

;; For benchmarking with data, pass it as a function argument.
;; Criterium hoists function arguments to prevent constant folding:

(def sample-map
  "Sample data for benchmarking."
  {:a 1 :b 2 :c 3 :d 4 :e 5})

^:kindly/hide-code
(bench-display (bench/bench (sample-map :c)))

;; ## Understanding the Output
;;
;; The default output shows several key metrics:

;; ### Elapsed Time Statistics
;;
;; The primary metric is elapsed time, shown as mean with confidence bounds:

^:kindly/hide-code
(kind/code "Elapsed Time: 58.8 ns  3σ [55.9 61.7]  min 56.3")

;; The first number is the mean, the numbers in brackets are the 3-sigma range
;; for the time (in the same units as the mean) assuming a log-normal time
;; distribution, followed by the minimum value seen.

;; ### Outliers
;;
;; Criterium detects and reports statistical outliers:

^:kindly/hide-code
(kind/code "Outliers (outliers / samples): low-severe 0 (0.0%), ...")

;; Outlier categories:
;; - **low-severe**: Unusually fast executions
;; - **low-mild**: Somewhat fast executions
;; - **high-mild**: Somewhat slow executions
;; - **high-severe**: Unusually slow executions

;; ### Collection Plan
;;
;; Shows how samples were collected:

^:kindly/hide-code
(kind/code "Sample Scheme: 200 samples with batch-size 172 (34400 evaluations)")

;; This is the sampling scheme for the actual measurement (not the
;; warmup). Samples are batched so the total sample is not significantly
;; effected by the resolution of the timer.

;; ## Using last-bench
;;
;; The `last-bench` function returns detailed results from the most recent
;; benchmark, allowing programmatic access to all metrics.

(defn examine-bench-results
  "Run a benchmark and examine the results programmatically."
  []
  ;; Run a benchmark
  (bench/bench (reduce + (range 100)))

  ;; Retrieve the complete results
  (bench/last-bench))

;; ### Accessing Specific Metrics
;;
;; The result map contains all collected data. Common access patterns:

(do
  (bench/bench (reduce + (range 100)))
  (util/stats-value
   (:data (bench/last-bench))
   :log-stats
   :elapsed-time
   :mean))

;; ### Comparing Implementations
;;
;; Use `last-bench` to compare different implementations:

(def numbers-for-sum
  "Test data for comparing sum implementations."
  (vec (range 1000)))

(do
  (println "With reduce")
  (bench/bench (reduce + numbers-for-sum))
  (let [^double reduce-mean (util/stats-value
                             (:data (bench/last-bench))
                             :stats
                             :elapsed-time
                             :mean)]
    (println "With apply")
    (bench/bench (apply + numbers-for-sum))
    (let [^double apply-mean (util/stats-value
                              (:data (bench/last-bench))
                              :stats
                              :elapsed-time
                              :mean)]
      {:reduce-ns reduce-mean
       :apply-ns  apply-mean
       :ratio     (/ apply-mean reduce-mean)})))

;; ## Best Practices

;; ### Isolate What You Measure
;;
;; Separate data setup from the code being measured. You can use local bindings
;; directly in bench expressions:

(defn bench-with-setup
  "Demonstrate using local bindings in bench."
  []
  (let [large-data (vec (range 10000))]
    (bench/bench (reduce + large-data))))

;; ### Avoid Side Effects
;;
;; Pure expressions produce more consistent measurements:

(defn bench-pure
  "Benchmark a pure computation."
  []
  (bench/bench (Math/sqrt 12345.67)))

;; ### Understand JVM Warmup
;;
;; The JIT compiler optimizes hot code paths. Criterium handles warmup
;; automatically, but initial runs may be slower.

;; ## Using Local Bindings
;;
;; The bench macro supports local bindings from the enclosing scope.
;; This makes it easy to benchmark with dynamically created data:

(let [data (vec (range 1000))]
  (bench/bench (reduce + data)))

;; Local bindings work in loop contexts too:

(doseq [size [100 1000 10000]]
  (println
   "Size:" size
   "," (-> (bench/bench (reduce + (range size)) :return-value [] :viewer :none)
           (util/stats-value :stats :elapsed-time :mean))
   "ns"))

;; ## Running Examples
;;
;; Execute any of the functions above to see benchmarking in action:

(comment
  ;; Examine results
  (examine-bench-results)

  ;; With setup
  (bench-with-setup)

  ;; Pure computation
  (bench-pure))
