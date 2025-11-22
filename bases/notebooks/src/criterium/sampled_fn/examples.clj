(ns criterium.sampled-fn.examples
  (:require
   [criterium.analyse]
   [criterium.bench :as bench]
   [criterium.collector-configs :as collector-configs]
   [criterium.sampled-fn :as sampled-fn]
   [criterium.sampler :as sampler]))

;; # Sampling Function Execution with criterium
;;
;; This notebook demonstrates how to use criterium's sampled-fn functionality
;; to analyze function behavior in situ - that is, while the function is being
;; used in its normal context rather than in an isolated benchmark.

;; ## Overview
;;
;; Sometimes we want to understand how a function performs in the context of
;; real usage rather than in isolation. The `sampled-fn` namespace provides
;; tools to wrap functions and collect timing data during normal execution.
;;
;; Key benefits of this approach:
;; - Measures real-world usage patterns
;; - Captures performance variations in context
;; - No artificial benchmark setup required

;; ## Basic Example - Timing a Sleep Function
;;
;; Let's start with a simple example - measuring the actual time taken
;; by Thread/sleep calls.

(defn sleep
  "A simple function that sleeps for 1ms"
  []
  (Thread/sleep 1))

;; We'll create a basic collection function that:
;; 1. Wraps our sleep function with sampling
;; 2. Calls it multiple times
;; 3. Analyzes and displays the results

(defn collect-sleep-samples
  "Collect and analyze samples of the sleep function"
  []
  (with-redefs
    [sleep
     (sampled-fn/sample-fn sleep collector-configs/default-collector-config)]

    ;; Execute the function multiple times
    (dotimes [_ 1000] (sleep))

    ;; Retrieve and analyze the samples
    (let [samples (sampler/samples-map sleep)]
      (->> {:samples samples}
           (bench/analyze
            [:transform-log
             [:quantiles {:quantiles [0.5 0.9 0.99]}] ; median and percentiles
             :outliers
             [:stats {}]
             [:stats {:samples-id :log-samples :id :log-stats}]
             :histogram])
           (bench/view
            [[:stats {:metric-ids [:memory]}]
             [:stats {:stats-id :log-stats}]
             :histogram]
            :portal)))))

;; ## More Realistic Example - Network Calls
;;
;; Let's simulate and measure a more realistic scenario - network calls
;; with varying latency.

(defn simulate-network-latency
  "Simulate a network call with variable latency"
  []
  (let [base-latency 50
        jitter (rand-int 20)]
    (Thread/sleep (+ base-latency jitter))
    {:status :success}))

(defn collect-network-samples
  "Collect and analyze samples of simulated network calls"
  []
  (with-redefs
    [simulate-network-latency
     (sampled-fn/sample-fn
      simulate-network-latency
      collector-configs/default-collector-config)]

    ;; Simulate a series of network calls
    (dotimes [_ 100]
      (simulate-network-latency))

    ;; Analyze the timing distribution
    (let [samples (sampler/samples-map simulate-network-latency)]
      (->> {:samples samples}
           (bench/analyze
            [:transform-log
             [:quantiles {:quantiles [0.5 0.9 0.95 0.99]}]
             :outliers
             [:stats {}]
             [:stats {:samples-id :log-samples :id :log-stats}]
             :histogram])
           (bench/view
            [[:stats {:metric-ids [:memory]}]
             [:stats {:stats-id :log-stats}]
             :histogram]
            :print)))))

;; ## Interpreting Results
;;
;; The results show several key metrics:
;;
;; 1. Quantiles: Show the distribution of execution times
;;    - 50th percentile (median) represents typical performance
;;    - Higher percentiles show worst-case scenarios
;;
;; 2. Outliers: Identify unusual execution times that might indicate problems
;;
;; 3. Histogram: Visualizes the distribution of execution times
;;    - Helps identify patterns and clusters
;;    - Shows if performance is consistent or varies widely

;; ## Running the Examples
;;
;; Execute either example to see the analysis:

;; Run the simple sleep example
(with-out-str (collect-sleep-samples))

;; Run the network latency example
(with-out-str (collect-network-samples))

;; The results will be displayed in Portal
