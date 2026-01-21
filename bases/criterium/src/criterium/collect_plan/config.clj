(ns criterium.collect-plan.config
  (:require
   [criterium.collect-plan :as collect-plan]
   [criterium.util.invariant :refer [have?]]
   [criterium.util.units :as units]))

(def ^Long DEFAULT-BATCH-TIME-NS
  ;; This value is a trade-off.
  ;; - We want enough time to make system timestamp quantisation insignificant,
  ;; - We want to limit the time to avoid the thread scheduler taking the thread
  ;;   during sampling.
  ;; - JII compilation happens on the sample pipeline batch, so increasing the
  ;;   batch time requires longer to reach JIT compilation thresholds.
  ;;
  ;; The value is based on a timestamp granularity of ~30ns.
  ;; On linux sched_min_granularity_ns is often around 10ms.
  ;;
  ;; If you see sine waves or saw tooth patterns in the sample times of very
  ;; fast functions, then is this probably to this value being too low.
  ;; https://en.wikipedia.org/wiki/Nyquist_frequency
  (* 10 ^long units/MICROSEC-NS))

(def ^Long DEFAULT-LIMIT-TIME-NS
  ;; This limit is so that, by default, we don't spend more time that what
  ;; a casual user might be willing to spend.
  (* 10 ^long units/SEC-NS))

(def TARGET-ESTIMATION-SAMPLES 1000)
(def TARGET-WARMUP-SAMPLES 150000)
(def TARGET-SAMPLES 200)

(defmulti collect-plan-config
  "Create a collect-plan configuration map from an id and options.

  Available collect-plan types:

  :one-shot - Single measurement without JIT warmup.
    Options:
      :num-warmup      - Number of warmup invocations before measurement. Use 1 to
                         skip JVM first-invocation allocation overhead. Default: 0
      :max-gc-attempts - Maximum GC attempts after warmup. Default: 3

  :with-jit-warmup - Multiple samples with JIT warmup phase.
    Options:
      :num-estimation-samples - Samples for timing estimation. Default: 1000
      :num-warmup-samples     - Samples for JIT warmup. Default: 150000
      :num-measure-samples    - Samples for measurement. Default: 200
      :max-gc-attempts        - Maximum GC attempts. Default: 6
      :limit-time-ns          - Time limit in nanoseconds. Default: 10s
      :batch-time-ns          - Target batch time. Default: 10µs
      :thread-priority        - Optional thread priority during measurement

  :without-jit-warmup - Multiple samples without warmup phase.
    Same options as :with-jit-warmup but :num-warmup-samples defaults to 0."
  (fn [collect-plan-id _options] collect-plan-id))

(defmethod collect-plan-config :default
  [collect-plan-id _]
  (throw (ex-info "Unknown collect-plan" {:collect-plan collect-plan-id})))

(defmethod collect-plan-config :one-shot
  ;; Configure a one-shot collection plan.
  ;;
  ;; Options:
  ;;   :num-warmup      - Number of warmup invocations before measurement. Use 1 to
  ;;                      skip JVM first-invocation allocation overhead. Default: 0
  ;;   :max-gc-attempts - Maximum GC attempts after warmup. Default: 3
  ;;
  ;; Returns config map with :scheme-type :one-shot.
  [_collect-plan-id
   {:keys [max-gc-attempts num-warmup]
    :or   {max-gc-attempts 3
           num-warmup      0}
    :as   _options}]
  {:scheme-type     :one-shot
   :max-gc-attempts max-gc-attempts
   :num-warmup      num-warmup})

(defmethod collect-plan-config :with-jit-warmup
  [_collect-plan-id
   {:keys [num-estimation-samples
           num-warmup-samples
           num-measure-samples
           max-gc-attempts
           thread-priority
           limit-time-ns
           batch-time-ns]
    :or   {num-estimation-samples TARGET-ESTIMATION-SAMPLES
           num-warmup-samples     TARGET-WARMUP-SAMPLES
           num-measure-samples    TARGET-SAMPLES
           limit-time-ns          DEFAULT-LIMIT-TIME-NS
           batch-time-ns          DEFAULT-BATCH-TIME-NS
           max-gc-attempts        6}
    :as   _options}]
  {:scheme-type            :with-jit-warmup
   :batch-time-ns          batch-time-ns
   :max-gc-attempts        max-gc-attempts
   :thread-priority        thread-priority
   :limit-time-ns          limit-time-ns
   :num-estimation-samples num-estimation-samples
   :num-warmup-samples     num-warmup-samples
   :num-measure-samples    num-measure-samples})

(defmethod collect-plan-config :without-jit-warmup
  [_collect-plan-id
   options]
  (collect-plan-config
   :without-jit-warmup
   (merge {:num-warmup-samples 0} options)))

(defn ensure-pipeline-stages
  "Add any injected stages that aren't already present."
  [collect-plan-id collector-config]
  {:pre [(have? keyword? collect-plan-id)]}
  (let [stages   (set (:stages collector-config))
        injected (collect-plan/required-stages collect-plan-id)]
    (update
     collector-config
     :stages
     (fnil into [])
     (remove stages injected))))
