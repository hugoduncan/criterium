(ns criterium.collect-plan.config
  (:require
   [criterium.collect-plan :as collect-plan]
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

(defn full-collect-plan
  [{:keys [num-estimation-samples
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

(defn one-shot-collect-plan
  [{:keys [max-gc-attempts]
    :as   _options}]
  {:scheme-type     :one-shot
   :max-gc-attempts (or max-gc-attempts 3)})

(defn ensure-pipeline-stages
  "Add any injected stages that aren't already present."
  [{:keys [collector-config collect-plan] :as options}]
  (let [stages   (set (:stages collector-config))
        injected (collect-plan/required-stages collect-plan)]
    (update-in
     options
     [:collector-config :stages]
     (fnil into [])
     (remove stages injected))))
