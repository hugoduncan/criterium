(ns criterium.collect-plan
  "Collection plan to control the collection of metrics from a measured."
  (:require
   [criterium.array :as arr]
   [criterium.collect :as collect]
   [criterium.collect-plan.impl :as impl]
   [criterium.collector :as collector]
   [criterium.jvm :as jvm]
   [criterium.measured :as measured]
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have have?]]))

(defn required-stages
  "Metrics collection stages required for the given collection-scheme"
  [collect-plan-id]
  {:pre [(have? keyword? collect-plan-id)]}
  (impl/required-stages* collect-plan-id))

(defmethod impl/required-stages* :one-shot
  [_collect-plan]
  [])

(defmethod impl/required-stages* :with-jit-warmup
  [_collect-plan]
  [:measured-args
   :compilation
   :garbage-collector])

(def identity-transforms
  {:sample-> (fn sample-> ^double [v] v)
   :->sample (fn ->sample ^double [v] v)})

(defn- batch-transforms
  [^long batch-size]
  {:sample-> (fn sample-> ^double [^double v]
               (/ v batch-size))
   :->sample (fn ->sample ^double [^double v]
               (* v batch-size))})

(defmethod impl/collect* :one-shot
  ;; Collect a single sample with optional warmup invocations.
  ;;
  ;; The :num-warmup option specifies how many throw-away invocations to run
  ;; before measurement. This is useful for skipping JVM first-invocation
  ;; allocation overhead (use :num-warmup 1).
  ;;
  ;; GC is forced after warmup invocations (even when :num-warmup is 0),
  ;; then a single measurement is taken.
  ;;
  ;; Returns a sampled data map.
  [collect-plan collector measured]
  (let [{:keys [^long max-gc-attempts ^long num-warmup]
         :or   {num-warmup 0}} collect-plan]
    ;; Warmup invocations (if any)
    (dotimes [_ num-warmup]
      (collect/throw-away-collection measured))
    ;; Force GC after warmup
    (collect/force-gc! max-gc-attempts)
    ;; Actual measurement
    (let [args            (measured/args measured)
          sample          (collector/collect collector measured args 1)
          elapsed-time-ns (metric/elapsed-time sample)]
      {:samples
       {:type                    :criterium/metrics-samples
        :metrics-defs            (:metrics-defs collector)
        :metric->values          (collect/sample-maps->map-of-samples
                                  [sample]
                                  (:metrics-defs collector))
        :transform               identity-transforms
        :batch-size              1
        :elapsed-time            elapsed-time-ns
        :total-benchmark-time-ns elapsed-time-ns
        :eval-count              1
        :num-samples             1
        :expr-value              (:expr-value sample)}})))

(defn- collected-data-map
  [collection-map]
  (let [metric->values (collect/transform collection-map)
        batch-size (:batch-size collection-map)
        expr-values (metric->values [:expr-value])]
    (util/->collected-metrics-map
     (merge
      collection-map
      {:metric->values metric->values
       :metrics-defs (have (:metrics-defs (:collector collection-map)))
       :expr-value (if expr-values
                     (arr/last-object expr-values)
                     :criterium/not-collected)
       :type :criterium/metrics-samples
       :transform (if (= 1 batch-size)
                    identity-transforms
                    (batch-transforms batch-size))}))))

(defmethod impl/collect* :with-jit-warmup
  ;; Sample measured with estimation, warmup and forced GC.
  ;; Return a sampled data map.
  [collect-plan collector measured]
  {:pre [(fn? (:f collector))
         (measured/measured? measured)]}
  (let [{:keys [^long batch-time-ns
                ^long limit-time-ns
                ^long max-gc-attempts
                ^long thread-priority
                ^long num-estimation-samples
                ^long num-warmup-samples
                ^long num-measure-samples
                keep-estimation?
                keep-warmup?
                keep-final-gc?]} collect-plan]

    (util/with-thread-priority thread-priority
      (let [start-time-ns (jvm/timestamp)
            ;; Start by running GC.
            _             (collect/force-gc-no-capture! max-gc-attempts)

            ;; First sample is always much longer than subsequent ones
            _ (collect/throw-away-collection measured)

            total-samples (+ num-estimation-samples
                             num-warmup-samples
                             num-measure-samples)

            t0             (collect/elapsed-time-point-estimate measured)
            est-batch-size (collect/batch-size t0 batch-time-ns)

            frac-est        (double (/ num-estimation-samples total-samples))
            num-est-samples (max (min num-estimation-samples
                                      (long (/ (* limit-time-ns frac-est)
                                               (* t0 est-batch-size))))
                                 1)
            est-data        (collect/elapsed-time-min-estimate
                             measured
                             num-est-samples
                             est-batch-size)
            t1              (long (:t est-data))

            warmup-batch-size (collect/batch-size t1 batch-time-ns)
            remaining-samples (+ num-warmup-samples num-measure-samples)
            remaining-time    (- limit-time-ns (long (:total-time est-data)) t0)
            batch-time        (* t1 warmup-batch-size)

            projected-time (* batch-time remaining-samples)

            limit-result        (impl/limit-samples
                                 limit-time-ns
                                 num-warmup-samples
                                 num-measure-samples
                                 (:total-time est-data)
                                 remaining-time
                                 projected-time)
            num-warmup-samples  (:num-warmup-samples limit-result)
            num-measure-samples (:num-measure-samples limit-result)

            _ (collect/force-gc! max-gc-attempts)

            warmup-data (collect/warmup
                         collector
                         measured
                         num-warmup-samples
                         warmup-batch-size)

            t2         (max
                        1
                        (double (/ (long (:min-time warmup-data))
                                   warmup-batch-size)))
            batch-size (collect/batch-size t2 batch-time-ns)

            ;; Enter garbage Free zone
            _                       (collect/force-gc-no-capture! max-gc-attempts)
            sample-data             (collect/collect-arrays
                                     collector measured batch-size num-measure-samples)
            final-gc-data           (collect/force-gc! max-gc-attempts)
            ;; Leave garbage Free zone
            total-benchmark-time-ns (jvm/elapsed-time
                                     start-time-ns
                                     (jvm/timestamp))
            samples                 (cond-> (collected-data-map sample-data)
                                      true
                                      (assoc :total-benchmark-time-ns
                                             total-benchmark-time-ns)
                                      (:time-limited? limit-result)
                                      (assoc
                                       :time-limit
                                       {:limited?          true
                                        :projected-time-ns (:projected-time-ns
                                                            limit-result)
                                        :limit-time-ns     limit-time-ns}))]

        (cond->
          {:samples samples}
          keep-estimation? (assoc :estimation (collected-data-map est-data))
          keep-warmup?     (assoc :warmup (collected-data-map warmup-data))
          keep-final-gc?   (assoc :final-gc
                                  (collected-data-map final-gc-data)))))))

(defn collect
  "Collect metrics from the measured according to the collect-plan.
  Return a results-map."
  [collect-plan collector measured]
  (impl/collect* collect-plan collector measured))
