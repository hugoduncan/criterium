(ns criterium.collect-plan
  "Collection plan to control the collection of metrics from a measured."
  (:require
   [criterium.collect :as collect]
   [criterium.collect-plan.impl :as impl]
   [criterium.collector :as collector]
   [criterium.measured :as measured]
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]))

(defn required-stages
  "Metrics collection stages required for the given collection-scheme"
  [collect-plan]
  (impl/required-stages* collect-plan))

(defmethod impl/required-stages* :one-shot
  [_collect-plan]
  [])

(defmethod impl/required-stages* :with-jit-warmup
  [_collect-plan]
  (mapv
   collector/maybe-var-get-stage
   [:measured-args
    :compilation
    :garbage-collector]))

(defmethod impl/collect* :one-shot
  ;; Collects a Single sample measured with no warmup of the measured function.
  ;; Forces GC.
  ;; Return a sampled data map.
  [collect-plan metrics-configs pipeline measured]
  (let [args   (measured/args measured)
        sample (collector/collect pipeline measured args 1)]
    (collect/force-gc! (:max-gc-attempts collect-plan))
    {:batch-size   1
     :elapsed-time (metric/elapsed-time sample)
     :eval-count   1
     :samples      (with-meta
                     ((collect/sample-maps->map-of-samples metrics-configs)
                      [sample])
                     {:type      :criterium/samples
                      :transform {:sample-> identity :->sample identity}})
     :num-samples  1
     :expr-value   (:expr-value sample)}))

(defmethod impl/collect* :with-jit-warmup
  ;; Sample measured with estimation, warmup and forced GC.
  ;; Return a sampled data map.
  [collect-plan metrics-configs pipeline measured]
  {:pre [(fn? (:f pipeline))
         (measured/measured? measured)]}
  (let [{:keys [^long batch-time-ns
                ^long limit-time-ns
                ^long max-gc-attempts
                ^long thread-priority
                ^long num-estimation-samples
                ^long num-warmup-samples
                ^long num-measure-samples]} collect-plan]

    ;; Start by running GC.
    (collect/force-gc-no-capture! max-gc-attempts)

    ;; ;; All evaluations in estimation and warmup are with different pipelines,
    ;; ;; so we need to JIT the user specified pipeline.
    ;; (toolkit/warmup-pipeline pipeline)

    ;; First sample is always much longer than subsequent ones
    (collect/throw-away-collection measured)

    (util/with-thread-priority thread-priority
      (let [total-samples (+ num-estimation-samples
                             num-warmup-samples
                             num-measure-samples)
            t0            (collect/elapsed-time-point-estimate measured)
            batch-size    (collect/batch-size t0 batch-time-ns)
            frac-est      (double (/ num-estimation-samples total-samples))
            num-samples   (min num-estimation-samples
                               (long (/ (* limit-time-ns frac-est)
                                        (* t0 batch-size))))
            est-data      (collect/elapsed-time-min-estimate
                           measured
                           num-samples
                           batch-size)
            t1            (long (:t est-data))
            batch-size    (collect/batch-size t1 batch-time-ns)

            remaining-samples (+ num-warmup-samples num-measure-samples)
            remaining-time    (- limit-time-ns (long (:total-time est-data)) t0)
            batch-time        (* t1 batch-size)
            projected-time    (* batch-time remaining-samples)

            [num-warmup-samples
             num-measure-samples] (impl/limit-samples
                                   limit-time-ns
                                   num-warmup-samples
                                   num-measure-samples
                                   (:total-time est-data)
                                   remaining-time
                                   projected-time)

            _ (collect/force-gc! max-gc-attempts)

            ;; Use the same batch size and pipeline for warmup and sampling to
            ;; make use of any loop unrolling JIT has done.
            warmup-data (collect/warmup
                         pipeline measured num-warmup-samples batch-size)

            batch-size (Long. batch-size)

            ;; Enter garbage Free zone
            _             (collect/force-gc-no-capture! max-gc-attempts)
            sample-data   (collect/collect-arrays
                           pipeline measured batch-size num-measure-samples)
            final-gc-data (collect/force-gc! max-gc-attempts)
            ;; Leave garbage Free zone (actually the end is in force-gc!, after
            ;; sample collection)

            sample-data (collect/transform metrics-configs pipeline sample-data)

            batch-size ^long (:batch-size sample-data)
            sample->   (fn sample-> ^double [v]
                         (/ (double v) (double batch-size)))
            ->sample   (fn ->sample ^double [v]
                         (* (double v) (double batch-size)))]
        (assoc
         (update
          sample-data
          :samples
          with-meta
          {:transform {:sample-> sample-> :->sample ->sample}
           :type      :criterium/samples})
         :estimation est-data
         :warmup     warmup-data
         :final-gc   final-gc-data
         :num-samples num-measure-samples
         :expr-value (last
                      ((:samples sample-data) [:expr-value])))))))

(defn collect
  "Collect metrics from the measured according to the collect-plan."
  [collect-plan metrics-configs pipeline measured]
  (-> (impl/collect* collect-plan metrics-configs pipeline measured)
      (vary-meta assoc :type :criterium/sampled)))
