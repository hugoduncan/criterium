(ns criterium.pipeline.spec-test
  (:require
   [criterium.pipeline.spec :as pipeline-spec]
   [orchestra.spec.test :as stest]))



;; (stest/unstrument)

;; (stest/unstrument
;;  [`pipeline/sample
;;   `pipeline/metrics
;;   `pipeline/pipeline
;;   `pipeline/pipeline*])

#_(clojure.spec.test.alpha/check
   [`metric/elapsed-time-metric
    `pipeline/with-class-loader
    `pipeline/with-compilation
    `pipeline/with-memory
    `pipeline/with-runtime-memory
    `pipeline/with-finalization
    `pipeline/with-garbage-collector
    `metric/elapsed-time
    `metric/total-memory
    `metric/divide
    `pipeline/pipeline*
    `pipeline/pipeline
    `pipeline/sample])
