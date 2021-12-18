(ns criterium.time.spec
  (:require
   [clojure.spec.alpha :as s]
   [criterium.domain :as domain]
   [criterium.measured :as measured]
   [criterium.sample-scheme :as sample-scheme]
   [criterium.pipeline :as pipeline]
   [criterium.pipeline.fns :as pipeline-fns]))

;; (s/def ::input-state
;;   (s/keys :req-un [::measured/measured
;;                    ::config/config]))

;; (s/def ::full-state
;;   (s/keys :req-un [::measured/measured
;;                    ::config/config
;;                    ::pipeline/pipeline
;;                    ::pipeline/metrics
;;                    ::sample-scheme/sample-result]))

;; (s/fdef measure
;;   :args (s/cat :input-state ::input-state)
;;   :ret ::full-state)


(s/def ::limit-eval-count (s/or :empty? nil? :limit ::domain/eval-count))
(s/def ::limit-time-s (s/or :empty? nil? :limit (s/and number? pos?)))
(s/def ::pipeline (s/coll-of
                   (s/or :pipeline-fn ::pipeline-fns/pipeline-fn-kw
                         :terminal-fn ::pipeline-fns/terminal-fn-kw)))
(s/def ::options (s/keys
                  :opt-un [::histogram
                           ::view-samples
                           ::limit-eval-count
                           ::limit-time-s
                           ::pipeline
                           ::sample-scheme
                           ::report]))
