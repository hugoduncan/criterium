(ns criterium.sample-scheme.spec
  (:require
   [clojure.spec.alpha :as s]
   [criterium.domain :as domain]))

(s/def ::scheme-type #{:one-shot :full})

(s/def ::max-gc-attempts nat-int?)

(s/def ::batch-time-ns ::domain/elapsed-time)

(s/def ::estimation-budget ::budget/budget)
(s/def ::warmup-budget ::budget/budget)
(s/def ::sample-budget ::budget/budget)

(s/def ::full-config
  (s/keys
   :req-un
   [::scheme-type
    ::estimation-budget
    ::warmup-budget
    ::sample-budget
    ::max-gc-attempts
    ::batch-time-ns]))

(s/def ::one-shot-config (s/keys :req-un [::scheme-type ::max-gc-attempts]))

(s/def ::sample-scheme (s/or :full-config ::one-shot-config))

(s/def ::samples (s/coll-of ::pipeline/sample))

(s/def ::sampled (s/keys :req-un
                         [::domain/batch-size
                          ::domain/eval-count
                          ::samples]))

(s/def ::input-state  (s/keys :req-un [::pipeline/pipeline-fn
                                       ::pipeline/metrics
                                       ::measured/measured
                                       :criterium.config/config]))

(s/def ::output-state  (s/keys :req-un [::pipeline/pipeline-fn
                                        ::pipeline/metrics
                                        ::measured/measured
                                        :criterium.config/config
                                        ::sampled]))

(s/fdef sample*
  :args (s/cat :pipeline-fn ::pipeline/pipeline-fn
               :metrics ::pipeline/metrics
               :measured ::measured/measured
               :config :criterium.config/config)
  :ret ::sampled)

(s/fdef sample
  :args (s/cat :input-state ::input-state)
  :ret ::output-state)
