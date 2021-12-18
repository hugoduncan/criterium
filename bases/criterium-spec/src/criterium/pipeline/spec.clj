(ns criterium.pipeline.spec
  (:require
   [clojure.spec.alpha :as s]
   [criterium.domain :as domain]
   [criterium.measured :as measured]
   [criterium.measured.spec]
   [criterium.pipeline.fns :as fns]))

;;; Pipeline Functions Spec Definitions

(s/def ::pipeline-fn-kw (set (keys fns/pipeline-fns)))
(s/def ::terminal-fn-kw (set (keys fns/terminal-fns)))


;;; Spec definitions

(s/def ::state any?)
(s/def ::expr-value any?)

(s/def ::sample (s/keys :req-un [::domain/elapsed-time]))

(s/def ::pipeline-fn
  (s/fspec
   :args (s/cat :sample ::sample
                :measured ::measured/measured
                :state any?
                :eval-count ::domain/eval-count)
   :ret  ::sample))

(s/fdef time-metric
  :args (s/cat :sample ::sample
               :measured ::measured/measured)
  :ret  (s/keys
         :req-un [::state
                  ::domain/elapsed-time
                  ::expr-value]))

(s/fdef with-class-loader
  :args (s/cat :fn ::pipeline-fn)
  :ret  ::pipeline-fn)

(s/fdef with-compilation
  :args (s/cat :fn ::pipeline-fn)
  :ret  ::pipeline-fn)

(s/fdef with-memory
  :args (s/cat :fn ::pipeline-fn)
  :ret  ::pipeline-fn)

(s/fdef with-runtime-memory
  :args (s/cat :fn ::pipeline-fn)
  :ret  ::pipeline-fn)

(s/fdef with-finalization
  :args (s/cat :fn ::pipeline-fn)
  :ret  ::pipeline-fn)

(s/fdef with-garbage-collector
  :args (s/cat :fn ::pipeline-fn)
  :ret  ::pipeline-fn)

(s/def ::pipeline-fn-kws (s/coll-of ::fns/pipeline-fn-kw))
(s/def ::stages ::pipeline-fn-kws)
(s/def ::termonator ::terminal-fn-kw)
(s/def ::pipeline-config (s/keys :req-un [::stages ::terminator]))
(s/def ::metrics (s/coll-of (s/or :pipeline-fn-kw ::pipeline-fn-kw
                                  :terminal-fn-kw ::terminal-fn-kw)))

(s/def ::pipeline-in-state (s/keys :req-un [:criterium.config/config]))
(s/def ::pipeline-out-state (s/keys
                             :req-un [::pipeline-fn ::metrics]))

(s/fdef metrics
  :args (s/cat :pipeline-config ::pipeline-config)
  :ret ::metrics)

(s/fdef pipeline*
  :args (s/cat :pipeline-config ::pipeline-config)
  :ret ::pipeline-fn)

(s/fdef pipeline
  :args (s/cat :pipeline-in-state ::pipeline-in-state)
  :ret ::pipeline-out-state)

(s/fdef execute
  :args (s/cat :pipeline-fn ::pipeline-fn
               :measured ::measured/measured
               :eval-count ::domain/eval-count)
  :ret ::sample)
