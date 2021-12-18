(ns criterium.config.spec
  (:require
   [clojure.spec.alpha :as s]))


(s/def ::verbose boolean?)
(s/def ::pipeline-config ::pipeline/pipeline-config)
(s/def ::analysis ::analyse-config/analysis-config)
(s/def ::report ::view/report-config)
(s/def ::config (s/keys :req-un [::verbose
                                 ::pipeline-config
                                 ::analysis
                                 ::sample-scheme/sample-scheme
                                 ::report
                                 ::return-value]))
