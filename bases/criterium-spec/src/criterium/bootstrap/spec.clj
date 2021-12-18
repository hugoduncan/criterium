(ns criterium.bootstrap.spec
  (:require
   [clojure.spec.alpha :as s]))


(s/def ::bootstrap-size nat-int?)

(s/def ::bootstrap-stats-config
  (s/keys :req-un [::analysis-type
                   ::analyse/quantiles
                   ::bootstrap-size
                   ::output-path]))
