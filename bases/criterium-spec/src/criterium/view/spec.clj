(ns criterium.view.spec
  (:require
   [clojure.spec.alpha :as s]))

(s/def ::stats-config
  (s/keys :req-un [::report-type ::stats-path]))

(s/def ::bootstrap-stats-config
  (s/keys :req-un [::report-type ::bootstrap-stats-path]))

(s/def ::event-stats-config
  (s/keys :req-un [::report-type ::stats-path]))

(s/def ::final-gc-warnings-config
  (s/keys :req-un [::report-type ::warn-threshold ::sampled-path]))

(s/def ::outlier-counts-config
  (s/keys :req-un [::report-type ::outlier-counts-path ::sampled-path]))

(s/def ::histogram-config
  (s/keys :req-un [::report-type ::sampled-path ::stats-path]))

(s/def ::view-samples-config
  (s/keys :req-un [::report-type ::sampled-path ::stats-path]))

(s/def ::metrics-config
  (s/keys :req-un [::report-type ::sampled-path]))

(s/def ::report-config-map
  (s/or
   :stats ::stats-config
   :bootstrap-stats ::bootstrap-stats-config
   :event-stats ::event-stats-config
   :final-gc-warnings ::final-gc-warnings-config
   :outlier-count ::outlier-counts-config
   :histogram ::histogram-config
   :view-samples ::view-samples-config
   :metrics ::metrics-config))

(s/def ::report-config
  (s/coll-of ::report-config-map))

(s/def ::analysis (s/keys))

(s/def ::state (s/keys :req-un [:criterium.config/config
                                :criterium.sample-scheme/sampled]))

(s/fdef report
  :args (s/cat :state ::state)
  :ret ::state)
