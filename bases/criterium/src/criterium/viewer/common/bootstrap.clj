(ns criterium.viewer.common.bootstrap
  "Bootstrap statistics view helpers.

  Provides functions for formatting bootstrap estimates and building
  bootstrap statistics table rows."
  (:require
   [criterium.util.format :as format]
   [criterium.util.helpers :as util]))

(defn format-bootstrap-estimate
  "Format a BcaEstimate for display, returning a map with :value and :ci keys.
  Applies transforms (e.g., batch-size division) and unit scaling."
  [estimate metric-config transforms]
  (when estimate
    (let [{:keys [dimension ^double scale]} metric-config
          tform #(util/transform-sample-> % transforms)
          quantiles (:estimate-quantiles estimate)
          ci-lower (when (seq quantiles) (-> quantiles first :value))
          ci-upper (when (seq quantiles) (-> quantiles second :value))
          point-est (:point-estimate estimate)
          fmt-val (fn [v] (when v
                            (format/format-value
                             dimension
                             (* scale (double (tform v))))))]
      {:value (fmt-val point-est)
       :ci-lower (fmt-val ci-lower)
       :ci-upper (fmt-val ci-upper)})))

(defn bootstrap-stat-row
  "Create a row for the bootstrap stats table.
  Column order: median first, then mean, then percentile spread.
  Applies transforms (e.g., batch-size division) to all values."
  [metric-config stat transforms]
  (let [{:keys [mean quantiles]} stat
        mean-fmt (format-bootstrap-estimate mean metric-config transforms)
        p10 (format-bootstrap-estimate (get quantiles 0.1) metric-config transforms)
        p50 (format-bootstrap-estimate (get quantiles 0.5) metric-config transforms)
        p90 (format-bootstrap-estimate (get quantiles 0.9) metric-config transforms)]
    {:metric (:label metric-config)
     :median (:value p50)
     :median-ci-lower (:ci-lower p50)
     :median-ci-upper (:ci-upper p50)
     :mean (:value mean-fmt)
     :mean-ci-lower (:ci-lower mean-fmt)
     :mean-ci-upper (:ci-upper mean-fmt)
     :p10 (:value p10)
     :p90 (:value p90)}))
