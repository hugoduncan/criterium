(ns criterium.viewer.common.modal
  "Modal analysis view helpers.

  Provides functions for formatting mode locations and iterating over
  multimodal metrics for display."
  (:require
   [criterium.metric :as metric]
   [criterium.util.format :as format]
   [criterium.util.helpers :as util]))

(defn format-mode-location
  "Format a mode location for display.
  Applies metric scale and transforms, then formats with appropriate dimension."
  [location metric-config transforms]
  (let [{:keys [dimension scale]} metric-config
        loc (util/transform-sample-> location transforms)]
    (format/format-value dimension (* (double scale) loc))))

(defn for-each-multimodal-metric
  "Iterate over metrics with multimodal distributions (n-modes > 1).
  Calls (f {:metric-config mc :n-modes n :modes modes :transforms transforms})
  for each metric where n-modes > 1.
  Returns nil."
  [data-map modes-id f]
  (let [modes-id (or modes-id :modes)
        modes-map (get data-map modes-id)]
    (when modes-map
      (let [transforms (util/get-transforms data-map modes-id)
            metrics-defs (:metrics-defs modes-map)
            metric-configs (when metrics-defs
                             (metric/all-metric-configs metrics-defs))
            all-modes (:modes modes-map)]
        (doseq [metric-config metric-configs]
          (when-let [modes-data (get all-modes (:path metric-config))]
            (let [n-modes (:n-modes modes-data)
                  modes (:modes modes-data)]
              (when (and n-modes (> (long n-modes) 1))
                (f {:metric-config metric-config
                    :n-modes n-modes
                    :modes modes
                    :transforms transforms})))))))))
