(ns criterium.analyse.metrics-samples
  (:require
   [criterium.analyse.methods :as methods]
   [criterium.collect-plan :as collect-plan]
   [criterium.util.helpers :as util]
   [criterium.util.histogram :as histogram]
   [criterium.util.invariant :refer [have]]
   [criterium.util.kde :as kde]
   [criterium.util.sampled-stats :as sampled-stats]
   [criterium.util.stats :as stats]))

(def ^:private metrics-samples-keys
  "Keys for :criterium/metrics-samples type, used for select-keys."
  #{:type
    :transform
    :source-id
    :metric->values
    :num-samples
    :batch-size
    :metrics-defs
    :expr-value})

(derive :criterium/collected-metrics-samples :criterium/metrics-samples)

(defmethod methods/transform :criterium/metrics-samples
  [metrics-samples metric-configs f inv-f _options]
  (let [metric->values (util/metric->values metrics-samples)
        metric->values' (reduce
                         (fn x-path [result path]
                           (assoc
                            result
                            path
                            (mapv f (metric->values path))))
                         {}
                         (mapv :path metric-configs))]
    (->
     (select-keys
      metrics-samples
      (disj metrics-samples-keys :metrics-defs))
     (merge
      {:metric->values metric->values'
       :transform {:sample-> inv-f :->sample f}}))))

(defmethod methods/quantiles :criterium/metrics-samples
  [metrics-samples metric-configs options]
  (let [quantiles (sampled-stats/quantiles
                   (util/metric->values metrics-samples)
                   metric-configs
                   options)]
    {:type :criterium/quantiles
     :quantiles quantiles
     :transform collect-plan/identity-transforms}))

(defn outlier-count
  [low-severe low-mild high-mild high-severe]
  {:low-severe low-severe
   :low-mild low-mild
   :high-mild high-mild
   :high-severe high-severe})

(defn classifier
  [[^double low-severe ^double low-mild ^double high-mild ^double high-severe]]
  (fn [^double x i]
    (when-not (<= low-mild x high-mild)
      [i (cond
           (<= x low-severe) :low-severe
           (< low-severe x low-mild) :low-mild
           (> high-severe x high-mild) :high-mild
           (>= x high-severe) :high-severe)])))

(defn samples-outliers [metric-configs all-quantiles samples]
  (reduce
   (fn sample-m [result metric-config]
     (let [path (:path metric-config)
           quantiles (have map? (get-in all-quantiles path)
                           {:all-quantiles all-quantiles})
           thresholds (stats/boxplot-outlier-thresholds
                       (get quantiles 0.25)
                       (get quantiles 0.75))
           classifier (classifier thresholds)
           outliers (when (apply not= thresholds)
                      (into {}
                            (mapv classifier
                                  (get samples path)
                                  (range))))
           outlier-counts (reduce-kv
                           (fn [counts _i v]
                             (update counts v inc))
                           (outlier-count 0 0 0 0)
                           outliers)]
       (update-in result path
                  assoc
                  :thresholds thresholds
                  :outliers outliers
                  :outlier-counts outlier-counts)))
   {}
   metric-configs))

(defmethod methods/outliers :criterium/metrics-samples
  [metrics-samples all-quantiles metric-configs _options]
  (let [outliers (samples-outliers
                  metric-configs
                  (util/quantiles all-quantiles)
                  (util/metric->values metrics-samples))]
    {:type :criterium/outliers
     :outliers outliers
     :num-samples (:num-samples metrics-samples)
     :transform collect-plan/identity-transforms}))

(defmethod methods/stats :criterium/metrics-samples
  [metrics-samples outliers metric-configs options]
  (let [metric->values (util/metric->values metrics-samples)
        stats (sampled-stats/sample-stats
               metric->values
               (when outliers (util/outliers outliers))
               metric-configs
               options)]
    {:type :criterium/stats
     :stats stats
     :transform collect-plan/identity-transforms}))

(defmethod methods/event-stats :criterium/metrics-samples
  [metrics-samples metrics-defs _options]
  (let [metric->values (util/metric->values metrics-samples)
        event-stats (sampled-stats/event-stats
                     metrics-defs
                     metric->values)]
    {:type :criterium/event-stats
     :event-stats event-stats
     :batch-size (:batch-size metrics-samples)
     :transform collect-plan/identity-transforms}))

(defn- remove-outliers
  [samples outliers]
  (into [] (comp
            (map-indexed
             (fn [i s] (when-not (outliers i) s)))
            (filter some?))
        samples))

(defn histogram
  [metric->values quantiles outliers metric-config]
  (try
    (let [p (:path metric-config)
          iqr (when-let [qs (get-in quantiles p)]
                (- (double (get qs 0.75)) (double (get qs 0.25))))
          samples (metric->values p)
          outliers (get-in outliers p)
          samples (if-let [ols (:outliers outliers)]
                    (remove-outliers samples ols)
                    samples)]
      (histogram/histogram samples iqr))
    (catch clojure.lang.ExceptionInfo e
      (let [data (ex-data e)]
        (when-not (#{:histogram/no-values :histogram/same-values}
                   (:error data))
          (throw e))))))

(defmethod methods/histogram :criterium/metrics-samples
  [metrics-samples quantiles outliers metric-configs _options]
  (let [histograms (->> metric-configs
                        (mapv
                         (juxt :path
                               #(histogram
                                 (util/metric->values metrics-samples)
                                 (util/quantiles quantiles)
                                 (util/outliers outliers)
                                 %)))
                        (filterv (comp some? second))
                        (into {}))]
    {:type :criterium/histogram
     :histograms histograms
     :transform collect-plan/identity-transforms}))

(defn kde-for-metric
  "Compute KDE for a single metric's samples, filtering outliers if present."
  [metric->values outliers metric-config options]
  (try
    (let [p (:path metric-config)
          samples (metric->values p)
          outliers (get-in outliers p)
          samples (if-let [ols (:outliers outliers)]
                    (remove-outliers samples ols)
                    samples)]
      (when (seq samples)
        (kde/kde samples options)))
    (catch clojure.lang.ExceptionInfo e
      (let [data (ex-data e)]
        (when-not (#{:kde/no-data :kde/constant-data} (:error data))
          (throw e))))))

(defmethod methods/kde :criterium/metrics-samples
  [metrics-samples outliers metric-configs options]
  (let [metric->values (util/metric->values metrics-samples)
        outliers (when outliers (util/outliers outliers))
        kdes (->> metric-configs
                  (mapv
                   (juxt :path
                         #(kde-for-metric metric->values outliers % options)))
                  (filterv (comp some? second))
                  (into {}))]
    (when (seq kdes)
      {:type :criterium/kde
       :kdes kdes
       :transform collect-plan/identity-transforms})))

(defn modes-for-metric
  "Compute modes with statistical validation for a single metric.
  
  Takes KDE output and raw samples, runs Silverman's test for k=1 up to max-modes,
  and computes confidence intervals for detected modes."
  [kde-data samples outliers metric-config options]
  (try
    (let [{:keys [grid density bandwidth]} kde-data
          {:keys [max-modes n-bootstrap alpha n-points]
           :or {max-modes 5 n-bootstrap 200 alpha 0.05 n-points 512}} options
          p (:path metric-config)
          ;; Filter outliers from samples
          outliers-data (get-in outliers p)
          samples (if-let [ols (:outliers outliers-data)]
                    (remove-outliers samples ols)
                    samples)
          ;; Find modes from existing KDE density
          grid-arr (double-array grid)
          density-arr (double-array density)
          all-modes (kde/find-modes grid-arr density-arr)
          ;; Run Silverman's test for each k from 1 to max-modes
          silverman-results
          (into {}
                (for [k (range 1 (inc (min max-modes (count all-modes))))]
                  [k (kde/silverman-test samples k
                                         {:n-bootstrap n-bootstrap
                                          :n-points n-points
                                          :alpha alpha})]))
          ;; Determine validated number of modes
          ;; Find smallest k where p-value >= alpha (fail to reject H0: <= k modes)
          validated-k (or (some (fn [k]
                                  (when (>= (get-in silverman-results [k :p-value]) alpha)
                                    k))
                                (range 1 (inc (min max-modes (count all-modes)))))
                          (count all-modes))
          ;; Compute confidence intervals for modes
          modes-with-ci (kde/mode-confidence-intervals
                         samples bandwidth grid-arr validated-k
                         {:n-bootstrap n-bootstrap
                          :alpha alpha})
          ;; Mark significance based on Silverman results
          modes-with-significance
          (vec (map-indexed
                (fn [i mode]
                  (let [k (inc i)
                        test-result (get silverman-results k)
                        significant? (and test-result
                                          (< (:p-value test-result) alpha))]
                    (assoc mode :significant? significant?)))
                modes-with-ci))]
      {:modes modes-with-significance
       :n-modes validated-k
       :silverman {:k-tested (vec (keys silverman-results))
                   :p-values (into {} (map (fn [[k v]] [k (:p-value v)])
                                           silverman-results))
                   :critical-bandwidths (into {} (map (fn [[k v]]
                                                        [k (:critical-bandwidth v)])
                                                      silverman-results))}})
    (catch Exception e
      (when-not (#{:kde/no-data :kde/constant-data}
                 (:error (ex-data e)))
        (throw e)))))

(defmethod methods/modes :criterium/kde
  [kde-map samples outliers metric-configs options]
  (let [kdes (:kdes kde-map)
        metric->values (util/metric->values samples)
        outliers (when outliers (util/outliers outliers))
        modes-results
        (->> metric-configs
             (mapv
              (fn [metric-config]
                (let [p (:path metric-config)
                      kde-data (get kdes p)
                      sample-values (metric->values p)]
                  (when (and kde-data sample-values)
                    [p (modes-for-metric kde-data sample-values outliers
                                         metric-config options)]))))
             (filterv (comp some? second))
             (into {}))]
    (when (seq modes-results)
      {:type :criterium/modes
       :modes modes-results
       :transform (:transform kde-map)})))
