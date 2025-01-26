(ns criterium.analyse.digest-samples
  (:require
   [criterium.analyse.methods :as methods]
   [criterium.collect-plan :as collect-plan]
   [criterium.types :as types]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have have?]]
   [criterium.util.stats :as stats]
   [criterium.util.t-digest :as t-digest]))

(defmethod methods/transform :criterium/digest
  [digest-samples metric-configs f inv-f options]
  {:have [(have? types/digest-samples-map? digest-samples)]}
  (let [metric->digest  (util/metric->digest digest-samples)
        metric->digest' (reduce
                         (fn x-path [result path]
                           (assoc
                            result
                            path
                            (t-digest/transform
                             (metric->digest path)
                             f)))
                         {}
                         (mapv :path metric-configs))]
    (->
     (select-keys
      digest-samples
      types/digest-samples-keys)
     (merge
      {:metric->digest metric->digest'
       :transform      {:sample-> inv-f :->sample f}}))))

(defmethod methods/quantiles :criterium/digest
  [digest-samples metric-configs options]
  {:have [(have? types/digest-samples-map? digest-samples)]}
  (let [metric->digest (util/metric->digest digest-samples)
        quantiles      (into [0.25 0.5 0.75] (:quantiles options))
        quantiles      (reduce
                        (fn qs [result path]
                          (let [digest (metric->digest path)]
                            (assoc-in
                             result path
                             (zipmap
                              quantiles
                              (mapv
                               (partial t-digest/quantile digest)
                               quantiles)))))
                        {}
                        (mapv :path metric-configs))]
    {:type      :criterium/quantiles
     :quantiles quantiles
     :transform collect-plan/identity-transforms}))

(defn outlier-count
  [low-severe low-mild high-mild high-severe]
  {:low-severe  low-severe
   :low-mild    low-mild
   :high-mild   high-mild
   :high-severe high-severe})

(defn classifier
  [[^double low-severe ^double low-mild ^double high-mild ^double high-severe]]
  (fn [^double x i]
    (when-not (<= low-mild x high-mild)
      [i (cond
           (<= x low-severe)           :low-severe
           (< low-severe x low-mild)   :low-mild
           (> high-severe x high-mild) :high-mild
           (>= x high-severe)          :high-severe)])))

(defn digest-outliers
  [digest quantiles]
  (let [thresholds     (stats/boxplot-outlier-thresholds
                        (get quantiles 0.25)
                        (get quantiles 0.75))
        classifier     (classifier thresholds)
        outliers       (when (apply not= thresholds)
                         (into {}
                               (mapv classifier
                                     (t-digest/centroid-means digest)
                                     (range))))
        outlier-counts (reduce-kv
                        (fn [counts _i v]
                          (update counts v inc))
                        (outlier-count 0 0 0 0)
                        outliers)]
    {:thresholds     thresholds
     :outliers       outliers
     :outlier-counts outlier-counts}))

(defmethod methods/outliers :criterium/digest
  [digest-samples all-quantiles metric-configs _options]
  {:have [(have? types/digest-samples-map? digest-samples)]}
  (let [metric->digest (util/metric->digest digest-samples)
        quantiles      (util/quantiles all-quantiles)
        outliers       (reduce
                        (fn qs [result path]
                          (let [digest    (metric->digest path)
                                quantiles (get-in quantiles path)]
                            (assoc-in
                             result path
                             (digest-outliers digest quantiles) )))
                        {}
                        (mapv :path metric-configs))]
    {:type        :criterium/outliers
     :outliers    outliers
     :num-samples (t-digest/sample-count (first (vals metric->digest)))
     :transform   collect-plan/identity-transforms}))

(defn- digest-sample-states
  [digest outliers]
  (let [mean        (t-digest/mean digest)
        variance    (t-digest/variance digest mean)
        sigma       (Math/sqrt variance)
        three-sigma (* 3.0 sigma)]
    {:n                 (t-digest/sample-count digest)
     :mean              mean
     :variance          variance
     :sigma             sigma
     :mean-plus-3sigma  (+ mean three-sigma)
     :mean-minus-3sigma (- mean three-sigma)
     :min-val           (t-digest/minimum digest)
     :max-val           (t-digest/maximum digest)}))

(defmethod methods/stats :criterium/digest
  [digest-samples outliers metric-configs options]
  {:have [(have? types/digest-samples-map? digest-samples)]}
  (let [metric->digest (util/metric->digest digest-samples)
        outliers       (when outliers (util/outliers outliers))
        stats          (reduce
                        (fn qs [result path]
                          (let [digest (have (metric->digest path))]
                            (assoc-in
                             result path
                             (digest-sample-states digest outliers) )))
                        {}
                        (mapv :path metric-configs))]
    {:type      :criterium/stats
     :stats     stats
     :transform collect-plan/identity-transforms}))
