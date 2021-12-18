(ns criterium.viewer.common
  (:require
   [clojure.string :as str]
   [criterium.util.format :as format]))

(defn metrics-map
  [sample metrics]
  (reduce
   (fn [res metric]
     (conj res
           {:metric (:label metric)
            :value  (format/format-value
                     (:dimension metric)
                     (* (first (sample (:path metric)))
                        (:scale metric)))}))
   []
   metrics))

(defn stats-map
  [stats metric-configs]
  (reduce
   (fn [res metric]
     (let [stat (get-in stats (:path metric))]
       (conj res
             (reduce
              (fn [res k]
                (assoc res k
                       (format/format-value
                        (:dimension metric)
                        (* (get stat k)
                           (:scale metric)))))
              {:metric (:label metric)}
              [:mean :min-val :mean-minus-3sigma :mean-plus-3sigma :max-val]))))
   []
   metric-configs))

(defn composite-key [path]
  (keyword (str/join "-" (mapv name path))))

(defn event-stats-metrics [event-stats res k metric]
  {:pre [(map? metric)]}
  (let [ms                (:values metric)
        sample-count-path (conj (pop (:path (first ms))) :sample-count)
        sample-count      (event-stats sample-count-path)]
    (if (and sample-count (pos? sample-count))
      (let [v (reduce
               (fn [res m]
                 (assoc res
                        (composite-key (rest (:path m)))
                        (format/format-value
                         (:dimension m)
                         (* (event-stats (:path m))
                            (:scale m)))))
               {:metric (:label metric)}
               (into [{:path      [k :sample-count]
                       :dimension :count
                       :scale     1}]
                     ms))]
        (conj res v))
      res)))

(defn event-stats
  [metrics ev-stats]
  {:pre [ev-stats]}
  (reduce-kv
   (fn [res k metric]
     (if-let [groups (:groups metric)]
       (into res (event-stats groups ev-stats))
       (event-stats-metrics ev-stats res k metric)))
   []
   metrics))

(defn quantiles
  [metric-configs all-quantiles]
  {:pre [all-quantiles]}
  (reduce
   (fn [res metric-config]
     (let [quantiles (get-in all-quantiles (:path metric-config))]
       (conj res
             (reduce-kv
              (fn [res q v]
                (assoc res
                       q
                       (format/format-value
                        (:dimension metric-config)
                        (* v (:scale metric-config)))))
              {:metric (:label metric-config)}
              quantiles))))
   []
   metric-configs))

(defn outlier-counts
  [metrics outliers]
  (reduce
   (fn [res metric]
     (let [mcs (:outlier-counts (get-in outliers (:path metric)))]
       (if (some pos? (vals mcs))
         (conj res (assoc mcs :metric (:label metric)))
         res)))
   []
   metrics))

(defn- sampled-scheme-data
  [sampled]
  (assoc
   (select-keys sampled [:batch-size :num-samples])
   :num-evals
   (* (:num-samples sampled) (:batch-size sampled))))

(defn collect-plan-data
  [sampled]
  [(merge
    {:phase :sample}
    (sampled-scheme-data sampled))
   (merge
    {:phase :warmup}
    (sampled-scheme-data (sampled :warmup)))
   (merge
    {:phase :estimation}
    (sampled-scheme-data (sampled :estimation)))])
