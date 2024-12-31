(ns criterium.viewer.common
  (:require
   [clojure.string :as str]
   [criterium.metric :as metric]
   [criterium.util.format :as format]
   [criterium.util.helpers :as util]
   [criterium.util.histogram :as histogram]
   [criterium.util.invariant :refer [have?]]))

(defn metrics-map
  [sample metrics]
  (reduce
   (fn [res metric]
     (conj res
           {:metric (:label metric)
            :value  (format/format-value
                     (:dimension metric)
                     (* (double (first (sample (:path metric))))
                        (double (:scale metric))))}))
   []
   metrics))

(defn stats-map
  [stats metric-configs]
  (reduce
   (fn [res metric]
     (let [stat (get-in stats (:path metric))]
       (conj res
             (reduce
              (fn add-key-k [res k]
                (assoc res k
                       (format/format-value
                        (:dimension metric)
                        (* (double (get stat k))
                           (double (:scale metric))))))
              {:metric (:label metric)}
              [:mean :min-val :mean-minus-3sigma :mean-plus-3sigma :max-val]))))
   []
   (filterv (metric/type-pred :quantitative) metric-configs)))

(defn composite-key [path]
  (keyword (str/join "-" (mapv name path))))

(defn event-stats-metrics
  [event-stats k metric ms]
  {:post [(have? (some-fn nil? map?) %)]}
  (let [sample-count-path (conj (pop (:path (first ms))) :sample-count)
        sample-count      (event-stats sample-count-path)]
    (when (and sample-count (pos? (long sample-count)))
      (reduce
       (fn [res m]
         (assoc res
                (composite-key (rest (:path m)))
                (format/format-value
                 (:dimension m)
                 (* (double (get event-stats (:path m)))
                    (double (:scale m))))))
       {:metric (:label metric)}
       (into [{:path      sample-count-path
               :dimension :count
               :scale     1}]
             ms)))))

(defn event-stats
  [metrics-defs ev-stats]
  {:pre  [ev-stats]
   :post [(have? vector? %)]}
  (reduce-kv
   (fn [res k metric]
     (if-let [groups (:groups metric)]
       (into res (event-stats groups ev-stats))
       (if-let [m (event-stats-metrics ev-stats k metric (:values metric))]
         (conj res m)
         res)))
   []
   metrics-defs))

(defn quantiles
  [metric-configs all-quantiles]
  {:pre [(have? all-quantiles)]}
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
                        (* (double v) (double (:scale metric-config))))))
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
  (when sampled
    (assoc
     (select-keys sampled [:batch-size :num-samples])
     :num-evals
     (* (long (:num-samples sampled)) (long (:batch-size sampled))))))

(defn collect-plan-data
  [bench-map]
  (let [samples-schema    (sampled-scheme-data
                           (-> bench-map :data :samples))
        warmup-scheme     (sampled-scheme-data
                           (some-> bench-map :data :warmup))
        estimation-scheme (sampled-scheme-data
                           (some-> bench-map :data :estimation))]
    (cond-> [(merge {:phase :sample} samples-schema)]
      warmup-scheme
      (conj (merge {:phase :warmup} warmup-scheme))
      estimation-scheme
      (conj (merge {:phase :estimation} estimation-scheme)))))

(defn column-data->maps
  "Convert data where each column's values are stored in vectors."
  [column-data column-keys column-tforms]
  (let [make-row (fn [& vals]
                   (zipmap
                    column-keys
                    (mapv
                     (fn [k v] ((column-tforms k identity) v))
                     column-keys vals)))]
    (apply mapv make-row (map column-data column-keys))))

(defn- remove-outliers
  [samples outliers]
  (into [] (comp
            (filter some?)
            (map-indexed (fn [i s] (when-not (outliers i) s))))
        samples))

(defn histogram
  [metric->values quantiles outliers transforms metric-config]
  (let [p         (:path metric-config)
        iqr       (when-let [qs (get-in quantiles p)]
                    (- (double (get qs 0.75)) (double (get qs 0.25))))
        samples   (metric->values p)
        outliers  (get-in outliers p)
        samples   (if outliers
                    (remove-outliers samples outliers)
                    samples)
        res       (histogram/histogram samples iqr)
        transform #(util/transform-sample-> % transforms)]
    (-> res
        (update :centers #(mapv transform %))
        (update :min  transform)
        (update :max  transform)
        (assoc :metric-config metric-config))))
