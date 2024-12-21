(ns criterium.viewer.common
  (:require
   [clojure.string :as str]
   [criterium.metric :as metric]
   [criterium.util.format :as format]
   [criterium.util.invariant :refer [have?]]
   [criterium.util.helpers :as util]))

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
              (fn add-key-k [res k]
                (assoc res k
                       (format/format-value
                        (:dimension metric)
                        (* (get stat k)
                           (:scale metric)))))
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
    (when (and sample-count (pos? sample-count))
      (reduce
       (fn [res m]
         (assoc res
                (composite-key (rest (:path m)))
                (format/format-value
                 (:dimension m)
                 (* (get event-stats (:path m))
                    (:scale m)))))
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
  (when sampled
    (assoc
     (select-keys sampled [:batch-size :num-samples])
     :num-evals
     (* (:num-samples sampled) (:batch-size sampled)))))

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
