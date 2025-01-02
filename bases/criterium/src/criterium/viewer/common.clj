(ns criterium.viewer.common
  (:require
   [clojure.string :as str]
   [criterium.metric :as metric]
   [criterium.util.format :as format]
   [criterium.util.helpers :as util]
   [criterium.util.histogram :as histogram]
   [criterium.util.invariant :refer [have have?]]))

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
  [stats metric-configs transforms]
  (reduce
   (fn [res metric]
     (let [stat          (util/transform-vals->
                          (get-in stats (:path metric))
                          transforms)
           min-val       (double (:min-val stat))
           metric-scale  (double (:scale metric))
           [scale label] (format/scale
                          (:dimension metric)
                          (* metric-scale min-val))
           scale         (* (double scale) metric-scale)]
       (conj res
             (reduce
              (fn add-key-k [res k]
                (assoc res k
                       (format/round (* (double (get stat k)) scale) 4)))
              {:_metric (str  (:label metric) " " label)} ; underscore so it sorts first
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
                 (:dimension (have :dimension m))
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
  [metric-configs all-quantiles transforms]
  {:pre [(have? all-quantiles)]}
  (reduce
   (fn [res metric-config]
     (let [quantiles    (get-in all-quantiles (:path metric-config))
           median-val   (double
                         (util/transform-sample-> (quantiles 0.5) transforms))
           metric-scale (double (:scale metric-config))
           [scale unit] (format/scale
                         (:dimension metric-config)
                         (* metric-scale median-val))
           scale        (* (double scale) metric-scale)]
       (conj res
             (reduce-kv
              (fn [res q v]
                (assoc
                 res
                 q
                 (format/round
                  (* (util/transform-sample-> (double v) transforms) scale)
                  4)))
              {:metric (str (:label metric-config) " " unit)}
              quantiles))))
   []
   metric-configs))

(defn outlier-counts
  [metrics outliers]
  (reduce
   (fn [res metric]
     (let [mcs (:outlier-counts (get-in outliers (:path metric)))]
       (if (some pos? (vals mcs))
         (conj res (assoc mcs :_metric (:label metric)))
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
                           (-> bench-map :samples))
        warmup-scheme     (sampled-scheme-data
                           (some-> bench-map :warmup))
        estimation-scheme (sampled-scheme-data
                           (some-> bench-map :estimation))]
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
            (map-indexed
             (fn [i s] (when-not ((:outliers outliers) i) s)))
            (filter some?))
        samples))

(defn histogram
  [metric->values quantiles outliers transforms metric-config]
  (let [p            (:path metric-config)
        iqr          (when-let [qs (get-in quantiles p)]
                       (- (double (get qs 0.75)) (double (get qs 0.25))))
        samples      (metric->values p)
        outliers     (get-in outliers p)
        samples      (if outliers
                       (remove-outliers samples outliers)
                       samples)
        res          (histogram/histogram samples iqr)
        transform    #(util/transform-sample-> % transforms)
        min-val      (double (transform (:min res)))
        metric-scale (double (:scale metric-config))
        [scale unit] (format/scale
                      (:dimension metric-config)
                      (* metric-scale min-val))
        scale        (* (double scale) metric-scale)
        round        #(format/round % 4)
        t-center     (comp round (partial * scale) transform)
        t-density    #(format/round % 3)]
    (-> res
        (update :centers #(mapv t-center %))
        (update :min t-center)
        (update :max t-center)
        (update :density #(mapv t-density %))
        (assoc
         :metric-config metric-config
         :unit unit))))
