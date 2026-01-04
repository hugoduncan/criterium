(ns criterium.util.sampled-stats
  (:require
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have have?]]
   [criterium.util.stats :as stats]))

(defn pair-fn [k f]
  (fn [x] [k (f x)]))

(def stats-fns
  ;; called on sorted values
  (juxt
   (pair-fn :mean stats/mean)
   (pair-fn :median (partial stats/quantile 0.5))
   (pair-fn :variance stats/variance)
   (pair-fn :min-val first)
   (pair-fn :max-val last)))

(defn sample-quantiles
  [quantiles vs]
  {:pre [(have? seq vs)]}
  (reduce
   (fn [m q] (assoc m q (stats/quantile q vs)))
   {}
   quantiles))

(defn assoc-mean-3-sigma [{:keys [mean variance] :as stats}]
  (when (some? variance)
    (let [three-sigma       (* 3.0 (Math/sqrt variance))
          mean-plus-3sigma  (+ ^double mean three-sigma)
          mean-minus-3sigma (- ^double mean three-sigma)]
      (assoc stats
             :mean-plus-3sigma mean-plus-3sigma
             :mean-minus-3sigma mean-minus-3sigma))))

(defn samples-for-path [metric->values path]
  {:pre [(have? map? metric->values)]}
  (->> (have seq (metric->values path)
             {:path path :metric->values metric->values})
       (filterv some?)
       (mapv double)))

(defn scale-vals [m scale-1]
  (util/update-vals m scale-1))

(defn quantiles-for
  [path samples config]
  {:pre [(have? seq path)
         (have? seq samples)
         (have? map? samples)]}
  (have :quantiles config)
  (have (comp not :tail-quantile) config)
  (let [qs (vec (sort (into #{0.1 0.25 0.5 0.75 0.9} (:quantiles config))))
        vs (sort (samples-for-path samples path))]
    (sample-quantiles qs vs)))

(defn stats-for
  [vs _config]
  {:pre [(have? seq vs)]}
  (let [vs (sort vs)]
    (-> (into {} (stats-fns vs))
        (assoc-mean-3-sigma)
        (assoc :n (count vs)))))

(defn quantiles
  [samples metric-configs config]
  {:pre [(have? seq samples)]}
  (reduce
   (fn [res path]
     (assoc-in res path (quantiles-for path samples config)))
   {}
   (map :path metric-configs)))

(defn sample-stats
  [metric->values outliers metric-configs config]
  (reduce
   (fn [res path]
     (let [ols              (:outliers (get-in outliers path) {})
           vs               (samples-for-path metric->values path)
           without-outliers (if ols
                              (into []
                                    (comp
                                     (map-indexed (fn [i v] (when-not (ols i) v)))
                                     (filter some?))
                                    vs)
                              vs)]
       (if (seq vs)
         (assoc-in res path (stats-for without-outliers config))
         res)))
   {}
   (mapv :path metric-configs)))

(defn event-stats
  "Return the stats for events like JIT compilation and garbage-collector."
  [metrics-defs samples]
  (reduce-kv
   (fn [stats _k metric]
     (if-let [groups (:groups metric)]
       (merge stats (event-stats groups samples))
       (let [ms           (:values metric)
             all-vs       (mapv #(get samples (:path %)) ms)
             all-vals     (mapv (fn [vs] (reduce + 0 vs)) all-vs)
             sample-count (reduce
                           +
                           0
                           (apply mapv
                                  (fn [& vs] (if (some pos? vs) 1 0))
                                  all-vs))]
         (merge stats
                (-> (zipmap
                     (map :path ms)
                     all-vals)
                    (assoc
                     (conj (vec (butlast (:path (first ms)))) :sample-count)
                     sample-count))))))
   {}
   metrics-defs))
