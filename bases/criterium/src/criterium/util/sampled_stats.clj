(ns criterium.util.sampled-stats
  (:require
   [criterium.util.helpers :as util]
   [criterium.util.stats :as stats]))

(defn pair-fn [k f]
  (fn [x] [k (f x)]))

(def stats-fns
  ;; called on sorted values
  (juxt
   (pair-fn :mean  stats/mean)
   (pair-fn :variance stats/variance)
   (pair-fn :min-val first)
   (pair-fn :max-val last)))

(defn sample-quantiles
  [quantiles vs]
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

(defn samples-for-path [samples path]
  (->> (get samples path)
       (filterv some?)
       (mapv double)))

(defn scale-vals [m scale-1]
  (util/update-vals m scale-1))

(defn quantiles-for
  [path samples config transforms]
  {:pre [(seq path) (seq samples)]}
  (assert (:quantiles config))
  (assert (not (:tail-quantile config)))
  (let [qs      (into [0.25 0.5 0.75] (:quantiles config))
        scale-1 (fn [v] (util/transform-sample-> v transforms))
        vs      (sort (samples-for-path samples path))]
    (scale-vals (sample-quantiles qs vs) scale-1)))

(defn stats-for
  [vs _config transforms]
  {:pre [(seq vs)]}
  (let [scale-1 (fn [v] (util/transform-sample-> v transforms))
        vs      (sort vs)]
    (-> (into {} (stats-fns vs))
        (assoc-mean-3-sigma)
        (scale-vals scale-1))))  ; variance scaled once to account for batching

(defn quantiles
  [samples metric-configs transforms config]
  (reduce
   (fn [res path]
     (assoc-in
      res path
      (quantiles-for
       path
       samples
       config
       transforms)))
   {}
   (map :path metric-configs)))

(defn sample-stats
  [sampled samples-id outliers metric-configs config]
  (reduce
   (fn [res path]
     (let [ols             (:outliers (get-in outliers path))
           [vs transforms] (loop [samples-id samples-id]
                             (when-not samples-id
                               (throw
                                (ex-info "Failed to get samples"
                                         {:path    path
                                          :sampled sampled})))
                             (let [samples (get sampled samples-id)
                                   vs      (samples-for-path samples path)]
                               (when-not samples
                                 (throw (ex-info "invalid samples-id"
                                                 {:samples-id samples-id})))
                               (if (not-empty vs)
                                 [vs (util/get-transforms sampled samples-id)]
                                 (recur (-> samples meta :source-id)))))]
       (assert (seq vs) path)
       (assoc-in
        res path
        (stats-for
         (->>
          (map (fn [v i] (when-not (get ols i) v)) vs (range))
          (filterv some?))
         config
         transforms))))
   {}
   (map :path metric-configs)))

(defn event-stats
  "Return the stats for events like JIT compilation and garbage-collector."
  [metrics-configs samples]
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
   metrics-configs))
