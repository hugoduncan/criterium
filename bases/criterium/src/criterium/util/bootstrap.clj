(ns criterium.util.bootstrap
  "Bootsrap statistics"
  (:require
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.util.probability :as probability]
   [criterium.util.stats :as stats]
   [criterium.util.well :as well]))

(defn bootstrap-sample
  "Bootstrap sampling of a statistic, using resampling with replacement."
  [data statistic size rng-factory]
  (assert (nat-int? size))
  (stats/transpose
   (for [_ (range size)] (statistic (sort (stats/sample data (rng-factory)))))))

(defn bootstrap-estimate
  "Mean, variance and confidence interval. This uses the bootstrapped
  statistic's variance for the confidence interval, but we should use BCa of
  ABC."
  [sampled-stat]
  (let [n     (count sampled-stat)
        m     (stats/mean sampled-stat n)
        v     (stats/variance* sampled-stat m n)
        ;; stats ((juxt mean variance) sampled-stat)
        stats [m v]]
    (conj stats
          (apply stats/confidence-interval stats))))

(defn scale-bootstrap-estimate [estimate ^double scale]
  [(* ^double (:point-estimate estimate) scale)
   (map #(* scale ^double (:value %1)) (:estimate-quantiles estimate))])

(defn drop-at [n coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (concat (take n s) (next (drop n s))))))

(defn jacknife
  "Jacknife statistics on data."
  [data statistic]
  (stats/transpose
   (map #(statistic (drop-at %1 data)) (range (count data)))))

(defn bca-nonparametric-eval
  "Calculate bootstrap values for given estimate and samples"
  [size z-alpha estimate samples jack-samples]
  {:pre [(> (count jack-samples) 1)]}
  (let [z0                    (probability/normal-quantile
                               (/ (count (filter (partial > estimate) samples))
                                  ^long size))
        jack-mean             (stats/mean jack-samples)
        jack-deviation        (map #(- jack-mean ^double %1) jack-samples)
        ^double sqr-deviation (reduce + 0.0 (map util/sqrd jack-deviation))
        acc                   (if (zero? sqr-deviation)
                                Double/POSITIVE_INFINITY
                                (/ ^double (reduce
                                            + 0.0
                                            (map util/cubed jack-deviation))
                                   (* 6.0 (Math/pow sqr-deviation 1.5))))
        tt                    (map
                               (fn [^double x]
                                 (probability/normal-cdf
                                  (+ z0 (/ (+ z0 x) (- 1.0 (* acc (+ z0 x)))))))
                               z-alpha)
        ooo                   (map
                               (fn [^double x] (util/trunc (* x ^long size)))
                               tt)
        sorted-samples        (sort samples)
        confpoints            (map (partial nth sorted-samples) ooo)]
    [confpoints z0 acc jack-mean jack-samples]))

(defn bca-nonparametric
  "Non-parametric BCa estimate of a statistic on data. Size bootstrap samples
  are used. Confidence values are returned at the alpha normal
  quantiles. rng-factory is a method that returns a random number generator to
  use for the sampling.

  An introduction to the bootstrap.  Efron, B., & Tibshirani, R. J. (1993).

  See http://lib.stat.cmu.edu/S/bootstrap.funs for Efron's original
   implementation."
  [data statistic size alpha rng-factory]
  (assert (nat-int? size))
  (let [data         (sort data)
        estimate     (statistic data)
        samples      (bootstrap-sample data statistic size rng-factory)
        jack-samples (jacknife data statistic)
        alpha        (if (vector? alpha) alpha [alpha])
        z-alpha      (map probability/normal-quantile alpha)]
    (if (vector? estimate)
      (map
       (partial bca-nonparametric-eval size z-alpha)
       estimate samples jack-samples)
      (bca-nonparametric-eval size z-alpha estimate samples jack-samples))))

(defrecord BcaEstimate
  [point-estimate
   estimate-quantiles])

(defn- bca-to-estimate
  [alpha bca-estimate]
  (assert (= 0.5 (first alpha)) alpha)
  (->BcaEstimate
   (first (first bca-estimate))
   (mapv
    (fn [value z] {:value value :alpha z})
    (next (first bca-estimate))
    (next alpha))))

(defn bootstrap-bca
  "Bootstrap a statistic. Statistic can produce multiple statistics as a vector
   so you can use juxt to pass multiple statistics.
   http://en.wikipedia.org/wiki/Bootstrapping_(statistics)"
  [data statistic size alpha rng-factory]
  (assert (nat-int? size))
  (let [bca (bca-nonparametric data statistic size alpha rng-factory)]
    (if (vector? bca)
      (bca-to-estimate alpha bca)
      (map (partial bca-to-estimate alpha) bca))))

(defn bootstrap
  "Bootstrap a statistic. Statistic can produce multiple statistics as a vector
   so you can use juxt to pass multiple statistics.
   http://en.wikipedia.org/wiki/Bootstrapping_(statistics)"
  [data statistic size rng-factory]
  (let [samples (bootstrap-sample data statistic size rng-factory)]
    (if (vector? (first samples))
      (map bootstrap-estimate samples)
      (bootstrap-estimate samples))))

(defn- scale-bootstrap-stat [scale-f stat]
  (-> stat
      (update :point-estimate scale-f)
      (update :estimate-quantiles
              #(mapv (fn [q] (update q :value scale-f)) %))))

(defn assoc-bootstrap-mean-3-sigma
  [{:keys [mean variance] :as stats}]
  (let [three-sigma       (* 3 (Math/sqrt (:point-estimate variance)))
        mean-plus-3sigma  (+ ^double (:point-estimate mean) three-sigma)
        mean-minus-3sigma (- ^double (:point-estimate mean) three-sigma)]
    (assoc stats
           :mean-plus-3sigma {:point-estimate mean-plus-3sigma}
           :mean-minus-3sigma {:point-estimate mean-minus-3sigma})))

(defn scale-bootstrap-values
  [stats f]
  (util/update-vals stats f))

(def stats-fn-map
  {:mean     stats/mean
   :variance stats/variance
   :min-val  stats/min
   :max-val  stats/max})

(defn stats-fns
  [quantiles]
  (into
   (vec (vals stats-fn-map))
   (map #(partial stats/quantile %) quantiles)))

(defn stats-fn
  [fs]
  (fn [vs]
    (mapv #(% vs) fs)))

(defn bootstrap-stats-for
  [samples opts transforms]
  {:pre [(:quantiles opts)
         (:estimate-quantiles opts)]}
  (let [vs        (mapv double samples)
        quantiles (into [0.25 0.5 0.75] (:quantiles opts))
        stats-fn  (stats-fn (stats-fns quantiles))
        stats     (bootstrap-bca
                   vs
                   stats-fn
                   (:bootstrap-size opts (long (* (count vs) 0.8)))
                   (into [0.5] (:estimate-quantiles opts))
                   well/well-rng-1024a)
        scale-1   (fn [v] (util/transform-sample-> v transforms))
        scale-f   (partial scale-bootstrap-stat scale-1)
        ks        (keys stats-fn-map)]
    (-> (zipmap ks stats)
        (assoc-bootstrap-mean-3-sigma)
        (scale-bootstrap-values scale-f)
        (assoc :quantiles
               (zipmap quantiles (map scale-f (drop (count ks) stats)))))))

(defn bootstrap-stats*
  [samples metric-configs transforms config]
  (reduce
   (fn [res path]
     (assoc-in
      res path
      (bootstrap-stats-for
       (get samples path)
       config
       transforms)))
   {}
   (map :path metric-configs)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn bootstrap-stats
  ;; add stats to the result
  ([] (bootstrap-stats {}))
  ([{:keys [id metric-ids samples-id] :as analysis}]
   (fn [sampled]
     (let [id             (or id :bootstrap-stats)
           samples-id     (or samples-id :samples)
           metric-configs (metric/metric-configs-of-type
                           (:metrics-configs sampled)
                           :quantitative metric-ids)
           samples        (get sampled samples-id)
           transforms     (util/get-transforms sampled samples-id)
           result         (bootstrap-stats*
                           samples
                           metric-configs
                           transforms
                           analysis)]
       (assoc sampled id result)))))
