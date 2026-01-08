(ns stats.bootstrap
  "Bootstrap resampling and confidence interval estimation.

  Provides core bootstrap algorithms for statistical inference:
  - bootstrap-sample: Resampling with replacement
  - bootstrap-estimate: Mean, variance and confidence intervals
  - jacknife: Leave-one-out resampling
  - bca-nonparametric: Bias-corrected and accelerated bootstrap
  - bootstrap-bca: Bootstrap with BCa confidence intervals

  References:
  - Efron, B., & Tibshirani, R. J. (1993). An introduction to the bootstrap.
  - http://lib.stat.cmu.edu/S/bootstrap.funs"
  (:require
   [stats.core :as core]
   [stats.probability :as probability]
   [stats.sampling :as sampling]
   [utils.interface :as utils]))

(defn bootstrap-sample
  "Bootstrap sampling of a statistic, using resampling with replacement.

  Returns transposed results: if statistic returns a vector, returns a vector
  of vectors where each inner vector contains all samples for that statistic."
  [data statistic size rng-factory]
  (assert (nat-int? size))
  (core/transpose
   (for [_ (range size)] (statistic (sort (sampling/sample data (rng-factory)))))))

(defn bootstrap-estimate
  "Mean, variance and confidence interval from bootstrapped samples.

  Returns [mean variance [lower upper]] where the confidence interval
  uses the bootstrapped statistic's variance."
  [sampled-stat]
  (let [n     (count sampled-stat)
        m     (core/mean sampled-stat n)
        v     (core/variance* sampled-stat m n)
        stats [m v]]
    (conj stats
          (apply sampling/confidence-interval stats))))

(defn drop-at
  "Return coll with element at index n removed."
  [n coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (concat (take n s) (next (drop n s))))))

(defn jacknife
  "Jacknife statistics on data.

  Computes the statistic on each leave-one-out sample of the data.
  Returns transposed results like bootstrap-sample."
  [data statistic]
  (core/transpose
   (map #(statistic (drop-at %1 data)) (range (count data)))))

(defn- nan-safe-compare
  "Comparator that handles NaN values by sorting them to the end.
  Uses Java's Double/compare which treats NaN as greater than all other values."
  ^long [^double a ^double b]
  (Double/compare a b))

(defn bca-nonparametric-eval
  "Calculate bootstrap values for given estimate and samples.

  Internal function used by bca-nonparametric."
  [size z-alpha estimate samples jack-samples]
  {:pre [(> (count jack-samples) 1)]}
  (let [z0                    (probability/normal-quantile
                               (/ (count (filter (partial > estimate) samples))
                                  ^long size))
        jack-mean             (core/mean jack-samples)
        jack-deviation        (map #(- jack-mean ^double %1) jack-samples)
        ^double sqr-deviation (reduce + 0.0 (map utils/sqrd jack-deviation))
        acc                   (if (zero? sqr-deviation)
                                Double/POSITIVE_INFINITY
                                (/ ^double (reduce
                                            + 0.0
                                            (map utils/cubed jack-deviation))
                                   (* 6.0 (Math/pow sqr-deviation 1.5))))
        tt                    (map
                               (fn [^double x]
                                 (probability/normal-cdf
                                  (+ z0 (/ (+ z0 x) (- 1.0 (* acc (+ z0 x)))))))
                               z-alpha)
        ooo                   (map
                               (fn [^double x] (utils/trunc (* x ^long size)))
                               tt)
        sorted-samples        (sort nan-safe-compare samples)
        confpoints            (map (partial nth sorted-samples) ooo)]
    [confpoints z0 acc jack-mean jack-samples]))

(defn bca-nonparametric
  "Non-parametric BCa estimate of a statistic on data.

  Size bootstrap samples are used. Confidence values are returned at the
  alpha normal quantiles. rng-factory is a function that returns a random
  number generator to use for the sampling.

  References:
  - An introduction to the bootstrap. Efron, B., & Tibshirani, R. J. (1993).
  - http://lib.stat.cmu.edu/S/bootstrap.funs for Efron's original implementation."
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
  "Convert BCa result to BcaEstimate record."
  [alpha bca-estimate]
  (assert (= 0.5 (first alpha)) alpha)
  (->BcaEstimate
   (first (first bca-estimate))
   (mapv
    (fn [value z] {:value value :alpha z})
    (next (first bca-estimate))
    (next alpha))))

(defn bootstrap-bca
  "Bootstrap a statistic with BCa confidence intervals.

  Statistic can produce multiple statistics as a vector, so you can use juxt
  to pass multiple statistics.

  Returns a BcaEstimate record with :point-estimate and :estimate-quantiles.

  See: http://en.wikipedia.org/wiki/Bootstrapping_(statistics)"
  [data statistic size alpha rng-factory]
  (assert (nat-int? size))
  (let [bca (bca-nonparametric data statistic size alpha rng-factory)]
    (if (vector? bca)
      (bca-to-estimate alpha bca)
      (map (partial bca-to-estimate alpha) bca))))

(defn bootstrap
  "Bootstrap a statistic.

  Statistic can produce multiple statistics as a vector, so you can use juxt
  to pass multiple statistics.

  Returns [mean variance [lower upper]] for each statistic.

  See: http://en.wikipedia.org/wiki/Bootstrapping_(statistics)"
  [data statistic size rng-factory]
  (let [samples (bootstrap-sample data statistic size rng-factory)]
    (if (vector? (first samples))
      (map bootstrap-estimate samples)
      (bootstrap-estimate samples))))

(defn scale-bootstrap-estimate
  "Scale a BcaEstimate by the given scale factor."
  [estimate ^double scale]
  [(* ^double (:point-estimate estimate) scale)
   (map #(* scale ^double (:value %1)) (:estimate-quantiles estimate))])

(defn scale-bootstrap-stat
  "Scale a bootstrap stat using the given scale function."
  [scale-f stat]
  (-> stat
      (update :point-estimate scale-f)
      (update :estimate-quantiles
              #(mapv (fn [q] (update q :value scale-f)) %))))

(defn assoc-bootstrap-mean-3-sigma
  "Add :mean-plus-3sigma and :mean-minus-3sigma to stats map."
  [{:keys [mean variance] :as stats}]
  (let [three-sigma       (* 3 (Math/sqrt (:point-estimate variance)))
        mean-plus-3sigma  (+ ^double (:point-estimate mean) three-sigma)
        mean-minus-3sigma (- ^double (:point-estimate mean) three-sigma)]
    (assoc stats
           :mean-plus-3sigma {:point-estimate mean-plus-3sigma}
           :mean-minus-3sigma {:point-estimate mean-minus-3sigma})))

(defn scale-bootstrap-values
  "Apply function f to all values in stats map."
  [stats f]
  (utils/update-vals stats f))

(def stats-fn-map
  "Map of stat keywords to their corresponding functions."
  {:mean     core/mean
   :variance core/variance
   :min-val  core/min
   :max-val  core/max
   :skewness core/skewness
   :kurtosis core/kurtosis
   :cv       core/cv})

(defn stats-fns
  "Build vector of stat functions including quantile functions for given quantiles."
  [quantiles]
  (into
   (vec (vals stats-fn-map))
   (map #(partial core/quantile %) quantiles)))

(defn stats-fn
  "Combine multiple stat functions into one that returns a vector of results."
  [fs]
  (fn [vs]
    (mapv #(% vs) fs)))
