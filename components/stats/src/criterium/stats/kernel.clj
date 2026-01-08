(ns criterium.stats.kernel
  "Kernel functions for density estimation.

  Provides kernel weight functions and basic kernel density estimators
  for modal estimation and bandwidth selection."
  (:require
   [criterium.utils.interface :as utils]))

(defn modal-estimation-constant
  "Kernel function for estimation of multi-modality.
  h-k is the critical bandwidth, sample-variance is the observed sample variance.
  Equation 7, Nonparametric assessment of multimodality for univariate
  data. Salgado-Ugarte IH, Shimizu M"
  ^double [^double h-k ^double sample-variance]
  (Math/sqrt (+ 1 (/ (utils/sqr h-k) sample-variance))))

(defn smoothed-sample
  "Smoothed estimation function.
  Generates a lazy sequence of smoothed values from data using kernel smoothing."
  [^double c-k ^double h-k data deviates]
  (lazy-seq
   (cons
    (* c-k (+ ^double (first data)
              (* h-k ^double (first deviates))))
    (when-let [n (next data)]
      (smoothed-sample c-k h-k n (next deviates))))))

(defn gaussian-weight
  "Weight function for gaussian kernel.
  K(t) = (1/sqrt(2*pi)) * exp(-t^2/2)"
  ^double [^double t]
  (let [k (Math/pow (* 2 Math/PI) -0.5)]
    (* k (Math/exp (/ (* t t) -2)))))

(defn kernel-density-estimator
  "Kernel density estimator for x, given n samples X, weights K and width h.
  Computes f(x) = (1/nh) * sum_i K((x - X_i)/h)"
  [h K n X x]
  (/ ^double (reduce
              (fn ^double [^double a ^double b]
                (+ a
                   ^double (K (/ (- ^double x b) ^double h))))
              0.0 X)
     (* (long n) (double h))))
