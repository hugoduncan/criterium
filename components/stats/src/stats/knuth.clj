(ns stats.knuth
  "Knuth's Bayesian histogram binning algorithm.

   Implements optimal bin count selection by maximizing a log-posterior
   based on Knuth (2019) DOI: 10.1016/j.dsp.2019.102581

   The algorithm finds the optimal number of equal-width bins M by maximizing:
   F(M|x,I) = n·log(M) + logΓ(M/2) - M·logΓ(1/2) - logΓ((2n+M)/2) + Σₖ₌₁ᴹ logΓ(nₖ + 1/2)

   where n = sample count, nₖ = count in bin k."
  (:require
   [stats.probability :as prob]))

;;; Constants

(def ^:private ^:const log-gamma-half
  "Precomputed logΓ(1/2) = log(√π)."
  (prob/log-gamma 0.5))

;;; Binning

(defn- bin-counts
  "Compute bin counts for M equal-width bins.
  Returns a long array of counts for each bin."
  ^longs [samples ^long num-bins ^double min-val ^double max-val]
  (let [counts    (long-array num-bins)
        range-val (- max-val min-val)
        width     (/ range-val (double num-bins))
        last-bin  (dec num-bins)]
    (doseq [^double x samples]
      (let [bin-idx (long (/ (- x min-val) width))
            ;; Clamp to valid range (handles edge case where x == max-val)
            bin-idx (min last-bin (max 0 bin-idx))]
        (aset counts bin-idx (inc (aget counts bin-idx)))))
    counts))

;;; Log-posterior

(defn log-posterior
  "Compute Knuth's log-posterior for M bins given sample count and bin counts.

  F(M|x,I) = n·log(M) + logΓ(M/2) - M·logΓ(1/2) - logΓ((2n+M)/2) + Σₖ₌₁ᴹ logΓ(nₖ + 1/2)

  Parameters:
    n - total sample count
    bin-counts - sequence of counts per bin

  Returns the log-posterior value (higher is better)."
  ^double [^long n bin-counts]
  (let [m          (count bin-counts)
        m-double   (double m)
        n-double   (double n)
        ;; n·log(M)
        term1      (* n-double (Math/log m-double))
        ;; logΓ(M/2)
        term2      (prob/log-gamma (/ m-double 2.0))
        ;; -M·logΓ(1/2)
        term3      (- (* m-double (double log-gamma-half)))
        ;; -logΓ((2n+M)/2)
        term4      (- (prob/log-gamma (/ (+ (* 2.0 n-double) m-double) 2.0)))
        ;; Σₖ₌₁ᴹ logΓ(nₖ + 1/2)
        term5      (double
                    (reduce
                     (fn ^double [^double sum ^long nk]
                       (+ sum (prob/log-gamma (+ (double nk) 0.5))))
                     0.0
                     bin-counts))]
    (+ term1 term2 term3 term4 term5)))

;;; Optimal bin selection

(defn optimal-bins
  "Find optimal number of bins using Knuth's Bayesian method.

  Searches M ∈ [1, max-bins] for the value that maximizes the log-posterior.

  Parameters:
    samples - sequence of numeric values
    opts - optional map with:
      :max-bins - maximum M to search (default: 50)
      :min - pre-computed minimum value (avoids redundant scan)
      :max - pre-computed maximum value (avoids redundant scan)

  Returns map with:
    :optimal-bins - the optimal number of bins M
    :log-posterior - the log-posterior value at optimal M

  Throws:
    ex-info {:error :knuth/no-samples} for empty input
    ex-info {:error :knuth/same-values} when all values are identical"
  ([samples] (optimal-bins samples {}))
  ([samples {:keys [max-bins min max] :or {max-bins 50}}]
   (when (empty? samples)
     (throw (ex-info "Input samples cannot be empty"
                     {:error :knuth/no-samples})))
   (let [min-val (double (or min (reduce clojure.core/min samples)))
         max-val (double (or max (reduce clojure.core/max samples)))]
     (when (= min-val max-val)
       (throw (ex-info "All sample values are identical - cannot determine optimal bins"
                       {:error   :knuth/same-values
                        :min-val min-val
                        :max-val max-val})))
     (let [n             (count samples)
           samples-vec   (vec samples)
           max-bins-long (long max-bins)]
       ;; Search over M = 1 to max-bins
       (loop [m       (long 1)
              best-m  (long 1)
              best-lp Double/NEGATIVE_INFINITY]
         (if (> m max-bins-long)
           {:optimal-bins  best-m
            :log-posterior best-lp}
           (let [counts (bin-counts samples-vec m min-val max-val)
                 lp     (log-posterior n counts)]
             (if (> lp best-lp)
               (recur (inc m) m lp)
               (recur (inc m) best-m best-lp)))))))))
