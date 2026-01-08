(ns criterium.optimisation.regression
  "Linear regression algorithms."
  (:require
   [criterium.utils.interface :as utils]))

(defn- muld
  ^double [^double a ^double b]
  (* a b))

(defn sum-square-delta
  "Sum of squared differences from a mean value."
  ^double [vs ^double mv]
  (reduce + (map (comp utils/sqrd (fn [^double x] (- x mv))) vs)))

(defn linear-regression
  "Perform simple linear regression: y = a0 + a1*x.

  Returns a map with:
  - :coeffs [a0 a1] - intercept and slope
  - :variance - residual variance (MSE with n-2 degrees of freedom)
  - :r-sqr - coefficient of determination (R-squared)"
  [xs ys]
  (let [n             (count xs)
        mx            (/ ^double (reduce + xs) n)
        my            (/ ^double (reduce + ys) n)
        nmxmy         (* n mx my)
        nmxmx         (* n mx mx)
        s1            (- ^double (reduce + (map muld xs ys))
                         nmxmy)
        s2            (- ^double (reduce + (map utils/sqrd xs))
                         nmxmx)
        a1            (/ s1 s2)
        a0            (- my (* a1 mx))
        f             (fn ^double [^double x] (+ a0 (* a1 x)))
        pred-ys       (mapv f xs)
        sqr-residuals (mapv (comp utils/sqrd -) ys pred-ys)
        ss-residuals  (double (reduce + sqr-residuals))
        variance      (/ ss-residuals (- n 2))
        ss-total      (sum-square-delta ys my)
        r-sqr         (- 1 (/ ss-residuals ss-total))]
    {:coeffs   [a0 a1]
     :variance variance
     :r-sqr    r-sqr}))
