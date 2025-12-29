(ns criterium.validation.probability-validation-test
  "Validation tests for criterium.util.probability against R reference implementations.

  Tests skip gracefully when R/Rserve is unavailable."
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.test.assert :refer [approx=]]
   [criterium.util.probability :as prob]
   [criterium.validation.r :as r]))

;;; Test quantiles
;; Standard quantile points to test across the distribution
(def standard-quantiles [0.001 0.01 0.05 0.1 0.25 0.5 0.75 0.9 0.95 0.99 0.999])

;;; Test x values for CDF
;; Central values where CDF is not too small or too close to 1
(def central-x-values [-2.0 -1.5 -1.0 -0.5 0.0 0.5 1.0 1.5 2.0])
;; Tail values where CDF is very small or very close to 1
(def tail-x-values [-4.0 -3.0 3.0 4.0])

;;; Tests

(deftest normal-quantile-validation-test
  ;; Validates criterium.util.probability/normal-quantile against R's qnorm() function.
  ;; R's qnorm(p) returns the quantile of the standard normal distribution at probability p.
  ;; The Wichura AS241 algorithm is highly accurate (machine precision for central values).
  (testing "normal-quantile"
    (if-not (r/r-available?)
      (do
        (println "Skipping normal-quantile validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "at standard probability values"
          (doseq [p standard-quantiles]
            (testing (str "at p=" p)
              (let [r-q (first (r/r-eval (str "qnorm(" p ")")))
                    clj-q (prob/normal-quantile p)]
                (is (approx= r-q clj-q 1e-10)
                    (format "normal-quantile mismatch at p=%.3f: R=%.15f, clj=%.15f"
                            p r-q clj-q))))))

        (testing "at the median (p=0.5)"
          (let [r-q (first (r/r-eval "qnorm(0.5)"))
                clj-q (prob/normal-quantile 0.5)]
            ;; Median of standard normal should be exactly 0
            (is (approx= r-q clj-q 1e-14)
                (format "median mismatch: R=%.15f, clj=%.15f" r-q clj-q))
            (is (approx= 0.0 clj-q 1e-14)
                "median of standard normal should be 0")))

        (testing "at symmetric probability pairs"
          ;; qnorm(p) = -qnorm(1-p) for any p
          (doseq [^double p [0.1 0.25 0.05 0.01]]
            (testing (str "p=" p " and p=" (- 1.0 p))
              (let [q-low (prob/normal-quantile p)
                    q-high (prob/normal-quantile (- 1.0 p))]
                (is (approx= (- q-low) q-high 1e-10)
                    (format "symmetry mismatch: q(%.2f)=%.15f, q(%.2f)=%.15f"
                            p q-low (- 1.0 p) q-high))))))

        (testing "at extreme probabilities"
          (doseq [p [0.0001 0.00001 0.9999 0.99999]]
            (testing (str "at p=" p)
              (let [r-q (first (r/r-eval (str "qnorm(" p ")")))
                    clj-q (prob/normal-quantile p)]
                ;; Slightly larger tolerance for extreme values
                (is (approx= r-q clj-q 1e-8)
                    (format "normal-quantile mismatch at p=%.5f: R=%.15f, clj=%.15f"
                            p r-q clj-q))))))))))

(deftest normal-cdf-validation-test
  ;; Validates criterium.util.probability/normal-cdf against R's pnorm() function.
  ;; R's pnorm(x) returns the cumulative probability P(X < x) for standard normal.
  ;; Note: The implementation uses polynomial erf approximation with max error 1.5e-7.
  (testing "normal-cdf"
    (if-not (r/r-available?)
      (do
        (println "Skipping normal-cdf validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "at central x values"
          ;; For central values, CDF is not too small or too close to 1,
          ;; so relative tolerance works well.
          (doseq [x central-x-values]
            (testing (str "at x=" x)
              (let [r-p (first (r/r-eval (str "pnorm(" x ")")))
                    clj-p (prob/normal-cdf x)]
                ;; 1e-5 tolerance accounts for erf approximation propagation
                (is (approx= r-p clj-p 1e-5)
                    (format "normal-cdf mismatch at x=%.1f: R=%.15f, clj=%.15f"
                            x r-p clj-p))))))

        (testing "at tail x values"
          ;; For tail values, CDF is very small or very close to 1.
          ;; Use absolute tolerance since relative error is misleading.
          (doseq [x tail-x-values]
            (testing (str "at x=" x)
              (let [r-p (first (r/r-eval (str "pnorm(" x ")")))
                    clj-p (prob/normal-cdf x)
                    abs-diff (Math/abs (- r-p clj-p))]
                ;; Absolute error should be < 1e-6 (well within erf max error)
                (is (< abs-diff 1e-6)
                    (format "normal-cdf mismatch at x=%.1f: R=%.15f, clj=%.15f, diff=%.2e"
                            x r-p clj-p abs-diff))))))

        (testing "at x=0 (the median)"
          (let [r-p (first (r/r-eval "pnorm(0)"))
                clj-p (prob/normal-cdf 0.0)]
            ;; CDF at 0 should be exactly 0.5
            (is (approx= r-p clj-p 1e-10)
                (format "median CDF mismatch: R=%.15f, clj=%.15f" r-p clj-p))
            (is (approx= 0.5 clj-p 1e-10)
                "CDF at 0 should be 0.5")))

        (testing "symmetry around the mean"
          ;; pnorm(x) + pnorm(-x) = 1 for any x
          (doseq [^double x [0.5 1.0 2.0 3.0]]
            (testing (str "x=" x " and x=" (- x))
              (let [p-pos (prob/normal-cdf x)
                    p-neg (prob/normal-cdf (- x))]
                (is (approx= 1.0 (+ p-pos p-neg) 1e-10)
                    (format "symmetry mismatch: pnorm(%.1f)=%.15f, pnorm(%.1f)=%.15f, sum=%.15f"
                            x p-pos (- x) p-neg (+ p-pos p-neg)))))))

        (testing "at extreme x values"
          ;; For extreme values (|x| >= 5), CDF is extremely small or close to 1.
          ;; Use absolute tolerance since these are edge cases.
          (doseq [x [-5.0 -6.0 5.0 6.0]]
            (testing (str "at x=" x)
              (let [r-p (first (r/r-eval (str "pnorm(" x ")")))
                    clj-p (prob/normal-cdf x)
                    abs-diff (Math/abs (- r-p clj-p))]
                ;; Absolute error should be < 1e-6
                (is (< abs-diff 1e-6)
                    (format "normal-cdf mismatch at x=%.1f: R=%.15f, clj=%.15f, diff=%.2e"
                            x r-p clj-p abs-diff))))))))))

(deftest normal-cdf-quantile-inverse-test
  ;; Verifies that normal-cdf and normal-quantile are inverses.
  ;; pnorm(qnorm(p)) ≈ p and qnorm(pnorm(x)) ≈ x
  (testing "normal-cdf and normal-quantile"
    (testing "are inverses"
      (testing "pnorm(qnorm(p)) = p"
        (doseq [p [0.1 0.25 0.5 0.75 0.9 0.95 0.99]]
          (testing (str "at p=" p)
            (let [x (prob/normal-quantile p)
                  p-back (prob/normal-cdf x)]
              ;; Due to erf approximation, use 1e-6 tolerance
              (is (approx= p p-back 1e-6)
                  (format "inverse mismatch: p=%.2f, qnorm(p)=%.15f, pnorm(qnorm(p))=%.15f"
                          p x p-back))))))

      (testing "qnorm(pnorm(x)) = x"
        (doseq [x [-2.0 -1.0 0.0 1.0 2.0]]
          (testing (str "at x=" x)
            (let [p (prob/normal-cdf x)
                  x-back (prob/normal-quantile p)]
              ;; Due to erf approximation, use 1e-6 tolerance
              (is (approx= x x-back 1e-5)
                  (format "inverse mismatch: x=%.1f, pnorm(x)=%.15f, qnorm(pnorm(x))=%.15f"
                          x p x-back)))))))))
