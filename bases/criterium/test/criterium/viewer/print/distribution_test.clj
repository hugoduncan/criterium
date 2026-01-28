(ns criterium.viewer.print.distribution-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.array :as arr]
   [criterium.collect-plan :as collect-plan]
   [criterium.collector.metrics :as metrics]
   [criterium.test-utils :refer [trimmed-lines]]
   [criterium.view :as view]
   [criterium.viewer.print.distribution :as dist]))

;; Tests multimethod registration, data processing, and output formatting.
;; Distribution fit analysis compares statistical models (Gamma, Log-normal,
;; Inverse Gaussian, Weibull) using AIC/BIC criteria and goodness-of-fit tests.

;;; Test data fixtures

(def sample-metric-defs
  (select-keys (metrics/metrics) [:elapsed-time]))

(def gamma-best-fit
  "Distribution fit results where Gamma is the best model."
  {:fits
   {[:elapsed-time]
    {:n 100
     :best-model :gamma
     :distributions
     {:gamma {:aic 150.0 :delta-aic 0.0 :bic 155.0
              :ks {:statistic 0.05 :p-value 0.85}
              :cvm {:statistic 0.02 :p-value 0.90}}
      :lognormal {:aic 160.0 :delta-aic 10.0 :bic 165.0
                  :ks {:statistic 0.08 :p-value 0.60}}}
     :parameter-cis
     {:gamma {:shape {:point-estimate 2.5 :ci-lower 2.0 :ci-upper 3.0}
              :scale {:point-estimate 0.01 :ci-lower 0.008 :ci-upper 0.012}}}}}})

(def gamma-best-fit-with-params
  "Distribution fit results with full params for chart rendering."
  {:fits
   {[:elapsed-time]
    {:n 50
     :best-model :gamma
     :sample-range [1000000.0 5000000.0]  ; 1-5ms in nanoseconds
     :distributions
     {:gamma {:aic 150.0 :delta-aic 0.0 :bic 155.0
              :params {:shape 2.5 :scale 1000000.0}}}
     :parameter-cis
     {:gamma {:shape {:point-estimate 2.5 :ci-lower 2.0 :ci-upper 3.0}
              :scale {:point-estimate 1000000.0 :ci-lower 800000.0 :ci-upper 1200000.0}}}}}})

(def small-sample-fit
  "Distribution fit with small sample warning."
  {:fits
   {[:elapsed-time]
    {:n 25
     :warning true
     :best-model :lognormal
     :distributions
     {:lognormal {:aic 80.0 :delta-aic 0.0 :bic 82.0}}
     :parameter-cis
     {:lognormal {:mu {:point-estimate 5.0 :ci-lower 4.5 :ci-upper 5.5}
                  :sigma {:point-estimate 0.5 :ci-lower 0.3 :ci-upper 0.7}}}}}})

(def error-and-skip-fit
  "Distribution fit with error and skipped results."
  {:fits
   {[:elapsed-time]
    {:n 50
     :best-model nil
     :distributions
     {:gamma {:error "convergence failed"}
      :weibull {:skipped :non-positive-values}}}}})

(defn data-map-with-fit
  "Create data map with distribution fit and sample metadata."
  [fit-data]
  {:distribution-fit fit-data
   :samples {:metrics-defs sample-metric-defs}})

(defn- make-sample-values
  "Create sample values array for testing."
  [n ^double mean ^double stddev]
  (let [rng (java.util.Random. 42)]
    (arr/->double-array
     (double-array
      (repeatedly n #(+ mean (* stddev (.nextGaussian rng))))))))

(defn data-map-with-fit-and-samples
  "Create data map with distribution fit, sample metadata, and sample values."
  [fit-data]
  {:distribution-fit fit-data
   :samples {:type :criterium/metrics-samples
             :metrics-defs sample-metric-defs
             :metric->values {[:elapsed-time] (make-sample-values 50 2500000.0 500000.0)}
             :transform collect-plan/identity-transforms
             :batch-size 1
             :eval-count 50
             :num-samples 50}})

;;; Distribution Models Tests

(deftest print-distribution-models-test
  (testing "print-distribution-models"
    (testing "prints model comparison with AIC, BIC, and GOF tests"
      (let [^String output (with-out-str
                             (dist/print-distribution-models
                              {}
                              (data-map-with-fit gamma-best-fit)))]
        (is (.contains output "Distribution Model Comparison:"))
        (is (.contains output "Elapsed Time: Distribution Models (n=100"))
        (is (.contains output "Gamma: AIC=150.0"))
        (is (.contains output "<- BEST"))
        (is (.contains output "K-S: D=0.0500 p=0.8500"))
        (is (.contains output "CvM: D=0.0200 p=0.9000"))
        (is (.contains output "Log-normal: AIC=160.0 (Δ10.0)"))))

    (testing "shows small sample warning"
      (let [^String output (with-out-str
                             (dist/print-distribution-models
                              {}
                              (data-map-with-fit small-sample-fit)))]
        (is (.contains output "WARNING: small sample"))))

    (testing "displays errors and skipped distributions"
      (let [^String output (with-out-str
                             (dist/print-distribution-models
                              {}
                              (data-map-with-fit error-and-skip-fit)))]
        (is (.contains output "error - convergence failed"))
        (is (.contains output "skipped (non-positive-values)"))))

    (testing "returns nil with no output when no data"
      (is (= ""
             (with-out-str
               (dist/print-distribution-models {} {})))))))

;;; Distribution Parameter CIs Tests

(deftest print-distribution-parameter-cis-test
  (testing "print-distribution-parameter-cis"
    (testing "prints parameter confidence intervals for best model"
      (let [^String output (with-out-str
                             (dist/print-distribution-parameter-cis
                              {}
                              (data-map-with-fit gamma-best-fit)))]
        (is (.contains output "Distribution Parameter Confidence Intervals:"))
        (is (.contains output "Gamma Parameter CIs"))
        (is (.contains output "shape: 2.500 [2.000, 3.000]"))
        (is (.contains output "scale: 0.01000 [0.008000, 0.01200]"))))

    (testing "uses custom distribution-fit-id when specified"
      (let [custom-data {:custom-fit gamma-best-fit
                         :samples {:metrics-defs sample-metric-defs}}
            output (with-out-str
                     (dist/print-distribution-parameter-cis
                      {:distribution-fit-id :custom-fit}
                      (assoc custom-data :custom-fit (:fits gamma-best-fit))))]
        ;; No output since :custom-fit doesn't contain :fits
        (is (= "" output))))

    (testing "returns nil with no output when no data"
      (is (= ""
             (with-out-str
               (dist/print-distribution-parameter-cis {} {})))))))

;;; Multimethod Registration Tests

(deftest distribution-models-multimethod-test
  (testing "view/distribution-models*"
    (testing "dispatches to print implementation"
      (let [lines (trimmed-lines
                   (with-out-str
                     (view/distribution-models*
                      :print
                      {}
                      (data-map-with-fit gamma-best-fit))))]
        (is (some #(.contains ^String % "Distribution Model Comparison") lines))))))

(deftest distribution-parameter-cis-multimethod-test
  (testing "view/distribution-parameter-cis*"
    (testing "dispatches to print implementation"
      (let [lines (trimmed-lines
                   (with-out-str
                     (view/distribution-parameter-cis*
                      :print
                      {}
                      (data-map-with-fit gamma-best-fit))))]
        (is (some #(.contains ^String % "Parameter CIs") lines))))))

;;; Chart Views Tests

(deftest distribution-pdf-test
  ;; Tests ASCII PDF chart rendering for distribution fits.
  ;; Verifies chart structure and content.
  (testing "distribution-pdf*"
    (testing "renders ASCII PDF chart with fitted distribution"
      (let [data-map (data-map-with-fit-and-samples gamma-best-fit-with-params)
            output (with-out-str
                     (view/distribution-pdf* :print {} data-map))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "PDF") lines)
            "Should show PDF in header")
        (is (some #(str/includes? % "Gamma") lines)
            "Should show distribution name")
        (is (some #(str/includes? % "n=50") lines)
            "Should show sample count")
        (is (some #(str/includes? % "|") lines)
            "Should have axis markers")
        (is (some #(or (str/includes? % "*") (str/includes? % ".")) lines)
            "Should have chart characters")))

    (testing "produces no output when no distribution fit data"
      (is (= ""
             (with-out-str
               (view/distribution-pdf* :print {} {})))))))

(deftest distribution-cdf-test
  ;; Tests ASCII CDF chart rendering showing ECDF.
  ;; Verifies chart structure and sample count display.
  (testing "distribution-cdf*"
    (testing "renders ASCII CDF chart with ECDF"
      (let [data-map (data-map-with-fit-and-samples gamma-best-fit-with-params)
            output (with-out-str
                     (view/distribution-cdf* :print {} data-map))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "CDF") lines)
            "Should show CDF in header")
        (is (some #(str/includes? % "n=50") lines)
            "Should show sample count")
        (is (some #(str/includes? % "|") lines)
            "Should have axis markers")
        (is (some #(str/includes? % "*") lines)
            "Should have point characters")))

    (testing "produces no output when no samples"
      (is (= ""
             (with-out-str
               (view/distribution-cdf* :print {} {})))))))

(deftest distribution-qq-test
  ;; Tests ASCII Q-Q plot rendering for distribution fits.
  ;; Verifies chart structure showing theoretical vs observed quantiles.
  (testing "distribution-qq*"
    (testing "renders ASCII Q-Q plot for best-fit distribution"
      (let [data-map (data-map-with-fit-and-samples gamma-best-fit-with-params)
            output (with-out-str
                     (view/distribution-qq* :print {} data-map))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "Q-Q") lines)
            "Should show Q-Q in header")
        (is (some #(str/includes? % "Gamma") lines)
            "Should show distribution name")
        (is (some #(str/includes? % "n=50") lines)
            "Should show sample count")
        (is (some #(str/includes? % "|") lines)
            "Should have axis markers")
        (is (some #(str/includes? % "*") lines)
            "Should have point characters")))

    (testing "produces no output when no distribution fit data"
      (is (= ""
             (with-out-str
               (view/distribution-qq* :print {} {})))))))
