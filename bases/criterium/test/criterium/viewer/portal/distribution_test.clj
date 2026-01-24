(ns criterium.viewer.portal.distribution-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.collector.metrics :as metrics]
   [criterium.test-data :as test-data]
   [criterium.view :as view]
   [criterium.viewer.portal.core :as portal.core]
   [criterium.viewer.portal.distribution]))

;;; Test fixtures

(def sample-metric-defs
  (select-keys (metrics/metrics) [:elapsed-time]))

(def gamma-best-fit
  "Distribution fit results where Gamma is the best model."
  {:fits
   {[:elapsed-time]
    {:n 100
     :best-model :gamma
     :distributions
     {:gamma {:aic 500.0
              :delta-aic 0.0
              :bic 505.0
              :ks-test {:statistic 0.05 :p-value 0.8}
              :cvm-test {:statistic 0.02 :p-value 0.9}}
      :lognormal {:aic 510.0
                  :delta-aic 10.0
                  :bic 515.0
                  :ks-test {:statistic 0.08 :p-value 0.6}
                  :cvm-test {:statistic 0.04 :p-value 0.7}}}}}})

(def error-and-skip-fit
  "Distribution fit with error and skipped results."
  {:fits
   {[:elapsed-time]
    {:n 50
     :best-model nil
     :distributions
     {:gamma {:error "convergence failed"}
      :weibull {:skipped :negative-values}}}}})

(def gamma-with-cis
  "Distribution fit with parameter CIs."
  {:fits
   {[:elapsed-time]
    {:best-model :gamma
     :parameter-cis
     {:gamma {:shape {:point-estimate 2.0
                      :ci-lower 1.5
                      :ci-upper 2.5}
              :rate {:point-estimate 0.5
                     :ci-lower 0.3
                     :ci-upper 0.7}}}}}})

;;; Distribution Models Tests

(deftest distribution-models-test
  ;; Tests the portal viewer output for distribution-models results.
  ;; Verifies table structure with model comparison metrics.
  (testing "distribution-models*"
    (testing "produces table with model comparison data"
      (let [tapped (atom [])
            data-map {:distribution-fit gamma-best-fit}]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (view/distribution-models* :portal {} data-map))
        (is (= 1 (count @tapped)))
        (let [rows (first @tapped)]
          (is (= 2 (count rows)))
          (let [gamma-row (first (filter #(= "Gamma" (:distribution %)) rows))
                lognormal-row (first (filter #(= "Log-normal" (:distribution %)) rows))]
            (is (= "fitted" (:status gamma-row)))
            (is (= true (:best? gamma-row)))
            (is (= "500.0" (:aic gamma-row)))
            (is (= "0.0" (:delta-aic gamma-row)))
            (is (= "fitted" (:status lognormal-row)))
            (is (= false (:best? lognormal-row)))
            (is (= "510.0" (:aic lognormal-row)))))))

    (testing "handles error results"
      (let [tapped (atom [])
            data-map {:distribution-fit error-and-skip-fit}]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (view/distribution-models* :portal {} data-map))
        (is (= 1 (count @tapped)))
        (let [rows (first @tapped)
              error-row (first (filter #(= "Gamma" (:distribution %)) rows))]
          (is (= "error" (:status error-row)))
          (is (= "-" (:aic error-row))))))

    (testing "handles skipped results"
      (let [tapped (atom [])
            data-map {:distribution-fit error-and-skip-fit}]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (view/distribution-models* :portal {} data-map))
        (is (= 1 (count @tapped)))
        (let [rows (first @tapped)
              skipped-row (first (filter #(= "Weibull" (:distribution %)) rows))]
          (is (= "negative-values" (:status skipped-row))))))

    (testing "uses custom distribution-fit-id"
      (let [tapped (atom [])
            data-map {:my-fit gamma-best-fit}]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (view/distribution-models* :portal {:distribution-fit-id :my-fit} data-map))
        (is (= 1 (count @tapped)))))

    (testing "returns nil when distribution-fit not found"
      (let [tapped (atom [])]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (is (nil? (view/distribution-models* :portal {} {}))))
        (is (empty? @tapped))))))

;;; Distribution Parameter CIs Tests

(deftest distribution-parameter-cis-test
  ;; Tests the portal viewer output for distribution-parameter-cis results.
  ;; Verifies table format with parameter estimates and confidence intervals.
  (testing "distribution-parameter-cis*"
    (testing "produces table with parameter CIs"
      (let [tapped (atom [])
            data-map {:distribution-fit gamma-with-cis}]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (view/distribution-parameter-cis* :portal {} data-map))
        (is (= 1 (count @tapped)))
        (let [rows (first @tapped)]
          (is (= 2 (count rows)))
          (is (every? #(= "Gamma" (:distribution %)) rows))
          (is (some #(= "shape" (:parameter %)) rows))
          (is (some #(= "rate" (:parameter %)) rows)))))

    (testing "returns nil when no parameter CIs"
      (let [tapped (atom [])
            data-map {:distribution-fit
                      {:fits {[:elapsed-time]
                              {:best-model nil
                               :parameter-cis {}}}}}]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (view/distribution-parameter-cis* :portal {} data-map))
        (is (empty? @tapped))))))

;;; Distribution PDF Tests

(deftest distribution-pdf-test
  ;; Tests the portal viewer output for distribution-pdf chart.
  ;; Verifies Vega-Lite spec is produced.
  (testing "distribution-pdf*"
    (testing "produces Vega-Lite spec when data exists"
      (let [tapped (atom [])
            ;; Use kde-data-map which has simpler requirements
            data-map (test-data/kde-data-map)]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-vega-lite #(swap! tapped conj %)]
          (view/distribution-pdf* :portal {} data-map))
        (is (= 1 (count @tapped)))
        (is (map? (first @tapped)))))

    (testing "returns nil when KDE not found"
      (let [tapped (atom [])]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-vega-lite #(swap! tapped conj %)]
          (is (nil? (view/distribution-pdf* :portal {} {}))))
        (is (empty? @tapped))))))

;;; Distribution CDF Tests

(deftest distribution-cdf-test
  ;; Tests the portal viewer output for distribution-cdf chart.
  ;; Verifies Vega-Lite spec is produced.
  (testing "distribution-cdf*"
    (testing "produces Vega-Lite spec when data exists"
      (let [tapped (atom [])
            ;; Use kde-data-map and add samples for ECDF
            data-map (merge (test-data/kde-data-map)
                            {:samples (:samples (test-data/distribution-cdf-data-map))})]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-vega-lite #(swap! tapped conj %)]
          (view/distribution-cdf* :portal {} data-map))
        (is (= 1 (count @tapped)))
        (is (map? (first @tapped)))))

    (testing "returns nil when KDE not found"
      (let [tapped (atom [])]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-vega-lite #(swap! tapped conj %)]
          (is (nil? (view/distribution-cdf* :portal {} {}))))
        (is (empty? @tapped))))))

;;; Distribution Q-Q Tests

(deftest distribution-qq-test
  ;; Tests the portal viewer output for distribution-qq chart.
  ;; Verifies Vega-Lite spec is produced.
  (testing "distribution-qq*"
    (testing "produces Vega-Lite spec when data exists"
      (let [tapped (atom [])
            ;; Use kde-data-map and add samples for Q-Q
            data-map (merge (test-data/kde-data-map)
                            {:samples (:samples (test-data/distribution-qq-data-map))})]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-vega-lite #(swap! tapped conj %)]
          (view/distribution-qq* :portal {} data-map))
        (is (= 1 (count @tapped)))
        (is (map? (first @tapped)))))

    (testing "returns nil when KDE not found"
      (let [tapped (atom [])]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-vega-lite #(swap! tapped conj %)]
          (is (nil? (view/distribution-qq* :portal {} {}))))
        (is (empty? @tapped))))))
