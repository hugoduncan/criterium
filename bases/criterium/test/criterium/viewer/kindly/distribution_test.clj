(ns criterium.viewer.kindly.distribution-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.test-data :as test-data]
   [criterium.view :as view]
   [criterium.viewer.kindly :as kindly]
   [criterium.viewer.kindly.distribution]))

;;; Test fixtures

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
  ;; Tests the kindly viewer output for distribution-models results.
  ;; Verifies table structure with model comparison metrics.
  (testing "distribution-models*"
    (testing "produces table with model comparison data"
      (reset! kindly/accumulated [])
      (let [data-map {:distribution-fit gamma-best-fit}]
        (view/distribution-models* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result)) "Expected heading and table")
          (let [[heading table] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= ["**Distribution Models: elapsed-time (n=100)**"] heading))
            (is (= :kind/table (:kindly/kind (meta table))))
            (is (= 2 (count table)))
            (let [gamma-row (first
                             (filter #(= "Gamma" (:distribution %)) table))
                  lognormal-row (first
                                 (filter
                                  #(= "Log-normal" (:distribution %))
                                  table))]
              (is (= "fitted" (:status gamma-row)))
              (is (= true (:best? gamma-row)))
              (is (= "500.0" (:aic gamma-row)))
              (is (= "0.0" (:delta-aic gamma-row)))
              (is (= "fitted" (:status lognormal-row)))
              (is (= false (:best? lognormal-row)))
              (is (= "510.0" (:aic lognormal-row))))))))

    (testing "handles error results"
      (reset! kindly/accumulated [])
      (let [data-map {:distribution-fit error-and-skip-fit}]
        (view/distribution-models* :kindly {} data-map)
        (let [result (kindly/flush)
              [_heading table] result
              error-row (first (filter #(= "Gamma" (:distribution %)) table))]
          (is (= "error" (:status error-row)))
          (is (= "-" (:aic error-row))))))

    (testing "handles skipped results"
      (reset! kindly/accumulated [])
      (let [data-map {:distribution-fit error-and-skip-fit}]
        (view/distribution-models* :kindly {} data-map)
        (let [result (kindly/flush)
              [_heading table] result
              skipped-row (first
                           (filter #(= "Weibull" (:distribution %)) table))]
          (is (= "negative-values" (:status skipped-row))))))

    (testing "uses custom distribution-fit-id"
      (reset! kindly/accumulated [])
      (let [data-map {:my-fit gamma-best-fit}]
        (view/distribution-models*
         :kindly
         {:distribution-fit-id :my-fit}
         data-map)
        (let [result (kindly/flush)]
          (is (= 2 (count result))))))

    (testing "returns nil when distribution-fit not found"
      (reset! kindly/accumulated [])
      (is (nil? (view/distribution-models* :kindly {} {})))
      (is (nil? (kindly/flush))))))

;;; Distribution Parameter CIs Tests

(deftest distribution-parameter-cis-test
  ;; Tests the kindly viewer output for distribution-parameter-cis results.
  ;; Verifies table format with parameter estimates and confidence intervals.
  (testing "distribution-parameter-cis*"
    (testing "produces table with parameter CIs"
      (reset! kindly/accumulated [])
      (let [data-map {:distribution-fit gamma-with-cis}]
        (view/distribution-parameter-cis* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result)) "Expected heading and table")
          (let [[heading table] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= :kind/table (:kindly/kind (meta table))))
            (is (= 2 (count table)))
            (is (every? #(= "Gamma" (:distribution %)) table))
            (is (some #(= "shape" (:parameter %)) table))
            (is (some #(= "rate" (:parameter %)) table))))))

    (testing "returns nil when no parameter CIs"
      (reset! kindly/accumulated [])
      (let [data-map {:distribution-fit
                      {:fits {[:elapsed-time]
                              {:best-model nil
                               :parameter-cis {}}}}}]
        (view/distribution-parameter-cis* :kindly {} data-map)
        (is (nil? (kindly/flush)))))))

;;; Distribution PDF Tests

(deftest distribution-pdf-test
  ;; Tests the kindly viewer output for distribution-pdf chart.
  ;; Verifies Vega-Lite spec is produced.
  (testing "distribution-pdf*"
    (testing "produces Vega-Lite spec when data exists"
      (reset! kindly/accumulated [])
      (let [data-map (test-data/kde-data-map)]
        (view/distribution-pdf* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result)) "Expected heading and chart")
          (let [[heading chart] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= ["**Distribution PDF**"] heading))
            (is (= :kind/vega-lite (:kindly/kind (meta chart))))
            (is (map? chart))))))

    (testing "returns nil when KDE not found"
      (reset! kindly/accumulated [])
      (is (nil? (view/distribution-pdf* :kindly {} {})))
      (is (nil? (kindly/flush))))))

;;; Distribution CDF Tests

(deftest distribution-cdf-test
  ;; Tests the kindly viewer output for distribution-cdf chart.
  ;; Verifies Vega-Lite spec is produced.
  (testing "distribution-cdf*"
    (testing "produces Vega-Lite spec when data exists"
      (reset! kindly/accumulated [])
      (let [data-map (merge (test-data/kde-data-map)
                            {:samples (:samples
                                       (test-data/distribution-cdf-data-map))})]
        (view/distribution-cdf* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result)) "Expected heading and chart")
          (let [[heading chart] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= ["**Distribution CDF**"] heading))
            (is (= :kind/vega-lite (:kindly/kind (meta chart))))
            (is (map? chart))))))

    (testing "returns nil when KDE not found"
      (reset! kindly/accumulated [])
      (is (nil? (view/distribution-cdf* :kindly {} {})))
      (is (nil? (kindly/flush))))))

;;; Distribution Q-Q Tests

(deftest distribution-qq-test
  ;; Tests the kindly viewer output for distribution-qq chart.
  ;; Verifies Vega-Lite spec is produced.
  (testing "distribution-qq*"
    (testing "produces Vega-Lite spec when data exists"
      (reset! kindly/accumulated [])
      (let [data-map (merge (test-data/kde-data-map)
                            {:samples (:samples
                                       (test-data/distribution-qq-data-map))})]
        (view/distribution-qq* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result)) "Expected heading and chart")
          (let [[heading chart] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= ["**Q-Q Plot**"] heading))
            (is (= :kind/vega-lite (:kindly/kind (meta chart))))
            (is (map? chart))))))

    (testing "returns nil when KDE not found"
      (reset! kindly/accumulated [])
      (is (nil? (view/distribution-qq* :kindly {} {})))
      (is (nil? (kindly/flush))))))
