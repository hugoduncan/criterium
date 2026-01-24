(ns criterium.viewer.portal.autocorrelation-test
  ;; Tests multimethod registration, data processing, and output formatting
  ;; for autocorrelation analysis views in portal viewer.
  ;;
  ;; Autocorrelation views display ACF plots, classification assessments,
  ;; and effective sample size statistics.
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.collector.metrics :as metrics]
   [criterium.view :as view]
   [criterium.viewer.portal.autocorrelation]
   [criterium.viewer.portal.core :as portal.core]))

(deftest autocorrelation-portal-test
  ;; Tests autocorrelation* multimethod (no-op for portal, uses acf-plot instead)
  (testing "autocorrelation*"
    (testing "returns nil for portal viewer"
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            data-map {:autocorrelation
                      {:type :criterium/autocorrelation
                       :metrics-defs metrics-defs
                       :source-id :samples
                       :autocorrelation
                       {[:elapsed-time]
                        {:acf {1 0.12 2 0.08}
                         :lag-1 {:value 0.12 :severity :minor}
                         :effective-sample-size {:n-original 200
                                                 :n-effective 157
                                                 :ratio 0.785}}}}}]
        (is (nil? (view/autocorrelation* :portal {} data-map)))))))

(deftest acf-plot-portal-test
  ;; Tests acf-plot* multimethod for rendering ACF charts in Portal
  (testing "acf-plot*"
    (testing "produces vega-lite spec when data exists"
      (let [tapped (atom [])
            metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            data-map {:autocorrelation
                      {:type :criterium/autocorrelation
                       :metrics-defs metrics-defs
                       :source-id :samples
                       :autocorrelation
                       {[:elapsed-time]
                        {:acf {1 0.12 2 0.08 3 0.05}
                         :lag-1 {:value 0.12 :severity :minor}
                         :effective-sample-size {:n-original 200
                                                 :n-effective 157
                                                 :ratio 0.785}}}}}]
        (with-redefs [portal.core/heading #(swap! tapped conj {:heading %})
                      portal.core/portal-vega-lite #(swap! tapped conj {:vega-lite %})]
          (view/acf-plot* :portal {} data-map))
        (is (= 2 (count @tapped)))
        (is (= {:heading "Autocorrelation: Elapsed Time"} (first @tapped)))
        (is (contains? (second @tapped) :vega-lite))
        (let [spec (:vega-lite (second @tapped))]
          (is (contains? spec :layer))
          (is (contains? spec :title)))))

    (testing "uses custom autocorrelation-id"
      (let [tapped (atom [])
            metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            data-map {:my-acf
                      {:type :criterium/autocorrelation
                       :metrics-defs metrics-defs
                       :source-id :samples
                       :autocorrelation
                       {[:elapsed-time]
                        {:acf {1 0.08}
                         :lag-1 {:value 0.08 :severity :none}
                         :effective-sample-size {:n-original 100
                                                 :n-effective 95
                                                 :ratio 0.95}}}}}]
        (with-redefs [portal.core/heading #(swap! tapped conj {:heading %})
                      portal.core/portal-vega-lite #(swap! tapped conj {:vega-lite %})]
          (view/acf-plot* :portal {:autocorrelation-id :my-acf} data-map))
        (is (= 2 (count @tapped)))))

    (testing "handles missing data gracefully"
      (let [tapped (atom [])]
        (with-redefs [portal.core/heading #(swap! tapped conj {:heading %})
                      portal.core/portal-vega-lite #(swap! tapped conj {:vega-lite %})]
          (view/acf-plot* :portal {} {}))
        (is (empty? @tapped))))))

(deftest autocorrelation-classification-portal-test
  ;; Tests autocorrelation-classification* multimethod for displaying
  ;; sample independence assessment tables in Portal
  (testing "autocorrelation-classification*"
    (testing "produces table for warning classification"
      (let [tapped (atom [])
            metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            data-map {:autocorrelation-classification
                      {:type :criterium/autocorrelation-classification
                       :metrics-defs metrics-defs
                       :source-id :autocorrelation
                       :classification-data
                       {[:elapsed-time]
                        {:ljung-box {:q-statistic 35.0 :df 20 :p-value 0.02}
                         :classification :warning
                         :pattern :transient-effects
                         :detected-period nil}}}
                      :autocorrelation
                      {:type :criterium/autocorrelation
                       :metrics-defs metrics-defs
                       :autocorrelation
                       {[:elapsed-time]
                        {:acf {1 0.25}
                         :lag-1 {:value 0.25 :severity :moderate}
                         :anomalous-lags [1]
                         :lag-severities {1 :moderate}}}}}]
        (with-redefs [portal.core/heading #(swap! tapped conj {:heading %})
                      portal.core/portal-table #(swap! tapped conj {:table %})]
          (view/autocorrelation-classification* :portal {} data-map))
        (is (= 2 (count @tapped)))
        (is (= {:heading "Sample Independence Classification"} (first @tapped)))
        (let [table (:table (second @tapped))]
          (is (some #(= "Assessment" (:metric %)) table))
          (is (some #(= "Warning" (:value %)) table))
          (is (some #(= "Pattern" (:metric %)) table)))))

    (testing "does not produce output for :pass classification"
      (let [tapped (atom [])
            metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            data-map {:autocorrelation-classification
                      {:type :criterium/autocorrelation-classification
                       :metrics-defs metrics-defs
                       :source-id :autocorrelation
                       :classification-data
                       {[:elapsed-time]
                        {:ljung-box {:q-statistic 12.0 :df 20 :p-value 0.85}
                         :classification :pass
                         :pattern :clean
                         :detected-period nil}}}
                      :autocorrelation
                      {:type :criterium/autocorrelation
                       :metrics-defs metrics-defs
                       :autocorrelation
                       {[:elapsed-time]
                        {:acf {1 0.05}
                         :lag-1 {:value 0.05 :severity :none}}}}}]
        (with-redefs [portal.core/heading #(swap! tapped conj {:heading %})
                      portal.core/portal-table #(swap! tapped conj {:table %})]
          (view/autocorrelation-classification* :portal {} data-map))
        (is (empty? @tapped))))

    (testing "handles missing data gracefully"
      (let [tapped (atom [])]
        (with-redefs [portal.core/heading #(swap! tapped conj {:heading %})
                      portal.core/portal-table #(swap! tapped conj {:table %})]
          (view/autocorrelation-classification* :portal {} {}))
        (is (empty? @tapped))))))

(deftest effective-sample-size-portal-test
  ;; Tests effective-sample-size* multimethod for displaying ESS statistics
  (testing "effective-sample-size*"
    (testing "produces table when ci-inflation-factor > 1"
      (let [tapped (atom [])
            metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            data-map {:effective-sample-size
                      {:type :criterium/effective-sample-size
                       :metrics-defs metrics-defs
                       :source-id :autocorrelation
                       :effective-sample-size-data
                       {[:elapsed-time]
                        {:effective-sample-size {:n-original 200
                                                 :n-effective 120
                                                 :ratio 0.60}
                         :ci-inflation-factor 1.67}}}
                      :autocorrelation
                      {:type :criterium/autocorrelation
                       :metrics-defs metrics-defs
                       :autocorrelation
                       {[:elapsed-time]
                        {:lag-1 {:value 0.25 :severity :moderate}}}}}]
        (with-redefs [portal.core/heading #(swap! tapped conj {:heading %})
                      portal.core/portal-table #(swap! tapped conj {:table %})]
          (view/effective-sample-size* :portal {} data-map))
        (is (= 2 (count @tapped)))
        (is (= {:heading "Effective Sample Size"} (first @tapped)))
        (let [table (:table (second @tapped))]
          (is (some #(= "Effective sample size" (:metric %)) table))
          (is (some #(= "CI inflation factor" (:metric %)) table)))))

    (testing "does not produce output when ci-inflation-factor = 1"
      (let [tapped (atom [])
            metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            data-map {:effective-sample-size
                      {:type :criterium/effective-sample-size
                       :metrics-defs metrics-defs
                       :source-id :autocorrelation
                       :effective-sample-size-data
                       {[:elapsed-time]
                        {:effective-sample-size {:n-original 200
                                                 :n-effective 200
                                                 :ratio 1.0}
                         :ci-inflation-factor 1.0}}}
                      :autocorrelation
                      {:type :criterium/autocorrelation
                       :metrics-defs metrics-defs
                       :autocorrelation
                       {[:elapsed-time]
                        {:lag-1 {:value 0.0 :severity :none}}}}}]
        (with-redefs [portal.core/heading #(swap! tapped conj {:heading %})
                      portal.core/portal-table #(swap! tapped conj {:table %})]
          (view/effective-sample-size* :portal {} data-map))
        (is (empty? @tapped))))

    (testing "handles missing data gracefully"
      (let [tapped (atom [])]
        (with-redefs [portal.core/heading #(swap! tapped conj {:heading %})
                      portal.core/portal-table #(swap! tapped conj {:table %})]
          (view/effective-sample-size* :portal {} {}))
        (is (empty? @tapped))))))
