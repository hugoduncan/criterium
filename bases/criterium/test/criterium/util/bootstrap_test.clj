(ns criterium.util.bootstrap-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse-test :refer [metrics-samples]]
   [criterium.collect-plan :as collect-plan]
   [criterium.test-utils :refer [test-max-error]]
   [criterium.util.bootstrap :as bootstrap]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have]]
   [criterium.util.sampled-stats-test :as sampled-stats-test]
   [criterium.util.stats :as stats]
   [criterium.util.well :as well]
   [stats.interface :as stats-interface]))

(deftest bootstrap-estimate-test
  (is (= [1.0 0.0 [1.0 1.0]]
         (bootstrap/bootstrap-estimate (take 20 (repeatedly (constantly 1))))))
  (is (= [2.0 0.0 [2.0 2.0]]
         (bootstrap/bootstrap-estimate (take 20 (repeatedly (constantly 2))))))
  ;; (is (= [1/2 0.26315789473684204 [-0.5054587850434509 1.5054587850434509]]
  ;;        (bootstrap-estimate (take 20 (cycle [0 1])))))
  (let [[m s [l u]] (bootstrap/bootstrap-estimate
                     (take 1000000 (repeatedly rand)))]
    (is (test-max-error 0.5 m 1e-2))
    (is (test-max-error 0.0 l 0.2))
    (is (test-max-error 1.0 u 0.2))
    (is (test-max-error 0.0833 s 0.2))))

(deftest bootstrap-estimate-scale-test
  (is (= [1e-9 [1e-8 1e-8]]
         (bootstrap/scale-bootstrap-estimate
          (bootstrap/->BcaEstimate 1 [{:value 10} {:value 10}])
          1e-9))))

(deftest bootstrap-test
  (is (= [1.0 0.0 [1.0 1.0]]
         (bootstrap/bootstrap (take 20 (repeatedly (constantly 1)))
                              stats/mean
                              100
                              well/well-rng-1024a)))
  (is (=  [[1.0 0.0 [1.0 1.0]] [0.0 0.0 [0.0 0.0]]]
          (bootstrap/bootstrap (take 20 (repeatedly (constantly 1)))
                               (juxt stats/mean stats/variance)
                               100
                               well/well-rng-1024a))))

(deftest bootstrap-bca-test
  (let [ci 0.95]
    (is (= (bootstrap/map->BcaEstimate
            {:point-estimate     1.0
             :estimate-quantiles [{:value 1.0 :alpha 0.95}
                                  {:value 1.0 :alpha (- 1 0.95)}]})
           (bootstrap/bootstrap-bca (take 20 (repeatedly (constantly 1)))
                                    stats/mean
                                    100
                                    [0.5 ci (- 1.0 ci)]
                                    well/well-rng-1024a)))
    (is (=  [(bootstrap/map->BcaEstimate
              {:point-estimate     1.0
               :estimate-quantiles [{:value 1.0 :alpha 0.95}
                                    {:value 1.0 :alpha (- 1 0.95)}]})
             (bootstrap/map->BcaEstimate
              {:point-estimate     0.0
               :estimate-quantiles [{:value 0.0 :alpha 0.95}
                                    {:value 0.0 :alpha (- 1 0.95)}]})]
            (bootstrap/bootstrap-bca (take 20 (repeatedly (constantly 1)))
                                     (juxt stats/mean stats/variance)
                                     100
                                     [0.5 ci (- 1.0 ci)]
                                     well/well-rng-1024a)))))

#_(comment
    (let [f (fn [n] (take n (repeatedly rand)))]
      (dissoc (criterium.bench/measure (f 1000000)) :expr-value))

    (let [f (fn [n] (take n (criterium.util.well/well-rng-1024a)))]
      (dissoc (criterium.bench/measure (f 1000000)) :expr-value))

    (criterium.bench/time (bootstrap-estimate (take 1000000 (repeatedly rand))))

    (let [f (fn [n] (bootstrap-estimate (take n (repeatedly rand))))]
      (double (/ (criterium.toolkit/elapsed-time
                  (-> (criterium.bench/measure
                       (f 1000000))
                      (dissoc :expr-value)))
                 (double units/MILLISEC-NS))))

    (def m (criterium.arg-gen/for-all
            [v (clojure.test.check.generators/vector
                (clojure.test.check.generators/double* {:inifinte? false :NaN? false
                                                        :min       0     :max  1})
                1000000)]
            (bootstrap-estimate v)))

    (dissoc (criterium.measure/measure m {}) :state))

(deftest bootstrap-stats-for-test
  ;; Tests for bootstrap-stats-for now pass samples twice (filtered, unfiltered)
  ;; since the function signature changed to support robust-stats option.
  (testing "constant input"
    (let [samples (mapv double (repeat 100 1))
          stats   (bootstrap/bootstrap-stats-for
                   samples samples
                   {:estimate-quantiles [0.025 0.975] :quantiles [0.99]}
                   sampled-stats-test/identity-transforms)
          result  (bootstrap/->BcaEstimate
                   1.0
                   [{:value 1.0 :alpha 0.025}
                    {:value 1.0 :alpha 0.975}])]
      (is (= result (-> stats :mean)))
      (is (= result (-> stats :quantiles (get 0.25))))
      (is (= result (-> stats :quantiles (get 0.75))))
      (is (= result (-> stats :quantiles (get 0.99))))
      (is (= (bootstrap/->BcaEstimate
              0.0
              [{:value 0.0 :alpha 0.025}
               {:value 0.0 :alpha 0.975}])
             (-> stats :variance)))))

  (testing "sequential input"
    (let [samples (mapv double (range 101))
          stats   (bootstrap/bootstrap-stats-for
                   samples samples
                   {:estimate-quantiles [0.025 0.975] :quantiles [0.99]}
                   sampled-stats-test/identity-transforms)]
      (let [{m                       :point-estimate
             [{l :value} {u :value}] :estimate-quantiles}
            (-> stats :mean)]
        (is (< l m u))
        (is (< l 50 u)))
      (let [{m                       :point-estimate
             [{l :value} {u :value}] :estimate-quantiles}
            (-> stats :variance)]
        (is (< l m u))
        (is (< l 858.5 u)))))

  (testing "reverse sequential input"
    (let [samples (mapv double (reverse (range 101)))
          stats   (bootstrap/bootstrap-stats-for
                   samples samples
                   {:estimate-quantiles [0.025 0.975] :quantiles [0.99]}
                   sampled-stats-test/identity-transforms)]
      (let [{m                       :point-estimate
             [{l :value} {u :value}] :estimate-quantiles}
            (-> stats :mean)]
        (is (< l m u))
        (is (< l 50 u)))
      (let [{m                       :point-estimate
             [{l :value} {u :value}] :estimate-quantiles}
            (-> stats :variance)]
        (is (< l m u))
        (is (< l 858.5 u))))))

;; Test minimum sample size check for bootstrap reliability
(deftest bootstrap-stats-for-min-samples-test
  (testing "bootstrap-stats-for"
    (testing "when sample count is at or above default threshold"
      (testing "does not set :low-sample-count?"
        (let [samples (mapv double (range 30))
              stats (bootstrap/bootstrap-stats-for
                     samples samples
                     {:estimate-quantiles [0.025 0.975] :quantiles [0.99]}
                     sampled-stats-test/identity-transforms)]
          (is (nil? (:low-sample-count? stats))))))

    (testing "when sample count is below default threshold"
      (testing "sets :low-sample-count? true"
        (let [samples (mapv double (range 20))
              ;; First run suppresses stdout, second captures return value
              _ (with-out-str
                  (bootstrap/bootstrap-stats-for
                   samples samples
                   {:estimate-quantiles [0.025 0.975] :quantiles [0.99]}
                   sampled-stats-test/identity-transforms))
              result (bootstrap/bootstrap-stats-for
                      samples samples
                      {:estimate-quantiles [0.025 0.975] :quantiles [0.99]}
                      sampled-stats-test/identity-transforms)]
          (is (true? (:low-sample-count? result)))))

      (testing "prints warning"
        (let [samples (mapv double (range 20))
              output (with-out-str
                       (bootstrap/bootstrap-stats-for
                        samples samples
                        {:estimate-quantiles [0.025 0.975] :quantiles [0.99]}
                        sampled-stats-test/identity-transforms))]
          (is (re-find #"Warning.*bootstrap sample count.*20.*below minimum.*30"
                       output)))))

    (testing "when custom :min-samples is specified"
      (testing "uses custom threshold"
        (let [samples (mapv double (range 15))
              ;; With min-samples=10, 15 samples should be fine
              result (bootstrap/bootstrap-stats-for
                      samples samples
                      {:estimate-quantiles [0.025 0.975]
                       :quantiles [0.99]
                       :min-samples 10}
                      sampled-stats-test/identity-transforms)]
          (is (nil? (:low-sample-count? result)))))

      (testing "warns when below custom threshold"
        (let [samples (mapv double (range 5))
              output (with-out-str
                       (bootstrap/bootstrap-stats-for
                        samples samples
                        {:estimate-quantiles [0.025 0.975]
                         :quantiles [0.99]
                         :min-samples 10}
                        sampled-stats-test/identity-transforms))]
          (is (re-find #"Warning.*5.*below minimum.*10" output)))))))

;; todo add helpers for constant samples
;; integration test of time with bootstrap
(defn sample-values
  "Generate batched samples with the given mean and standard deviation."
  [batch-size num-samples random-seed mean sigma]
  (let [batch-size (long batch-size)
        values     (->> (sampled-stats-test/random-values
                         random-seed mean sigma)
                        (take num-samples)
                        vec)]
    (mapv #(* (double %) batch-size) values)))

(deftest analyse-bootstrap-test
  (let [batch-size     100
        num-samples    1000
        samples        {[:v] (sample-values batch-size num-samples 123 10.0 1.0)}
        metric-samples (assoc
                        (metrics-samples samples batch-size)
                        :metrics-defs
                        {:v
                         {:type   :quantitative
                          :values [{:path      [:v]
                                    :type      :quantitative
                                    :dimension :time
                                    :scale     1
                                    :label     "v"}]}})
        result         ((bootstrap/bootstrap-stats
                         {:quantiles          [0.99]
                          :estimate-quantiles [0.025 0.975]
                          :bootstrap-size     100})
                        {:samples metric-samples})
        point          (have
                        (-> result
                            :bootstrap-stats
                            util/bootstrap
                            :v
                            :mean
                            :point-estimate))]
    (is (test-max-error 10.0 point 0.1 "mean")
        (str "Value: " point))))

;; Test that outlier filtering removes outlier samples before bootstrap resampling.
;; This prevents extreme values from propagating through bootstrap resamples.
(deftest bootstrap-stats-outlier-filtering-test
  (testing "bootstrap-stats"
    (testing "filters outliers when outliers-id is provided"
      (let [batch-size 100
            num-samples 100
            ;; Create samples with extreme outliers at indices 0 and 1
            ;; Normal values around 10.0 * batch-size = 1000, outliers at 1000000.0
            base-samples (sample-values batch-size (- num-samples 2) 123 10.0 1.0)
            outlier-samples (into [1000000.0 1000000.0] base-samples)
            samples {[:v] outlier-samples}
            metric-samples (assoc
                            (metrics-samples samples batch-size)
                            :metrics-defs
                            {:v
                             {:type :quantitative
                              :values [{:path [:v]
                                        :type :quantitative
                                        :dimension :time
                                        :scale 1
                                        :label "v"}]}})
            ;; Create outliers map marking indices 0 and 1 as outliers
            outliers-map {:type :criterium/outliers
                          :outliers {:v {:outliers {0 :high-severe
                                                    1 :high-severe}
                                         :outlier-counts {:low-severe 0
                                                          :low-mild 0
                                                          :high-mild 0
                                                          :high-severe 2}}}
                          :metrics-defs {:v
                                         {:type :quantitative
                                          :values [{:path [:v]
                                                    :type :quantitative
                                                    :dimension :time
                                                    :scale 1
                                                    :label "v"}]}}
                          :num-samples num-samples
                          :source-id :samples
                          :quantiles-id :quantiles
                          :transform collect-plan/identity-transforms}
            ;; Test without outlier filtering - mean will be affected by outliers
            ;; Put outliers data under a different key so default :outliers won't find it
            result-with-outliers
            ((bootstrap/bootstrap-stats
              {:quantiles [0.99]
               :estimate-quantiles [0.025 0.975]
               :bootstrap-size 100
               :outliers-id :my-outliers}) ; Use a key that doesn't exist
             {:samples metric-samples})   ; No outliers data
            ;; Test with outlier filtering - outliers should be removed
            result-without-outliers
            ((bootstrap/bootstrap-stats
              {:quantiles [0.99]
               :estimate-quantiles [0.025 0.975]
               :bootstrap-size 100
               :outliers-id :outliers})
             {:samples metric-samples
              :outliers outliers-map})
            mean-with (-> result-with-outliers
                          :bootstrap-stats
                          util/bootstrap
                          :v
                          :mean
                          :point-estimate)
            mean-without (-> result-without-outliers
                             :bootstrap-stats
                             util/bootstrap
                             :v
                             :mean
                             :point-estimate)]
        ;; Mean without outliers should be much closer to 10.0
        (is (< mean-without 20.0)
            (str "Mean without outliers should be close to 10: " mean-without))
        ;; Mean with outliers (no filtering) will be significantly higher
        (is (> mean-with 100.0)
            (str "Mean with outliers should be affected by extremes: " mean-with))
        ;; The filtered result should record the outliers-id used
        (is (= :outliers (-> result-without-outliers :bootstrap-stats :outliers-id)))
        ;; No outliers data available, so outliers-id is nil
        (is (nil? (-> result-with-outliers :bootstrap-stats :outliers-id)))))))

;; Verifies that all quantiles computed by bootstrap-stats-for share the same
;; bootstrap resamples. This is critical for statistical validity - if quantiles
;; were bootstrapped separately, they would use different resamples and lose
;; the correlation structure of the data.
(deftest bootstrap-quantiles-share-resamples-test
  (testing "bootstrap-stats-for"
    (testing "computes all quantiles from the same resamples"
      ;; Track how many times stats-fn is invoked during bootstrap-sample.
      ;; If all quantiles share resamples, the combined stats-fn should be
      ;; called exactly bootstrap-size times (once per resample).
      (let [invocation-count  (atom 0)
            bootstrap-size    50
            samples           (mapv double (range 101))
            ;; Wrap stats-fn to track invocations
            original-stats-fn stats-interface/stats-fn
            tracking-stats-fn (fn [fs]
                                (let [combined (original-stats-fn fs)]
                                  (fn [vs]
                                    (swap! invocation-count inc)
                                    (combined vs))))]
        (with-redefs [stats-interface/stats-fn tracking-stats-fn]
          (bootstrap/bootstrap-stats-for
           samples samples
           {:estimate-quantiles [0.025 0.975]
            :quantiles          [0.99]
            :bootstrap-size     bootstrap-size}
           sampled-stats-test/identity-transforms))
        ;; The combined stats-fn should be called:
        ;; - Once for the original estimate
        ;; - Once per bootstrap resample (bootstrap-size times)
        ;; - Once per jackknife sample (n times, where n = sample count)
        ;; Total = 1 + bootstrap-size + n
        (let [expected-calls (+ 1 bootstrap-size (count samples))]
          (is (= expected-calls @invocation-count)
              (str "Expected " expected-calls " stats-fn calls "
                   "(1 estimate + " bootstrap-size " bootstrap + "
                   (count samples) " jackknife), got " @invocation-count
                   ". If higher, quantiles may be bootstrapped separately.")))))))

;; Tests for :robust-stats option - allows computing some stats from unfiltered
;; data (robust stats like median) while others use filtered data (mean).
(deftest bootstrap-stats-robust-stats-test
  ;; Test that robust-stats option enables selective outlier filtering.
  ;; When :robust-stats is specified, those stats use unfiltered data
  ;; while other stats use filtered data.
  (testing "bootstrap-stats-for"
    (testing "with :robust-stats [:quantiles]"
      (testing "computes quantiles from unfiltered data, mean from filtered"
        ;; Create samples where filtering makes a big difference
        ;; Unfiltered: [1000000, 1000000, 10, 10, 10, ...] - mean ~20000
        ;; Filtered: [10, 10, 10, ...] - mean ~10
        (let [base-samples (mapv double (repeat 98 10.0))
              outlier-samples (into [1000000.0 1000000.0] base-samples)
              ;; When samples differ, robust stats use unfiltered
              result (bootstrap/bootstrap-stats-for
                      base-samples        ; filtered (without outliers)
                      outlier-samples     ; unfiltered (with outliers)
                      {:estimate-quantiles [0.025 0.975]
                       :quantiles [0.99]
                       :robust-stats [:quantiles]
                       :bootstrap-size 50}
                      sampled-stats-test/identity-transforms)
              ;; Mean should be computed from filtered data (close to 10)
              mean-estimate (-> result :mean :point-estimate)
              ;; Median (0.5 quantile) should be computed from unfiltered data
              ;; With outliers included, median is still ~10 (robust)
              median-estimate (-> result :quantiles (get 0.5) :point-estimate)]
          ;; Mean from filtered data should be close to 10
          (is (< mean-estimate 20.0)
              (str "Mean should be from filtered data (close to 10): " mean-estimate))
          ;; Median from unfiltered data should also be ~10 (robust to outliers)
          (is (< 5.0 median-estimate 20.0)
              (str "Median should be robust even with outliers: " median-estimate)))))

    (testing "without :robust-stats (same data)"
      (testing "uses single-pass computation"
        ;; When filtered and unfiltered are the same, should use single pass
        (let [samples (mapv double (range 101))
              result (bootstrap/bootstrap-stats-for
                      samples samples
                      {:estimate-quantiles [0.025 0.975]
                       :quantiles [0.99]
                       :robust-stats [:quantiles]
                       :bootstrap-size 50}
                      sampled-stats-test/identity-transforms)]
          ;; Should have all stats computed
          (is (some? (-> result :mean)))
          (is (some? (-> result :variance)))
          (is (some? (-> result :quantiles (get 0.5)))))))

    (testing "with :robust-stats [:mean :variance]"
      (testing "computes mean and variance from unfiltered data"
        (let [base-samples (mapv double (repeat 98 10.0))
              outlier-samples (into [1000000.0 1000000.0] base-samples)
              result (bootstrap/bootstrap-stats-for
                      base-samples        ; filtered
                      outlier-samples     ; unfiltered
                      {:estimate-quantiles [0.025 0.975]
                       :quantiles [0.99]
                       :robust-stats [:mean :variance]
                       :bootstrap-size 50}
                      sampled-stats-test/identity-transforms)
              mean-estimate (-> result :mean :point-estimate)]
          ;; Mean should be computed from unfiltered data (affected by outliers)
          (is (> mean-estimate 1000.0)
              (str "Mean should be from unfiltered data (high due to outliers): "
                   mean-estimate)))))

    (testing "with specific quantile in :robust-stats"
      (testing "only that quantile uses unfiltered data"
        ;; Test that we can specify individual quantiles as robust
        (let [base-samples (mapv double (repeat 98 10.0))
              outlier-samples (into [1000000.0 1000000.0] base-samples)
              result (bootstrap/bootstrap-stats-for
                      base-samples
                      outlier-samples
                      {:estimate-quantiles [0.025 0.975]
                       :quantiles [0.99]
                       :robust-stats [0.5]  ; Only median is robust
                       :bootstrap-size 50}
                      sampled-stats-test/identity-transforms)
              median (-> result :quantiles (get 0.5) :point-estimate)
              q25 (-> result :quantiles (get 0.25) :point-estimate)]
          ;; Both should be ~10 in this case, but from different data sources
          (is (< 5.0 median 20.0)
              (str "Median should be from unfiltered data: " median))
          (is (< 5.0 q25 20.0)
              (str "Q25 should be from filtered data: " q25)))))))

;; Integration test: verify robust-stats works through the full bootstrap-stats
;; analysis function pipeline.
(deftest bootstrap-stats-robust-stats-integration-test
  ;; Test that :robust-stats works through the full analysis pipeline.
  (testing "bootstrap-stats analysis function"
    (testing "with :robust-stats [:quantiles]"
      (let [batch-size 100
            num-samples 100
            ;; Normal values around 10.0 * batch-size = 1000
            base-samples (sample-values batch-size (- num-samples 2) 123 10.0 1.0)
            ;; Add outliers at indices 0 and 1
            outlier-samples (into [1000000.0 1000000.0] base-samples)
            samples {[:v] outlier-samples}
            metric-samples (assoc
                            (metrics-samples samples batch-size)
                            :metrics-defs
                            {:v
                             {:type :quantitative
                              :values [{:path [:v]
                                        :type :quantitative
                                        :dimension :time
                                        :scale 1
                                        :label "v"}]}})
            ;; Create outliers map marking indices 0 and 1 as outliers
            outliers-map {:type :criterium/outliers
                          :outliers {:v {:outliers {0 :high-severe
                                                    1 :high-severe}
                                         :outlier-counts {:low-severe 0
                                                          :low-mild 0
                                                          :high-mild 0
                                                          :high-severe 2}}}
                          :metrics-defs {:v
                                         {:type :quantitative
                                          :values [{:path [:v]
                                                    :type :quantitative
                                                    :dimension :time
                                                    :scale 1
                                                    :label "v"}]}}
                          :num-samples num-samples
                          :source-id :samples
                          :quantiles-id :quantiles
                          :transform collect-plan/identity-transforms}
            result ((bootstrap/bootstrap-stats
                     {:quantiles [0.99]
                      :estimate-quantiles [0.025 0.975]
                      :bootstrap-size 100
                      :outliers-id :outliers
                      :robust-stats [:quantiles]})
                    {:samples metric-samples
                     :outliers outliers-map})
            mean-estimate (-> result
                              :bootstrap-stats
                              util/bootstrap
                              :v
                              :mean
                              :point-estimate)
            median-estimate (-> result
                                :bootstrap-stats
                                util/bootstrap
                                :v
                                :quantiles
                                (get 0.5)
                                :point-estimate)]
        ;; Mean should be computed from filtered data (close to 10)
        (is (< mean-estimate 20.0)
            (str "Mean should use filtered data (close to 10): " mean-estimate))
        ;; Median is robust - even with unfiltered data, should be ~10
        (is (< 5.0 median-estimate 20.0)
            (str "Median should be robust to outliers: " median-estimate))))))
