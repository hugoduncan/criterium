(ns criterium.util.bootstrap-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse-test :refer [metrics-samples]]
   [criterium.array :as arr]
   [criterium.collect-plan :as collect-plan]
   [criterium.random.interface :as random]
   [criterium.stats.bootstrap :as bootstrap-stats]
   [criterium.stats.core :as stats]
   [criterium.test-utils :refer [test-max-error]]
   [criterium.util.bootstrap :as bootstrap]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have]]
   [criterium.util.sampled-stats-test :as sampled-stats-test]))

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
                              random/make-well-rng-1024a)))
  (is (=  [[1.0 0.0 [1.0 1.0]] [0.0 0.0 [0.0 0.0]]]
          (bootstrap/bootstrap (take 20 (repeatedly (constantly 1)))
                               (juxt stats/mean stats/variance)
                               100
                               random/make-well-rng-1024a))))

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
                                    random/make-well-rng-1024a)))
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
                                     random/make-well-rng-1024a)))))

#_(comment
    (let [f (fn [n] (take n (repeatedly rand)))]
      (dissoc (criterium.bench/measure (f 1000000)) :expr-value))

    (let [rng (random/make-well-rng-1024a)
          f   (fn [n] (vec (repeatedly n #(random/next-double! rng))))]
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
                (clojure.test.check.generators/double*
                 {:inifinte? false
                  :NaN? false
                  :min       0
                  :max  1})
                1000000)]
            (bootstrap-estimate v)))

    (dissoc (criterium.measure/measure m {}) :state))

(deftest bootstrap-stats-for-test
  (testing "constant input"
    (let [samples (arr/->double-array (double-array (repeat 100 1)))
          stats   (bootstrap/bootstrap-stats-for
                   samples
                   {:estimate-quantiles [0.025 0.975] :quantiles [0.99]})
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
             (-> stats :variance)))
      (testing "includes skewness, kurtosis, and cv"
        (is (contains? stats :skewness))
        (is (contains? stats :kurtosis))
        (is (contains? stats :cv)))))

  (testing "sequential input"
    (let [samples (arr/->double-array (double-array (range 101)))
          stats   (bootstrap/bootstrap-stats-for
                   samples
                   {:estimate-quantiles [0.025 0.975] :quantiles [0.99]})]
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
    (let [samples (arr/->double-array (double-array (reverse (range 101))))
          stats   (bootstrap/bootstrap-stats-for
                   samples
                   {:estimate-quantiles [0.025 0.975] :quantiles [0.99]})]
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
        (let [samples (arr/->double-array (double-array (range 30)))
              stats (bootstrap/bootstrap-stats-for
                     samples
                     {:estimate-quantiles [0.025 0.975] :quantiles [0.99]})]
          (is (nil? (:low-sample-count? stats))))))

    (testing "when sample count is below default threshold"
      (testing "sets :low-sample-count? true"
        (let [samples (arr/->double-array (double-array (range 20)))
              ;; First run suppresses stdout, second captures return value
              _ (with-out-str
                  (bootstrap/bootstrap-stats-for
                   samples
                   {:estimate-quantiles [0.025 0.975] :quantiles [0.99]}))
              result (bootstrap/bootstrap-stats-for
                      samples
                      {:estimate-quantiles [0.025 0.975] :quantiles [0.99]})]
          (is (true? (:low-sample-count? result)))))

      (testing "prints warning"
        (let [samples (arr/->double-array (double-array (range 20)))
              output (with-out-str
                       (bootstrap/bootstrap-stats-for
                        samples
                        {:estimate-quantiles [0.025 0.975] :quantiles [0.99]}))]
          (is (re-find #"Warning.*bootstrap sample count.*20.*below minimum.*30"
                       output)))))

    (testing "when custom :min-samples is specified"
      (testing "uses custom threshold"
        (let [samples (arr/->double-array (double-array (range 15)))
              ;; With min-samples=10, 15 samples should be fine
              result (bootstrap/bootstrap-stats-for
                      samples
                      {:estimate-quantiles [0.025 0.975]
                       :quantiles [0.99]
                       :min-samples 10})]
          (is (nil? (:low-sample-count? result)))))

      (testing "warns when below custom threshold"
        (let [samples (arr/->double-array (double-array (range 5)))
              output (with-out-str
                       (bootstrap/bootstrap-stats-for
                        samples
                        {:estimate-quantiles [0.025 0.975]
                         :quantiles [0.99]
                         :min-samples 10}))]
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

(deftest ^:slow analyse-bootstrap-test
  ;; Tests that bootstrap-stats stores raw values and transforms are
  ;; applied when viewing via the source-id chain.
  (let [batch-size     100
        num-samples    1000
        samples        {[:v] (sample-values
                              batch-size
                              num-samples
                              123
                              10.0
                              1.0)}
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
        ;; Apply transforms when reading bootstrap result (as viewer would)
        transforms
        (util/get-transforms result :bootstrap-stats)
        raw-point
        (have
         (-> result
             :bootstrap-stats
             util/bootstrap
             :v
             :mean
             :point-estimate))
        point
        (util/transform-sample-> raw-point transforms)]
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
            ;; Apply transforms when reading results (as viewer would)
            transforms-with (util/get-transforms result-with-outliers :bootstrap-stats)
            transforms-without (util/get-transforms result-without-outliers :bootstrap-stats)
            raw-mean-with (-> result-with-outliers
                              :bootstrap-stats
                              util/bootstrap
                              :v
                              :mean
                              :point-estimate)
            raw-mean-without (-> result-without-outliers
                                 :bootstrap-stats
                                 util/bootstrap
                                 :v
                                 :mean
                                 :point-estimate)
            mean-with (util/transform-sample-> raw-mean-with transforms-with)
            mean-without (util/transform-sample-> raw-mean-without transforms-without)]
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

;;; ESS-adjusted confidence interval tests
;; When :ess-id is provided and effective sample size data exists,
;; bootstrap-stats should inflate CI widths and add :adjusted-estimate-quantiles.

(deftest bootstrap-stats-ess-adjustment-test
  (testing "bootstrap-stats"
    (testing "with :ess-id option"
      (let [batch-size 100
            num-samples 50
            ;; Create simple samples
            samples {[:v] (vec (range (* num-samples batch-size) 0 (- batch-size)))}
            metric-samples (assoc
                            (metrics-samples samples batch-size)
                            :metrics-defs
                            {:v {:type :quantitative
                                 :values [{:path [:v]
                                           :type :quantitative
                                           :dimension :time
                                           :scale 1
                                           :label "v"}]}})
            ;; Create effective-sample-size data with a known inflation factor
            ;; ci-inflation-factor of 2.0 means CIs should double in width
            ess-data {:type :criterium/effective-sample-size
                      :effective-sample-size-data {[:v] {:effective-sample-size {:n-original 50
                                                                                 :n-effective 12
                                                                                 :ratio 0.24}
                                                         :ci-inflation-factor 2.0}}
                      :metrics-defs {:v {:type :quantitative
                                         :values [{:path [:v]}]}}}]

        (testing "adds :adjusted-estimate-quantiles when ess data is present"
          (let [result ((bootstrap/bootstrap-stats
                         {:quantiles [0.99]
                          :estimate-quantiles [0.025 0.975]
                          :bootstrap-size 50
                          :ess-id :effective-sample-size})
                        {:samples metric-samples
                         :effective-sample-size ess-data})
                mean-stats (-> result :bootstrap-stats util/bootstrap :v :mean)]
            ;; Should have both original and adjusted quantiles
            (is (some? (:estimate-quantiles mean-stats)))
            (is (some? (:adjusted-estimate-quantiles mean-stats)))
            ;; Both should have same number of entries
            (is (= (count (:estimate-quantiles mean-stats))
                   (count (:adjusted-estimate-quantiles mean-stats))))
            ;; Adjusted should be wider than original
            (let [orig-lower (double (-> mean-stats :estimate-quantiles first :value))
                  orig-upper (double (-> mean-stats :estimate-quantiles second :value))
                  adj-lower (-> mean-stats :adjusted-estimate-quantiles first :value)
                  adj-upper (-> mean-stats :adjusted-estimate-quantiles second :value)
                  point (double (:point-estimate mean-stats))]
              ;; Adjusted CI should be wider: adj-lower < orig-lower, adj-upper > orig-upper
              (is (<= adj-lower orig-lower)
                  (str "Adjusted lower bound " adj-lower " should be <= original " orig-lower))
              (is (>= adj-upper orig-upper)
                  (str "Adjusted upper bound " adj-upper " should be >= original " orig-upper))
              ;; Check inflation factor is applied correctly
              ;; adj-lower = p - 2.0 * (p - orig-lower)
              ;; adj-upper = p + 2.0 * (orig-upper - p)
              (is (test-max-error
                   (- point (* 2.0 (- point orig-lower))) adj-lower 1e-6)
                  "Lower bound should be inflated by factor 2.0")
              (is (test-max-error
                   (+ point (* 2.0 (- orig-upper point))) adj-upper 1e-6)
                  "Upper bound should be inflated by factor 2.0"))))

        (testing "includes :ess-id in result when ess data used"
          (let [result ((bootstrap/bootstrap-stats
                         {:quantiles [0.99]
                          :estimate-quantiles [0.025 0.975]
                          :ess-id :effective-sample-size})
                        {:samples metric-samples
                         :effective-sample-size ess-data})]
            (is (= :effective-sample-size (-> result :bootstrap-stats :ess-id)))))

        (testing "does not add adjusted CIs when :ess-id not provided"
          (let [result ((bootstrap/bootstrap-stats
                         {:quantiles [0.99]
                          :estimate-quantiles [0.025 0.975]
                          :bootstrap-size 50})
                        {:samples metric-samples
                         :effective-sample-size ess-data})
                mean-stats (-> result :bootstrap-stats util/bootstrap :v :mean)]
            (is (some? (:estimate-quantiles mean-stats)))
            (is (nil? (:adjusted-estimate-quantiles mean-stats)))
            (is (nil? (-> result :bootstrap-stats :ess-id)))))

        (testing "does not add adjusted CIs when ess data missing"
          (let [result ((bootstrap/bootstrap-stats
                         {:quantiles [0.99]
                          :estimate-quantiles [0.025 0.975]
                          :bootstrap-size 50
                          :ess-id :effective-sample-size})
                        {:samples metric-samples})
                mean-stats (-> result :bootstrap-stats util/bootstrap :v :mean)]
            (is (some? (:estimate-quantiles mean-stats)))
            (is (nil? (:adjusted-estimate-quantiles mean-stats)))
            ;; :ess-id should be nil when no data found
            (is (nil? (-> result :bootstrap-stats :ess-id)))))

        (testing "does not adjust CIs when inflation factor is 1.0"
          (let [no-inflation-ess {:type :criterium/effective-sample-size
                                  :effective-sample-size-data {[:v] {:ci-inflation-factor 1.0}}
                                  :metrics-defs {:v {:type :quantitative
                                                     :values [{:path [:v]}]}}}
                result ((bootstrap/bootstrap-stats
                         {:quantiles [0.99]
                          :estimate-quantiles [0.025 0.975]
                          :bootstrap-size 50
                          :ess-id :effective-sample-size})
                        {:samples metric-samples
                         :effective-sample-size no-inflation-ess})
                mean-stats (-> result :bootstrap-stats util/bootstrap :v :mean)]
            ;; Should not have adjusted quantiles when inflation is 1.0
            (is (nil? (:adjusted-estimate-quantiles mean-stats)))))

        (testing "adjusts :quantiles in addition to top-level stats"
          (let [result ((bootstrap/bootstrap-stats
                         {:quantiles [0.99]
                          :estimate-quantiles [0.025 0.975]
                          :bootstrap-size 50
                          :ess-id :effective-sample-size})
                        {:samples metric-samples
                         :effective-sample-size ess-data})
                q50-stats (-> result :bootstrap-stats util/bootstrap :v :quantiles (get 0.5))]
            ;; Quantile stats should also have adjusted CIs
            (is (some? (:estimate-quantiles q50-stats)))
            (is (some? (:adjusted-estimate-quantiles q50-stats)))))))))

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
            samples           (arr/->double-array (double-array (range 101)))
            num-samples       (arr/length samples)
            ;; Wrap stats-fn to track invocations
            original-stats-fn bootstrap-stats/stats-fn
            tracking-stats-fn (fn [fs]
                                (let [combined (original-stats-fn fs)]
                                  (fn [vs]
                                    (swap! invocation-count inc)
                                    (combined vs))))]
        (with-redefs [bootstrap-stats/stats-fn tracking-stats-fn]
          (bootstrap/bootstrap-stats-for
           samples
           {:estimate-quantiles [0.025 0.975]
            :quantiles          [0.99]
            :bootstrap-size     bootstrap-size}))
        ;; The combined stats-fn should be called:
        ;; - Once for the original estimate
        ;; - Once per bootstrap resample (bootstrap-size times)
        ;; - Once per jackknife sample (n times, where n = sample count)
        ;; Total = 1 + bootstrap-size + n
        (let [expected-calls (+ 1 bootstrap-size num-samples)]
          (is (= expected-calls @invocation-count)
              (str "Expected " expected-calls " stats-fn calls "
                   "(1 estimate + " bootstrap-size " bootstrap + "
                   num-samples " jackknife), got " @invocation-count
                   ". If higher, quantiles may be bootstrapped separately.")))))))
