(ns criterium.util.bootstrap-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.test-utils :refer [test-max-error]]
   [criterium.util.bootstrap :as bootstrap]
   [criterium.util.sampled-stats-test :as sampled-stats-test]
   [criterium.util.stats :as stats]
   [criterium.util.well :as well]))

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
  (testing "constant input"
    (let [samples (mapv double (repeat 100 1))
          stats   (bootstrap/bootstrap-stats-for
                   samples
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
                   samples
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
                   samples
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

;; todo add helpers for constant samples
;; integration test of time with bootstrap
(defn sample-values
  "Generate batched samples with the given mean and standard deviation."
  [batch-size num-samples random-seed mean sigma]
  (let [values (->> (sampled-stats-test/random-values
                     random-seed mean sigma)
                    (take num-samples)
                    vec)]
    (mapv #(* % batch-size) values)))

(deftest analyse-bootstrap-test
  (let [batch-size  100
        num-samples 1000
        samples     {[:v] (sample-values batch-size num-samples 123 10.0 1.0)}
        result      ((bootstrap/bootstrap-stats
                      {:quantiles          [0.99]
                       :estimate-quantiles [0.025 0.975]
                       :bootstrap-size     100
                       :sampled-path       [:sampled]
                       :output-path        [:analysis :bootstrap-stats]})
                     {:metrics-configs {:v {:type   :quantitative
                                            :values [{:path [:v]
                                                      :type :quantitative}]}}
                      :samples         (with-meta
                                         samples
                                         {:transform
                                          {:sample-> #(/ % 100.0)
                                           :->sample #(* 100.0 %)}})
                      :batch-size      batch-size
                      :eval-count      (* num-samples batch-size)
                      :elapsed-time    1})
        point       (-> result
                        :bootstrap-stats
                        :v
                        :mean
                        :point-estimate)]
    (is (test-max-error 10.0 point 0.1 "mean")
        (str "Value: " point))))
