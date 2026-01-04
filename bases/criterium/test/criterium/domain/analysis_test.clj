(ns criterium.domain.analysis-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.domain :as domain]
   [criterium.domain.analysis :as analysis]
   [criterium.domain.test-util :refer [sample-data sample-data-2
                                       mock-bench-result
                                       mock-bench-result-with-defs]]))

;; Tests for AIC/BIC computation functions.
;; Validates information criterion formulas for model selection.

(deftest compute-aic-test
  ;; Tests compute-aic (AICc with small-sample correction).
  ;; Formula: AICc = n*ln(RSS/n) + 2k + (2k(k+1))/(n-k-1)
  ;; Contracts: returns correct value, handles edge cases.
  (testing "compute-aic"
    (testing "computes AICc with small-sample correction"
      ;; Known values: n=10, k=2, RSS=5.0
      ;; base-aic = 10*ln(5/10) + 2*2 = 10*(-0.693147) + 4 = -2.931
      ;; correction = (2*2*(2+1))/(10-2-1) = 12/7 = 1.714
      ;; AICc = -2.931 + 1.714 = -1.217
      (let [result (double (analysis/compute-aic 5.0 10 2))]
        (is (< (Math/abs (- result -1.217)) 0.01))))
    (testing "returns nil when n <= k+1"
      ;; Correction term has division by (n-k-1), undefined when n <= k+1
      (is (nil? (analysis/compute-aic 5.0 3 2)))
      (is (nil? (analysis/compute-aic 5.0 2 2))))
    (testing "works with minimum valid sample size"
      ;; n=4, k=2 gives n-k-1=1, minimum valid
      (is (some? (analysis/compute-aic 5.0 4 2))))
    (testing "handles k=1 (simple regression)"
      ;; n=5, k=1, RSS=2.0
      ;; base-aic = 5*ln(2/5) + 2*1 = 5*(-0.916) + 2 = -2.581
      ;; correction = (2*1*2)/(5-1-1) = 4/3 = 1.333
      ;; AICc = -2.581 + 1.333 = -1.248
      (let [result (double (analysis/compute-aic 2.0 5 1))]
        (is (< (Math/abs (- result -1.248)) 0.01))))
    (testing "lower AICc indicates better fit"
      ;; With same n and k, lower RSS gives lower AICc
      (let [aic-low-rss (analysis/compute-aic 1.0 10 2)
            aic-high-rss (analysis/compute-aic 5.0 10 2)]
        (is (< aic-low-rss aic-high-rss))))))

(deftest compute-bic-test
  ;; Tests compute-bic (Bayesian Information Criterion).
  ;; Formula: BIC = n*ln(RSS/n) + k*ln(n)
  ;; Contracts: returns correct value, handles edge cases.
  (testing "compute-bic"
    (testing "computes BIC correctly"
      ;; Known values: n=10, k=2, RSS=5.0
      ;; BIC = 10*ln(5/10) + 2*ln(10) = 10*(-0.693) + 2*2.303 = -2.325
      (let [result (double (analysis/compute-bic 5.0 10 2))]
        (is (< (Math/abs (- result -2.325)) 0.01))))
    (testing "returns nil when n = 0"
      (is (nil? (analysis/compute-bic 5.0 0 2))))
    (testing "handles k=1 (simple regression)"
      ;; n=5, k=1, RSS=2.0
      ;; BIC = 5*ln(2/5) + 1*ln(5) = 5*(-0.916) + 1.609 = -2.972
      (let [result (double (analysis/compute-bic 2.0 5 1))]
        (is (< (Math/abs (- result -2.972)) 0.01))))
    (testing "penalizes complexity more than AIC for large n"
      ;; BIC penalty is k*ln(n), AIC penalty is 2k
      ;; For n > e^2 ≈ 7.4, BIC penalizes complexity more
      (let [;; For n=20, k=3: BIC penalty = 3*ln(20) = 8.99
            ;; AIC base penalty = 2*3 = 6
            bic (analysis/compute-bic 10.0 20 3)
            aic (analysis/compute-aic 10.0 20 3)]
        ;; For large n, BIC is larger (more penalized) than AICc
        (is (> bic aic))))
    (testing "lower BIC indicates better fit"
      ;; With same n and k, lower RSS gives lower BIC
      (let [bic-low-rss (analysis/compute-bic 1.0 10 2)
            bic-high-rss (analysis/compute-bic 5.0 10 2)]
        (is (< bic-low-rss bic-high-rss))))))

;; Tests for domain analysis function extract.
;; Validates extracting metric values across runs with coordinate-value pairs,
;; handling missing metrics, preserving order, and applying transforms.

(deftest extract-test
  ;; Tests the extract function which extracts metric values from domain runs.
  ;; Extract now returns a :metrics map with metric-id keys.
  ;; Contracts: returns domain-extract?, preserves order, handles missing data.
  (testing "extract"
    (testing "with explicit metric-path"
      (testing "returns a domain-extract result with :metrics map"
        (let [d (domain/domain
                 {:coord {:n 100}
                  :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
              result (analysis/extract d [:stats :elapsed-time :mean])]
          (is (= :criterium/domain-extract (:type result)))
          (is (= :criterium/domain-extract (:type result)))
          (is (contains? (:metrics result) :elapsed-time))
          (is (= [:stats :elapsed-time :mean]
                 (get-in result [:metrics :elapsed-time :metric])))))
      (testing "contains coordinate-value pairs in metric :data"
        (let [d (domain/domain
                 {:coord {:n 100}
                  :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                 {:coord {:n 200}
                  :data (mock-bench-result {:elapsed-time {:mean 2.0}})})
              result (analysis/extract d [:stats :elapsed-time :mean])]
          (is (= [[{:n 100} 1.0] [{:n 200} 2.0]]
                 (get-in result [:metrics :elapsed-time :data])))))
      (testing "preserves run order"
        (let [d (domain/domain
                 {:coord {:n 300}
                  :data (mock-bench-result {:elapsed-time {:mean 3.0}})}
                 {:coord {:n 100}
                  :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                 {:coord {:n 200}
                  :data (mock-bench-result {:elapsed-time {:mean 2.0}})})
              result (analysis/extract d [:stats :elapsed-time :mean])]
          (is (= [[{:n 300} 3.0] [{:n 100} 1.0] [{:n 200} 2.0]]
                 (get-in result [:metrics :elapsed-time :data])))))
      (testing "returns nil for missing metrics"
        (let [d (domain/domain
                 {:coord {:n 100}
                  :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                 {:coord {:n 200}
                  :data (mock-bench-result {:other-metric {:mean 2.0}})})
              result (analysis/extract d [:stats :elapsed-time :mean])]
          (is (= [[{:n 100} 1.0] [{:n 200} nil]]
                 (get-in result [:metrics :elapsed-time :data])))))
      (testing "returns nil for missing value-key"
        (let [d (domain/domain
                 {:coord :a
                  :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                 {:coord :b
                  :data (mock-bench-result {:elapsed-time {:variance 0.5}})})
              result (analysis/extract d [:stats :elapsed-time :mean])]
          (is (= [[:a 1.0] [:b nil]]
                 (get-in result [:metrics :elapsed-time :data])))))
      (testing "handles keyword coordinates"
        (let [d (domain/domain
                 {:coord :baseline
                  :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                 {:coord :optimized
                  :data (mock-bench-result {:elapsed-time {:mean 0.5}})})
              result (analysis/extract d [:stats :elapsed-time :mean])]
          (is (= [[:baseline 1.0] [:optimized 0.5]]
                 (get-in result [:metrics :elapsed-time :data])))))
      (testing "returns empty vector in :data for empty domain"
        (let [result (analysis/extract (domain/domain)
                                       [:stats :elapsed-time :mean])]
          (is (= :criterium/domain-extract (:type result)))
          (is (= [] (get-in result [:metrics :elapsed-time :data])))))
      (testing "extracts different value-keys"
        (let [d (domain/domain
                 {:coord {:n 100}
                  :data (mock-bench-result
                         {:elapsed-time {:mean 1.0 :variance 0.1}})})]
          (is (= [[{:n 100} 1.0]]
                 (get-in (analysis/extract d [:stats :elapsed-time :mean])
                         [:metrics :elapsed-time :data])))
          (is (= [[{:n 100} 0.1]]
                 (get-in (analysis/extract d [:stats :elapsed-time :variance])
                         [:metrics :elapsed-time :data]))))))
    (testing "with :with-error-bounds option"
      (testing "sets :with-error-bounds flag in metric result"
        (let [d (domain/domain
                 {:coord {:n 100}
                  :data (mock-bench-result
                         {:elapsed-time {:mean 1.0
                                         :mean-plus-3sigma 1.2
                                         :mean-minus-3sigma 0.8}})})
              result (analysis/extract d [:stats :elapsed-time :mean]
                                       {:with-error-bounds true})]
          (is (true? (get-in result [:metrics :elapsed-time :with-error-bounds])))))
      (testing "returns value maps with :value, :lower, :upper"
        (let [d (domain/domain
                 {:coord {:n 100}
                  :data (mock-bench-result
                         {:elapsed-time {:mean 1.0
                                         :mean-plus-3sigma 1.2
                                         :mean-minus-3sigma 0.8}})}
                 {:coord {:n 200}
                  :data (mock-bench-result
                         {:elapsed-time {:mean 2.0
                                         :mean-plus-3sigma 2.5
                                         :mean-minus-3sigma 1.5}})})
              result (analysis/extract d [:stats :elapsed-time :mean]
                                       {:with-error-bounds true})]
          (is (= [[{:n 100} {:value 1.0 :lower 0.8 :upper 1.2}]
                  [{:n 200} {:value 2.0 :lower 1.5 :upper 2.5}]]
                 (get-in result [:metrics :elapsed-time :data])))))
      (testing "returns nil for missing mean value"
        (let [d (domain/domain
                 {:coord {:n 100}
                  :data (mock-bench-result
                         {:elapsed-time {:variance 0.1}})})
              result (analysis/extract d [:stats :elapsed-time :mean]
                                       {:with-error-bounds true})]
          (is (= [[{:n 100} nil]]
                 (get-in result [:metrics :elapsed-time :data])))))
      (testing "only applies to :mean value-key"
        (let [d (domain/domain
                 {:coord {:n 100}
                  :data (mock-bench-result
                         {:elapsed-time {:mean 1.0
                                         :variance 0.1
                                         :mean-plus-3sigma 1.2
                                         :mean-minus-3sigma 0.8}})})
              result (analysis/extract d [:stats :elapsed-time :variance]
                                       {:with-error-bounds true})]
          (is (false? (get-in result [:metrics :elapsed-time :with-error-bounds])))
          (is (= [[{:n 100} 0.1]]
                 (get-in result [:metrics :elapsed-time :data]))))))))

(deftest extract-implementations-test
  ;; Tests that extract preserves :impl-axis and :implementations from source domain.
  ;; This enables fit-complexity to detect multi-implementation data.
  (testing "extract"
    (testing "preserves :impl-axis and :implementations from domain with multi-impl"
      (let [d (domain/domain
               {:coord {:n 100 :impl :vec} :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
               {:coord {:n 100 :impl :list} :data (mock-bench-result {:elapsed-time {:mean 2.0}})}
               {:impl-axis :impl
                :implementations [:vec :list]})
            result (analysis/extract d [:stats :elapsed-time :mean])]
        (is (= :impl (:impl-axis result)))
        (is (= [:vec :list] (:implementations result)))))
    (testing "does not include :impl-axis when domain has single implementation"
      (let [d (domain/domain
               {:coord {:n 100} :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
            result (analysis/extract d [:stats :elapsed-time :mean])]
        (is (not (contains? result :impl-axis)))))))

;; Tests for domain group-by-axis function.
;; Validates partitioning runs by axis key values, returning a map
;; of axis-value to sub-domain.

(deftest group-by-axis-test
  (testing "group-by-axis"
    (testing "returns a domain-grouped result"
      (let [d (domain/domain
               {:coord {:n 100 :impl :foo} :data sample-data})
            result (analysis/group-by-axis d :impl)]
        (is (= :criterium/domain-grouped (:type result)))
        (is (= :criterium/domain-grouped (:type result)))
        (is (= :impl (:axis result)))))
    (testing "groups runs by axis key value"
      (let [d (domain/domain
               {:coord {:n 100 :impl :foo} :data sample-data}
               {:coord {:n 200 :impl :foo} :data sample-data-2}
               {:coord {:n 100 :impl :bar} :data {:third "result"}})
            grouped (:data (analysis/group-by-axis d :impl))]
        (is (= #{:foo :bar} (set (keys grouped))))
        (is (= :criterium/domain (:type (get grouped :foo))))
        (is (= 2 (count (:runs (get grouped :foo)))))
        (is (= 1 (count (:runs (get grouped :bar)))))))
    (testing "groups keyword coords under nil"
      (let [d (domain/domain
               {:coord :baseline :data sample-data}
               {:coord {:n 100 :impl :foo} :data sample-data-2})
            grouped (:data (analysis/group-by-axis d :impl))]
        (is (= #{:foo nil} (set (keys grouped))))
        (is (= 1 (count (:runs (get grouped nil)))))
        (is (= :baseline (-> grouped (get nil) :runs first :coord)))))
    (testing "groups runs missing axis key under nil"
      (let [d (domain/domain
               {:coord {:n 100} :data sample-data}
               {:coord {:n 100 :impl :foo} :data sample-data-2})
            grouped (:data (analysis/group-by-axis d :impl))]
        (is (= #{:foo nil} (set (keys grouped))))
        (is (= {:n 100} (-> grouped (get nil) :runs first :coord)))))
    (testing "returns empty map in :data for empty domain"
      (let [result (analysis/group-by-axis (domain/domain) :impl)]
        (is (= :criterium/domain-grouped (:type result)))
        (is (= {} (:data result)))))
    (testing "preserves run order within groups"
      (let [d (domain/domain
               {:coord {:n 300 :impl :foo} :data sample-data}
               {:coord {:n 100 :impl :foo} :data sample-data}
               {:coord {:n 200 :impl :foo} :data sample-data})
            grouped (:data (analysis/group-by-axis d :impl))
            coords (mapv :coord (:runs (get grouped :foo)))]
        (is (= [{:n 300 :impl :foo}
                {:n 100 :impl :foo}
                {:n 200 :impl :foo}]
               coords))))
    (testing "each group is a valid domain"
      (let [d (domain/domain
               {:coord {:n 100 :impl :foo} :data sample-data}
               {:coord {:n 100 :impl :bar} :data sample-data-2})
            grouped (:data (analysis/group-by-axis d :impl))]
        (doseq [[_ sub-domain] grouped]
          (is (= :criterium/domain (:type sub-domain))))))))

;; Tests for domain compare-by function.
;; Validates comparing metric values across axis dimensions, producing
;; structured output for analysis.

(deftest compare-by-test
  ;; Tests compare-by which compares metric values across an axis.
  ;; Supports both single-metric (with metric-path) and multi-metric (nil metric-path) modes.
  ;; Multi-metric mode discovers metrics from first run's metrics-defs.
  (testing "compare-by"
    (testing "with explicit metric-path (single-metric mode)"
      (testing "returns a domain-comparison result"
        (let [d (domain/domain
                 {:coord {:n 100 :impl :foo}
                  :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                 {:coord {:n 100 :impl :bar}
                  :data (mock-bench-result {:elapsed-time {:mean 2.0}})})
              result (analysis/compare-by d :impl [:stats :elapsed-time :mean])]
          (is (= :criterium/domain-comparison (:type result)))
          (is (= :criterium/domain-comparison (:type result)))
          (is (= :impl (:axis result)))
          (is (= [:stats :elapsed-time :mean] (:metric result)))
          (is (map? (:data result)))))
      (testing "data entries contain coord and value"
        (let [d (domain/domain
                 {:coord {:n 100 :impl :foo}
                  :data (mock-bench-result {:elapsed-time {:mean 1.5}})})
              result (analysis/compare-by d :impl [:stats :elapsed-time :mean])
              entry (first (get-in result [:data :foo]))]
          (is (= {:n 100 :impl :foo} (:coord entry)))
          (is (= 1.5 (:value entry)))))
      (testing "groups runs by axis value"
        (let [d (domain/domain
                 {:coord {:n 100 :impl :foo}
                  :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                 {:coord {:n 200 :impl :foo}
                  :data (mock-bench-result {:elapsed-time {:mean 2.0}})}
                 {:coord {:n 100 :impl :bar}
                  :data (mock-bench-result {:elapsed-time {:mean 3.0}})})
              result (analysis/compare-by d :impl [:stats :elapsed-time :mean])]
          (is (= 2 (count (get-in result [:data :foo]))))
          (is (= 1 (count (get-in result [:data :bar]))))
          (is (= [1.0 2.0] (mapv :value (get-in result [:data :foo]))))
          (is (= [3.0] (mapv :value (get-in result [:data :bar]))))))
      (testing "handles missing metrics with nil values"
        (let [d (domain/domain
                 {:coord {:impl :foo}
                  :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                 {:coord {:impl :bar}
                  :data (mock-bench-result {:other-metric {:mean 2.0}})})
              result (analysis/compare-by d :impl [:stats :elapsed-time :mean])]
          (is (= 1.0 (:value (first (get-in result [:data :foo])))))
          (is (nil? (:value (first (get-in result [:data :bar])))))))
      (testing "groups keyword coords under nil"
        (let [d (domain/domain
                 {:coord :baseline
                  :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                 {:coord {:impl :foo}
                  :data (mock-bench-result {:elapsed-time {:mean 2.0}})})
              result (analysis/compare-by d :impl [:stats :elapsed-time :mean])]
          (is (contains? (:data result) nil))
          (is (= :baseline (:coord (first (get-in result [:data nil])))))))
      (testing "returns empty :data for empty domain"
        (let [result (analysis/compare-by (domain/domain) :impl
                                          [:stats :elapsed-time :mean])]
          (is (= :criterium/domain-comparison (:type result)))
          (is (= :impl (:axis result)))
          (is (= {} (:data result)))))
      (testing "preserves run order within groups"
        (let [d (domain/domain
                 {:coord {:n 300 :impl :foo}
                  :data (mock-bench-result {:elapsed-time {:mean 3.0}})}
                 {:coord {:n 100 :impl :foo}
                  :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                 {:coord {:n 200 :impl :foo}
                  :data (mock-bench-result {:elapsed-time {:mean 2.0}})})
              result (analysis/compare-by d :impl [:stats :elapsed-time :mean])
              values (mapv :value (get-in result [:data :foo]))]
          (is (= [3.0 1.0 2.0] values))))
      (testing "includes :implementations when domain has them"
        (let [d (domain/domain
                 {:coord {:n 100 :impl :foo}
                  :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                 {:coord {:n 100 :impl :bar}
                  :data (mock-bench-result {:elapsed-time {:mean 2.0}})}
                 {:implementations [:foo :bar]})
              result (analysis/compare-by d :impl [:stats :elapsed-time :mean])]
          (is (= [:foo :bar] (:implementations result)))))
      (testing "omits :implementations when domain lacks them"
        (let [d (domain/domain
                 {:coord {:n 100 :impl :foo}
                  :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
              result (analysis/compare-by d :impl [:stats :elapsed-time :mean])]
          (is (not (contains? result :implementations))))))
    (testing "with nil metric-path (multi-metric mode)"
      (testing "returns :metrics map instead of :data/:metric"
        (let [d (domain/domain
                 {:coord {:n 100 :impl :foo}
                  :data (mock-bench-result-with-defs
                         {:elapsed-time {:mean 1.0}
                          :thread-allocation {:mean 100}})}
                 {:coord {:n 100 :impl :bar}
                  :data (mock-bench-result-with-defs
                         {:elapsed-time {:mean 2.0}
                          :thread-allocation {:mean 200}})})
              result (analysis/compare-by d :impl nil)]
          (is (= :criterium/domain-comparison (:type result)))
          (is (contains? result :metrics))
          (is (not (contains? result :metric)))
          (is (not (contains? result :data)))))
      (testing "discovers all quantitative metrics from first run"
        (let [d (domain/domain
                 {:coord {:n 100 :impl :foo}
                  :data (mock-bench-result-with-defs
                         {:elapsed-time {:mean 1.0}
                          :thread-allocation {:mean 100}})}
                 {:coord {:n 100 :impl :bar}
                  :data (mock-bench-result-with-defs
                         {:elapsed-time {:mean 2.0}
                          :thread-allocation {:mean 200}})})
              result (analysis/compare-by d :impl nil)]
          (is (contains? (:metrics result) :elapsed-time))
          (is (contains? (:metrics result) :thread-allocation))))
      (testing "each metric has :metric path and :data grouped by impl"
        (let [d (domain/domain
                 {:coord {:n 100 :impl :foo}
                  :data (mock-bench-result-with-defs {:elapsed-time {:mean 1.0}})}
                 {:coord {:n 100 :impl :bar}
                  :data (mock-bench-result-with-defs {:elapsed-time {:mean 2.0}})})
              result (analysis/compare-by d :impl nil)
              elapsed-metric (get-in result [:metrics :elapsed-time])]
          (is (= [:stats :elapsed-time :mean] (:metric elapsed-metric)))
          (is (map? (:data elapsed-metric)))
          (is (contains? (:data elapsed-metric) :foo))
          (is (contains? (:data elapsed-metric) :bar))))
      (testing "includes :implementations when domain has them"
        (let [d (domain/domain
                 {:coord {:n 100 :impl :foo}
                  :data (mock-bench-result-with-defs {:elapsed-time {:mean 1.0}})}
                 {:coord {:n 100 :impl :bar}
                  :data (mock-bench-result-with-defs {:elapsed-time {:mean 2.0}})}
                 {:implementations [:foo :bar]})
              result (analysis/compare-by d :impl nil)]
          (is (= [:foo :bar] (:implementations result)))))
      (testing "supports :metric-ids option to filter metrics"
        (let [d (domain/domain
                 {:coord {:n 100 :impl :foo}
                  :data (mock-bench-result-with-defs
                         {:elapsed-time {:mean 1.0}
                          :thread-allocation {:mean 100}
                          :memory {:mean 1000}})}
                 {:coord {:n 100 :impl :bar}
                  :data (mock-bench-result-with-defs
                         {:elapsed-time {:mean 2.0}
                          :thread-allocation {:mean 200}
                          :memory {:mean 2000}})})
              result (analysis/compare-by d :impl nil
                                          {:metric-ids [:elapsed-time :memory]})]
          (is (contains? (:metrics result) :elapsed-time))
          (is (contains? (:metrics result) :memory))
          (is (not (contains? (:metrics result) :thread-allocation))))))))

;; Tests for domain analysis pipeline functions.
;; Validates composable analysis transformers that operate on data-maps,
;; following the same pattern as criterium.analyse functions.

(deftest domain-extract-fn-test
  ;; Tests the factory function that creates extract pipelines.
  ;; Contracts: returns function, extracts from data-map, supports options.
  (testing "domain-extract-fn"
    (testing "returns a function"
      (is (fn? (analysis/domain-extract-fn)))
      (is (fn? (analysis/domain-extract-fn {}))))
    (testing "extracts metric from domain in data-map"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
            f (analysis/domain-extract-fn
               {:id :mean :metric-path [:stats :elapsed-time :mean]})
            result (f {:domain d})]
        (is (contains? result :domain))
        (is (contains? result :mean))
        (is (= :criterium/domain-extract (:type (:mean result))))))
    (testing "uses default :id when not specified"
      (let [d (domain/domain
               {:coord :a
                :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
            f (analysis/domain-extract-fn
               {:metric-path [:stats :elapsed-time :mean]})
            result (f {:domain d})]
        (is (contains? result :extract))))
    (testing "uses custom :domain-id"
      (let [d (domain/domain
               {:coord :a
                :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
            f (analysis/domain-extract-fn
               {:id :mean
                :domain-id :my-domain
                :metric-path [:stats :elapsed-time :mean]})
            result (f {:my-domain d})]
        (is (contains? result :mean))
        (is (= :criterium/domain-extract (:type (:mean result))))))
    (testing "preserves other keys in data-map"
      (let [d (domain/domain
               {:coord :a
                :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
            f (analysis/domain-extract-fn
               {:id :mean :metric-path [:stats :elapsed-time :mean]})
            result (f {:domain d :other-key "value"})]
        (is (= "value" (:other-key result)))))
    (testing "passes :with-error-bounds option to extract"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-bench-result {:elapsed-time {:mean 1.0
                                                         :mean-plus-3sigma 1.2
                                                         :mean-minus-3sigma 0.8}})})
            f (analysis/domain-extract-fn
               {:id :mean
                :metric-path [:stats :elapsed-time :mean]
                :with-error-bounds true})
            result (f {:domain d})
            extract (:mean result)]
        (is (true? (get-in extract [:metrics :elapsed-time :with-error-bounds])))
        (is (= {:value 1.0 :lower 0.8 :upper 1.2}
               (second (first (get-in extract [:metrics :elapsed-time :data])))))))))

(deftest domain-group-by-fn-test
  (testing "domain-group-by-fn"
    (testing "returns a function"
      (is (fn? (analysis/domain-group-by-fn)))
      (is (fn? (analysis/domain-group-by-fn {}))))
    (testing "groups domain by axis in data-map"
      (let [d (domain/domain
               {:coord {:n 100 :impl :foo} :data sample-data}
               {:coord {:n 200 :impl :bar} :data sample-data-2})
            f (analysis/domain-group-by-fn {:id :by-impl :axis-key :impl})
            result (f {:domain d})]
        (is (contains? result :domain))
        (is (contains? result :by-impl))
        (is (= :criterium/domain-grouped (:type (:by-impl result))))))
    (testing "uses default :id when not specified"
      (let [d (domain/domain
               {:coord {:impl :foo} :data sample-data})
            f (analysis/domain-group-by-fn {:axis-key :impl})
            result (f {:domain d})]
        (is (contains? result :grouped))))
    (testing "uses custom :domain-id"
      (let [d (domain/domain
               {:coord {:impl :foo} :data sample-data})
            f (analysis/domain-group-by-fn
               {:id :by-impl :domain-id :src :axis-key :impl})
            result (f {:src d})]
        (is (contains? result :by-impl))
        (is (= :criterium/domain-grouped (:type (:by-impl result))))))
    (testing "preserves other keys in data-map"
      (let [d (domain/domain
               {:coord {:impl :foo} :data sample-data})
            f (analysis/domain-group-by-fn {:id :by-impl :axis-key :impl})
            result (f {:domain d :config {:some "config"}})]
        (is (= {:some "config"} (:config result)))))))

(deftest domain-compare-fn-test
  ;; Tests the domain-compare-fn pipeline function which wraps compare-by.
  ;; Supports both single-metric and multi-metric modes via :metric-path option.
  (testing "domain-compare-fn"
    (testing "returns a function"
      (is (fn? (analysis/domain-compare-fn)))
      (is (fn? (analysis/domain-compare-fn {}))))
    (testing "with :metric-path (single-metric mode)"
      (testing "compares metric across axis in data-map"
        (let [d (domain/domain
                 {:coord {:impl :foo}
                  :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                 {:coord {:impl :bar}
                  :data (mock-bench-result {:elapsed-time {:mean 2.0}})})
              f (analysis/domain-compare-fn
                 {:id :impl-time
                  :axis-key :impl
                  :metric-path [:stats :elapsed-time :mean]})
              result (f {:domain d})]
          (is (contains? result :domain))
          (is (contains? result :impl-time))
          (is (= :criterium/domain-comparison (:type (:impl-time result))))))
      (testing "uses default :id when not specified"
        (let [d (domain/domain
                 {:coord {:impl :foo}
                  :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
              f (analysis/domain-compare-fn
                 {:axis-key :impl :metric-path [:stats :elapsed-time :mean]})
              result (f {:domain d})]
          (is (contains? result :comparison))))
      (testing "uses custom :domain-id"
        (let [d (domain/domain
                 {:coord {:impl :foo}
                  :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
              f (analysis/domain-compare-fn
                 {:id :cmp
                  :domain-id :source
                  :axis-key :impl
                  :metric-path [:stats :elapsed-time :mean]})
              result (f {:source d})]
          (is (contains? result :cmp))
          (is (= :criterium/domain-comparison (:type (:cmp result))))))
      (testing "preserves other keys in data-map"
        (let [d (domain/domain
                 {:coord {:impl :foo}
                  :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
              f (analysis/domain-compare-fn
                 {:id :cmp :axis-key :impl
                  :metric-path [:stats :elapsed-time :mean]})
              result (f {:domain d :meta {:info "data"}})]
          (is (= {:info "data"} (:meta result))))))
    (testing "without :metric-path (multi-metric mode)"
      (testing "discovers all metrics and returns :metrics map"
        (let [d (domain/domain
                 {:coord {:impl :foo}
                  :data (mock-bench-result-with-defs
                         {:elapsed-time {:mean 1.0}
                          :thread-allocation {:mean 100}})}
                 {:coord {:impl :bar}
                  :data (mock-bench-result-with-defs
                         {:elapsed-time {:mean 2.0}
                          :thread-allocation {:mean 200}})})
              f (analysis/domain-compare-fn {:id :cmp :axis-key :impl})
              result (f {:domain d})]
          (is (contains? result :cmp))
          (is (contains? (:cmp result) :metrics))
          (is (contains? (get-in result [:cmp :metrics]) :elapsed-time))
          (is (contains? (get-in result [:cmp :metrics]) :thread-allocation))))
      (testing "supports :metric-ids to filter metrics"
        (let [d (domain/domain
                 {:coord {:impl :foo}
                  :data (mock-bench-result-with-defs
                         {:elapsed-time {:mean 1.0}
                          :thread-allocation {:mean 100}
                          :memory {:mean 1000}})}
                 {:coord {:impl :bar}
                  :data (mock-bench-result-with-defs
                         {:elapsed-time {:mean 2.0}
                          :thread-allocation {:mean 200}
                          :memory {:mean 2000}})})
              f (analysis/domain-compare-fn
                 {:id :cmp
                  :axis-key :impl
                  :metric-ids [:elapsed-time]})
              result (f {:domain d})]
          (is (contains? (get-in result [:cmp :metrics]) :elapsed-time))
          (is (not (contains? (get-in result [:cmp :metrics]) :thread-allocation)))
          (is (not (contains? (get-in result [:cmp :metrics]) :memory))))))))

;; Tests for composing multiple pipeline functions.
;; Validates that pipeline functions can be composed together
;; to build complex analysis pipelines.

(deftest pipeline-composition-test
  (testing "pipeline composition"
    (testing "chains multiple extracts"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-bench-result
                       {:elapsed-time {:mean 1.0 :variance 0.1}})})
            result (-> {:domain d}
                       ((analysis/domain-extract-fn
                         {:id :mean
                          :metric-path [:stats :elapsed-time :mean]}))
                       ((analysis/domain-extract-fn
                         {:id :var
                          :metric-path [:stats :elapsed-time :variance]})))]
        (is (contains? result :domain))
        (is (contains? result :mean))
        (is (contains? result :var))
        (is (= :criterium/domain-extract (:type (:mean result))))
        (is (= :criterium/domain-extract (:type (:var result))))))
    (testing "chains extract with group-by"
      (let [d (domain/domain
               {:coord {:n 100 :impl :foo}
                :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
               {:coord {:n 100 :impl :bar}
                :data (mock-bench-result {:elapsed-time {:mean 2.0}})})
            result (-> {:domain d}
                       ((analysis/domain-extract-fn
                         {:id :mean
                          :metric-path [:stats :elapsed-time :mean]}))
                       ((analysis/domain-group-by-fn
                         {:id :by-impl :axis-key :impl})))]
        (is (= :criterium/domain-extract (:type (:mean result))))
        (is (= :criterium/domain-grouped (:type (:by-impl result))))))
    (testing "chains multiple analysis types"
      (let [d (domain/domain
               {:coord {:n 100 :impl :foo}
                :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
               {:coord {:n 200 :impl :foo}
                :data (mock-bench-result {:elapsed-time {:mean 2.0}})}
               {:coord {:n 100 :impl :bar}
                :data (mock-bench-result {:elapsed-time {:mean 1.5}})})
            result (-> {:domain d}
                       ((analysis/domain-extract-fn
                         {:id :mean
                          :metric-path [:stats :elapsed-time :mean]}))
                       ((analysis/domain-group-by-fn
                         {:id :by-impl :axis-key :impl}))
                       ((analysis/domain-compare-fn
                         {:id :impl-time
                          :axis-key :impl
                          :metric-path [:stats :elapsed-time :mean]}))
                       ((analysis/domain-compare-fn
                         {:id :n-time
                          :axis-key :n
                          :metric-path [:stats :elapsed-time :mean]})))]
        (is (= #{:domain :mean :by-impl :impl-time :n-time}
               (set (keys result))))
        (is (= :criterium/domain-extract (:type (:mean result))))
        (is (= :criterium/domain-grouped (:type (:by-impl result))))
        (is (= :criterium/domain-comparison (:type (:impl-time result))))
        (is (= :criterium/domain-comparison (:type (:n-time result))))))))

;; Tests for domain-regression? predicate and fit-complexity function.
;; Validates regression fitting for algorithmic complexity analysis.

(deftest fit-complexity-test
  ;; Tests regression model fitting on domain-extract data.
  ;; fit-complexity now takes multi-metric extract and returns :regressions map.
  ;; Contracts: identifies complexity, filters invalid data, handles error bounds.
  (testing "fit-complexity"
    (testing "returns a domain-regression result with :regressions map"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 100} 100.0]
                                                     [{:n 200} 200.0]
                                                     [{:n 300} 300.0]]}}}
            result (analysis/fit-complexity extract :n)]
        (is (= :criterium/domain-regression (:type result)))
        (is (= :criterium/domain-regression (:type result)))
        (is (= :n (:axis result)))
        (is (contains? (:regressions result) :elapsed-time))
        (is (= [:stats :elapsed-time :mean]
               (get-in result [:regressions :elapsed-time :metric])))))
    (testing "identifies linear complexity with perfect fit"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 100} 100.0]
                                                     [{:n 200} 200.0]
                                                     [{:n 300} 300.0]
                                                     [{:n 400} 400.0]]}}}
            result (analysis/fit-complexity extract :n)
            regression (get-in result [:regressions :elapsed-time])
            linear (first (filter #(= :linear (:id %)) (:models regression)))]
        (is (= :linear (:best-fit regression)))
        (is (> (:r-squared linear) 0.99))))
    (testing "identifies quadratic complexity"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 10} 100.0]
                                                     [{:n 20} 400.0]
                                                     [{:n 30} 900.0]
                                                     [{:n 40} 1600.0]]}}}
            result (analysis/fit-complexity extract :n)
            regression (get-in result [:regressions :elapsed-time])
            quadratic (first (filter #(= :quadratic (:id %)) (:models regression)))]
        (is (= :quadratic (:best-fit regression)))
        (is (> (:r-squared quadratic) 0.99))))
    (testing "filters out nil values"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 100} 100.0]
                                                     [{:n 200} nil]
                                                     [{:n 300} 300.0]]}}}
            result (analysis/fit-complexity extract :n)]
        (is (= :criterium/domain-regression (:type result)))
        (is (seq (get-in result [:regressions :elapsed-time :models])))))
    (testing "filters out coordinates missing axis key"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 100} 100.0]
                                                     [{:m 200} 200.0]
                                                     [{:n 300} 300.0]]}}}
            result (analysis/fit-complexity extract :n)]
        (is (= :criterium/domain-regression (:type result)))
        (is (seq (get-in result [:regressions :elapsed-time :models])))))
    (testing "filters out keyword coordinates"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 100} 100.0]
                                                     [:baseline 50.0]
                                                     [{:n 300} 300.0]]}}}
            result (analysis/fit-complexity extract :n)]
        (is (= :criterium/domain-regression (:type result)))
        (is (seq (get-in result [:regressions :elapsed-time :models])))))
    (testing "returns empty models with insufficient data"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 100} 100.0]]}}}
            result (analysis/fit-complexity extract :n)
            regression (get-in result [:regressions :elapsed-time])]
        (is (= :criterium/domain-regression (:type result)))
        (is (empty? (:models regression)))
        (is (nil? (:best-fit regression)))))
    (testing "returns empty models for empty extract"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data []}}}
            result (analysis/fit-complexity extract :n)
            regression (get-in result [:regressions :elapsed-time])]
        (is (= :criterium/domain-regression (:type result)))
        (is (empty? (:models regression)))))
    (testing "supports custom models"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 100} 100.0]
                                                     [{:n 200} 200.0]
                                                     [{:n 300} 300.0]]}}}
            models {:cubic {:transform (fn [n] (let [n (double n)] (* n n n)))
                            :label "O(n³)"}}
            result (analysis/fit-complexity extract :n models)
            regression (get-in result [:regressions :elapsed-time])]
        (is (= :criterium/domain-regression (:type result)))
        (is (= 1 (count (:models regression))))
        (is (= :cubic (:id (first (:models regression)))))))
    (testing "handles multiple metrics"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 100} 100.0]
                                                     [{:n 200} 200.0]
                                                     [{:n 300} 300.0]]}
                               :thread-allocation {:metric [:stats :thread-allocation :mean]
                                                   :data [[{:n 100} 1000.0]
                                                          [{:n 200} 2000.0]
                                                          [{:n 300} 3000.0]]}}}
            result (analysis/fit-complexity extract :n)]
        (is (= :criterium/domain-regression (:type result)))
        (is (contains? (:regressions result) :elapsed-time))
        (is (contains? (:regressions result) :thread-allocation))
        (is (= :linear (get-in result [:regressions :elapsed-time :best-fit])))
        (is (= :linear (get-in result [:regressions :thread-allocation :best-fit])))))
    (testing "handles error-bound data"
      (testing "extracts :value from error-bound maps"
        (let [extract {:type :criterium/domain-extract
                       :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                                :with-error-bounds true
                                                :data [[{:n 100} {:value 100.0 :lower 90.0 :upper 110.0}]
                                                       [{:n 200} {:value 200.0 :lower 180.0 :upper 220.0}]
                                                       [{:n 300} {:value 300.0 :lower 270.0 :upper 330.0}]]}}}
              result (analysis/fit-complexity extract :n)
              regression (get-in result [:regressions :elapsed-time])
              linear (first (filter #(= :linear (:id %)) (:models regression)))]
          (is (= :criterium/domain-regression (:type result)))
          (is (= :linear (:best-fit regression)))
          (is (> (:r-squared linear) 0.99))))
      (testing "filters out nil error-bound values"
        (let [extract {:type :criterium/domain-extract
                       :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                                :with-error-bounds true
                                                :data [[{:n 100} {:value 100.0 :lower 90.0 :upper 110.0}]
                                                       [{:n 200} nil]
                                                       [{:n 300} {:value 300.0 :lower 270.0 :upper 330.0}]]}}}
              result (analysis/fit-complexity extract :n)]
          (is (= :criterium/domain-regression (:type result)))
          (is (seq (get-in result [:regressions :elapsed-time :models]))))))))

(deftest fit-complexity-multi-impl-test
  ;; Tests fit-complexity with multi-implementation domains.
  ;; When extract has multiple implementations, data is grouped by implementation
  ;; and models are fit separately for each.
  (testing "fit-complexity with multiple implementations"
    (testing "returns regression with :impl-axis and :implementations"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:vec :list]
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 100 :impl :vec} 100.0]
                                                     [{:n 200 :impl :vec} 200.0]
                                                     [{:n 100 :impl :list} 150.0]
                                                     [{:n 200 :impl :list} 300.0]]}}}
            result (analysis/fit-complexity extract :n)]
        (is (= :criterium/domain-regression (:type result)))
        (is (= :impl (:impl-axis result)))
        (is (= [:vec :list] (:implementations result)))))
    (testing "groups regression by implementation in :by-impl"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:vec :list]
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 100 :impl :vec} 100.0]
                                                     [{:n 200 :impl :vec} 200.0]
                                                     [{:n 100 :impl :list} 150.0]
                                                     [{:n 200 :impl :list} 300.0]]}}}
            result (analysis/fit-complexity extract :n)
            regression (get-in result [:regressions :elapsed-time])]
        (is (contains? regression :by-impl))
        (is (contains? (:by-impl regression) :vec))
        (is (contains? (:by-impl regression) :list))))
    (testing "fits models separately per implementation"
      (let [;; vec is linear: 100, 200, 300
            ;; list is quadratic-ish: 100, 400, 900
            extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:vec :list]
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 10 :impl :vec} 100.0]
                                                     [{:n 20 :impl :vec} 200.0]
                                                     [{:n 30 :impl :vec} 300.0]
                                                     [{:n 10 :impl :list} 100.0]
                                                     [{:n 20 :impl :list} 400.0]
                                                     [{:n 30 :impl :list} 900.0]]}}}
            result (analysis/fit-complexity extract :n)
            by-impl (get-in result [:regressions :elapsed-time :by-impl])]
        (is (= :linear (get-in by-impl [:vec :best-fit])))
        (is (= :quadratic (get-in by-impl [:list :best-fit])))))
    (testing "each implementation has independent :models and :best-fit"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:vec :list]
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 100 :impl :vec} 100.0]
                                                     [{:n 200 :impl :vec} 200.0]
                                                     [{:n 100 :impl :list} 150.0]
                                                     [{:n 200 :impl :list} 300.0]]}}}
            result (analysis/fit-complexity extract :n)
            by-impl (get-in result [:regressions :elapsed-time :by-impl])]
        (is (seq (get-in by-impl [:vec :models])))
        (is (some? (get-in by-impl [:vec :best-fit])))
        (is (seq (get-in by-impl [:list :models])))
        (is (some? (get-in by-impl [:list :best-fit])))))
    (testing "handles empty implementation group"
      (let [extract {:type :criterium/domain-extract
                     :impl-axis :impl
                     :implementations [:vec]
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 100 :impl :vec} 100.0]]}}}
            result (analysis/fit-complexity extract :n)
            by-impl (get-in result [:regressions :elapsed-time :by-impl])]
        ;; Single point can't fit a model
        (is (empty? (get-in by-impl [:vec :models])))))
    (testing "does not include :by-impl for single-impl domains"
      (let [extract {:type :criterium/domain-extract
                     :implementations [:default]
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 100} 100.0]
                                                     [{:n 200} 200.0]]}}}
            result (analysis/fit-complexity extract :n)
            regression (get-in result [:regressions :elapsed-time])]
        (is (not (contains? result :impl-axis)))
        (is (not (contains? regression :by-impl)))
        (is (contains? regression :models))))))

(deftest domain-regression-fn-test
  ;; Tests the factory function that creates regression pipelines.
  ;; Contracts: returns function, fits regression from data-map, supports options.
  (testing "domain-regression-fn"
    (testing "returns a function"
      (is (fn? (analysis/domain-regression-fn)))
      (is (fn? (analysis/domain-regression-fn {}))))
    (testing "fits regression to extract in data-map"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 100} 100.0]
                                                     [{:n 200} 200.0]
                                                     [{:n 300} 300.0]]}}}
            f (analysis/domain-regression-fn {:id :scaling :axis :n})
            result (f {:extract extract})]
        (is (contains? result :extract))
        (is (contains? result :scaling))
        (is (= :criterium/domain-regression (:type (:scaling result))))))
    (testing "uses default :id when not specified"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 100} 100.0]
                                                     [{:n 200} 200.0]]}}}
            f (analysis/domain-regression-fn {:axis :n})
            result (f {:extract extract})]
        (is (contains? result :regression))))
    (testing "uses custom :extract-id"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 100} 100.0]
                                                     [{:n 200} 200.0]]}}}
            f (analysis/domain-regression-fn
               {:id :scaling :extract-id :my-extract :axis :n})
            result (f {:my-extract extract})]
        (is (contains? result :scaling))
        (is (= :criterium/domain-regression (:type (:scaling result))))))
    (testing "preserves other keys in data-map"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 100} 100.0]
                                                     [{:n 200} 200.0]]}}}
            f (analysis/domain-regression-fn {:id :scaling :axis :n})
            result (f {:extract extract :other-key "value"})]
        (is (= "value" (:other-key result)))))
    (testing "composes with domain-extract-fn"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-bench-result {:elapsed-time {:mean 100.0}})}
               {:coord {:n 200}
                :data (mock-bench-result {:elapsed-time {:mean 200.0}})}
               {:coord {:n 300}
                :data (mock-bench-result {:elapsed-time {:mean 300.0}})})
            result (-> {:domain d}
                       ((analysis/domain-extract-fn
                         {:id :extract
                          :metric-path [:stats :elapsed-time :mean]}))
                       ((analysis/domain-regression-fn
                         {:id :scaling :axis :n})))]
        (is (= :criterium/domain-extract (:type (:extract result))))
        (is (= :criterium/domain-regression (:type (:scaling result))))
        (is (= :linear (get-in (:scaling result) [:regressions :elapsed-time :best-fit])))))))

;; Tests for domain plan execution functions.
;; Validates the domain-plan pattern for bundled analysis and viewing,
;; following the same pattern as criterium.benchmark for samples.

(deftest ->domain-analyse-test
  ;; Tests for creating composite analysis functions from specs.
  ;; Validates spec resolution, composition, and error handling.
  (testing "->domain-analyse"
    (testing "creates analysis function from single spec"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
            analyse (analysis/->domain-analyse
                     [[:domain-extract-fn
                       {:id :extract :metric-path [:stats :elapsed-time :mean]}]])
            result (analyse {:domain d})]
        (is (map? result))
        (is (contains? result :domain))
        (is (contains? result :extract))
        (is (= :criterium/domain-extract (:type (:extract result))))))
    (testing "creates analysis function from multiple specs"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-bench-result {:elapsed-time {:mean 100.0}})}
               {:coord {:n 200}
                :data (mock-bench-result {:elapsed-time {:mean 200.0}})})
            analyse (analysis/->domain-analyse
                     [[:domain-extract-fn
                       {:id :extract :metric-path [:stats :elapsed-time :mean]}]
                      [:domain-regression-fn {:id :scaling :axis :n}]])
            result (analyse {:domain d})]
        (is (contains? result :extract))
        (is (contains? result :scaling))
        (is (= :criterium/domain-extract (:type (:extract result))))
        (is (= :criterium/domain-regression (:type (:scaling result))))))
    (testing "handles empty spec vector"
      (let [analyse (analysis/->domain-analyse [])
            result (analyse {:domain (domain/domain) :other "data"})]
        (is (= {:domain (domain/domain) :other "data"} result))))
    (testing "handles nil spec"
      (let [analyse (analysis/->domain-analyse nil)
            result (analyse {:domain (domain/domain)})]
        (is (map? result))))
    (testing "throws on non-sequential spec"
      (is (thrown? clojure.lang.ExceptionInfo
                   (analysis/->domain-analyse :not-a-sequence))))))

(deftest ->domain-view-test
  ;; Tests for creating composite view functions from specs.
  ;; Validates spec resolution, viewer dispatch, and side effects.
  (testing "->domain-view"
    (testing "creates view function from single spec"
      (let [view-fn (analysis/->domain-view [[:domain-extract {}]])]
        (is (fn? view-fn))))
    (testing "creates view function from multiple specs"
      (let [view-fn (analysis/->domain-view
                     [[:domain-extract {}]
                      [:domain-regression {}]])]
        (is (fn? view-fn))))
    (testing "view function returns viewer output (nil for :none viewer)"
      (let [view-fn (analysis/->domain-view [[:domain-extract {}]])
            data-map {:domain (domain/domain)
                      :extract {:type :criterium/domain-extract
                                :metric [:stats :elapsed-time :mean]
                                :data []}}
            result (view-fn :none data-map)]
        (is (nil? result))))
    (testing "handles empty spec vector"
      (let [view-fn (analysis/->domain-view [])
            result (view-fn :none {:data "map"})]
        (is (nil? result))))
    (testing "handles nil spec"
      (let [view-fn (analysis/->domain-view nil)
            result (view-fn :none {:data "map"})]
        (is (nil? result))))
    (testing "throws on non-sequential spec"
      (is (thrown? clojure.lang.ExceptionInfo
                   (analysis/->domain-view :not-a-sequence))))))

(deftest options->domain-plan-test
  ;; Tests for merging options into base domain plans.
  ;; Validates option override behavior.
  (testing "options->domain-plan"
    (testing "returns base plan when no options"
      (let [base {:analyse [[:domain-extract-fn {}]]
                  :view [[:domain-extract {}]]
                  :viewer :print}]
        (is (= base (analysis/options->domain-plan base)))))
    (testing "overrides viewer option"
      (let [base {:analyse [] :view [] :viewer :print}
            result (analysis/options->domain-plan base :viewer :portal)]
        (is (= :portal (:viewer result)))))
    (testing "overrides analyse option"
      (let [base {:analyse [[:domain-extract-fn {}]] :view [] :viewer :print}
            new-analyse [[:domain-compare-fn {}]]
            result (analysis/options->domain-plan base :analyse new-analyse)]
        (is (= new-analyse (:analyse result)))))
    (testing "overrides view option"
      (let [base {:analyse [] :view [[:domain-extract {}]] :viewer :print}
            new-view [[:domain-comparison {}]]
            result (analysis/options->domain-plan base :view new-view)]
        (is (= new-view (:view result)))))
    (testing "combines multiple overrides"
      (let [base {:analyse [] :view [] :viewer :print}
            result (analysis/options->domain-plan base
                                                  :viewer :portal
                                                  :analyse [[:domain-extract-fn {}]])]
        (is (= :portal (:viewer result)))
        (is (= [[:domain-extract-fn {}]] (:analyse result)))
        (is (= [] (:view result)))))))

(deftest analyse-domain-test
  ;; Tests for executing domain analysis with a plan.
  ;; Validates full analysis pipeline execution with viewing.
  (testing "analyse-domain"
    (testing "executes analysis plan and returns data-map"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
            plan {:analyse [[:domain-extract-fn
                             {:id :extract
                              :metric-path [:stats :elapsed-time :mean]}]]
                  :view [[:domain-extract {:extract-id :extract}]]
                  :viewer :none}
            result (analysis/analyse-domain plan d)]
        (is (map? result))
        (is (contains? result :domain))
        (is (contains? result :extract))
        (is (= :criterium/domain-extract (:type (:extract result))))))
    (testing "chains multiple analysis functions"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-bench-result {:elapsed-time {:mean 100.0}})}
               {:coord {:n 200}
                :data (mock-bench-result {:elapsed-time {:mean 200.0}})})
            plan {:analyse [[:domain-extract-fn
                             {:id :extract
                              :metric-path [:stats :elapsed-time :mean]}]
                            [:domain-regression-fn {:id :scaling :axis :n}]]
                  :view []
                  :viewer :none}
            result (analysis/analyse-domain plan d)]
        (is (= :criterium/domain-extract (:type (:extract result))))
        (is (= :criterium/domain-regression (:type (:scaling result))))))
    (testing "defaults viewer to :print"
      (let [d (domain/domain)
            plan {:analyse [] :view []}
            result (analysis/analyse-domain plan d)]
        (is (map? result))))
    (testing "preserves domain in result"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
            plan {:analyse [] :view [] :viewer :none}
            result (analysis/analyse-domain plan d)]
        (is (= d (:domain result)))))
    (testing "includes viewer output in result"
      (let [d (domain/domain)
            plan {:analyse [] :view [] :viewer :none}
            result (analysis/analyse-domain plan d)]
        (is (contains? result :viewer))
        (is (= :criterium/viewer-output (get-in result [:viewer :type])))))
    (testing "works with empty analyse and view"
      (let [d (domain/domain)
            plan {:analyse [] :view [] :viewer :none}
            result (analysis/analyse-domain plan d)]
        (is (= d (:domain result)))
        (is (contains? result :viewer))))))
