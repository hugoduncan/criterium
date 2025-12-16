(ns criterium.domain-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.collect-plan :as collect-plan]
   [criterium.domain :as domain]
   [criterium.domain.analysis :as analysis]
   [criterium.domain.builder :as builder]
   [criterium.domain-plans :as domain-plans]
   [criterium.measured :as measured]))

;; Tests for domain type, predicates, and construction functions.
;; Validates the core domain data structure that holds multiple
;; benchmark runs indexed by coordinates.

(def sample-data {:some "bench-result"})
(def sample-data-2 {:other "result"})

(deftest run?-test
  (testing "run?"
    (testing "returns true for valid run with keyword coord"
      (is (true? (domain/run? {:coord :baseline :data sample-data}))))
    (testing "returns true for valid run with map coord"
      (is (true? (domain/run? {:coord {:n 100} :data sample-data}))))
    (testing "returns true for valid run with multi-key coord"
      (is (true? (domain/run? {:coord {:n 100 :impl :foo} :data sample-data}))))
    (testing "returns false for map missing :coord"
      (is (false? (domain/run? {:data sample-data}))))
    (testing "returns false for map missing :data"
      (is (false? (domain/run? {:coord :baseline}))))
    (testing "returns false for empty map"
      (is (false? (domain/run? {}))))
    (testing "returns false for non-map"
      (is (false? (domain/run? [:coord :baseline :data sample-data])))
      (is (false? (domain/run? nil)))
      (is (false? (domain/run? "run"))))))

(deftest domain?-test
  (testing "domain?"
    (testing "returns true for empty domain"
      (is (true? (domain/domain? {:type :criterium/domain :runs []}))))
    (testing "returns true for domain with runs"
      (is (true? (domain/domain?
                  {:type :criterium/domain
                   :runs [{:coord :a :data sample-data}]}))))
    (testing "returns false for wrong type"
      (is (false? (domain/domain? {:type :other :runs []}))))
    (testing "returns false for missing type"
      (is (false? (domain/domain? {:runs []}))))
    (testing "returns false when runs is not a vector"
      (is (false? (domain/domain? {:type :criterium/domain :runs '()})))
      (is (false? (domain/domain? {:type :criterium/domain :runs nil}))))
    (testing "returns false for non-map"
      (is (false? (domain/domain? nil)))
      (is (false? (domain/domain? "domain"))))))

(deftest domain-extract?-test
  ;; Tests the domain-extract? predicate for the multi-metric structure.
  ;; Domain extracts now contain a :metrics map with metric-id keys.
  (testing "domain-extract?"
    (testing "returns true for valid domain-extract result with :metrics map"
      (is (true? (domain/domain-extract?
                  {:type :criterium/domain-extract
                   :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                            :data [[{:n 100} 1.0]]}}}))))
    (testing "returns true for multiple metrics"
      (is (true? (domain/domain-extract?
                  {:type :criterium/domain-extract
                   :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                            :data [[{:n 100} 1.0]]}
                             :thread-allocation {:metric [:stats :thread-allocation :mean]
                                                 :data [[{:n 100} 512.0]]}}}))))
    (testing "returns true for empty metrics map"
      (is (true? (domain/domain-extract?
                  {:type :criterium/domain-extract
                   :metrics {}}))))
    (testing "returns false for wrong type"
      (is (false? (domain/domain-extract?
                   {:type :other :metrics {}}))))
    (testing "returns false for missing :metrics"
      (is (false? (domain/domain-extract?
                   {:type :criterium/domain-extract :data []}))))
    (testing "returns false for :metrics not being a map"
      (is (false? (domain/domain-extract?
                   {:type :criterium/domain-extract :metrics []}))))
    (testing "returns false for non-map"
      (is (false? (domain/domain-extract? nil)))
      (is (false? (domain/domain-extract? "extract"))))))

(deftest domain-grouped?-test
  (testing "domain-grouped?"
    (testing "returns true for valid domain-grouped result"
      (is (true? (domain/domain-grouped?
                  {:type :criterium/domain-grouped
                   :axis :impl
                   :data {:foo {:type :criterium/domain :runs []}}}))))
    (testing "returns true for empty data"
      (is (true? (domain/domain-grouped?
                  {:type :criterium/domain-grouped
                   :axis :impl
                   :data {}}))))
    (testing "returns false for wrong type"
      (is (false? (domain/domain-grouped?
                   {:type :other :axis :impl :data {}}))))
    (testing "returns false for missing :axis"
      (is (false? (domain/domain-grouped?
                   {:type :criterium/domain-grouped :data {}}))))
    (testing "returns false for missing :data"
      (is (false? (domain/domain-grouped?
                   {:type :criterium/domain-grouped :axis :impl}))))
    (testing "returns false for non-map"
      (is (false? (domain/domain-grouped? nil)))
      (is (false? (domain/domain-grouped? "grouped"))))))

(deftest domain-comparison?-test
  (testing "domain-comparison?"
    (testing "returns true for valid domain-comparison result"
      (is (true? (domain/domain-comparison?
                  {:type :criterium/domain-comparison
                   :axis :impl
                   :metric [:stats :elapsed-time :mean]
                   :data {:foo [{:coord {:impl :foo} :value 1.0}]}}))))
    (testing "returns true for empty data"
      (is (true? (domain/domain-comparison?
                  {:type :criterium/domain-comparison
                   :axis :impl
                   :metric [:stats :elapsed-time :mean]
                   :data {}}))))
    (testing "returns false for wrong type"
      (is (false? (domain/domain-comparison?
                   {:type :other :axis :impl :metric [] :data {}}))))
    (testing "returns false for missing :axis"
      (is (false? (domain/domain-comparison?
                   {:type :criterium/domain-comparison :metric [] :data {}}))))
    (testing "returns false for missing :metric"
      (is (false? (domain/domain-comparison?
                   {:type :criterium/domain-comparison :axis :impl :data {}}))))
    (testing "returns false for missing :data"
      (is (false? (domain/domain-comparison?
                   {:type :criterium/domain-comparison :axis :impl :metric []}))))
    (testing "returns false for non-map"
      (is (false? (domain/domain-comparison? nil)))
      (is (false? (domain/domain-comparison? "comparison"))))))

(deftest domain-test
  (testing "domain"
    (testing "with no arguments creates empty domain"
      (let [d (domain/domain)]
        (is (domain/domain? d))
        (is (= :criterium/domain (:type d)))
        (is (= [] (:runs d)))))
    (testing "with single run creates domain containing that run"
      (let [run {:coord :baseline :data sample-data}
            d (domain/domain run)]
        (is (domain/domain? d))
        (is (= [run] (:runs d)))))
    (testing "with multiple runs creates domain containing all runs"
      (let [run1 {:coord {:n 100} :data sample-data}
            run2 {:coord {:n 1000} :data sample-data-2}
            d (domain/domain run1 run2)]
        (is (domain/domain? d))
        (is (= [run1 run2] (:runs d)))))
    (testing "preserves keyword coordinates"
      (let [d (domain/domain {:coord :impl-a :data sample-data})]
        (is (= :impl-a (-> d :runs first :coord)))))
    (testing "preserves map coordinates"
      (let [d (domain/domain {:coord {:n 100 :impl :foo} :data sample-data})]
        (is (= {:n 100 :impl :foo} (-> d :runs first :coord)))))
    (testing "preserves run order"
      (let [runs (mapv #(hash-map :coord {:n %} :data sample-data)
                       [100 200 300 400 500])
            d (apply domain/domain runs)]
        (is (= [100 200 300 400 500]
               (mapv #(-> % :coord :n) (:runs d))))))))

;; Tests for domain accumulation functions (add-run, remove-run).
;; Validates adding and removing runs while maintaining immutability
;; and handling duplicate coordinates correctly.

(deftest add-run-test
  (testing "add-run"
    (testing "adds run to empty domain"
      (let [d (domain/domain)
            d2 (domain/add-run d :baseline sample-data)]
        (is (domain/domain? d2))
        (is (= 1 (count (:runs d2))))
        (is (= {:coord :baseline :data sample-data} (first (:runs d2))))))
    (testing "adds run with map coord"
      (let [d (domain/domain)
            d2 (domain/add-run d {:n 100} sample-data)]
        (is (= {:n 100} (-> d2 :runs first :coord)))))
    (testing "appends to existing runs"
      (let [d (domain/domain {:coord :a :data sample-data})
            d2 (domain/add-run d :b sample-data-2)]
        (is (= 2 (count (:runs d2))))
        (is (= [:a :b] (mapv :coord (:runs d2))))))
    (testing "replaces run with same keyword coord"
      (let [d (domain/domain {:coord :baseline :data sample-data})
            d2 (domain/add-run d :baseline sample-data-2)]
        (is (= 1 (count (:runs d2))))
        (is (= sample-data-2 (-> d2 :runs first :data)))))
    (testing "replaces run with same map coord"
      (let [d (domain/domain {:coord {:n 100} :data sample-data})
            d2 (domain/add-run d {:n 100} sample-data-2)]
        (is (= 1 (count (:runs d2))))
        (is (= sample-data-2 (-> d2 :runs first :data)))))
    (testing "preserves position when replacing"
      (let [d (-> (domain/domain)
                  (domain/add-run :a sample-data)
                  (domain/add-run :b sample-data)
                  (domain/add-run :c sample-data))
            d2 (domain/add-run d :b sample-data-2)]
        (is (= [:a :b :c] (mapv :coord (:runs d2))))
        (is (= sample-data-2 (-> d2 :runs second :data)))))
    (testing "returns new domain (immutable)"
      (let [d (domain/domain)
            d2 (domain/add-run d :x sample-data)]
        (is (= 0 (count (:runs d))))
        (is (= 1 (count (:runs d2))))))))

(deftest remove-run-test
  (testing "remove-run"
    (testing "removes run by keyword coord"
      (let [d (domain/domain {:coord :baseline :data sample-data})
            d2 (domain/remove-run d :baseline)]
        (is (domain/domain? d2))
        (is (= 0 (count (:runs d2))))))
    (testing "removes run by map coord"
      (let [d (domain/domain {:coord {:n 100} :data sample-data})
            d2 (domain/remove-run d {:n 100})]
        (is (= 0 (count (:runs d2))))))
    (testing "preserves other runs"
      (let [d (-> (domain/domain)
                  (domain/add-run :a sample-data)
                  (domain/add-run :b sample-data)
                  (domain/add-run :c sample-data))
            d2 (domain/remove-run d :b)]
        (is (= 2 (count (:runs d2))))
        (is (= [:a :c] (mapv :coord (:runs d2))))))
    (testing "returns unchanged domain when coord not found"
      (let [d (domain/domain {:coord :a :data sample-data})
            d2 (domain/remove-run d :nonexistent)]
        (is (= 1 (count (:runs d2))))
        (is (= :a (-> d2 :runs first :coord)))))
    (testing "returns new domain (immutable)"
      (let [d (domain/domain {:coord :x :data sample-data})
            d2 (domain/remove-run d :x)]
        (is (= 1 (count (:runs d))))
        (is (= 0 (count (:runs d2))))))))

;; Tests for domain query functions (runs, coords, axes).
;; Validates retrieving and filtering runs, extracting coordinates,
;; and inferring dimension keys from the domain structure.

(deftest runs-test
  (testing "runs"
    (testing "with one argument"
      (testing "returns all runs from domain"
        (let [run1 {:coord :a :data sample-data}
              run2 {:coord :b :data sample-data-2}
              d (domain/domain run1 run2)]
          (is (= [run1 run2] (domain/runs d)))))
      (testing "returns empty vector for empty domain"
        (is (= [] (domain/runs (domain/domain)))))
      (testing "preserves run order"
        (let [runs (mapv #(hash-map :coord {:n %} :data sample-data)
                         [100 200 300])
              d (apply domain/domain runs)]
          (is (= runs (domain/runs d))))))
    (testing "with partial coordinate"
      (testing "filters by keyword coord"
        (let [d (domain/domain {:coord :a :data sample-data}
                               {:coord :b :data sample-data-2})]
          (is (= [{:coord :a :data sample-data}]
                 (domain/runs d :a)))))
      (testing "filters by exact map coord"
        (let [d (domain/domain {:coord {:n 100} :data sample-data}
                               {:coord {:n 200} :data sample-data-2})]
          (is (= [{:coord {:n 100} :data sample-data}]
                 (domain/runs d {:n 100})))))
      (testing "filters by partial map coord"
        (let [d (domain/domain {:coord {:n 100} :data sample-data}
                               {:coord {:n 100 :impl :foo} :data sample-data-2}
                               {:coord {:n 200} :data {:third "result"}})]
          (is (= [{:coord {:n 100} :data sample-data}
                  {:coord {:n 100 :impl :foo} :data sample-data-2}]
                 (domain/runs d {:n 100})))))
      (testing "returns empty vector when no match"
        (let [d (domain/domain {:coord :a :data sample-data})]
          (is (= [] (domain/runs d :nonexistent)))))
      (testing "does not match keyword coord with map partial"
        (let [d (domain/domain {:coord :baseline :data sample-data})]
          (is (= [] (domain/runs d {:n 100}))))))))

(deftest coords-test
  (testing "coords"
    (testing "returns all coordinates as vector"
      (let [d (domain/domain {:coord :a :data sample-data}
                             {:coord {:n 100} :data sample-data-2})]
        (is (= [:a {:n 100}] (domain/coords d)))))
    (testing "returns empty vector for empty domain"
      (is (= [] (domain/coords (domain/domain)))))
    (testing "preserves order"
      (let [d (domain/domain {:coord {:n 300} :data sample-data}
                             {:coord {:n 100} :data sample-data}
                             {:coord {:n 200} :data sample-data})]
        (is (= [{:n 300} {:n 100} {:n 200}] (domain/coords d)))))
    (testing "handles mixed keyword and map coords"
      (let [d (domain/domain {:coord :baseline :data sample-data}
                             {:coord {:n 100} :data sample-data}
                             {:coord :optimized :data sample-data})]
        (is (= [:baseline {:n 100} :optimized] (domain/coords d)))))))

(deftest axes-test
  (testing "axes"
    (testing "returns set of dimension keys from map coords"
      (let [d (domain/domain {:coord {:n 100} :data sample-data}
                             {:coord {:n 200 :impl :foo} :data sample-data-2})]
        (is (= #{:n :impl} (domain/axes d)))))
    (testing "returns empty set for empty domain"
      (is (= #{} (domain/axes (domain/domain)))))
    (testing "returns empty set for domain with only keyword coords"
      (let [d (domain/domain {:coord :a :data sample-data}
                             {:coord :b :data sample-data-2})]
        (is (= #{} (domain/axes d)))))
    (testing "ignores keyword coords when extracting axes"
      (let [d (domain/domain {:coord :baseline :data sample-data}
                             {:coord {:n 100} :data sample-data})]
        (is (= #{:n} (domain/axes d)))))
    (testing "collects all keys from multi-key coords"
      (let [d (domain/domain {:coord {:n 100 :impl :foo :version 1}
                              :data sample-data})]
        (is (= #{:n :impl :version} (domain/axes d)))))))

(deftest implementations-test
  ;; Tests the implementations accessor and domain :impl-axis/:implementations.
  ;; The :impl-axis specifies which coordinate axis represents different
  ;; implementations. The :implementations key holds the list of impl keys.
  (testing "implementations"
    (testing "returns [:default] for domain without explicit implementations"
      (let [d (domain/domain {:coord {:n 100} :data sample-data})]
        (is (= [:default] (domain/implementations d)))))
    (testing "returns the implementation keys when set"
      (let [d (domain/domain
               {:coord {:n 100 :impl :vec} :data sample-data}
               {:impl-axis :impl
                :implementations [:vec :list]})]
        (is (= [:vec :list] (domain/implementations d)))))
    (testing "impl-axis returns the axis key"
      (let [d (domain/domain
               {:coord {:n 100 :impl :vec} :data sample-data}
               {:impl-axis :impl
                :implementations [:vec :list]})]
        (is (= :impl (domain/impl-axis d)))))
    (testing "impl-axis returns nil for domain without multi-impl"
      (let [d (domain/domain {:coord {:n 100} :data sample-data})]
        (is (nil? (domain/impl-axis d)))))
    (testing "domain with :impl-axis is valid"
      (let [d (domain/domain
               {:coord {:n 100 :impl :vec} :data sample-data}
               {:impl-axis :impl
                :implementations [:vec :list]})]
        (is (domain/domain? d))))
    (testing "add-run preserves :impl-axis and :implementations"
      (let [d (domain/domain {:impl-axis :impl
                              :implementations [:vec :list]})
            d2 (domain/add-run d {:n 100 :impl :vec} sample-data)]
        (is (= :impl (domain/impl-axis d2)))
        (is (= [:vec :list] (domain/implementations d2)))))
    (testing "remove-run preserves :impl-axis and :implementations"
      (let [d (domain/domain
               {:coord {:n 100 :impl :vec} :data sample-data}
               {:impl-axis :impl
                :implementations [:vec :list]})
            d2 (domain/remove-run d {:n 100 :impl :vec})]
        (is (= :impl (domain/impl-axis d2)))
        (is (= [:vec :list] (domain/implementations d2)))))))

;; Tests for domain analysis function extract.
;; Validates extracting metric values across runs with coordinate-value pairs,
;; handling missing metrics, preserving order, and applying transforms.

(defn mock-bench-result
  "Create a mock bench result with proper structure for stats-value.
  stats-data is a map of {metric-id {value-key value}}."
  [stats-data]
  {:stats {:type :criterium/stats
           :transform collect-plan/identity-transforms
           :stats stats-data
           :metrics-defs {}
           :batch-size 1
           :source-id nil
           :outliers-id nil}})

(defn mock-bench-result-with-defs
  "Create a mock bench result with metrics-defs for multi-metric discovery.
  stats-data is a map of {metric-id {value-key value}}."
  [stats-data]
  {:stats {:type :criterium/stats
           :transform collect-plan/identity-transforms
           :stats stats-data
           :metrics-defs (into {}
                               (map (fn [k] [k {:type :quantitative}]))
                               (keys stats-data))
           :batch-size 1
           :source-id nil
           :outliers-id nil}})

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
          (is (domain/domain-extract? result))
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
          (is (domain/domain-extract? result))
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

;; Tests for domain select function.
;; Validates filtering domain to sub-domain by partial coordinate match,
;; returning a new domain with matching runs.

(deftest select-test
  (testing "select"
    (testing "filters by keyword coord"
      (let [d (domain/domain {:coord :a :data sample-data}
                             {:coord :b :data sample-data-2})
            d2 (domain/select d :a)]
        (is (domain/domain? d2))
        (is (= 1 (count (:runs d2))))
        (is (= :a (-> d2 :runs first :coord)))))
    (testing "filters by exact map coord"
      (let [d (domain/domain {:coord {:n 100} :data sample-data}
                             {:coord {:n 200} :data sample-data-2})
            d2 (domain/select d {:n 100})]
        (is (domain/domain? d2))
        (is (= [{:coord {:n 100} :data sample-data}] (:runs d2)))))
    (testing "filters by partial map coord"
      (let [d (domain/domain {:coord {:n 100} :data sample-data}
                             {:coord {:n 100 :impl :foo} :data sample-data-2}
                             {:coord {:n 200} :data {:third "result"}})
            d2 (domain/select d {:n 100})]
        (is (= 2 (count (:runs d2))))
        (is (= [{:coord {:n 100} :data sample-data}
                {:coord {:n 100 :impl :foo} :data sample-data-2}]
               (:runs d2)))))
    (testing "filters by multiple partial coord keys"
      (let [d (domain/domain {:coord {:n 100 :impl :foo} :data sample-data}
                             {:coord {:n 100 :impl :bar} :data sample-data-2}
                             {:coord {:n 200 :impl :foo} :data {:third "result"}})
            d2 (domain/select d {:impl :foo})]
        (is (= 2 (count (:runs d2))))
        (is (= [{:coord {:n 100 :impl :foo} :data sample-data}
                {:coord {:n 200 :impl :foo} :data {:third "result"}}]
               (:runs d2)))))
    (testing "returns empty domain when no match"
      (let [d (domain/domain {:coord :a :data sample-data})
            d2 (domain/select d :nonexistent)]
        (is (domain/domain? d2))
        (is (= [] (:runs d2)))))
    (testing "preserves run order"
      (let [runs [(hash-map :coord {:n 100 :impl :foo} :data sample-data)
                  (hash-map :coord {:n 300 :impl :foo} :data sample-data)
                  (hash-map :coord {:n 200 :impl :foo} :data sample-data)]
            d (apply domain/domain
                     (concat runs [{:coord {:n 100 :impl :bar} :data sample-data}]))
            d2 (domain/select d {:impl :foo})]
        (is (= runs (:runs d2)))))
    (testing "returns new domain (immutable)"
      (let [d (domain/domain {:coord :a :data sample-data}
                             {:coord :b :data sample-data-2})
            d2 (domain/select d :a)]
        (is (= 2 (count (:runs d))))
        (is (= 1 (count (:runs d2))))))
    (testing "does not match keyword coord with map partial"
      (let [d (domain/domain {:coord :baseline :data sample-data})
            d2 (domain/select d {:n 100})]
        (is (= [] (:runs d2)))))))

;; Tests for domain group-by-axis function.
;; Validates partitioning runs by axis key values, returning a map
;; of axis-value to sub-domain.

(deftest group-by-axis-test
  (testing "group-by-axis"
    (testing "returns a domain-grouped result"
      (let [d (domain/domain
               {:coord {:n 100 :impl :foo} :data sample-data})
            result (analysis/group-by-axis d :impl)]
        (is (domain/domain-grouped? result))
        (is (= :criterium/domain-grouped (:type result)))
        (is (= :impl (:axis result)))))
    (testing "groups runs by axis key value"
      (let [d (domain/domain
               {:coord {:n 100 :impl :foo} :data sample-data}
               {:coord {:n 200 :impl :foo} :data sample-data-2}
               {:coord {:n 100 :impl :bar} :data {:third "result"}})
            grouped (:data (analysis/group-by-axis d :impl))]
        (is (= #{:foo :bar} (set (keys grouped))))
        (is (domain/domain? (get grouped :foo)))
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
        (is (domain/domain-grouped? result))
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
          (is (domain/domain? sub-domain)))))))

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
          (is (domain/domain-comparison? result))
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
          (is (domain/domain-comparison? result))
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
          (is (domain/domain-comparison? result))
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

;; Tests for input sequence generators.
;; Validates generation of sequences useful for scaling analysis,
;; including powers of 2, arbitrary powers, logarithmic ranges,
;; and linear ranges.

(deftest powers-of-2-test
  (testing "powers-of-2"
    (testing "generates powers from 0"
      (is (= [1 2 4 8 16] (builder/powers-of-2 0 4))))
    (testing "generates powers from non-zero exponent"
      (is (= [16 32 64 128 256] (builder/powers-of-2 4 8))))
    (testing "handles single value range"
      (is (= [8] (builder/powers-of-2 3 3))))
    (testing "generates large powers"
      (is (= [1024 2048 4096] (builder/powers-of-2 10 12))))))

(deftest powers-of-test
  (testing "powers-of"
    (testing "generates powers of 10"
      (is (= [10 100 1000 10000] (builder/powers-of 10 1 4))))
    (testing "generates powers of 3"
      (is (= [1 3 9 27 81] (builder/powers-of 3 0 4))))
    (testing "generates powers of 2 (same as powers-of-2)"
      (is (= [1 2 4 8] (builder/powers-of 2 0 3))))
    (testing "handles single value range"
      (is (= [100] (builder/powers-of 10 2 2))))))

(deftest log-range-test
  (testing "log-range"
    (testing "generates 4 points from 1 to 1000"
      (let [result (builder/log-range 1 1000 4)]
        (is (= 4 (count result)))
        (is (= 1 (first result)))
        (is (= 1000 (last result)))))
    (testing "generates 5 points from 10 to 10000"
      (let [result (builder/log-range 10 10000 5)]
        (is (= 5 (count result)))
        (is (= 10 (first result)))
        (is (= 10000 (last result)))))
    (testing "produces increasing values"
      (let [result (builder/log-range 1 1000 5)]
        (is (apply < result))))
    (testing "handles 2-point range"
      (is (= [10 1000] (builder/log-range 10 1000 2))))))

(deftest linear-range-test
  (testing "linear-range"
    (testing "generates evenly spaced values"
      (is (= [100 200 300 400 500] (builder/linear-range 100 500 5))))
    (testing "handles range starting at 0"
      (is (= [0 250 500 750 1000] (builder/linear-range 0 1000 5))))
    (testing "handles 2-point range"
      (is (= [100 1000] (builder/linear-range 100 1000 2))))
    (testing "produces increasing values"
      (let [result (builder/linear-range 10 1000 10)]
        (is (apply < result))))
    (testing "handles single point"
      (is (= [500] (builder/linear-range 500 500 1))))))

;; Tests n-log-n-range generates values spaced along an n*log(n) curve.
;; Contracts: correct count, endpoints match, values strictly increasing,
;; edge cases (single point, 2 points), start validation, and even spacing
;; in the n*log(n) domain.
(deftest n-log-n-range-test
  (testing "n-log-n-range"
    (testing "generates correct count of values"
      (is (= 5 (count (builder/n-log-n-range 10 10000 5)))))
    (testing "first value equals start"
      (is (= 10 (first (builder/n-log-n-range 10 10000 5)))))
    (testing "last value equals end"
      (is (= 10000 (last (builder/n-log-n-range 10 10000 5)))))
    (testing "produces strictly increasing values"
      (let [result (builder/n-log-n-range 10 10000 5)]
        (is (apply < result))))
    (testing "produces evenly spaced values in n*log(n) domain"
      (let [result (builder/n-log-n-range 10 10000 7)
            f (fn [x] (* x (Math/log x)))
            y-values (map f result)
            diffs (map - (rest y-values) y-values)
            mean-diff (/ (reduce + diffs) (count diffs))
            ;; Allow 1% tolerance for rounding errors
            tolerance (* 0.01 mean-diff)]
        (is (every? #(< (Math/abs (- % mean-diff)) tolerance) diffs))))
    (testing "handles 2-point range"
      (is (= [10 1000] (builder/n-log-n-range 10 1000 2))))
    (testing "handles single point"
      (is (= [500] (builder/n-log-n-range 500 500 1))))
    (testing "throws for start below e^-1"
      (is (thrown? AssertionError (builder/n-log-n-range 0.1 100 5))))))

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
        (is (domain/domain-extract? (:mean result)))))
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
        (is (domain/domain-extract? (:mean result)))))
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
        (is (domain/domain-grouped? (:by-impl result)))))
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
        (is (domain/domain-grouped? (:by-impl result)))))
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
          (is (domain/domain-comparison? (:impl-time result)))))
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
          (is (domain/domain-comparison? (:cmp result)))))
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
        (is (domain/domain-extract? (:mean result)))
        (is (domain/domain-extract? (:var result)))))
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
        (is (domain/domain-extract? (:mean result)))
        (is (domain/domain-grouped? (:by-impl result)))))
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
        (is (domain/domain-extract? (:mean result)))
        (is (domain/domain-grouped? (:by-impl result)))
        (is (domain/domain-comparison? (:impl-time result)))
        (is (domain/domain-comparison? (:n-time result)))))))

;; Tests for domain-regression? predicate and fit-complexity function.
;; Validates regression fitting for algorithmic complexity analysis.

(deftest domain-regression?-test
  ;; Tests the domain-regression? predicate for the multi-metric structure.
  ;; Domain regressions now contain a :regressions map with metric-id keys.
  (testing "domain-regression?"
    (testing "returns true for valid domain-regression result with :regressions map"
      (is (true? (domain/domain-regression?
                  {:type :criterium/domain-regression
                   :axis :n
                   :regressions {:elapsed-time {:models [{:id :linear :label "O(n)" :r-squared 0.98}]
                                                :best-fit :linear}}}))))
    (testing "returns true for multiple metrics in regressions"
      (is (true? (domain/domain-regression?
                  {:type :criterium/domain-regression
                   :axis :n
                   :regressions {:elapsed-time {:models [{:id :linear :r-squared 0.98}]
                                                :best-fit :linear}
                                 :thread-allocation {:models [{:id :linear :r-squared 0.95}]
                                                     :best-fit :linear}}}))))
    (testing "returns true for empty regressions map"
      (is (true? (domain/domain-regression?
                  {:type :criterium/domain-regression
                   :axis :n
                   :regressions {}}))))
    (testing "returns false for wrong type"
      (is (false? (domain/domain-regression?
                   {:type :other :axis :n :regressions {}}))))
    (testing "returns false for missing :axis"
      (is (false? (domain/domain-regression?
                   {:type :criterium/domain-regression :regressions {}}))))
    (testing "returns false for missing :regressions"
      (is (false? (domain/domain-regression?
                   {:type :criterium/domain-regression :axis :n}))))
    (testing "returns false for :regressions not being a map"
      (is (false? (domain/domain-regression?
                   {:type :criterium/domain-regression :axis :n :regressions []}))))
    (testing "returns false for non-map"
      (is (false? (domain/domain-regression? nil)))
      (is (false? (domain/domain-regression? "regression"))))))

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
        (is (domain/domain-regression? result))
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
        (is (domain/domain-regression? result))
        (is (seq (get-in result [:regressions :elapsed-time :models])))))
    (testing "filters out coordinates missing axis key"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 100} 100.0]
                                                     [{:m 200} 200.0]
                                                     [{:n 300} 300.0]]}}}
            result (analysis/fit-complexity extract :n)]
        (is (domain/domain-regression? result))
        (is (seq (get-in result [:regressions :elapsed-time :models])))))
    (testing "filters out keyword coordinates"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 100} 100.0]
                                                     [:baseline 50.0]
                                                     [{:n 300} 300.0]]}}}
            result (analysis/fit-complexity extract :n)]
        (is (domain/domain-regression? result))
        (is (seq (get-in result [:regressions :elapsed-time :models])))))
    (testing "returns empty models with insufficient data"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 100} 100.0]]}}}
            result (analysis/fit-complexity extract :n)
            regression (get-in result [:regressions :elapsed-time])]
        (is (domain/domain-regression? result))
        (is (empty? (:models regression)))
        (is (nil? (:best-fit regression)))))
    (testing "returns empty models for empty extract"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data []}}}
            result (analysis/fit-complexity extract :n)
            regression (get-in result [:regressions :elapsed-time])]
        (is (domain/domain-regression? result))
        (is (empty? (:models regression)))))
    (testing "supports custom models"
      (let [extract {:type :criterium/domain-extract
                     :metrics {:elapsed-time {:metric [:stats :elapsed-time :mean]
                                              :data [[{:n 100} 100.0]
                                                     [{:n 200} 200.0]
                                                     [{:n 300} 300.0]]}}}
            models {:cubic {:transform (fn [n] (* n n n))
                            :label "O(n³)"}}
            result (analysis/fit-complexity extract :n models)
            regression (get-in result [:regressions :elapsed-time])]
        (is (domain/domain-regression? result))
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
        (is (domain/domain-regression? result))
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
          (is (domain/domain-regression? result))
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
          (is (domain/domain-regression? result))
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
        (is (domain/domain-regression? result))
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
        (is (domain/domain-regression? (:scaling result)))))
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
        (is (domain/domain-regression? (:scaling result)))))
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
        (is (domain/domain-extract? (:extract result)))
        (is (domain/domain-regression? (:scaling result)))
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
        (is (domain/domain-extract? (:extract result)))))
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
        (is (domain/domain-extract? (:extract result)))
        (is (domain/domain-regression? (:scaling result)))))
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
        (is (domain/domain-extract? (:extract result)))))
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
        (is (domain/domain-extract? (:extract result)))
        (is (domain/domain-regression? (:scaling result)))))
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

;; Tests for pre-defined domain plans.
;; Validates that domain-plans namespace provides valid plan structures.

(deftest domain-plans-structure-test
  ;; Tests for domain plan structure validation.
  ;; Validates that pre-defined plans have required keys.
  ;; Note: :viewer is optional - uses default viewer when not specified.
  (testing "domain-plans structure"
    (testing "complexity-analysis has required keys"
      (is (vector? (:analyse domain-plans/complexity-analysis)))
      (is (vector? (:view domain-plans/complexity-analysis))))
    (testing "implementation-comparison has required keys"
      (is (vector? (:analyse domain-plans/implementation-comparison)))
      (is (vector? (:view domain-plans/implementation-comparison))))
    (testing "extract-elapsed-time has required keys"
      (is (vector? (:analyse domain-plans/extract-elapsed-time)))
      (is (vector? (:view domain-plans/extract-elapsed-time))))))

(deftest domain-plans-integration-test
  ;; Tests for executing pre-defined domain plans.
  ;; Validates end-to-end plan execution with actual domains.
  (testing "domain-plans integration"
    (testing "complexity-analysis executes successfully"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-bench-result {:elapsed-time {:mean 100.0}})}
               {:coord {:n 200}
                :data (mock-bench-result {:elapsed-time {:mean 200.0}})}
               {:coord {:n 300}
                :data (mock-bench-result {:elapsed-time {:mean 300.0}})})
            plan (analysis/options->domain-plan
                  domain-plans/complexity-analysis
                  :viewer :none)
            result (analysis/analyse-domain plan d)]
        (is (contains? result :extract))
        (is (contains? result :regression))
        (is (domain/domain-extract? (:extract result)))
        (is (domain/domain-regression? (:regression result)))))
    (testing "implementation-comparison executes successfully"
      (let [d (domain/domain
               {:coord {:impl :foo}
                :data (mock-bench-result-with-defs {:elapsed-time {:mean 1.0}})}
               {:coord {:impl :bar}
                :data (mock-bench-result-with-defs {:elapsed-time {:mean 2.0}})}
               {:implementations [:foo :bar]})
            plan (analysis/options->domain-plan
                  domain-plans/implementation-comparison
                  :viewer :none)
            result (analysis/analyse-domain plan d)]
        (is (contains? result :comparison))
        (is (domain/domain-comparison? (:comparison result)))
        (is (contains? (:comparison result) :metrics))
        (is (= [:foo :bar] (:implementations (:comparison result))))))
    (testing "extract-elapsed-time executes successfully"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
            plan (analysis/options->domain-plan
                  domain-plans/extract-elapsed-time
                  :viewer :none)
            result (analysis/analyse-domain plan d)]
        (is (contains? result :extract))
        (is (domain/domain-extract? (:extract result)))))))

;; Tests for domain-builder and related utilities.
;; Validates automated benchmark running across parameter spaces
;; with adaptive time estimation and progress reporting.

(deftest cartesian-product-test
  ;; Tests for internal cartesian product helper.
  ;; Validates generation of all axis value combinations.
  (testing "cartesian-product"
    (testing "returns single empty map for empty axes"
      (is (= [{}] (#'builder/cartesian-product {}))))
    (testing "generates all combinations for single axis"
      (is (= [{:n 1} {:n 2} {:n 3}]
             (#'builder/cartesian-product {:n [1 2 3]}))))
    (testing "generates cartesian product for two axes"
      (let [result (#'builder/cartesian-product {:n [1 2] :m [10 20]})]
        (is (= 4 (count result)))
        (is (= #{:n :m} (set (keys (first result)))))
        (is (= #{[1 10] [1 20] [2 10] [2 20]}
               (set (map (juxt :n :m) result))))))
    (testing "generates cartesian product for three axes"
      (let [result (#'builder/cartesian-product {:a [1] :b [2 3] :c [4 5]})]
        (is (= 4 (count result)))
        (is (every? #(= #{:a :b :c} (set (keys %))) result))))))

(deftest dot-reporter-test
  ;; Tests for dot reporter protocol implementation.
  ;; Validates progress reporting interface.
  (testing "dot-reporter"
    (testing "creates DotReporter instance"
      (let [reporter (builder/dot-reporter)]
        (is (satisfies? builder/DomainBuilderReporter reporter))))
    (testing "report-start prints impl name and run count"
      (let [reporter (builder/dot-reporter)
            output (with-out-str (builder/report-start reporter :test 5))]
        (is (= "test (5 runs): " output))))
    (testing "report-run prints a dot"
      (let [reporter (builder/dot-reporter)
            output (with-out-str (builder/report-run reporter :test {:n 100} 0))]
        (is (= "." output))))
    (testing "report-end prints newline"
      (let [reporter (builder/dot-reporter)
            output (with-out-str (builder/report-end reporter :test))]
        (is (= "\n" output))))))

(deftest simplified-impl-map?-test
  ;; Tests for simplified implementation map detection.
  ;; Validates recognition of {impl-key Measured} form vs full impl-spec form.
  (testing "simplified-impl-map?"
    (testing "returns true for map with Measured values"
      (let [m (measured/expr (+ 1 2))]
        (is (true? (#'builder/simplified-impl-map? {:impl-a m})))))
    (testing "returns true for map with multiple Measured values"
      (let [m1 (measured/expr (+ 1 2))
            m2 (measured/expr (* 3 4))]
        (is (true? (#'builder/simplified-impl-map? {:impl-a m1 :impl-b m2})))))
    (testing "returns false for full impl-spec form"
      (let [m (measured/expr (+ 1 2))]
        (is (not (#'builder/simplified-impl-map?
                  {:impl-a {:measured m :args-builder (constantly (fn [] [1 2]))}})))))
    (testing "returns falsy for empty map"
      (is (not (#'builder/simplified-impl-map? {}))))
    (testing "returns falsy for non-map"
      (is (not (#'builder/simplified-impl-map? [(measured/expr (+ 1 2))]))))
    (testing "returns falsy for nil"
      (is (not (#'builder/simplified-impl-map? nil))))))

(deftest normalize-implementations-test
  ;; Tests for implementation map normalization.
  ;; Validates conversion from simplified to full impl-spec form.
  (testing "normalize-implementations"
    (testing "converts single Measured to impl-spec"
      (let [m (measured/expr (+ 1 2))
            result (#'builder/normalize-implementations {:impl-a m})]
        (is (= #{:impl-a} (set (keys result))))
        (is (= m (:measured (:impl-a result))))
        (is (fn? (:args-builder (:impl-a result))))))
    (testing "args-builder returns measured's args-fn"
      (let [m (measured/expr (+ 1 2))
            result (#'builder/normalize-implementations {:impl-a m})
            args-builder (:args-builder (:impl-a result))]
        ;; args-builder should return the same args-fn regardless of coord
        (is (= (args-builder {}) (args-builder {:n 100})))))
    (testing "preserves multiple implementations"
      (let [m1 (measured/expr (+ 1 2))
            m2 (measured/expr (* 3 4))
            result (#'builder/normalize-implementations {:impl-a m1 :impl-b m2})]
        (is (= #{:impl-a :impl-b} (set (keys result))))
        (is (= m1 (:measured (:impl-a result))))
        (is (= m2 (:measured (:impl-b result))))))))
