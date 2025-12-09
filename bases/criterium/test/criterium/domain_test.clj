(ns criterium.domain-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.collect-plan :as collect-plan]
   [criterium.domain :as domain]))

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
  (testing "domain-extract?"
    (testing "returns true for valid domain-extract result"
      (is (true? (domain/domain-extract?
                  {:type :criterium/domain-extract
                   :metric [:stats :elapsed-time :mean]
                   :data [[{:n 100} 1.0]]}))))
    (testing "returns true for empty data"
      (is (true? (domain/domain-extract?
                  {:type :criterium/domain-extract
                   :metric [:stats :elapsed-time :mean]
                   :data []}))))
    (testing "returns false for wrong type"
      (is (false? (domain/domain-extract?
                   {:type :other :metric [] :data []}))))
    (testing "returns false for missing :metric"
      (is (false? (domain/domain-extract?
                   {:type :criterium/domain-extract :data []}))))
    (testing "returns false for missing :data"
      (is (false? (domain/domain-extract?
                   {:type :criterium/domain-extract :metric []}))))
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
            d   (domain/domain run)]
        (is (domain/domain? d))
        (is (= [run] (:runs d)))))
    (testing "with multiple runs creates domain containing all runs"
      (let [run1 {:coord {:n 100} :data sample-data}
            run2 {:coord {:n 1000} :data sample-data-2}
            d    (domain/domain run1 run2)]
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
            d    (apply domain/domain runs)]
        (is (= [100 200 300 400 500]
               (mapv #(-> % :coord :n) (:runs d))))))))

;; Tests for domain accumulation functions (add-run, remove-run).
;; Validates adding and removing runs while maintaining immutability
;; and handling duplicate coordinates correctly.

(deftest add-run-test
  (testing "add-run"
    (testing "adds run to empty domain"
      (let [d  (domain/domain)
            d2 (domain/add-run d :baseline sample-data)]
        (is (domain/domain? d2))
        (is (= 1 (count (:runs d2))))
        (is (= {:coord :baseline :data sample-data} (first (:runs d2))))))
    (testing "adds run with map coord"
      (let [d  (domain/domain)
            d2 (domain/add-run d {:n 100} sample-data)]
        (is (= {:n 100} (-> d2 :runs first :coord)))))
    (testing "appends to existing runs"
      (let [d  (domain/domain {:coord :a :data sample-data})
            d2 (domain/add-run d :b sample-data-2)]
        (is (= 2 (count (:runs d2))))
        (is (= [:a :b] (mapv :coord (:runs d2))))))
    (testing "replaces run with same keyword coord"
      (let [d  (domain/domain {:coord :baseline :data sample-data})
            d2 (domain/add-run d :baseline sample-data-2)]
        (is (= 1 (count (:runs d2))))
        (is (= sample-data-2 (-> d2 :runs first :data)))))
    (testing "replaces run with same map coord"
      (let [d  (domain/domain {:coord {:n 100} :data sample-data})
            d2 (domain/add-run d {:n 100} sample-data-2)]
        (is (= 1 (count (:runs d2))))
        (is (= sample-data-2 (-> d2 :runs first :data)))))
    (testing "preserves position when replacing"
      (let [d  (-> (domain/domain)
                   (domain/add-run :a sample-data)
                   (domain/add-run :b sample-data)
                   (domain/add-run :c sample-data))
            d2 (domain/add-run d :b sample-data-2)]
        (is (= [:a :b :c] (mapv :coord (:runs d2))))
        (is (= sample-data-2 (-> d2 :runs second :data)))))
    (testing "returns new domain (immutable)"
      (let [d  (domain/domain)
            d2 (domain/add-run d :x sample-data)]
        (is (= 0 (count (:runs d))))
        (is (= 1 (count (:runs d2))))))))

(deftest remove-run-test
  (testing "remove-run"
    (testing "removes run by keyword coord"
      (let [d  (domain/domain {:coord :baseline :data sample-data})
            d2 (domain/remove-run d :baseline)]
        (is (domain/domain? d2))
        (is (= 0 (count (:runs d2))))))
    (testing "removes run by map coord"
      (let [d  (domain/domain {:coord {:n 100} :data sample-data})
            d2 (domain/remove-run d {:n 100})]
        (is (= 0 (count (:runs d2))))))
    (testing "preserves other runs"
      (let [d  (-> (domain/domain)
                   (domain/add-run :a sample-data)
                   (domain/add-run :b sample-data)
                   (domain/add-run :c sample-data))
            d2 (domain/remove-run d :b)]
        (is (= 2 (count (:runs d2))))
        (is (= [:a :c] (mapv :coord (:runs d2))))))
    (testing "returns unchanged domain when coord not found"
      (let [d  (domain/domain {:coord :a :data sample-data})
            d2 (domain/remove-run d :nonexistent)]
        (is (= 1 (count (:runs d2))))
        (is (= :a (-> d2 :runs first :coord)))))
    (testing "returns new domain (immutable)"
      (let [d  (domain/domain {:coord :x :data sample-data})
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
              d    (domain/domain run1 run2)]
          (is (= [run1 run2] (domain/runs d)))))
      (testing "returns empty vector for empty domain"
        (is (= [] (domain/runs (domain/domain)))))
      (testing "preserves run order"
        (let [runs (mapv #(hash-map :coord {:n %} :data sample-data)
                         [100 200 300])
              d    (apply domain/domain runs)]
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

;; Tests for domain analysis function extract.
;; Validates extracting metric values across runs with coordinate-value pairs,
;; handling missing metrics, preserving order, and applying transforms.

(defn mock-bench-result
  "Create a mock bench result with proper structure for stats-value.
  stats-data is a map of {metric-id {value-key value}}."
  [stats-data]
  {:stats {:type        :criterium/stats
           :transform   collect-plan/identity-transforms
           :stats       stats-data
           :metrics-defs {}
           :batch-size  1
           :source-id   nil
           :outliers-id nil}})

(deftest extract-test
  (testing "extract"
    (testing "returns a domain-extract result"
      (let [d      (domain/domain
                    {:coord {:n 100}
                     :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
            result (domain/extract d [:stats :elapsed-time :mean])]
        (is (domain/domain-extract? result))
        (is (= :criterium/domain-extract (:type result)))
        (is (= [:stats :elapsed-time :mean] (:metric result)))))
    (testing "contains coordinate-value pairs in :data"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
               {:coord {:n 200}
                :data (mock-bench-result {:elapsed-time {:mean 2.0}})})]
        (is (= [[{:n 100} 1.0] [{:n 200} 2.0]]
               (:data (domain/extract d [:stats :elapsed-time :mean]))))))
    (testing "preserves run order"
      (let [d (domain/domain
               {:coord {:n 300}
                :data (mock-bench-result {:elapsed-time {:mean 3.0}})}
               {:coord {:n 100}
                :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
               {:coord {:n 200}
                :data (mock-bench-result {:elapsed-time {:mean 2.0}})})]
        (is (= [[{:n 300} 3.0] [{:n 100} 1.0] [{:n 200} 2.0]]
               (:data (domain/extract d [:stats :elapsed-time :mean]))))))
    (testing "returns nil for missing metrics"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
               {:coord {:n 200}
                :data (mock-bench-result {:other-metric {:mean 2.0}})})]
        (is (= [[{:n 100} 1.0] [{:n 200} nil]]
               (:data (domain/extract d [:stats :elapsed-time :mean]))))))
    (testing "returns nil for missing value-key"
      (let [d (domain/domain
               {:coord :a
                :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
               {:coord :b
                :data (mock-bench-result {:elapsed-time {:variance 0.5}})})]
        (is (= [[:a 1.0] [:b nil]]
               (:data (domain/extract d [:stats :elapsed-time :mean]))))))
    (testing "handles keyword coordinates"
      (let [d (domain/domain
               {:coord :baseline
                :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
               {:coord :optimized
                :data (mock-bench-result {:elapsed-time {:mean 0.5}})})]
        (is (= [[:baseline 1.0] [:optimized 0.5]]
               (:data (domain/extract d [:stats :elapsed-time :mean]))))))
    (testing "returns empty vector in :data for empty domain"
      (let [result (domain/extract (domain/domain)
                                   [:stats :elapsed-time :mean])]
        (is (domain/domain-extract? result))
        (is (= [] (:data result)))))
    (testing "extracts different value-keys"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-bench-result
                       {:elapsed-time {:mean 1.0 :variance 0.1}})})]
        (is (= [[{:n 100} 1.0]]
               (:data (domain/extract d [:stats :elapsed-time :mean]))))
        (is (= [[{:n 100} 0.1]]
               (:data (domain/extract d [:stats :elapsed-time :variance]))))))))

;; Tests for domain select function.
;; Validates filtering domain to sub-domain by partial coordinate match,
;; returning a new domain with matching runs.

(deftest select-test
  (testing "select"
    (testing "filters by keyword coord"
      (let [d  (domain/domain {:coord :a :data sample-data}
                              {:coord :b :data sample-data-2})
            d2 (domain/select d :a)]
        (is (domain/domain? d2))
        (is (= 1 (count (:runs d2))))
        (is (= :a (-> d2 :runs first :coord)))))
    (testing "filters by exact map coord"
      (let [d  (domain/domain {:coord {:n 100} :data sample-data}
                              {:coord {:n 200} :data sample-data-2})
            d2 (domain/select d {:n 100})]
        (is (domain/domain? d2))
        (is (= [{:coord {:n 100} :data sample-data}] (:runs d2)))))
    (testing "filters by partial map coord"
      (let [d  (domain/domain {:coord {:n 100} :data sample-data}
                              {:coord {:n 100 :impl :foo} :data sample-data-2}
                              {:coord {:n 200} :data {:third "result"}})
            d2 (domain/select d {:n 100})]
        (is (= 2 (count (:runs d2))))
        (is (= [{:coord {:n 100} :data sample-data}
                {:coord {:n 100 :impl :foo} :data sample-data-2}]
               (:runs d2)))))
    (testing "filters by multiple partial coord keys"
      (let [d  (domain/domain {:coord {:n 100 :impl :foo} :data sample-data}
                              {:coord {:n 100 :impl :bar} :data sample-data-2}
                              {:coord {:n 200 :impl :foo} :data {:third "result"}})
            d2 (domain/select d {:impl :foo})]
        (is (= 2 (count (:runs d2))))
        (is (= [{:coord {:n 100 :impl :foo} :data sample-data}
                {:coord {:n 200 :impl :foo} :data {:third "result"}}]
               (:runs d2)))))
    (testing "returns empty domain when no match"
      (let [d  (domain/domain {:coord :a :data sample-data})
            d2 (domain/select d :nonexistent)]
        (is (domain/domain? d2))
        (is (= [] (:runs d2)))))
    (testing "preserves run order"
      (let [runs [(hash-map :coord {:n 100 :impl :foo} :data sample-data)
                  (hash-map :coord {:n 300 :impl :foo} :data sample-data)
                  (hash-map :coord {:n 200 :impl :foo} :data sample-data)]
            d    (apply domain/domain
                        (concat runs [{:coord {:n 100 :impl :bar} :data sample-data}]))
            d2   (domain/select d {:impl :foo})]
        (is (= runs (:runs d2)))))
    (testing "returns new domain (immutable)"
      (let [d  (domain/domain {:coord :a :data sample-data}
                              {:coord :b :data sample-data-2})
            d2 (domain/select d :a)]
        (is (= 2 (count (:runs d))))
        (is (= 1 (count (:runs d2))))))
    (testing "does not match keyword coord with map partial"
      (let [d  (domain/domain {:coord :baseline :data sample-data})
            d2 (domain/select d {:n 100})]
        (is (= [] (:runs d2)))))))

;; Tests for domain group-by-axis function.
;; Validates partitioning runs by axis key values, returning a map
;; of axis-value to sub-domain.

(deftest group-by-axis-test
  (testing "group-by-axis"
    (testing "returns a domain-grouped result"
      (let [d      (domain/domain
                    {:coord {:n 100 :impl :foo} :data sample-data})
            result (domain/group-by-axis d :impl)]
        (is (domain/domain-grouped? result))
        (is (= :criterium/domain-grouped (:type result)))
        (is (= :impl (:axis result)))))
    (testing "groups runs by axis key value"
      (let [d       (domain/domain
                     {:coord {:n 100 :impl :foo} :data sample-data}
                     {:coord {:n 200 :impl :foo} :data sample-data-2}
                     {:coord {:n 100 :impl :bar} :data {:third "result"}})
            grouped (:data (domain/group-by-axis d :impl))]
        (is (= #{:foo :bar} (set (keys grouped))))
        (is (domain/domain? (get grouped :foo)))
        (is (= 2 (count (:runs (get grouped :foo)))))
        (is (= 1 (count (:runs (get grouped :bar)))))))
    (testing "groups keyword coords under nil"
      (let [d       (domain/domain
                     {:coord :baseline :data sample-data}
                     {:coord {:n 100 :impl :foo} :data sample-data-2})
            grouped (:data (domain/group-by-axis d :impl))]
        (is (= #{:foo nil} (set (keys grouped))))
        (is (= 1 (count (:runs (get grouped nil)))))
        (is (= :baseline (-> grouped (get nil) :runs first :coord)))))
    (testing "groups runs missing axis key under nil"
      (let [d       (domain/domain
                     {:coord {:n 100} :data sample-data}
                     {:coord {:n 100 :impl :foo} :data sample-data-2})
            grouped (:data (domain/group-by-axis d :impl))]
        (is (= #{:foo nil} (set (keys grouped))))
        (is (= {:n 100} (-> grouped (get nil) :runs first :coord)))))
    (testing "returns empty map in :data for empty domain"
      (let [result (domain/group-by-axis (domain/domain) :impl)]
        (is (domain/domain-grouped? result))
        (is (= {} (:data result)))))
    (testing "preserves run order within groups"
      (let [d       (domain/domain
                     {:coord {:n 300 :impl :foo} :data sample-data}
                     {:coord {:n 100 :impl :foo} :data sample-data}
                     {:coord {:n 200 :impl :foo} :data sample-data})
            grouped (:data (domain/group-by-axis d :impl))
            coords  (mapv :coord (:runs (get grouped :foo)))]
        (is (= [{:n 300 :impl :foo}
                {:n 100 :impl :foo}
                {:n 200 :impl :foo}]
               coords))))
    (testing "each group is a valid domain"
      (let [d       (domain/domain
                     {:coord {:n 100 :impl :foo} :data sample-data}
                     {:coord {:n 100 :impl :bar} :data sample-data-2})
            grouped (:data (domain/group-by-axis d :impl))]
        (doseq [[_ sub-domain] grouped]
          (is (domain/domain? sub-domain)))))))

;; Tests for domain compare-by function.
;; Validates comparing metric values across axis dimensions, producing
;; structured output for analysis.

(deftest compare-by-test
  (testing "compare-by"
    (testing "returns a domain-comparison result"
      (let [d      (domain/domain
                    {:coord {:n 100 :impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                    {:coord {:n 100 :impl :bar}
                     :data (mock-bench-result {:elapsed-time {:mean 2.0}})})
            result (domain/compare-by d :impl [:stats :elapsed-time :mean])]
        (is (domain/domain-comparison? result))
        (is (= :criterium/domain-comparison (:type result)))
        (is (= :impl (:axis result)))
        (is (= [:stats :elapsed-time :mean] (:metric result)))
        (is (map? (:data result)))))
    (testing "data entries contain coord and value"
      (let [d      (domain/domain
                    {:coord {:n 100 :impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 1.5}})})
            result (domain/compare-by d :impl [:stats :elapsed-time :mean])
            entry  (first (get-in result [:data :foo]))]
        (is (= {:n 100 :impl :foo} (:coord entry)))
        (is (= 1.5 (:value entry)))))
    (testing "groups runs by axis value"
      (let [d      (domain/domain
                    {:coord {:n 100 :impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                    {:coord {:n 200 :impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 2.0}})}
                    {:coord {:n 100 :impl :bar}
                     :data (mock-bench-result {:elapsed-time {:mean 3.0}})})
            result (domain/compare-by d :impl [:stats :elapsed-time :mean])]
        (is (= 2 (count (get-in result [:data :foo]))))
        (is (= 1 (count (get-in result [:data :bar]))))
        (is (= [1.0 2.0] (mapv :value (get-in result [:data :foo]))))
        (is (= [3.0] (mapv :value (get-in result [:data :bar]))))))
    (testing "handles missing metrics with nil values"
      (let [d      (domain/domain
                    {:coord {:impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                    {:coord {:impl :bar}
                     :data (mock-bench-result {:other-metric {:mean 2.0}})})
            result (domain/compare-by d :impl [:stats :elapsed-time :mean])]
        (is (= 1.0 (:value (first (get-in result [:data :foo])))))
        (is (nil? (:value (first (get-in result [:data :bar])))))))
    (testing "groups keyword coords under nil"
      (let [d      (domain/domain
                    {:coord :baseline
                     :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                    {:coord {:impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 2.0}})})
            result (domain/compare-by d :impl [:stats :elapsed-time :mean])]
        (is (contains? (:data result) nil))
        (is (= :baseline (:coord (first (get-in result [:data nil])))))))
    (testing "returns empty :data for empty domain"
      (let [result (domain/compare-by (domain/domain) :impl
                                      [:stats :elapsed-time :mean])]
        (is (domain/domain-comparison? result))
        (is (= :impl (:axis result)))
        (is (= {} (:data result)))))
    (testing "preserves run order within groups"
      (let [d      (domain/domain
                    {:coord {:n 300 :impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 3.0}})}
                    {:coord {:n 100 :impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                    {:coord {:n 200 :impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 2.0}})})
            result (domain/compare-by d :impl [:stats :elapsed-time :mean])
            values (mapv :value (get-in result [:data :foo]))]
        (is (= [3.0 1.0 2.0] values))))))

;; Tests for input sequence generators.
;; Validates generation of sequences useful for scaling analysis,
;; including powers of 2, arbitrary powers, logarithmic ranges,
;; and linear ranges.

(deftest powers-of-2-test
  (testing "powers-of-2"
    (testing "generates powers from 0"
      (is (= [1 2 4 8 16] (domain/powers-of-2 0 4))))
    (testing "generates powers from non-zero exponent"
      (is (= [16 32 64 128 256] (domain/powers-of-2 4 8))))
    (testing "handles single value range"
      (is (= [8] (domain/powers-of-2 3 3))))
    (testing "generates large powers"
      (is (= [1024 2048 4096] (domain/powers-of-2 10 12))))))

(deftest powers-of-test
  (testing "powers-of"
    (testing "generates powers of 10"
      (is (= [10 100 1000 10000] (domain/powers-of 10 1 4))))
    (testing "generates powers of 3"
      (is (= [1 3 9 27 81] (domain/powers-of 3 0 4))))
    (testing "generates powers of 2 (same as powers-of-2)"
      (is (= [1 2 4 8] (domain/powers-of 2 0 3))))
    (testing "handles single value range"
      (is (= [100] (domain/powers-of 10 2 2))))))

(deftest log-range-test
  (testing "log-range"
    (testing "generates 4 points from 1 to 1000"
      (let [result (domain/log-range 1 1000 4)]
        (is (= 4 (count result)))
        (is (= 1 (first result)))
        (is (= 1000 (last result)))))
    (testing "generates 5 points from 10 to 10000"
      (let [result (domain/log-range 10 10000 5)]
        (is (= 5 (count result)))
        (is (= 10 (first result)))
        (is (= 10000 (last result)))))
    (testing "produces increasing values"
      (let [result (domain/log-range 1 1000 5)]
        (is (apply < result))))
    (testing "handles 2-point range"
      (is (= [10 1000] (domain/log-range 10 1000 2))))))

(deftest linear-range-test
  (testing "linear-range"
    (testing "generates evenly spaced values"
      (is (= [100 200 300 400 500] (domain/linear-range 100 500 5))))
    (testing "handles range starting at 0"
      (is (= [0 250 500 750 1000] (domain/linear-range 0 1000 5))))
    (testing "handles 2-point range"
      (is (= [100 1000] (domain/linear-range 100 1000 2))))
    (testing "produces increasing values"
      (let [result (domain/linear-range 10 1000 10)]
        (is (apply < result))))
    (testing "handles single point"
      (is (= [500] (domain/linear-range 500 500 1))))))

;; Tests for domain analysis pipeline functions.
;; Validates composable analysis transformers that operate on data-maps,
;; following the same pattern as criterium.analyse functions.

(deftest domain-extract-fn-test
  (testing "domain-extract-fn"
    (testing "returns a function"
      (is (fn? (domain/domain-extract-fn)))
      (is (fn? (domain/domain-extract-fn {}))))
    (testing "extracts metric from domain in data-map"
      (let [d       (domain/domain
                     {:coord {:n 100}
                      :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
            f       (domain/domain-extract-fn
                     {:id :mean :metric-path [:stats :elapsed-time :mean]})
            result  (f {:domain d})]
        (is (contains? result :domain))
        (is (contains? result :mean))
        (is (domain/domain-extract? (:mean result)))))
    (testing "uses default :id when not specified"
      (let [d      (domain/domain
                    {:coord :a
                     :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
            f      (domain/domain-extract-fn
                    {:metric-path [:stats :elapsed-time :mean]})
            result (f {:domain d})]
        (is (contains? result :extract))))
    (testing "uses custom :domain-id"
      (let [d      (domain/domain
                    {:coord :a
                     :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
            f      (domain/domain-extract-fn
                    {:id :mean
                     :domain-id :my-domain
                     :metric-path [:stats :elapsed-time :mean]})
            result (f {:my-domain d})]
        (is (contains? result :mean))
        (is (domain/domain-extract? (:mean result)))))
    (testing "preserves other keys in data-map"
      (let [d      (domain/domain
                    {:coord :a
                     :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
            f      (domain/domain-extract-fn
                    {:id :mean :metric-path [:stats :elapsed-time :mean]})
            result (f {:domain d :other-key "value"})]
        (is (= "value" (:other-key result)))))))

(deftest domain-group-by-fn-test
  (testing "domain-group-by-fn"
    (testing "returns a function"
      (is (fn? (domain/domain-group-by-fn)))
      (is (fn? (domain/domain-group-by-fn {}))))
    (testing "groups domain by axis in data-map"
      (let [d      (domain/domain
                    {:coord {:n 100 :impl :foo} :data sample-data}
                    {:coord {:n 200 :impl :bar} :data sample-data-2})
            f      (domain/domain-group-by-fn {:id :by-impl :axis-key :impl})
            result (f {:domain d})]
        (is (contains? result :domain))
        (is (contains? result :by-impl))
        (is (domain/domain-grouped? (:by-impl result)))))
    (testing "uses default :id when not specified"
      (let [d      (domain/domain
                    {:coord {:impl :foo} :data sample-data})
            f      (domain/domain-group-by-fn {:axis-key :impl})
            result (f {:domain d})]
        (is (contains? result :grouped))))
    (testing "uses custom :domain-id"
      (let [d      (domain/domain
                    {:coord {:impl :foo} :data sample-data})
            f      (domain/domain-group-by-fn
                    {:id :by-impl :domain-id :src :axis-key :impl})
            result (f {:src d})]
        (is (contains? result :by-impl))
        (is (domain/domain-grouped? (:by-impl result)))))
    (testing "preserves other keys in data-map"
      (let [d      (domain/domain
                    {:coord {:impl :foo} :data sample-data})
            f      (domain/domain-group-by-fn {:id :by-impl :axis-key :impl})
            result (f {:domain d :config {:some "config"}})]
        (is (= {:some "config"} (:config result)))))))

(deftest domain-compare-fn-test
  (testing "domain-compare-fn"
    (testing "returns a function"
      (is (fn? (domain/domain-compare-fn)))
      (is (fn? (domain/domain-compare-fn {}))))
    (testing "compares metric across axis in data-map"
      (let [d      (domain/domain
                    {:coord {:impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                    {:coord {:impl :bar}
                     :data (mock-bench-result {:elapsed-time {:mean 2.0}})})
            f      (domain/domain-compare-fn
                    {:id :impl-time
                     :axis-key :impl
                     :metric-path [:stats :elapsed-time :mean]})
            result (f {:domain d})]
        (is (contains? result :domain))
        (is (contains? result :impl-time))
        (is (domain/domain-comparison? (:impl-time result)))))
    (testing "uses default :id when not specified"
      (let [d      (domain/domain
                    {:coord {:impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
            f      (domain/domain-compare-fn
                    {:axis-key :impl :metric-path [:stats :elapsed-time :mean]})
            result (f {:domain d})]
        (is (contains? result :comparison))))
    (testing "uses custom :domain-id"
      (let [d      (domain/domain
                    {:coord {:impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
            f      (domain/domain-compare-fn
                    {:id :cmp
                     :domain-id :source
                     :axis-key :impl
                     :metric-path [:stats :elapsed-time :mean]})
            result (f {:source d})]
        (is (contains? result :cmp))
        (is (domain/domain-comparison? (:cmp result)))))
    (testing "preserves other keys in data-map"
      (let [d      (domain/domain
                    {:coord {:impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
            f      (domain/domain-compare-fn
                    {:id :cmp :axis-key :impl
                     :metric-path [:stats :elapsed-time :mean]})
            result (f {:domain d :meta {:info "data"}})]
        (is (= {:info "data"} (:meta result)))))))

;; Tests for composing multiple pipeline functions.
;; Validates that pipeline functions can be composed together
;; to build complex analysis pipelines.

(deftest pipeline-composition-test
  (testing "pipeline composition"
    (testing "chains multiple extracts"
      (let [d      (domain/domain
                    {:coord {:n 100}
                     :data (mock-bench-result
                            {:elapsed-time {:mean 1.0 :variance 0.1}})})
            result (-> {:domain d}
                       ((domain/domain-extract-fn
                         {:id :mean
                          :metric-path [:stats :elapsed-time :mean]}))
                       ((domain/domain-extract-fn
                         {:id :var
                          :metric-path [:stats :elapsed-time :variance]})))]
        (is (contains? result :domain))
        (is (contains? result :mean))
        (is (contains? result :var))
        (is (domain/domain-extract? (:mean result)))
        (is (domain/domain-extract? (:var result)))))
    (testing "chains extract with group-by"
      (let [d      (domain/domain
                    {:coord {:n 100 :impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                    {:coord {:n 100 :impl :bar}
                     :data (mock-bench-result {:elapsed-time {:mean 2.0}})})
            result (-> {:domain d}
                       ((domain/domain-extract-fn
                         {:id :mean
                          :metric-path [:stats :elapsed-time :mean]}))
                       ((domain/domain-group-by-fn
                         {:id :by-impl :axis-key :impl})))]
        (is (domain/domain-extract? (:mean result)))
        (is (domain/domain-grouped? (:by-impl result)))))
    (testing "chains multiple analysis types"
      (let [d      (domain/domain
                    {:coord {:n 100 :impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                    {:coord {:n 200 :impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 2.0}})}
                    {:coord {:n 100 :impl :bar}
                     :data (mock-bench-result {:elapsed-time {:mean 1.5}})})
            result (-> {:domain d}
                       ((domain/domain-extract-fn
                         {:id :mean
                          :metric-path [:stats :elapsed-time :mean]}))
                       ((domain/domain-group-by-fn
                         {:id :by-impl :axis-key :impl}))
                       ((domain/domain-compare-fn
                         {:id :impl-time
                          :axis-key :impl
                          :metric-path [:stats :elapsed-time :mean]}))
                       ((domain/domain-compare-fn
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
  (testing "domain-regression?"
    (testing "returns true for valid domain-regression result"
      (is (true? (domain/domain-regression?
                  {:type     :criterium/domain-regression
                   :axis     :n
                   :models   [{:id :linear :label "O(n)" :r-squared 0.98}]
                   :best-fit :linear}))))
    (testing "returns true for empty models"
      (is (true? (domain/domain-regression?
                  {:type     :criterium/domain-regression
                   :axis     :n
                   :models   []
                   :best-fit nil}))))
    (testing "returns false for wrong type"
      (is (false? (domain/domain-regression?
                   {:type :other :axis :n :models [] :best-fit nil}))))
    (testing "returns false for missing :axis"
      (is (false? (domain/domain-regression?
                   {:type :criterium/domain-regression :models [] :best-fit nil}))))
    (testing "returns false for missing :models"
      (is (false? (domain/domain-regression?
                   {:type :criterium/domain-regression :axis :n :best-fit nil}))))
    (testing "returns false for missing :best-fit"
      (is (false? (domain/domain-regression?
                   {:type :criterium/domain-regression :axis :n :models []}))))
    (testing "returns false for non-map"
      (is (false? (domain/domain-regression? nil)))
      (is (false? (domain/domain-regression? "regression"))))))

(deftest fit-complexity-test
  (testing "fit-complexity"
    (testing "returns a domain-regression result"
      (let [extract {:type   :criterium/domain-extract
                     :metric [:stats :elapsed-time :mean]
                     :data   [[{:n 100} 100.0]
                              [{:n 200} 200.0]
                              [{:n 300} 300.0]]}
            result  (domain/fit-complexity extract :n)]
        (is (domain/domain-regression? result))
        (is (= :criterium/domain-regression (:type result)))
        (is (= :n (:axis result)))
        (is (= [:stats :elapsed-time :mean] (:metric result)))))
    (testing "identifies linear complexity with perfect fit"
      (let [extract {:type   :criterium/domain-extract
                     :metric [:stats :elapsed-time :mean]
                     :data   [[{:n 100} 100.0]
                              [{:n 200} 200.0]
                              [{:n 300} 300.0]
                              [{:n 400} 400.0]]}
            result  (domain/fit-complexity extract :n)
            linear  (first (filter #(= :linear (:id %)) (:models result)))]
        (is (= :linear (:best-fit result)))
        (is (> (:r-squared linear) 0.99))))
    (testing "identifies quadratic complexity"
      (let [extract {:type   :criterium/domain-extract
                     :metric [:stats :elapsed-time :mean]
                     :data   [[{:n 10} 100.0]
                              [{:n 20} 400.0]
                              [{:n 30} 900.0]
                              [{:n 40} 1600.0]]}
            result    (domain/fit-complexity extract :n)
            quadratic (first (filter #(= :quadratic (:id %)) (:models result)))]
        (is (= :quadratic (:best-fit result)))
        (is (> (:r-squared quadratic) 0.99))))
    (testing "filters out nil values"
      (let [extract {:type   :criterium/domain-extract
                     :metric [:stats :elapsed-time :mean]
                     :data   [[{:n 100} 100.0]
                              [{:n 200} nil]
                              [{:n 300} 300.0]]}
            result  (domain/fit-complexity extract :n)]
        (is (domain/domain-regression? result))
        (is (seq (:models result)))))
    (testing "filters out coordinates missing axis key"
      (let [extract {:type   :criterium/domain-extract
                     :metric [:stats :elapsed-time :mean]
                     :data   [[{:n 100} 100.0]
                              [{:m 200} 200.0]
                              [{:n 300} 300.0]]}
            result  (domain/fit-complexity extract :n)]
        (is (domain/domain-regression? result))
        (is (seq (:models result)))))
    (testing "filters out keyword coordinates"
      (let [extract {:type   :criterium/domain-extract
                     :metric [:stats :elapsed-time :mean]
                     :data   [[{:n 100} 100.0]
                              [:baseline 50.0]
                              [{:n 300} 300.0]]}
            result  (domain/fit-complexity extract :n)]
        (is (domain/domain-regression? result))
        (is (seq (:models result)))))
    (testing "returns empty models with insufficient data"
      (let [extract {:type   :criterium/domain-extract
                     :metric [:stats :elapsed-time :mean]
                     :data   [[{:n 100} 100.0]]}
            result  (domain/fit-complexity extract :n)]
        (is (domain/domain-regression? result))
        (is (empty? (:models result)))
        (is (nil? (:best-fit result)))))
    (testing "returns empty models for empty extract"
      (let [extract {:type   :criterium/domain-extract
                     :metric [:stats :elapsed-time :mean]
                     :data   []}
            result  (domain/fit-complexity extract :n)]
        (is (domain/domain-regression? result))
        (is (empty? (:models result)))))
    (testing "supports custom models"
      (let [extract {:type   :criterium/domain-extract
                     :metric [:stats :elapsed-time :mean]
                     :data   [[{:n 100} 100.0]
                              [{:n 200} 200.0]
                              [{:n 300} 300.0]]}
            models  {:cubic {:transform (fn [n] (* n n n))
                             :label     "O(n³)"}}
            result  (domain/fit-complexity extract :n models)]
        (is (domain/domain-regression? result))
        (is (= 1 (count (:models result))))
        (is (= :cubic (:id (first (:models result)))))))))

(deftest domain-regression-fn-test
  (testing "domain-regression-fn"
    (testing "returns a function"
      (is (fn? (domain/domain-regression-fn)))
      (is (fn? (domain/domain-regression-fn {}))))
    (testing "fits regression to extract in data-map"
      (let [extract {:type   :criterium/domain-extract
                     :metric [:stats :elapsed-time :mean]
                     :data   [[{:n 100} 100.0]
                              [{:n 200} 200.0]
                              [{:n 300} 300.0]]}
            f       (domain/domain-regression-fn {:id :scaling :axis :n})
            result  (f {:extract extract})]
        (is (contains? result :extract))
        (is (contains? result :scaling))
        (is (domain/domain-regression? (:scaling result)))))
    (testing "uses default :id when not specified"
      (let [extract {:type   :criterium/domain-extract
                     :metric [:stats :elapsed-time :mean]
                     :data   [[{:n 100} 100.0]
                              [{:n 200} 200.0]]}
            f       (domain/domain-regression-fn {:axis :n})
            result  (f {:extract extract})]
        (is (contains? result :regression))))
    (testing "uses custom :extract-id"
      (let [extract {:type   :criterium/domain-extract
                     :metric [:stats :elapsed-time :mean]
                     :data   [[{:n 100} 100.0]
                              [{:n 200} 200.0]]}
            f       (domain/domain-regression-fn
                     {:id :scaling :extract-id :my-extract :axis :n})
            result  (f {:my-extract extract})]
        (is (contains? result :scaling))
        (is (domain/domain-regression? (:scaling result)))))
    (testing "preserves other keys in data-map"
      (let [extract {:type   :criterium/domain-extract
                     :metric [:stats :elapsed-time :mean]
                     :data   [[{:n 100} 100.0]
                              [{:n 200} 200.0]]}
            f       (domain/domain-regression-fn {:id :scaling :axis :n})
            result  (f {:extract extract :other-key "value"})]
        (is (= "value" (:other-key result)))))
    (testing "composes with domain-extract-fn"
      (let [d      (domain/domain
                    {:coord {:n 100}
                     :data (mock-bench-result {:elapsed-time {:mean 100.0}})}
                    {:coord {:n 200}
                     :data (mock-bench-result {:elapsed-time {:mean 200.0}})}
                    {:coord {:n 300}
                     :data (mock-bench-result {:elapsed-time {:mean 300.0}})})
            result (-> {:domain d}
                       ((domain/domain-extract-fn
                         {:id          :extract
                          :metric-path [:stats :elapsed-time :mean]}))
                       ((domain/domain-regression-fn
                         {:id :scaling :axis :n})))]
        (is (domain/domain-extract? (:extract result)))
        (is (domain/domain-regression? (:scaling result)))
        (is (= :linear (:best-fit (:scaling result))))))))
