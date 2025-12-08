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
    (testing "returns coordinate-value pairs for all runs"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
               {:coord {:n 200}
                :data (mock-bench-result {:elapsed-time {:mean 2.0}})})]
        (is (= [[{:n 100} 1.0] [{:n 200} 2.0]]
               (domain/extract d [:stats :elapsed-time :mean])))))
    (testing "preserves run order"
      (let [d (domain/domain
               {:coord {:n 300}
                :data (mock-bench-result {:elapsed-time {:mean 3.0}})}
               {:coord {:n 100}
                :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
               {:coord {:n 200}
                :data (mock-bench-result {:elapsed-time {:mean 2.0}})})]
        (is (= [[{:n 300} 3.0] [{:n 100} 1.0] [{:n 200} 2.0]]
               (domain/extract d [:stats :elapsed-time :mean])))))
    (testing "returns nil for missing metrics"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
               {:coord {:n 200}
                :data (mock-bench-result {:other-metric {:mean 2.0}})})]
        (is (= [[{:n 100} 1.0] [{:n 200} nil]]
               (domain/extract d [:stats :elapsed-time :mean])))))
    (testing "returns nil for missing value-key"
      (let [d (domain/domain
               {:coord :a
                :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
               {:coord :b
                :data (mock-bench-result {:elapsed-time {:variance 0.5}})})]
        (is (= [[:a 1.0] [:b nil]]
               (domain/extract d [:stats :elapsed-time :mean])))))
    (testing "handles keyword coordinates"
      (let [d (domain/domain
               {:coord :baseline
                :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
               {:coord :optimized
                :data (mock-bench-result {:elapsed-time {:mean 0.5}})})]
        (is (= [[:baseline 1.0] [:optimized 0.5]]
               (domain/extract d [:stats :elapsed-time :mean])))))
    (testing "returns empty vector for empty domain"
      (is (= [] (domain/extract (domain/domain)
                                [:stats :elapsed-time :mean]))))
    (testing "extracts different value-keys"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-bench-result
                       {:elapsed-time {:mean 1.0 :variance 0.1}})})]
        (is (= [[{:n 100} 1.0]]
               (domain/extract d [:stats :elapsed-time :mean])))
        (is (= [[{:n 100} 0.1]]
               (domain/extract d [:stats :elapsed-time :variance])))))))

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
    (testing "groups runs by axis key value"
      (let [d       (domain/domain
                     {:coord {:n 100 :impl :foo} :data sample-data}
                     {:coord {:n 200 :impl :foo} :data sample-data-2}
                     {:coord {:n 100 :impl :bar} :data {:third "result"}})
            grouped (domain/group-by-axis d :impl)]
        (is (= #{:foo :bar} (set (keys grouped))))
        (is (domain/domain? (get grouped :foo)))
        (is (= 2 (count (:runs (get grouped :foo)))))
        (is (= 1 (count (:runs (get grouped :bar)))))))
    (testing "groups keyword coords under nil"
      (let [d       (domain/domain
                     {:coord :baseline :data sample-data}
                     {:coord {:n 100 :impl :foo} :data sample-data-2})
            grouped (domain/group-by-axis d :impl)]
        (is (= #{:foo nil} (set (keys grouped))))
        (is (= 1 (count (:runs (get grouped nil)))))
        (is (= :baseline (-> grouped (get nil) :runs first :coord)))))
    (testing "groups runs missing axis key under nil"
      (let [d       (domain/domain
                     {:coord {:n 100} :data sample-data}
                     {:coord {:n 100 :impl :foo} :data sample-data-2})
            grouped (domain/group-by-axis d :impl)]
        (is (= #{:foo nil} (set (keys grouped))))
        (is (= {:n 100} (-> grouped (get nil) :runs first :coord)))))
    (testing "returns empty map for empty domain"
      (is (= {} (domain/group-by-axis (domain/domain) :impl))))
    (testing "preserves run order within groups"
      (let [d       (domain/domain
                     {:coord {:n 300 :impl :foo} :data sample-data}
                     {:coord {:n 100 :impl :foo} :data sample-data}
                     {:coord {:n 200 :impl :foo} :data sample-data})
            grouped (domain/group-by-axis d :impl)
            coords  (mapv :coord (:runs (get grouped :foo)))]
        (is (= [{:n 300 :impl :foo}
                {:n 100 :impl :foo}
                {:n 200 :impl :foo}]
               coords))))
    (testing "each group is a valid domain"
      (let [d       (domain/domain
                     {:coord {:n 100 :impl :foo} :data sample-data}
                     {:coord {:n 100 :impl :bar} :data sample-data-2})
            grouped (domain/group-by-axis d :impl)]
        (doseq [[_ sub-domain] grouped]
          (is (domain/domain? sub-domain)))))))

;; Tests for domain compare-by function.
;; Validates comparing metric values across axis dimensions, producing
;; structured output for analysis.

(deftest compare-by-test
  (testing "compare-by"
    (testing "returns structured comparison data"
      (let [d      (domain/domain
                    {:coord {:n 100 :impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                    {:coord {:n 100 :impl :bar}
                     :data (mock-bench-result {:elapsed-time {:mean 2.0}})})
            result (domain/compare-by d :impl [:stats :elapsed-time :mean])]
        (is (= :impl (:axis result)))
        (is (= [:stats :elapsed-time :mean] (:metric result)))
        (is (map? (:groups result)))))
    (testing "groups contain coord and value"
      (let [d      (domain/domain
                    {:coord {:n 100 :impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 1.5}})})
            result (domain/compare-by d :impl [:stats :elapsed-time :mean])
            entry  (first (get-in result [:groups :foo]))]
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
        (is (= 2 (count (get-in result [:groups :foo]))))
        (is (= 1 (count (get-in result [:groups :bar]))))
        (is (= [1.0 2.0] (mapv :value (get-in result [:groups :foo]))))
        (is (= [3.0] (mapv :value (get-in result [:groups :bar]))))))
    (testing "handles missing metrics with nil values"
      (let [d      (domain/domain
                    {:coord {:impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                    {:coord {:impl :bar}
                     :data (mock-bench-result {:other-metric {:mean 2.0}})})
            result (domain/compare-by d :impl [:stats :elapsed-time :mean])]
        (is (= 1.0 (:value (first (get-in result [:groups :foo])))))
        (is (nil? (:value (first (get-in result [:groups :bar])))))))
    (testing "groups keyword coords under nil"
      (let [d      (domain/domain
                    {:coord :baseline
                     :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                    {:coord {:impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 2.0}})})
            result (domain/compare-by d :impl [:stats :elapsed-time :mean])]
        (is (contains? (:groups result) nil))
        (is (= :baseline (:coord (first (get-in result [:groups nil])))))))
    (testing "returns empty groups for empty domain"
      (let [result (domain/compare-by (domain/domain) :impl
                                      [:stats :elapsed-time :mean])]
        (is (= :impl (:axis result)))
        (is (= {} (:groups result)))))
    (testing "preserves run order within groups"
      (let [d      (domain/domain
                    {:coord {:n 300 :impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 3.0}})}
                    {:coord {:n 100 :impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 1.0}})}
                    {:coord {:n 200 :impl :foo}
                     :data (mock-bench-result {:elapsed-time {:mean 2.0}})})
            result (domain/compare-by d :impl [:stats :elapsed-time :mean])
            values (mapv :value (get-in result [:groups :foo]))]
        (is (= [3.0 1.0 2.0] values))))))
