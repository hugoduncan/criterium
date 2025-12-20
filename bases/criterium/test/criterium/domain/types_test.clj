(ns criterium.domain.types-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.domain :as domain]
   [criterium.domain.test-util :refer [sample-data sample-data-2]]))

;; Tests for domain type, predicates, and construction functions.
;; Validates the core domain data structure that holds multiple
;; benchmark runs indexed by coordinates.

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
