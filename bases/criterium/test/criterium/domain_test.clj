(ns criterium.domain-test
  (:require
   [clojure.test :refer [deftest is testing]]
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
