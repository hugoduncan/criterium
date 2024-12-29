(ns criterium.trigger-test
  "Tests for the criterium.trigger namespace.

  Verifies the core functionality of the trigger mechanism including:
  - Trigger creation and state initialization
  - Sample collection behavior
  - Extra data attachment to samples
  - Sample retrieval and reset behavior"
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [criterium.analyse :as analyse]
   [criterium.trigger :as trigger]
   [criterium.util.helpers :as util]))

(def ^:private test-extra-data
  "Sample extra data for testing"
  {:test-key "test-value"})

(def ^:dynamic *trigger*
  "Dynamic var for holding the current test trigger instance")

(defn trigger-fixture
  "Test fixture that provides a fresh trigger for each test"
  [f]
  (binding [*trigger* (trigger/trigger)]
    (f)))

(use-fixtures :each trigger-fixture)

(deftest trigger-initialization-test
  (testing "A new trigger is initialized"
    (let [t (trigger/trigger)]
      (testing "Initial trigger state"
        (is (zero? (:last-triggered @t)) "Last triggered should start at 0")
        (is (empty? (:samples @t)) "Samples should start empty")))))

(deftest trigger-firing-test
  (testing "A trigger"
    (testing "First fire only records timestamp"
      (trigger/fire! *trigger*)
      (is (pos? (:last-triggered @*trigger*)) "Should record first timestamp")
      (is (empty? (:samples @*trigger*)) "Should not collect sample on first fire")))

  (testing "Second fire creates first sample"
    (trigger/fire! *trigger*)
    (is (= 1 (count (:samples @*trigger*))) "Should collect first sample")
    (let [sample (first (:samples @*trigger*))]
      (is (contains? sample :elapsed-time) "Sample should contain elapsed time")
      (is (pos? (:elapsed-time sample)) "Elapsed time should be positive")))

  (testing "Additional fires add more samples"
    (trigger/fire! *trigger*)
    (is (= 2 (count (:samples @*trigger*))) "Should accumulate samples")))

(deftest extra-data-test
  "Test attaching extra data to samples"
  (testing "Samples include attached extra data"
    (trigger/fire! *trigger*)
    (trigger/fire! *trigger* test-extra-data)
    (let [sample (first (:samples @*trigger*))]
      (is (= test-extra-data (select-keys sample (keys test-extra-data)))
          "Extra data should be merged into sample"))))

(deftest samples-retrieval-test
  (testing "trigger with extra data"
    (testing "Sample map structure"
      (trigger/fire! *trigger*)
      (trigger/fire! *trigger*)
      (let [sampled (trigger/samples! *trigger*)]
        (is (= 1 (:batch-size sampled)) "Batch size should be 1 for triggers")
        (is (= 1 (:eval-count sampled)) "Eval count should match sample count")
        (is (map? (:metric->values sampled))
            "Samples should be returned as a map")
        (is (pos? (first (get-in sampled [:metric->values [:elapsed-time]])))
            "Samples should contain positive elapsed times"))))

  (testing "Trigger reset after sample retrieval"
    (let [_ (trigger/samples! *trigger*)]
      (is (zero? (:last-triggered @*trigger*)) "Trigger should reset timestamp")
      (is (empty? (:samples @*trigger*)) "Samples should be cleared"))))

(deftest benchmark-integration-test
  (testing "A trigger sample map"
    (testing "can be analyzed with benchmark functions"
      (trigger/fire! *trigger*)
      (trigger/fire! *trigger*)
      (trigger/fire! *trigger*)
      (let [samples-map (trigger/samples! *trigger*)
            analyzed    ((analyse/stats {})
                         {:data {:samples samples-map}})]
        (is (-> analyzed :data :stats) "Should contain statistical analysis")
        (is (pos? (get-in
                   (-> analyzed :data :stats util/stats)
                   [:elapsed-time :mean]))
            "Should calculate positive mean elapsed time")))))
