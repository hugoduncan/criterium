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
   [criterium.array :as arr]
   [criterium.sampler :as sampler]
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
        (is (zero? (-> t :state deref :last-triggered))
            "Last triggered should start at 0")
        (is (empty? (-> t :state deref :samples))
            "Samples should start empty")))))

(deftest trigger-firing-test
  (testing "A trigger"
    (testing "First fire only records timestamp"
      (trigger/fire! *trigger*)
      (is (pos? (-> *trigger* :state deref :last-triggered))
          "Should record first timestamp")
      (is (empty? (-> *trigger* :state deref :samples))
          "Should not collect sample on first fire")))

  (testing "Second fire creates first sample"
    (trigger/fire! *trigger*)
    (is (= 1 (count (-> *trigger* :state deref :samples)))
        "Should collect first sample")
    (let [sample (first (-> *trigger* :state deref :samples))]
      (is (contains? sample :elapsed-time) "Sample should contain elapsed time")
      (is (pos? (:elapsed-time sample)) "Elapsed time should be positive")))

  (testing "Additional fires add more samples"
    (trigger/fire! *trigger*)
    (is (= 2 (count (-> *trigger* :state deref :samples)))
        "Should accumulate samples")))

(deftest extra-data-test
  (testing "Test attaching extra data to samples"
    (testing "Samples include attached extra data"
      (trigger/fire! *trigger*)
      (trigger/fire! *trigger* test-extra-data)
      (let [sample (first (-> *trigger* :state deref :samples))]
        (is (= test-extra-data (select-keys sample (keys test-extra-data)))
            "Extra data should be merged into sample")))))

(deftest sampler-protocol-test
  (testing "trigger with extra data"
    (testing "Sample map structure"
      (trigger/fire! *trigger*)
      (trigger/fire! *trigger*)
      (let [samples-map (sampler/samples-map *trigger*)]
        (is (= 1 (:batch-size samples-map))
            "Batch size should be 1 for triggers")
        (is (= 1 (:eval-count samples-map))
            "Eval count should match sample count")
        (is (map? (:metric->values samples-map))
            "Samples should be returned as a map")
        (is (pos?
             (arr/first-double
              (get-in samples-map [:metric->values [:elapsed-time]])))
            "Samples should contain positive elapsed times"))))

  (testing "Trigger reset after sample retrieval"
    (sampler/reset-samples! *trigger*)
    (let [samples-map (sampler/samples-map *trigger*)]
      (is (zero? (arr/length
                  (get-in samples-map [:metric->values [:elapsed-time]])))
          "Samples should be cleared"))))

(deftest benchmark-integration-test
  (testing "A trigger sample map"
    (testing "can be analyzed with benchmark functions"
      (trigger/fire! *trigger*)
      (trigger/fire! *trigger*)
      (trigger/fire! *trigger*)
      (let [samples-map (sampler/samples-map *trigger*)
            analyzed    ((analyse/stats {})
                         {:samples samples-map})]
        (is (-> analyzed :stats) "Should contain statistical analysis")
        (is (pos? (get-in
                   (-> analyzed :stats util/stats)
                   [:elapsed-time :mean]))
            "Should calculate positive mean elapsed time")))))
