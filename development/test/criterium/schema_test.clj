(ns criterium.schema-test
  "Tests for malli schemas defined in criterium.schema.
  Verifies that schemas correctly accept valid data and reject invalid data
  with appropriate error messages."
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.schema :as schema]))

;;; Test fixtures - minimal valid data for each schema type

(def valid-transform
  {:sample-> identity
   :->sample identity})

(def valid-collection-map
  {:eval-count 100
   :elapsed-time 1000000
   :collections []
   :num-samples 10
   :batch-size 1
   :collector {}})

(def valid-data-entry-map
  {:type :criterium/test
   :transform valid-transform})

(def valid-collected-metrics-map
  {:type :criterium/collected-metrics
   :transform valid-transform
   :metric->values {}
   :elapsed-time 1000000
   :num-samples 10
   :batch-size 1
   :eval-count 100
   :metrics-defs {}
   :expr-value nil})

(def valid-metrics-samples-map
  {:type :criterium/metrics-samples
   :transform valid-transform
   :source-id :samples
   :metric->values {}
   :num-samples 10
   :batch-size 1
   :metrics-defs {}
   :expr-value nil})

(def valid-digest-samples-map
  {:type :criterium/digest
   :transform valid-transform
   :source-id :digest
   :metric->digest {}
   :metrics-defs {}
   :expr-value nil})

(def valid-quantiles-map
  {:type :criterium/quantiles
   :quantiles {}
   :metrics-defs {}
   :source-id :samples
   :transform valid-transform})

(def valid-outliers-map
  {:type :criterium/outliers
   :outliers {}
   :metrics-defs {}
   :source-id :samples
   :quantiles-id :quantiles
   :num-samples 10
   :transform valid-transform})

(def valid-stats-map
  {:type :criterium/stats
   :stats {}
   :metrics-defs {}
   :transform valid-transform
   :batch-size 1
   :source-id :samples
   :outliers-id :outliers})

(def valid-event-stats-map
  {:type :criterium/event-stats
   :event-stats {}
   :metrics-defs {}
   :transform valid-transform
   :batch-size 1
   :source-id :samples})

(def valid-histogram-map
  {:type :criterium/histogram
   :histograms {}
   :metrics-defs {}
   :transform valid-transform
   :batch-size 1
   :source-id :samples
   :outliers-id :outliers})

(def valid-outlier-significance-map
  {:type :criterium/outlier-significance
   :outlier-significance {}
   :metrics-defs {}
   :source-id :samples
   :outliers-id :outliers})

(def valid-bootstrap-map
  {:type :criterium/bootstrap
   :bootstrap {}
   :metrics-defs {}
   :transform valid-transform
   :batch-size 1
   :source-id :samples
   :outliers-id nil})

(def valid-allocation-trace
  {:type :criterium/allocation-trace
   :records []
   :thread-id 1
   :eval-count 100
   :elapsed-time 1000000})

(def valid-measured
  {:args-fn (fn [] [])
   :f identity})

(def valid-collector-config
  {:stages [:warmup :timing]
   :terminator :elapsed-time})

(def valid-collector
  {:f identity
   :x (fn [] nil)
   :length 10
   :metrics-defs {}})

(def valid-collect-plan
  {:scheme-type :one-shot})

(def valid-bench-plan
  {:collect-plan valid-collect-plan
   :collector-config valid-collector-config
   :viewer :print
   :return-value [:data]})

;;; Domain type fixtures

(def valid-run-map
  {:coord {:n 100}
   :data {:entry valid-data-entry-map}})

(def valid-domain-map
  {:type :criterium/domain
   :runs []})

(def valid-domain-extract-map
  {:type :criterium/domain-extract
   :metrics {}})

(def valid-domain-grouped-map
  {:type :criterium/domain-grouped
   :axis :impl
   :data {}})

(def valid-domain-comparison-map-single
  {:type :criterium/domain-comparison
   :axis :impl
   :metric [:stats :elapsed-time :mean]
   :data {}})

(def valid-domain-comparison-map-multi
  {:type :criterium/domain-comparison
   :axis :impl
   :metrics {:elapsed-time {:data []}}})

(def valid-domain-regression-map
  {:type :criterium/domain-regression
   :axis :n
   :regressions {}})

;;; Helper function tests

(deftest validate-test
  ;; Tests the validate helper function that wraps malli validation
  ;; with the criterium registry.
  (testing "validate"
    (testing "returns true for valid data"
      (is (true? (schema/validate schema/collection-map valid-collection-map))))
    (testing "returns false for invalid data"
      (is (false? (schema/validate schema/collection-map {}))))
    (testing "works with registry keywords"
      (is (true? (schema/validate :criterium/collection-map valid-collection-map))))))

(deftest explain-test
  ;; Tests the explain helper function that provides detailed error messages
  ;; for validation failures.
  (testing "explain"
    (testing "returns nil for valid data"
      (is (nil? (schema/explain schema/collection-map valid-collection-map))))
    (testing "returns explanation for invalid data"
      (let [result (schema/explain schema/collection-map {})]
        (is (some? result))
        (is (contains? result :errors))))
    (testing "errors identify missing keys"
      (let [result (schema/explain schema/collection-map {:eval-count 100})]
        (is (some? (:errors result)))))))

(deftest validator-test
  ;; Tests the validator helper function that creates reusable validator
  ;; functions for a schema.
  (testing "validator"
    (testing "creates a function"
      (is (fn? (schema/validator schema/collection-map))))
    (testing "returned function validates correctly"
      (let [v (schema/validator schema/collection-map)]
        (is (true? (v valid-collection-map)))
        (is (false? (v {})))))))

;;; Registry tests

(deftest registry-test
  ;; Tests that the registry contains all expected schema keys.
  (testing "registry"
    (testing "contains all data type schemas"
      (is (contains? schema/registry :criterium/transform-map))
      (is (contains? schema/registry :criterium/collection-map))
      (is (contains? schema/registry :criterium/data-entry-map))
      (is (contains? schema/registry :criterium/collected-metrics-map))
      (is (contains? schema/registry :criterium/metrics-samples-map))
      (is (contains? schema/registry :criterium/digest-samples-map))
      (is (contains? schema/registry :criterium/quantiles-map))
      (is (contains? schema/registry :criterium/outliers-map))
      (is (contains? schema/registry :criterium/stats-map))
      (is (contains? schema/registry :criterium/event-stats-map))
      (is (contains? schema/registry :criterium/histogram-map))
      (is (contains? schema/registry :criterium/outlier-significance-map))
      (is (contains? schema/registry :criterium/bootstrap-map))
      (is (contains? schema/registry :criterium/result-map))
      (is (contains? schema/registry :criterium/benchmark-map))
      (is (contains? schema/registry :criterium/allocation-trace)))
    (testing "contains union type schemas"
      (is (contains? schema/registry :criterium/generic-metrics-samples-map))
      (is (contains? schema/registry :criterium/generic-data-map)))
    (testing "contains function input schemas"
      (is (contains? schema/registry :criterium/measured))
      (is (contains? schema/registry :criterium/collector-config))
      (is (contains? schema/registry :criterium/collector))
      (is (contains? schema/registry :criterium/collect-plan))
      (is (contains? schema/registry :criterium/analyse-plan))
      (is (contains? schema/registry :criterium/view-plan))
      (is (contains? schema/registry :criterium/viewer))
      (is (contains? schema/registry :criterium/bench-plan))
      (is (contains? schema/registry :criterium/data-map)))
    (testing "contains domain type schemas"
      (is (contains? schema/registry :criterium/coord))
      (is (contains? schema/registry :criterium/run-map))
      (is (contains? schema/registry :criterium/domain-map))
      (is (contains? schema/registry :criterium/domain-extract-map))
      (is (contains? schema/registry :criterium/domain-grouped-map))
      (is (contains? schema/registry :criterium/domain-comparison-map))
      (is (contains? schema/registry :criterium/domain-regression-map)))))

;;; Base schema tests

(deftest transform-map-test
  ;; Tests the transform-map schema which defines the structure for
  ;; transform functions used to convert between sample and display values.
  (testing "transform-map"
    (testing "accepts valid transform with functions"
      (is (true? (schema/validate schema/transform-map valid-transform))))
    (testing "rejects missing :sample-> key"
      (is (false? (schema/validate schema/transform-map {:->sample identity}))))
    (testing "rejects missing :->sample key"
      (is (false? (schema/validate schema/transform-map {:sample-> identity}))))
    (testing "rejects non-function values"
      (is (false? (schema/validate schema/transform-map {:sample-> "not-fn"
                                                         :->sample identity}))))
    (testing "rejects non-map"
      (is (false? (schema/validate schema/transform-map nil)))
      (is (false? (schema/validate schema/transform-map []))))))

(deftest collection-map-test
  ;; Tests the collection-map schema which defines the structure returned
  ;; by collection operations.
  (testing "collection-map"
    (testing "accepts valid collection map"
      (is (true? (schema/validate schema/collection-map valid-collection-map))))
    (testing "accepts extra keys"
      (is (true? (schema/validate schema/collection-map
                                  (assoc valid-collection-map :extra "allowed")))))
    (testing "rejects zero eval-count"
      (is (false? (schema/validate schema/collection-map
                                   (assoc valid-collection-map :eval-count 0)))))
    (testing "rejects negative eval-count"
      (is (false? (schema/validate schema/collection-map
                                   (assoc valid-collection-map :eval-count -1)))))
    (testing "rejects negative elapsed-time"
      (is (false? (schema/validate schema/collection-map
                                   (assoc valid-collection-map :elapsed-time -1)))))
    (testing "rejects missing keys"
      (is (false? (schema/validate schema/collection-map {})))
      (is (false? (schema/validate schema/collection-map
                                   (dissoc valid-collection-map :eval-count)))))
    (testing "rejects non-sequential collections"
      (is (false? (schema/validate schema/collection-map
                                   (assoc valid-collection-map :collections "not-seq")))))))

(deftest data-entry-map-test
  ;; Tests the data-entry-map schema which is the base type for result entries.
  (testing "data-entry-map"
    (testing "accepts valid data entry"
      (is (true? (schema/validate schema/data-entry-map valid-data-entry-map))))
    (testing "rejects missing :type"
      (is (false? (schema/validate schema/data-entry-map
                                   (dissoc valid-data-entry-map :type)))))
    (testing "rejects missing :transform"
      (is (false? (schema/validate schema/data-entry-map
                                   (dissoc valid-data-entry-map :transform)))))
    (testing "rejects non-keyword type"
      (is (false? (schema/validate schema/data-entry-map
                                   (assoc valid-data-entry-map :type "string")))))))

(deftest collected-metrics-map-test
  ;; Tests the collected-metrics-map schema for collected metrics from
  ;; a measurement run.
  (testing "collected-metrics-map"
    (testing "accepts valid collected metrics"
      (is (true? (schema/validate schema/collected-metrics-map
                                  valid-collected-metrics-map))))
    (testing "rejects missing required keys"
      (is (false? (schema/validate schema/collected-metrics-map {})))
      (is (false? (schema/validate schema/collected-metrics-map
                                   (dissoc valid-collected-metrics-map :metric->values)))))
    (testing "rejects invalid num-samples"
      (is (false? (schema/validate schema/collected-metrics-map
                                   (assoc valid-collected-metrics-map :num-samples 0)))))))

;;; Typed data map tests

(deftest metrics-samples-map-test
  ;; Tests the metrics-samples-map schema for maps with :type :criterium/metrics-samples.
  (testing "metrics-samples-map"
    (testing "accepts valid metrics samples"
      (is (true? (schema/validate schema/metrics-samples-map valid-metrics-samples-map))))
    (testing "rejects wrong type"
      (is (false? (schema/validate schema/metrics-samples-map
                                   (assoc valid-metrics-samples-map :type :wrong)))))
    (testing "rejects missing source-id"
      (is (false? (schema/validate schema/metrics-samples-map
                                   (dissoc valid-metrics-samples-map :source-id)))))
    (testing "rejects zero num-samples"
      (is (false? (schema/validate schema/metrics-samples-map
                                   (assoc valid-metrics-samples-map :num-samples 0)))))))

(deftest digest-samples-map-test
  ;; Tests the digest-samples-map schema for maps with :type :criterium/digest.
  (testing "digest-samples-map"
    (testing "accepts valid digest samples"
      (is (true? (schema/validate schema/digest-samples-map valid-digest-samples-map))))
    (testing "rejects wrong type"
      (is (false? (schema/validate schema/digest-samples-map
                                   (assoc valid-digest-samples-map :type :wrong)))))
    (testing "rejects missing metric->digest"
      (is (false? (schema/validate schema/digest-samples-map
                                   (dissoc valid-digest-samples-map :metric->digest)))))))

(deftest quantiles-map-test
  ;; Tests the quantiles-map schema for quantiles analysis results.
  (testing "quantiles-map"
    (testing "accepts valid quantiles map"
      (is (true? (schema/validate schema/quantiles-map valid-quantiles-map))))
    (testing "rejects wrong type"
      (is (false? (schema/validate schema/quantiles-map
                                   (assoc valid-quantiles-map :type :wrong)))))
    (testing "rejects missing quantiles"
      (is (false? (schema/validate schema/quantiles-map
                                   (dissoc valid-quantiles-map :quantiles)))))))

(deftest outliers-map-test
  ;; Tests the outliers-map schema for outliers analysis results.
  (testing "outliers-map"
    (testing "accepts valid outliers map"
      (is (true? (schema/validate schema/outliers-map valid-outliers-map))))
    (testing "rejects wrong type"
      (is (false? (schema/validate schema/outliers-map
                                   (assoc valid-outliers-map :type :wrong)))))
    (testing "rejects missing outliers"
      (is (false? (schema/validate schema/outliers-map
                                   (dissoc valid-outliers-map :outliers)))))
    (testing "rejects zero num-samples"
      (is (false? (schema/validate schema/outliers-map
                                   (assoc valid-outliers-map :num-samples 0)))))))

(deftest stats-map-test
  ;; Tests the stats-map schema for statistics analysis results.
  (testing "stats-map"
    (testing "accepts valid stats map"
      (is (true? (schema/validate schema/stats-map valid-stats-map))))
    (testing "rejects wrong type"
      (is (false? (schema/validate schema/stats-map
                                   (assoc valid-stats-map :type :wrong)))))
    (testing "rejects missing stats"
      (is (false? (schema/validate schema/stats-map
                                   (dissoc valid-stats-map :stats)))))
    (testing "rejects zero batch-size"
      (is (false? (schema/validate schema/stats-map
                                   (assoc valid-stats-map :batch-size 0)))))))

(deftest event-stats-map-test
  ;; Tests the event-stats-map schema for event statistics analysis results.
  (testing "event-stats-map"
    (testing "accepts valid event stats map"
      (is (true? (schema/validate schema/event-stats-map valid-event-stats-map))))
    (testing "rejects wrong type"
      (is (false? (schema/validate schema/event-stats-map
                                   (assoc valid-event-stats-map :type :wrong)))))
    (testing "rejects missing event-stats"
      (is (false? (schema/validate schema/event-stats-map
                                   (dissoc valid-event-stats-map :event-stats)))))))

(deftest histogram-map-test
  ;; Tests the histogram-map schema for histogram analysis results.
  (testing "histogram-map"
    (testing "accepts valid histogram map"
      (is (true? (schema/validate schema/histogram-map valid-histogram-map))))
    (testing "rejects wrong type"
      (is (false? (schema/validate schema/histogram-map
                                   (assoc valid-histogram-map :type :wrong)))))
    (testing "rejects missing histograms"
      (is (false? (schema/validate schema/histogram-map
                                   (dissoc valid-histogram-map :histograms)))))))

(deftest outlier-significance-map-test
  ;; Tests the outlier-significance-map schema for outlier significance results.
  (testing "outlier-significance-map"
    (testing "accepts valid outlier significance map"
      (is (true? (schema/validate schema/outlier-significance-map
                                  valid-outlier-significance-map))))
    (testing "rejects wrong type"
      (is (false? (schema/validate schema/outlier-significance-map
                                   (assoc valid-outlier-significance-map :type :wrong)))))
    (testing "rejects missing outlier-significance"
      (is (false? (schema/validate schema/outlier-significance-map
                                   (dissoc valid-outlier-significance-map
                                           :outlier-significance)))))))

(deftest bootstrap-map-test
  ;; Tests the bootstrap-map schema for bootstrap analysis results.
  (testing "bootstrap-map"
    (testing "accepts valid bootstrap map"
      (is (true? (schema/validate schema/bootstrap-map valid-bootstrap-map))))
    (testing "rejects wrong type"
      (is (false? (schema/validate schema/bootstrap-map
                                   (assoc valid-bootstrap-map :type :wrong)))))
    (testing "rejects missing bootstrap"
      (is (false? (schema/validate schema/bootstrap-map
                                   (dissoc valid-bootstrap-map :bootstrap)))))))

;;; Composite schema tests

(deftest result-map-test
  ;; Tests the result-map schema where all values must be data-entry-maps.
  (testing "result-map"
    (testing "accepts empty map"
      (is (true? (schema/validate schema/result-map {}))))
    (testing "accepts map with valid data entries"
      (is (true? (schema/validate schema/result-map
                                  {:entry1 valid-data-entry-map
                                   :entry2 valid-data-entry-map}))))
    (testing "rejects non-keyword keys"
      (is (false? (schema/validate schema/result-map
                                   {"string-key" valid-data-entry-map}))))
    (testing "rejects invalid data entry values"
      (is (false? (schema/validate schema/result-map
                                   {:entry1 {:not "valid"}}))))))

(deftest benchmark-map-test
  ;; Tests the benchmark-map schema containing a :data result-map.
  (testing "benchmark-map"
    (testing "accepts valid benchmark map"
      (is (true? (schema/validate schema/benchmark-map
                                  {:data {:entry valid-data-entry-map}}))))
    (testing "accepts benchmark with empty data"
      (is (true? (schema/validate schema/benchmark-map {:data {}}))))
    (testing "rejects missing :data key"
      (is (false? (schema/validate schema/benchmark-map {}))))
    (testing "rejects invalid data value"
      (is (false? (schema/validate schema/benchmark-map {:data "not-map"}))))))

;;; Allocation trace tests

(deftest allocation-trace-test
  ;; Tests the allocation-trace schema for allocation trace maps from
  ;; the native agent.
  (testing "allocation-trace"
    (testing "accepts valid allocation trace"
      (is (true? (schema/validate schema/allocation-trace valid-allocation-trace))))
    (testing "accepts non-empty records"
      (is (true? (schema/validate schema/allocation-trace
                                  (assoc valid-allocation-trace
                                         :records [{:type "Ljava/lang/String;"}])))))
    (testing "rejects wrong type"
      (is (false? (schema/validate schema/allocation-trace
                                   (assoc valid-allocation-trace :type :wrong)))))
    (testing "rejects missing records"
      (is (false? (schema/validate schema/allocation-trace
                                   (dissoc valid-allocation-trace :records)))))
    (testing "rejects missing thread-id"
      (is (false? (schema/validate schema/allocation-trace
                                   (dissoc valid-allocation-trace :thread-id)))))
    (testing "rejects zero eval-count"
      (is (false? (schema/validate schema/allocation-trace
                                   (assoc valid-allocation-trace :eval-count 0)))))))

;;; Union type tests

(deftest generic-metrics-samples-map-test
  ;; Tests the generic-metrics-samples-map schema which accepts either
  ;; :criterium/metrics-samples or :criterium/collected-metrics-samples types.
  (testing "generic-metrics-samples-map"
    (testing "accepts :criterium/metrics-samples type"
      (is (true? (schema/validate schema/generic-metrics-samples-map
                                  {:type :criterium/metrics-samples}))))
    (testing "accepts :criterium/collected-metrics-samples type"
      (is (true? (schema/validate schema/generic-metrics-samples-map
                                  {:type :criterium/collected-metrics-samples}))))
    (testing "rejects other types"
      (is (false? (schema/validate schema/generic-metrics-samples-map
                                   {:type :criterium/digest})))
      (is (false? (schema/validate schema/generic-metrics-samples-map
                                   {:type :other}))))))

(deftest generic-data-map-test
  ;; Tests the generic-data-map schema which accepts metrics-samples,
  ;; collected-metrics-samples, or digest types.
  (testing "generic-data-map"
    (testing "accepts :criterium/metrics-samples type"
      (is (true? (schema/validate schema/generic-data-map
                                  {:type :criterium/metrics-samples}))))
    (testing "accepts :criterium/collected-metrics-samples type"
      (is (true? (schema/validate schema/generic-data-map
                                  {:type :criterium/collected-metrics-samples}))))
    (testing "accepts :criterium/digest type"
      (is (true? (schema/validate schema/generic-data-map
                                  {:type :criterium/digest}))))
    (testing "rejects other types"
      (is (false? (schema/validate schema/generic-data-map
                                   {:type :criterium/stats}))))))

;;; Function input schema tests

(deftest measured-test
  ;; Tests the measured schema for Measured instances which wrap a function
  ;; with an argument generator.
  (testing "measured"
    (testing "accepts valid measured"
      (is (true? (schema/validate schema/measured valid-measured))))
    (testing "accepts with optional expr-fn"
      (is (true? (schema/validate schema/measured
                                  (assoc valid-measured :expr-fn (fn [] "expr"))))))
    (testing "rejects missing args-fn"
      (is (false? (schema/validate schema/measured
                                   (dissoc valid-measured :args-fn)))))
    (testing "rejects missing f"
      (is (false? (schema/validate schema/measured
                                   (dissoc valid-measured :f)))))
    (testing "rejects non-function args-fn"
      (is (false? (schema/validate schema/measured
                                   (assoc valid-measured :args-fn "not-fn")))))))

(deftest collector-config-test
  ;; Tests the collector-config schema for collector configuration maps.
  (testing "collector-config"
    (testing "accepts valid collector config"
      (is (true? (schema/validate schema/collector-config valid-collector-config))))
    (testing "rejects missing stages"
      (is (false? (schema/validate schema/collector-config
                                   (dissoc valid-collector-config :stages)))))
    (testing "rejects missing terminator"
      (is (false? (schema/validate schema/collector-config
                                   (dissoc valid-collector-config :terminator)))))
    (testing "rejects non-vector stages"
      (is (false? (schema/validate schema/collector-config
                                   (assoc valid-collector-config :stages '(:a :b))))))))

(deftest collector-test
  ;; Tests the collector schema for collector pipeline maps.
  (testing "collector"
    (testing "accepts valid collector"
      (is (true? (schema/validate schema/collector valid-collector))))
    (testing "rejects missing f"
      (is (false? (schema/validate schema/collector
                                   (dissoc valid-collector :f)))))
    (testing "rejects zero length"
      (is (false? (schema/validate schema/collector
                                   (assoc valid-collector :length 0)))))))

(deftest collect-plan-test
  ;; Tests the collect-plan schema for collection plan configuration.
  (testing "collect-plan"
    (testing "accepts valid collect plan"
      (is (true? (schema/validate schema/collect-plan valid-collect-plan))))
    (testing "rejects missing scheme-type"
      (is (false? (schema/validate schema/collect-plan {}))))
    (testing "rejects non-keyword scheme-type"
      (is (false? (schema/validate schema/collect-plan
                                   {:scheme-type "string"}))))))

(deftest analyse-plan-test
  ;; Tests the analyse-plan schema for analysis plan specifications.
  (testing "analyse-plan"
    (testing "accepts vector of keywords"
      (is (true? (schema/validate schema/analyse-plan [:stats :quantiles]))))
    (testing "accepts vector of vectors"
      (is (true? (schema/validate schema/analyse-plan [[:stats {:conf 0.95}]]))))
    (testing "accepts mixed vector"
      (is (true? (schema/validate schema/analyse-plan
                                  [:quantiles [:stats {:conf 0.95}]]))))
    (testing "accepts empty vector"
      (is (true? (schema/validate schema/analyse-plan []))))
    (testing "rejects non-vector"
      (is (false? (schema/validate schema/analyse-plan :keyword))))
    (testing "rejects invalid elements"
      (is (false? (schema/validate schema/analyse-plan ["string"]))))))

(deftest view-plan-test
  ;; Tests the view-plan schema for view plan specifications.
  (testing "view-plan"
    (testing "accepts vector of keywords"
      (is (true? (schema/validate schema/view-plan [:elapsed-time :stats]))))
    (testing "accepts vector of vectors"
      (is (true? (schema/validate schema/view-plan [[:elapsed-time {:format :ns}]]))))
    (testing "accepts empty vector"
      (is (true? (schema/validate schema/view-plan []))))
    (testing "rejects non-vector"
      (is (false? (schema/validate schema/view-plan :keyword))))))

(deftest viewer-test
  ;; Tests the viewer schema for viewer keyword.
  (testing "viewer"
    (testing "accepts keyword"
      (is (true? (schema/validate schema/viewer :print)))
      (is (true? (schema/validate schema/viewer :portal))))
    (testing "rejects non-keyword"
      (is (false? (schema/validate schema/viewer "print")))
      (is (false? (schema/validate schema/viewer nil))))))

(deftest bench-plan-test
  ;; Tests the bench-plan schema for benchmark plan configuration.
  (testing "bench-plan"
    (testing "accepts valid bench plan"
      (is (true? (schema/validate schema/bench-plan valid-bench-plan))))
    (testing "accepts with optional keys"
      (is (true? (schema/validate schema/bench-plan
                                  (assoc valid-bench-plan
                                         :analyse [:stats]
                                         :view [:elapsed-time]
                                         :verbose true)))))
    (testing "rejects missing collect-plan"
      (is (false? (schema/validate schema/bench-plan
                                   (dissoc valid-bench-plan :collect-plan)))))
    (testing "rejects missing collector-config"
      (is (false? (schema/validate schema/bench-plan
                                   (dissoc valid-bench-plan :collector-config)))))
    (testing "rejects missing viewer"
      (is (false? (schema/validate schema/bench-plan
                                   (dissoc valid-bench-plan :viewer)))))
    (testing "rejects missing return-value"
      (is (false? (schema/validate schema/bench-plan
                                   (dissoc valid-bench-plan :return-value)))))))

(deftest data-map-test
  ;; Tests the data-map schema for data maps passed through the pipeline.
  (testing "data-map"
    (testing "accepts map with samples"
      (is (true? (schema/validate schema/data-map {:samples {}}))))
    (testing "accepts map without samples"
      (is (true? (schema/validate schema/data-map {}))))
    (testing "accepts extra keys"
      (is (true? (schema/validate schema/data-map {:samples {}
                                                   :other-data {}}))))
    (testing "rejects non-map"
      (is (false? (schema/validate schema/data-map nil)))
      (is (false? (schema/validate schema/data-map []))))))

;;; Domain type schema tests

(deftest coord-test
  ;; Tests the coord schema for run coordinates - keyword or map.
  (testing "coord"
    (testing "accepts keyword"
      (is (true? (schema/validate schema/coord :baseline)))
      (is (true? (schema/validate schema/coord :impl-a))))
    (testing "accepts map of keyword to any"
      (is (true? (schema/validate schema/coord {:n 100})))
      (is (true? (schema/validate schema/coord {:n 100 :impl :foo})))
      (is (true? (schema/validate schema/coord {:impl :bar :size "large"}))))
    (testing "accepts empty map"
      (is (true? (schema/validate schema/coord {}))))
    (testing "rejects string"
      (is (false? (schema/validate schema/coord "baseline"))))
    (testing "rejects vector"
      (is (false? (schema/validate schema/coord [:a :b]))))
    (testing "rejects map with non-keyword keys"
      (is (false? (schema/validate schema/coord {"n" 100}))))))

(deftest run-map-test
  ;; Tests the run-map schema for run maps with :coord and :data keys.
  (testing "run-map"
    (testing "accepts valid run with keyword coord"
      (is (true? (schema/validate schema/run-map {:coord :baseline :data {}}))))
    (testing "accepts valid run with map coord"
      (is (true? (schema/validate schema/run-map valid-run-map))))
    (testing "rejects missing :coord"
      (is (false? (schema/validate schema/run-map {:data {}}))))
    (testing "rejects missing :data"
      (is (false? (schema/validate schema/run-map {:coord :a}))))
    (testing "rejects string coord"
      (is (false? (schema/validate schema/run-map {:coord "baseline" :data {}}))))
    (testing "rejects invalid :data (not result-map)"
      (is (false? (schema/validate schema/run-map {:coord :a :data "invalid"})))
      (is (false? (schema/validate schema/run-map {:coord :a :data {:some "not-data-entry"}}))))
    (testing "rejects non-map"
      (is (false? (schema/validate schema/run-map nil)))
      (is (false? (schema/validate schema/run-map []))))))

(deftest domain-map-test
  ;; Tests the domain-map schema for domain maps with :type :criterium/domain.
  (testing "domain-map"
    (testing "accepts valid empty domain"
      (is (true? (schema/validate schema/domain-map valid-domain-map))))
    (testing "accepts domain with runs"
      (is (true? (schema/validate schema/domain-map
                                  {:type :criterium/domain
                                   :runs [{:coord :a :data {}}]}))))
    (testing "rejects wrong type"
      (is (false? (schema/validate schema/domain-map {:type :other :runs []}))))
    (testing "rejects missing type"
      (is (false? (schema/validate schema/domain-map {:runs []}))))
    (testing "rejects non-vector runs"
      (is (false? (schema/validate schema/domain-map
                                   {:type :criterium/domain :runs '()}))))
    (testing "rejects non-map"
      (is (false? (schema/validate schema/domain-map nil))))))

(deftest domain-extract-map-test
  ;; Tests the domain-extract-map schema for domain extract results.
  (testing "domain-extract-map"
    (testing "accepts valid domain extract"
      (is (true? (schema/validate schema/domain-extract-map valid-domain-extract-map))))
    (testing "accepts with metrics content"
      (is (true? (schema/validate schema/domain-extract-map
                                  {:type :criterium/domain-extract
                                   :metrics {:elapsed-time {:data []}}}))))
    (testing "rejects wrong type"
      (is (false? (schema/validate schema/domain-extract-map
                                   {:type :other :metrics {}}))))
    (testing "rejects missing metrics"
      (is (false? (schema/validate schema/domain-extract-map
                                   {:type :criterium/domain-extract}))))
    (testing "rejects non-map metrics"
      (is (false? (schema/validate schema/domain-extract-map
                                   {:type :criterium/domain-extract :metrics []}))))))

(deftest domain-grouped-map-test
  ;; Tests the domain-grouped-map schema for domain grouped results.
  (testing "domain-grouped-map"
    (testing "accepts valid domain grouped"
      (is (true? (schema/validate schema/domain-grouped-map valid-domain-grouped-map))))
    (testing "rejects wrong type"
      (is (false? (schema/validate schema/domain-grouped-map
                                   {:type :other :axis :impl :data {}}))))
    (testing "rejects missing axis"
      (is (false? (schema/validate schema/domain-grouped-map
                                   {:type :criterium/domain-grouped :data {}}))))
    (testing "rejects missing data"
      (is (false? (schema/validate schema/domain-grouped-map
                                   {:type :criterium/domain-grouped :axis :impl}))))))

(deftest domain-comparison-map-test
  ;; Tests the domain-comparison-map schema for domain comparison results.
  ;; Supports single-metric (:metric + :data) and multi-metric (:metrics) formats.
  (testing "domain-comparison-map"
    (testing "accepts single-metric format with :metric and :data"
      (is (true? (schema/validate schema/domain-comparison-map
                                  valid-domain-comparison-map-single))))
    (testing "accepts multi-metric format with :metrics"
      (is (true? (schema/validate schema/domain-comparison-map
                                  valid-domain-comparison-map-multi))))
    (testing "rejects wrong type"
      (is (false? (schema/validate schema/domain-comparison-map
                                   {:type :other :axis :impl :metric [] :data {}}))))
    (testing "rejects missing axis"
      (is (false? (schema/validate schema/domain-comparison-map
                                   {:type :criterium/domain-comparison :metric [] :data {}}))))
    (testing "rejects missing both :metric/:data and :metrics"
      (is (false? (schema/validate schema/domain-comparison-map
                                   {:type :criterium/domain-comparison :axis :impl}))))))

(deftest domain-regression-map-test
  ;; Tests the domain-regression-map schema for domain regression results.
  (testing "domain-regression-map"
    (testing "accepts valid domain regression"
      (is (true? (schema/validate schema/domain-regression-map
                                  valid-domain-regression-map))))
    (testing "accepts with regressions content"
      (is (true? (schema/validate schema/domain-regression-map
                                  {:type :criterium/domain-regression
                                   :axis :n
                                   :regressions {:elapsed-time {:best-fit :linear}}}))))
    (testing "rejects wrong type"
      (is (false? (schema/validate schema/domain-regression-map
                                   {:type :other :axis :n :regressions {}}))))
    (testing "rejects missing axis"
      (is (false? (schema/validate schema/domain-regression-map
                                   {:type :criterium/domain-regression :regressions {}}))))
    (testing "rejects missing regressions"
      (is (false? (schema/validate schema/domain-regression-map
                                   {:type :criterium/domain-regression :axis :n}))))
    (testing "rejects non-map regressions"
      (is (false? (schema/validate schema/domain-regression-map
                                   {:type :criterium/domain-regression
                                    :axis :n
                                    :regressions []}))))))
