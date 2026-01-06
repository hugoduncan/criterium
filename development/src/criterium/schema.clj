(ns criterium.schema
  "Malli schemas for criterium data structures.

  Provides schema definitions for criterium's benchmarking data types.
  These schemas enable runtime validation and instrumentation during
  development and testing.

  Usage:
    (require '[malli.core :as m])
    (require '[criterium.schema :as schema])

    (m/validate schema/metrics-samples-map some-data)"
  (:require
   [malli.core :as m]))

;;; Transform schema
;; Transform maps contain functions for converting between sample and display values

(def transform-map
  "Schema for transform maps with :sample-> and :->sample functions."
  [:map
   [:sample-> fn?]
   [:->sample fn?]])

;;; Base schemas

(def collection-map
  "Schema for collection maps returned by collect operations."
  [:map
   [:eval-count pos-int?]
   [:elapsed-time nat-int?]
   [:collections sequential?]
   [:num-samples pos-int?]
   [:batch-size pos-int?]
   [:collector map?]])

(def data-entry-map
  "Schema for data entry maps - base type for result entries."
  [:map
   [:type keyword?]
   [:transform transform-map]])

(def collected-metrics-map
  "Schema for collected metrics from a measurement run."
  [:map
   [:type keyword?]
   [:transform transform-map]
   [:metric->values map?]
   [:elapsed-time nat-int?]
   [:num-samples pos-int?]
   [:batch-size pos-int?]
   [:eval-count pos-int?]
   [:metrics-defs map?]
   [:expr-value any?]])

;;; Typed data maps

(def metrics-samples-map
  "Schema for metrics samples with :type :criterium/metrics-samples."
  [:map
   [:type [:= :criterium/metrics-samples]]
   [:transform transform-map]
   [:source-id keyword?]
   [:metric->values map?]
   [:num-samples pos-int?]
   [:batch-size pos-int?]
   [:metrics-defs map?]
   [:expr-value any?]])

(def digest-samples-map
  "Schema for digest samples with :type :criterium/digest."
  [:map
   [:type [:= :criterium/digest]]
   [:transform transform-map]
   [:source-id keyword?]
   [:metric->digest map?]
   [:metrics-defs map?]
   [:expr-value any?]])

(def quantiles-map
  "Schema for quantiles analysis results."
  [:map
   [:type [:= :criterium/quantiles]]
   [:quantiles map?]
   [:metrics-defs map?]
   [:source-id keyword?]
   [:transform transform-map]])

(def outliers-map
  "Schema for outliers analysis results."
  [:map
   [:type [:= :criterium/outliers]]
   [:outliers map?]
   [:metrics-defs map?]
   [:source-id keyword?]
   [:quantiles-id keyword?]
   [:num-samples [:maybe pos?]]
   [:transform transform-map]])

(def stats-map
  "Schema for statistics analysis results."
  [:map
   [:type [:= :criterium/stats]]
   [:stats map?]
   [:metrics-defs map?]
   [:transform transform-map]
   [:batch-size pos-int?]
   [:source-id keyword?]
   [:outliers-id [:maybe keyword?]]])

(def event-stats-map
  "Schema for event statistics analysis results."
  [:map
   [:type [:= :criterium/event-stats]]
   [:event-stats map?]
   [:metrics-defs map?]
   [:transform transform-map]
   [:batch-size pos-int?]
   [:source-id keyword?]])

(def histogram-map
  "Schema for histogram analysis results."
  [:map
   [:type [:= :criterium/histogram]]
   [:histograms map?]
   [:metrics-defs map?]
   [:transform transform-map]
   [:batch-size pos-int?]
   [:source-id keyword?]
   [:outliers-id [:maybe keyword?]]])

(def outlier-significance-map
  "Schema for outlier significance analysis results."
  [:map
   [:type [:= :criterium/outlier-significance]]
   [:outlier-significance map?]
   [:metrics-defs map?]
   [:source-id keyword?]
   [:outliers-id [:maybe keyword?]]])

(def bootstrap-map
  "Schema for bootstrap analysis results."
  [:map
   [:type [:= :criterium/bootstrap]]
   [:bootstrap map?]
   [:metrics-defs map?]
   [:transform transform-map]
   [:batch-size pos-int?]
   [:source-id keyword?]
   [:outliers-id [:maybe keyword?]]])

;;; Composite schemas

(def result-map
  "Schema for result maps where all values are data-entry-maps."
  [:map-of keyword? data-entry-map])

(def benchmark-map
  "Schema for benchmark maps containing a :data result-map."
  [:map
   [:data result-map]])

;;; Allocation tracing

(def allocation-trace
  "Schema for allocation trace maps from the native agent."
  [:map
   [:type [:= :criterium/allocation-trace]]
   [:records sequential?]
   [:thread-id int?]
   [:eval-count pos-int?]
   [:elapsed-time nat-int?]])

;;; Union types for polymorphic predicates

(def generic-metrics-samples-map
  "Schema for maps with :type in #{:criterium/metrics-samples :criterium/collected-metrics-samples}."
  [:map
   [:type [:enum :criterium/metrics-samples :criterium/collected-metrics-samples]]])

(def generic-data-map
  "Schema for maps with :type in #{:criterium/metrics-samples :criterium/collected-metrics-samples :criterium/digest}."
  [:map
   [:type [:enum :criterium/metrics-samples :criterium/collected-metrics-samples :criterium/digest]]])

;;; Function input types
;; Schemas for types used as inputs to public API functions

(def measured
  "Schema for Measured instances - the unit of benchmarking.
  A Measured wraps a function with an argument generator."
  [:map
   [:args-fn fn?]
   [:f fn?]
   [:expr-fn {:optional true} [:maybe fn?]]])

(def collector-config
  "Schema for collector configuration maps."
  [:map
   [:stages [:vector keyword?]]
   [:terminator keyword?]])

(def collector
  "Schema for collector pipeline maps."
  [:map
   [:f fn?]
   [:x fn?]
   [:length pos-int?]
   [:metrics-defs map?]])

(def collect-plan
  "Schema for collection plan configuration."
  [:map
   [:scheme-type keyword?]])

(def analyse-plan
  "Schema for analysis plan - vector of analysis step specifications."
  [:vector [:or keyword? vector?]])

(def view-plan
  "Schema for view plan - vector of view component specifications."
  [:vector [:or keyword? vector?]])

(def viewer
  "Schema for viewer keyword."
  keyword?)

(def bench-plan
  "Schema for benchmark plan configuration."
  [:map
   [:collect-plan collect-plan]
   [:collector-config collector-config]
   [:viewer viewer]
   [:return-value vector?]
   [:analyse {:optional true} analyse-plan]
   [:view {:optional true} view-plan]
   [:verbose {:optional true} boolean?]
   [:with-allocation-trace {:optional true} boolean?]])

(def data-map
  "Schema for data maps passed through the benchmarking pipeline.
  Contains :samples key with metrics data."
  [:map
   [:samples {:optional true} map?]])

;;; Domain types
;; Schemas for criterium.domain data structures

(def coord
  "Schema for run coordinates - keyword label or map of dimension keys to values."
  [:or keyword? [:map-of keyword? :any]])

(def run-map
  "Schema for run maps with :coord and :data keys."
  [:map
   [:coord coord]
   [:data result-map]])

(def domain-map
  "Schema for domain maps with :type :criterium/domain."
  [:map
   [:type [:= :criterium/domain]]
   [:runs vector?]])

(def domain-extract-map
  "Schema for domain extract results with :metrics map."
  [:map
   [:type [:= :criterium/domain-extract]]
   [:metrics map?]])

(def domain-grouped-map
  "Schema for domain grouped results."
  [:map
   [:type [:= :criterium/domain-grouped]]
   [:axis :any]
   [:data :any]])

(def domain-comparison-map
  "Schema for domain comparison results.
  Supports single-metric format (:metric + :data) or multi-metric format (:metrics)."
  [:and
   [:map
    [:type [:= :criterium/domain-comparison]]
    [:axis :any]]
   [:or
    [:map
     [:metric :any]
     [:data :any]]
    [:map
     [:metrics :any]]]])

(def domain-regression-map
  "Schema for domain regression results with :regressions map."
  [:map
   [:type [:= :criterium/domain-regression]]
   [:axis :any]
   [:regressions map?]])

;;; Registry

(def registry
  "Malli registry with all criterium schemas."
  (merge
   (m/default-schemas)
   {;; Data type schemas
    :criterium/transform-map          transform-map
    :criterium/collection-map         collection-map
    :criterium/data-entry-map         data-entry-map
    :criterium/collected-metrics-map  collected-metrics-map
    :criterium/metrics-samples-map    metrics-samples-map
    :criterium/digest-samples-map     digest-samples-map
    :criterium/quantiles-map          quantiles-map
    :criterium/outliers-map           outliers-map
    :criterium/stats-map              stats-map
    :criterium/event-stats-map        event-stats-map
    :criterium/histogram-map          histogram-map
    :criterium/outlier-significance-map outlier-significance-map
    :criterium/bootstrap-map          bootstrap-map
    :criterium/result-map             result-map
    :criterium/benchmark-map          benchmark-map
    :criterium/allocation-trace       allocation-trace
    :criterium/generic-metrics-samples-map generic-metrics-samples-map
    :criterium/generic-data-map       generic-data-map
    ;; Function input type schemas
    :criterium/measured               measured
    :criterium/collector-config       collector-config
    :criterium/collector              collector
    :criterium/collect-plan           collect-plan
    :criterium/analyse-plan           analyse-plan
    :criterium/view-plan              view-plan
    :criterium/viewer                 viewer
    :criterium/bench-plan             bench-plan
    :criterium/data-map               data-map
    ;; Domain type schemas
    :criterium/coord                  coord
    :criterium/run-map                run-map
    :criterium/domain-map             domain-map
    :criterium/domain-extract-map     domain-extract-map
    :criterium/domain-grouped-map     domain-grouped-map
    :criterium/domain-comparison-map  domain-comparison-map
    :criterium/domain-regression-map  domain-regression-map}))

(defn validator
  "Create a validator function for a schema.
  Uses the criterium registry."
  [schema]
  (m/validator schema {:registry registry}))

(defn validate
  "Validate data against a schema using the criterium registry.
  Returns true if valid, false otherwise."
  [schema data]
  (m/validate schema data {:registry registry}))

(defn explain
  "Explain validation errors for data against a schema.
  Returns nil if valid, explanation map otherwise."
  [schema data]
  (m/explain schema data {:registry registry}))
