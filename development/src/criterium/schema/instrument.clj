(ns criterium.schema.instrument
  "Malli instrumentation for criterium public API and internal validators.

  Provides instrument! and unstrument! functions to enable runtime
  validation of function inputs and outputs during development and testing.

  Instruments:
  - criterium.bench public API functions
  - criterium.util.helpers typed accessor functions
  - criterium.schema.validators checkpoint functions for type validation

  The validators namespace provides identity functions that can be used as
  type validation checkpoints for multimethod inputs and other internal types.

  Usage:
    (require '[criterium.schema.instrument :as inst])

    ;; Enable instrumentation
    (inst/instrument!)

    ;; Run benchmarks - invalid inputs will throw validation errors
    (bench (+ 1 1))

    ;; Disable instrumentation
    (inst/unstrument!)"
  (:require
   [criterium.bench]
   [criterium.schema :as schema]
   [criterium.schema.validators]
   [criterium.util.helpers]
   [malli.core :as m]
   [malli.instrument :as mi]
   [malli.registry :as mr]))

;;; Registry setup
;; Set malli's default registry to include criterium schemas so that
;; function schema references like :criterium/measured resolve correctly.

(defonce ^{:private true :no-doc true} _registry-init
  (mr/set-default-registry! schema/registry))

;;; Function schemas for criterium.bench public API

(m/=> criterium.bench/default-viewer
      [:=> :cat :criterium/viewer])

(m/=> criterium.bench/set-default-viewer!
      [:=> [:cat :criterium/viewer] :any])

(m/=> criterium.bench/last-bench
      [:=> :cat [:maybe map?]])

(m/=> criterium.bench/collect-data-map
      [:=> [:cat :criterium/collector-config :criterium/collect-plan :criterium/measured]
       map?])

(m/=> criterium.bench/analyze
      [:=> [:cat :criterium/analyse-plan map?]
       map?])

(m/=> criterium.bench/view
      [:=> [:cat :criterium/view-plan :criterium/viewer map?]
       :any])

(m/=> criterium.bench/bench-measured
      [:=> [:cat :criterium/bench-plan :criterium/measured]
       :any])

;;; Function schemas for criterium.util.helpers typed accessors

(m/=> criterium.util.helpers/get-generic-data-entry
      [:=> [:cat :criterium/result-map keyword?]
       [:maybe :criterium/generic-data-map]])

(m/=> criterium.util.helpers/get-quantiles-entry
      [:=> [:cat :criterium/result-map keyword?]
       [:maybe :criterium/quantiles-map]])

;;; Function schemas for criterium.util.helpers accessor functions
;; These accessors extract typed data from wrapper maps

(m/=> criterium.util.helpers/data
      [:=> [:cat [:map [:data map?]]]
       map?])

(m/=> criterium.util.helpers/metric->values
      [:=> [:cat :criterium/generic-metrics-samples-map]
       map?])

(m/=> criterium.util.helpers/quantiles
      [:=> [:cat :criterium/quantiles-map]
       map?])

(m/=> criterium.util.helpers/outliers
      [:=> [:cat :criterium/outliers-map]
       map?])

(m/=> criterium.util.helpers/outlier-significance
      [:=> [:cat :criterium/outlier-significance-map]
       map?])

(m/=> criterium.util.helpers/stats
      [:=> [:cat :criterium/stats-map]
       map?])

(m/=> criterium.util.helpers/event-stats
      [:=> [:cat :criterium/event-stats-map]
       map?])

(m/=> criterium.util.helpers/bootstrap
      [:=> [:cat :criterium/bootstrap-map]
       map?])

(m/=> criterium.util.helpers/get-transforms
      [:=> [:cat :criterium/result-map keyword?]
       map?])

(m/=> criterium.util.helpers/stats-value
      [:=> [:cat :criterium/result-map keyword? keyword? keyword?]
       [:maybe number?]])

;;; Function schemas for criterium.schema.validators checkpoint functions
;; These are identity functions used to validate types at development time

(m/=> criterium.schema.validators/check-digest-samples-map
      [:=> [:cat :criterium/digest-samples-map]
       :criterium/digest-samples-map])

(m/=> criterium.schema.validators/check-generic-metrics-samples-map
      [:=> [:cat :criterium/generic-metrics-samples-map]
       :criterium/generic-metrics-samples-map])

(m/=> criterium.schema.validators/check-collection-map
      [:=> [:cat :criterium/collection-map]
       :criterium/collection-map])

(m/=> criterium.schema.validators/check-result-map
      [:=> [:cat :criterium/result-map]
       :criterium/result-map])

(m/=> criterium.schema.validators/check-metrics-samples-map
      [:=> [:cat :criterium/metrics-samples-map]
       :criterium/metrics-samples-map])

(m/=> criterium.schema.validators/check-quantiles-map
      [:=> [:cat :criterium/quantiles-map]
       :criterium/quantiles-map])

(m/=> criterium.schema.validators/check-outliers-map
      [:=> [:cat :criterium/outliers-map]
       :criterium/outliers-map])

(m/=> criterium.schema.validators/check-stats-map
      [:=> [:cat :criterium/stats-map]
       :criterium/stats-map])

(m/=> criterium.schema.validators/check-event-stats-map
      [:=> [:cat :criterium/event-stats-map]
       :criterium/event-stats-map])

(m/=> criterium.schema.validators/check-histogram-map
      [:=> [:cat :criterium/histogram-map]
       :criterium/histogram-map])

(m/=> criterium.schema.validators/check-outlier-significance-map
      [:=> [:cat :criterium/outlier-significance-map]
       :criterium/outlier-significance-map])

(m/=> criterium.schema.validators/check-bootstrap-map
      [:=> [:cat :criterium/bootstrap-map]
       :criterium/bootstrap-map])

(m/=> criterium.schema.validators/check-data-entry-map
      [:=> [:cat :criterium/data-entry-map]
       :criterium/data-entry-map])

;;; Instrumentation functions

(defn instrument!
  "Enable malli instrumentation for criterium public API.

  Wraps public API functions to validate inputs and outputs at runtime.
  Invalid data will cause exceptions with detailed error messages.

  Options are passed to malli.instrument/instrument! and can include:
    :report - Custom error reporter function
    :scope  - Set of validation points #{:input :output}

  Returns set of instrumented vars."
  ([]
   (instrument! {}))
  ([options]
   (mi/instrument!
    (merge
     {:filters [(mi/-filter-ns 'criterium.bench)
                (mi/-filter-ns 'criterium.util.helpers)
                (mi/-filter-ns 'criterium.schema.validators)]}
     options))))

(defn unstrument!
  "Disable malli instrumentation for criterium public API.

  Removes validation wrappers from public API functions.

  Returns set of unstrumented vars."
  ([]
   (unstrument! {}))
  ([options]
   (mi/unstrument!
    (merge
     {:filters [(mi/-filter-ns 'criterium.bench)
                (mi/-filter-ns 'criterium.util.helpers)
                (mi/-filter-ns 'criterium.schema.validators)]}
     options))))

(defn instrumented?
  "Check if a var is currently instrumented.

  Returns true if the var has been instrumented, false otherwise."
  [v]
  (boolean (some-> v deref meta :malli.instrument/original)))
