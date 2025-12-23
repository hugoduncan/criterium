(ns criterium.schema.instrument
  "Malli instrumentation for criterium public API.

  Provides instrument! and unstrument! functions to enable runtime
  validation of function inputs and outputs during development and testing.

  Only instruments the true public API in criterium.bench. Internal
  functions in criterium.collect have complex polymorphic behaviors
  that don't lend themselves to simple schema validation.

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
   [malli.core :as m]
   [malli.instrument :as mi]
   [malli.registry :as mr]))

;;; Registry setup
;; Set malli's default registry to include criterium schemas so that
;; function schema references like :criterium/measured resolve correctly.

(defonce ^{:private true :no-doc true} _registry-init
  (mr/set-default-registry!
   (mr/composite-registry
    (m/default-schemas)
    schema/registry)))

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

;;; Instrumentation functions

(defn instrument!
  "Enable malli instrumentation for criterium.bench public API.

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
     {:filters [(mi/-filter-ns 'criterium.bench)]}
     options))))

(defn unstrument!
  "Disable malli instrumentation for criterium.bench public API.

  Removes validation wrappers from public API functions.

  Returns set of unstrumented vars."
  ([]
   (unstrument! {}))
  ([options]
   (mi/unstrument!
    (merge
     {:filters [(mi/-filter-ns 'criterium.bench)]}
     options))))

(defn instrumented?
  "Check if a var is currently instrumented.

  Returns true if the var has been instrumented, false otherwise."
  [v]
  (boolean (:malli.instrument/instrumented (meta v))))
