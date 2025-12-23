(ns criterium.schema.instrument
  "Malli instrumentation for criterium public API.

  Provides instrument! and unstrument! functions to enable runtime
  validation of function inputs and outputs during development and testing.

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
   [criterium.collect]
   [criterium.schema :as schema]
   [malli.core :as m]
   [malli.instrument :as mi]))

;;; Function schemas for criterium.bench
;; Use inline schemas since m/=> only takes 2 args

(m/=> criterium.bench/default-viewer
      [:=> :cat keyword?])

(m/=> criterium.bench/set-default-viewer!
      [:=> [:cat keyword?] :any])

(m/=> criterium.bench/last-bench
      [:=> :cat [:maybe map?]])

(m/=> criterium.bench/collect-data-map
      [:=> [:cat map? map? [:map [:args-fn fn?] [:f fn?]]]
       map?])

(m/=> criterium.bench/analyze
      [:=> [:cat vector? map?]
       map?])

(m/=> criterium.bench/view
      [:=> [:cat vector? keyword? map?]
       :any])

(m/=> criterium.bench/bench-measured
      [:=> [:cat map? [:map [:args-fn fn?] [:f fn?]]]
       :any])

;;; Function schemas for criterium.collect

(m/=> criterium.collect/transform
      [:=> [:cat [:map
                  [:eval-count pos-int?]
                  [:elapsed-time nat-int?]
                  [:collections sequential?]
                  [:num-samples pos-int?]
                  [:batch-size pos-int?]
                  [:collector map?]]]
       map?])

(m/=> criterium.collect/force-gc-no-capture!
      [:=> [:cat pos-int?]
       nil?])

(m/=> criterium.collect/force-gc!
      [:=> [:cat pos-int?]
       [:map
        [:eval-count pos-int?]
        [:elapsed-time nat-int?]
        [:collections sequential?]
        [:num-samples pos-int?]
        [:batch-size pos-int?]
        [:collector map?]]])

(m/=> criterium.collect/batch-size
      [:=> [:cat pos-int? pos-int?]
       pos-int?])

(m/=> criterium.collect/throw-away-collection
      [:=> [:cat [:map [:args-fn fn?] [:f fn?]]]
       nil?])

(m/=> criterium.collect/collect-arrays
      [:=> [:cat
            [:map [:f fn?] [:x fn?] [:length pos-int?] [:metrics-defs map?]]
            [:map [:args-fn fn?] [:f fn?]]
            pos-int?
            pos-int?]
       [:map
        [:eval-count pos-int?]
        [:elapsed-time nat-int?]
        [:collections sequential?]
        [:num-samples pos-int?]
        [:batch-size pos-int?]
        [:collector map?]]])

(m/=> criterium.collect/elapsed-time-point-estimate
      [:=> [:cat [:map [:args-fn fn?] [:f fn?]]]
       int?])

(m/=> criterium.collect/elapsed-time-min-estimate
      [:=> [:cat [:map [:args-fn fn?] [:f fn?]] pos-int? pos-int?]
       [:map
        [:eval-count pos-int?]
        [:elapsed-time nat-int?]
        [:collections sequential?]
        [:num-samples pos-int?]
        [:batch-size pos-int?]
        [:collector map?]]])

(m/=> criterium.collect/warmup
      [:=> [:cat
            [:map [:f fn?] [:x fn?] [:length pos-int?] [:metrics-defs map?]]
            [:map [:args-fn fn?] [:f fn?]]
            pos-int?
            pos-int?]
       [:map
        [:eval-count pos-int?]
        [:elapsed-time nat-int?]
        [:collections sequential?]
        [:num-samples pos-int?]
        [:batch-size pos-int?]
        [:collector map?]]])

;;; Instrumentation functions

(defn instrument!
  "Enable malli instrumentation for criterium.bench and criterium.collect.

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
                (mi/-filter-ns 'criterium.collect)]}
     options))))

(defn unstrument!
  "Disable malli instrumentation for criterium.bench and criterium.collect.

  Removes validation wrappers from public API functions.

  Returns set of unstrumented vars."
  ([]
   (unstrument! {}))
  ([options]
   (mi/unstrument!
    (merge
     {:filters [(mi/-filter-ns 'criterium.bench)
                (mi/-filter-ns 'criterium.collect)]}
     options))))

(defn instrumented?
  "Check if a var is currently instrumented.

  Returns true if the var has been instrumented, false otherwise."
  [v]
  (boolean (:malli.instrument/instrumented (meta v))))
