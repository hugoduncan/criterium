(ns criterium.schema.validators
  "No-op validation checkpoint functions for criterium internal types.

  These functions serve as instrumentable checkpoints for validating
  data types used in multimethod dispatch and internal functions.
  Each function is an identity function that returns its input unchanged,
  but can be instrumented via malli to validate the input type.

  Usage:
    (require '[criterium.schema.validators :as v])
    (require '[criterium.schema.instrument :as inst])

    ;; Enable instrumentation
    (inst/instrument!)

    ;; Validate data explicitly
    (v/check-digest-samples-map some-digest-data)
    ;; => returns some-digest-data if valid, throws if invalid

  These checkpoints correspond to :pre/:post conditions in:
  - analyse/digest_samples.clj (digest-samples-map?)
  - analyse/metrics_samples.clj (generic-metrics-samples-map?)
  - collect.clj (collection-map?)
  - analyse.clj (result-map?)
  - trigger/impl.clj (metrics-samples-map?)")

;;; Validation checkpoint functions
;; Each returns its input unchanged - validation happens via malli instrumentation

(defn check-digest-samples-map
  "Validate digest-samples-map type.
  Used by analyse/digest_samples.clj multimethod implementations."
  [x] x)

(defn check-generic-metrics-samples-map
  "Validate generic-metrics-samples-map type.
  Used by analyse/metrics_samples.clj multimethod implementations."
  [x] x)

(defn check-collection-map
  "Validate collection-map type.
  Used as postcondition by collect.clj functions."
  [x] x)

(defn check-result-map
  "Validate result-map type.
  Used by analyse.clj and util/helpers.clj functions."
  [x] x)

(defn check-metrics-samples-map
  "Validate metrics-samples-map type.
  Used by trigger/impl.clj functions."
  [x] x)

(defn check-quantiles-map
  "Validate quantiles-map type.
  Used by accessor functions."
  [x] x)

(defn check-outliers-map
  "Validate outliers-map type.
  Used by accessor functions."
  [x] x)

(defn check-stats-map
  "Validate stats-map type.
  Used by accessor functions."
  [x] x)

(defn check-event-stats-map
  "Validate event-stats-map type.
  Used by accessor functions."
  [x] x)

(defn check-histogram-map
  "Validate histogram-map type.
  Used by accessor functions."
  [x] x)

(defn check-outlier-significance-map
  "Validate outlier-significance-map type.
  Used by accessor functions."
  [x] x)

(defn check-bootstrap-map
  "Validate bootstrap-map type.
  Used by accessor functions."
  [x] x)

(defn check-data-entry-map
  "Validate data-entry-map type.
  Base type for all data entries in result maps."
  [x] x)
