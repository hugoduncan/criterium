(ns criterium.types
  "Key sets for benchmarking data structures.

  These key sets are used for select-keys operations when transforming
  sample data. Type validation is provided by malli schemas in
  development/src/criterium/schema.clj during development and testing.")

(def metrics-samples-keys
  "Required keys for :criterium/metrics-samples type.
  Used by analyse/metrics_samples.clj for select-keys."
  #{:type
    :transform
    :source-id
    :metric->values
    :num-samples
    :batch-size
    :metrics-defs
    :expr-value})

(def digest-samples-keys
  "Required keys for :criterium/digest type.
  Used by analyse/digest_samples.clj for select-keys."
  #{:type
    :transform
    :source-id
    :metric->digest
    :metrics-defs
    :expr-value})
