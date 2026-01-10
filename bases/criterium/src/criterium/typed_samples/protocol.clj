(ns criterium.typed-samples.protocol
  "Protocol for typed sample collections.")

(defprotocol TypedSamples
  "Protocol for typed sample collections."
  (elem-type [this]
    "Returns the element type keyword: :double, :long, or :object.")
  (sample-count [this]
    "Returns the number of samples in the collection.")
  (fold [this f init]
    "Reduces over the samples with function f and initial value init.
    f is called as (f acc sample) for each sample."))
