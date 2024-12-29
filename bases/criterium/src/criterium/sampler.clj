(ns criterium.sampler
  "Protocol and utilities for working with performance sampling state.

  Provides a standard interface for components that collect and store
  performance metrics samples during execution.")


(defprotocol Sampler
  "Protocol for accessing sampling state"
  (samples-map [this] "Get the current samples collected")
  (reset-samples! [this] "Clear all collected samples"))
