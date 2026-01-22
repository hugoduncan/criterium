(ns criterium.viewer.kindly-test-helpers
  "Shared test utilities for kindly viewer tests.
  Provides helpers for extracting and verifying data from kindly view structures.")

(defn table-rows
  "Extract row-maps from a kindly table structure.
  Handles both old structure (plain vector of maps) and new structure
  ({:row-maps [...] :column-names [...]}) used when column ordering is specified."
  [table]
  (if (and (map? table) (contains? table :row-maps))
    (:row-maps table)
    table))
