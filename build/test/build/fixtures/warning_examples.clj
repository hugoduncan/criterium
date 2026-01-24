(ns build.fixtures.warning-examples
  "Test fixture namespace with intentional warnings for testing warning detection.

   DO NOT add type hints or fix warnings in this file - they are intentional.")

;;; Boxed math warnings

(defn boxed-add
  "Intentionally uses boxed math (Object + long)."
  [x]
  (+ x 1))

(defn boxed-multiply
  "Intentionally uses boxed math (Object * Object)."
  [x y]
  (* x y))

;;; Reflection warnings

(defn reflect-get-bytes
  "Intentionally triggers reflection warning."
  [s]
  (.getBytes s))

(defn reflect-to-upper
  "Intentionally triggers reflection warning."
  [s]
  (.toUpperCase s))
