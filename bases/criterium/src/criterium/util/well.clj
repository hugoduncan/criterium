;; Copyright (c) Hugo Duncan. All rights reserved.

;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns criterium.util.well
  "WELL RNG 1024a implementation.

  Re-exports from criterium.random.interface for backward compatibility.
  New code should use criterium.random.interface directly."
  (:require
   [criterium.random.well :as well]))

;;; Re-export from random.well for backward compatibility

(defmacro bit-shift-right-ns
  "A bit shift that doesn't do sign extension."
  [a b]
  `(well/bit-shift-right-ns ~a ~b))

(defmacro unsign
  "Convert a result based on a signed integer, and convert it to what it would
   have been for an unsigned integer."
  [x]
  `(well/unsign ~x))

(def int-max
  "Returns 2^32 - 1 (all 32 bits set)."
  well/int-max)

(defmacro limit-bits [x]
  `(well/limit-bits ~x))

(defmacro mat0-pos [t v]
  `(well/mat0-pos ~t ~v))

(defmacro mat0-neg [t v]
  `(well/mat0-neg ~t ~v))

(defmacro add-mod-32 [a b]
  `(well/add-mod-32 ~a ~b))

(def well-rng-1024a
  "Well RNG 1024a.
  Returns a lazy sequence of random doubles in [0,1).

  Arities:
  - () - Uses non-deterministic seed from rand-int
  - (seed) - Uses java.util.Random with given seed for deterministic output
  - (state index) - Uses explicit state array and index

  See: Improved Long-Period Generators Based on Linear Recurrences Modulo 2
  F. Panneton, P. L'Ecuyer and M. Matsumoto
  http://www.iro.umontreal.ca/~panneton/WELLRNG.html"
  well/well-rng-1024a)
