;; Copyright (c) Hugo Duncan. All rights reserved.

;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;; Improved Long-Period Generators Based on Linear Recurrences Modulo 2
;; F. Panneton, P. L'Ecuyer and M. Matsumoto
;; http://www.iro.umontreal.ca/~panneton/WELLRNG.html

(ns criterium.random.well)

;;; Macros to help convert unsigned algorithm to our implementation with signed
;;; integers.
;;; unsign is used to convert the [0.5,-0.5] range back onto [1,0]

(defmacro bit-shift-right-ns
  "A bit shift that doesn't do sign extension."
  [a b]
  `(let [n# ~b]
     (if (neg? n#)
       (bit-shift-left ~a (- n#))
       (bit-and
        (bit-shift-right Integer/MAX_VALUE (dec n#))
        (bit-shift-right ~a n#)))))

(defmacro unsign
  "Convert a result based on a signed integer, and convert it to what it would
   have been for an unsigned integer."
  [x]
  `(let [v# ~x]
     (if (neg? v#) (+ 1 v#) v#)))

(defn int-max ^long [] (bit-or (bit-shift-left Integer/MAX_VALUE 1) 1))

(defmacro limit-bits [x]
  `(bit-and (int-max) ~x))

(defmacro mat0-pos [t v]
  `(let [v# ~v] (bit-xor v# (bit-shift-right v# ~t))))

(defmacro mat0-neg [t v]
  `(let [v# ~v]
     (bit-xor v# (limit-bits (bit-shift-left v# (unchecked-negate ~t))))))

(defmacro add-mod-32 [a b]
  `(bit-and (unchecked-add ~a ~b) 0x01f))

;;; WellRng1024a implementation

(definterface IWellRng
  (^double nextDouble []))

(deftype WellRng1024a [^longs state
                       ^:unsynchronized-mutable ^long index]
  IWellRng
  (nextDouble [_]
    (let [m1        (long 3)
          m2        (long 24)
          m3        (long 10)
          fact      2.32830643653869628906e-10
          new-index (long (add-mod-32 index 31))
          z0        (aget state new-index)
          z1        (bit-xor (aget state index)
                             (mat0-pos 8 (aget state (add-mod-32 index m1))))
          z2        (bit-xor (mat0-neg -19 (aget state (add-mod-32 index m2)))
                             (mat0-neg -14 (aget state (add-mod-32 index m3))))]
      (aset state index (bit-xor z1 z2))
      (aset state new-index
            (bit-xor (bit-xor (mat0-neg -11 z0) (mat0-neg -7 z1))
                     (mat0-neg -13 z2)))
      (set! index new-index)
      (unsign (* (aget state new-index) fact))))

  Object
  (toString [_]
    (str "#<WellRng1024a index=" index ">")))

(defn next-double!
  "Generate the next random double in [0,1), mutating the RNG state.
  Returns a double."
  ^double [^WellRng1024a rng]
  (.nextDouble rng))

(defn make-well-rng-1024a
  "Create a WellRng1024a instance.
  Returns a mutable RNG that generates doubles in [0,1) via next-double!.

  Arities:
  - () - Uses non-deterministic seed from rand-int
  - (seed) - Uses java.util.Random with given seed for deterministic output

  See: Improved Long-Period Generators Based on Linear Recurrences Modulo 2
  F. Panneton, P. L'Ecuyer and M. Matsumoto
  http://www.iro.umontreal.ca/~panneton/WELLRNG.html"
  ([]
   (->WellRng1024a
    (long-array 32 (repeatedly 32 #(rand-int Integer/MAX_VALUE)))
    (rand-int 32)))
  ([^long seed]
   (let [rng (java.util.Random. seed)]
     (->WellRng1024a
      (long-array 32 (repeatedly 32 #(.nextInt rng Integer/MAX_VALUE)))
      (.nextInt rng 32)))))

