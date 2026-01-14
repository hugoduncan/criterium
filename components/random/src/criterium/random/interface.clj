(ns criterium.random.interface
  "Public API for the random component.

  Provides pseudo-random number generation:
  - WELL RNG 1024a algorithm for high-quality uniform random doubles
  - Ziggurat algorithm for normal (Gaussian) random variates

  Primary API (mutable RNGs for performance):
  - `make-well-rng-1024a` - Create a uniform RNG
  - `next-double!` - Generate next uniform random double in [0,1)
  - `make-normal-rng` - Create a normal/Gaussian RNG
  - `next-gaussian!` - Generate next standard normal variate

  References:
  - WELL RNG: Improved Long-Period Generators Based on Linear Recurrences
    Modulo 2, F. Panneton, P. L'Ecuyer and M. Matsumoto
    http://www.iro.umontreal.ca/~panneton/WELLRNG.html
  - Ziggurat: An improved Ziggurat method to generate normal random samples,
    Doornik, 2005"
  (:require
   [criterium.random.well :as well]
   [criterium.random.ziggurat :as ziggurat])
  (:import
   [criterium.random.well WellRng1024a]
   [criterium.random.ziggurat NormalRng]))

;;; Mutable WELL RNG API

(defn make-well-rng-1024a
  "Create a WellRng1024a instance for generating uniform random doubles.
  See `criterium.random.well/make-well-rng-1024a` for full documentation."
  (^WellRng1024a [] (well/make-well-rng-1024a))
  (^WellRng1024a [seed] (well/make-well-rng-1024a seed)))

(defn next-double!
  "Generate the next random double in [0,1), mutating the RNG state."
  ^double [^WellRng1024a rng]
  (well/next-double! rng))

;;; Mutable Ziggurat API

(defn make-normal-rng
  "Create a NormalRng instance for generating standard normal variates.
  See `criterium.random.ziggurat/make-normal-rng` for full documentation."
  (^NormalRng [] (ziggurat/make-normal-rng))
  (^NormalRng [uniform-rng] (ziggurat/make-normal-rng uniform-rng))
  (^NormalRng [uniform-rng c r v] (ziggurat/make-normal-rng uniform-rng c r v)))

(defn next-gaussian!
  "Generate the next random gaussian in N(0,1), mutating the RNG state."
  ^double [^NormalRng rng]
  (ziggurat/next-gaussian! rng))

