(ns random.interface
  "Public API for the random component.

  Provides pseudo-random number generation:
  - WELL RNG 1024a algorithm for high-quality uniform random doubles
  - Ziggurat algorithm for normal (Gaussian) random variates

  References:
  - WELL RNG: Improved Long-Period Generators Based on Linear Recurrences
    Modulo 2, F. Panneton, P. L'Ecuyer and M. Matsumoto
    http://www.iro.umontreal.ca/~panneton/WELLRNG.html
  - Ziggurat: An improved Ziggurat method to generate normal random samples,
    Doornik, 2005"
  (:require
   [random.well :as well]
   [random.ziggurat :as ziggurat]))

;;; WELL RNG

(defn well-rng-1024a
  "Well RNG 1024a.
  Returns a lazy sequence of random doubles in [0,1).

  Arities:
  - () - Uses non-deterministic seed from rand-int
  - (seed) - Uses java.util.Random with given seed for deterministic output
  - (state index) - Uses explicit state array and index

  See: Improved Long-Period Generators Based on Linear Recurrences Modulo 2
  F. Panneton, P. L'Ecuyer and M. Matsumoto
  http://www.iro.umontreal.ca/~panneton/WELLRNG.html"
  ([] (well/well-rng-1024a))
  ([seed] (well/well-rng-1024a seed))
  ([state index] (well/well-rng-1024a state index)))

;;; Ziggurat

(def ^:dynamic ^Long *zignor-c*
  "Number of blocks in the ziggurat. Default: 128"
  ziggurat/*zignor-c*)

(def ^:dynamic ^Double *zignor-r*
  "Start of the right tail. Default: 3.442619855899"
  ziggurat/*zignor-r*)

(def ^:dynamic ^Double *zignor-v*
  "Ziggurat volume parameter. Default: 9.91256303526217e-3"
  ziggurat/*zignor-v*)

(def zignor-init
  "Initialise ziggurat tables.
  Returns [s-adzigr s-adzigx r mask] for use with random-normal-zig."
  ziggurat/zignor-init)

(defn random-normal-zig
  "Pseudo-random normal variates using the Ziggurat algorithm.
  Returns a lazy sequence of standard normal deviates (mean=0, variance=1).

  Arities:
  - () - Uses WELL RNG with default ziggurat parameters
  - (rng-seq) - Uses provided RNG sequence with default parameters
  - (rng-seq c r v) - Uses provided RNG with custom ziggurat parameters
  - (c r v) - Uses WELL RNG with custom ziggurat parameters
  - (rng-seq tables) - Uses provided RNG with pre-initialized tables

  See: An improved Ziggurat method to generate normal random samples,
  Doornik, 2005"
  ([] (ziggurat/random-normal-zig))
  ([rng-seq] (ziggurat/random-normal-zig rng-seq))
  ([rng-seq-or-c arg2 arg3]
   (if (number? rng-seq-or-c)
     (ziggurat/random-normal-zig rng-seq-or-c arg2 arg3)
     (ziggurat/random-normal-zig rng-seq-or-c arg2 arg3)))
  ([rng-seq tables] (ziggurat/random-normal-zig rng-seq tables)))
