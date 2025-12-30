(ns criterium.util.ziggurat
  "Ziggurat algorithm for generating normal random variates.

  Re-exports from random.interface for backward compatibility.
  New code should use random.interface directly.

  See: An improved Ziggurat method to generate normal random samples,
  Doornik, 2005"
  (:require
   [random.ziggurat :as ziggurat]))

;;; Re-export from random.ziggurat for backward compatibility

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

(def random-normal-zig
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
  ziggurat/random-normal-zig)
