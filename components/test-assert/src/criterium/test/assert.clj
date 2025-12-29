(ns criterium.test.assert
  "clojure.test assertion utilities for approximate numeric comparisons.

  Provides `approx=` for comparing floating point numbers with configurable
  tolerance using both ULP (units in last place) and relative difference."
  (:require
   [clojure.test]))

;;; Constants

(def ^:private ^:const default-ulps 2)
(def ^:private ^:const default-rel-tolerance 1e-8)

;;; Error calculation

(defn abs-error
  "Absolute error between expected and actual values."
  ^double [^double expected ^double actual]
  (Math/abs (- expected actual)))

(defn rel-error
  "Relative error between expected and actual values."
  ^double [^double expected ^double actual]
  (let [e (abs-error expected actual)]
    (if (zero? expected)
      actual
      (/ e (Math/abs expected)))))

(defn ulp
  "Units in last place for the given value."
  ^double [x]
  (Math/ulp (double x)))

;;; Comparison

(defn compare-doubles
  "Compare expected and actual doubles.

  Uses both ULP and relative difference. Numbers are considered equal
  if either criterion matches. Relative difference is calculated
  relative to expected value.

  Returns nil if values are approximately equal, otherwise returns a map
  with comparison details.

  Parameters:
    expected - Expected value
    actual   - Actual value to compare against expected
    rel-tolerance - Maximum relative difference (default: 1e-8)
    ulps - Max units in last place difference (default: 2)"
  ([expected actual]
   (compare-doubles expected actual default-rel-tolerance default-ulps))
  ([^double expected ^double actual ^double rel-tolerance ^long ulps]
   (let [expected      (double expected)
         actual        (double actual)
         abs-expected  (Math/abs expected)
         diff          (- actual expected)
         abs-diff      (Math/abs diff)
         rel-diff      (if (zero? expected)
                         diff
                         (/ diff abs-expected))
         abs-rel-diff  (Math/abs rel-diff)
         ulp-tolerance (* ulps (Math/ulp abs-expected))]
     (when-not
      (or (<= abs-diff ulp-tolerance)
          (<= abs-rel-diff rel-tolerance))
       {:diff          diff
        :abs-diff      abs-diff
        :abs-rel-diff  abs-rel-diff
        :ulp-limit     ulp-tolerance
        :rel-tolerance rel-tolerance}))))

(defn element-diffs
  "Compare sequences element-wise, returning vector of differences."
  [expected actual rel-tolerance ulps]
  (let [element-diffs#
        (mapv #(compare-doubles
                %1
                %2
                (or rel-tolerance default-rel-tolerance)
                (or ulps default-ulps))
              expected
              actual)]
    (->> element-diffs#
         (map-indexed
          (fn [i diff]
            (when diff (assoc diff :i i))))
         (filterv some?)
         not-empty)))

;;; clojure.test integration

(defmethod clojure.test/assert-expr 'approx=
  [msg [_ expected actual & [rel-tolerance ulps]]]
  `(let [diff# (if (sequential? ~expected)
                 (cond
                   (not (sequential? ~actual))
                   {:type (type ~actual)}
                   (not= (count ~expected) (count ~actual))
                   {:count-expected (count ~expected)
                    :count-actual   (count ~actual)}
                   :else
                   (element-diffs ~expected ~actual ~rel-tolerance ~ulps))
                 (compare-doubles
                  ~expected
                  ~actual
                  ~(if rel-tolerance rel-tolerance default-rel-tolerance)
                  ~(if ulps ulps default-ulps)))]
     (clojure.test/do-report
      {:type     (if diff# :fail :pass)
       :message  ~msg
       :expected ~expected
       :actual   ~actual
       :diff     diff#})))

(defn approx=
  "Check if two numbers are approximately equal.

  Returns true if values are within tolerance, false otherwise.
  Prints comparison details when values differ."
  [a b & [rel-tolerance ulps]]
  (let [comp (compare-doubles
              a
              b
              (or rel-tolerance default-rel-tolerance)
              (or ulps default-ulps))]
    (when comp
      (prn :a a :b b comp))
    (nil? comp)))

;;; Assertion macros

(defmacro test-max-error
  "Assert that absolute error is less than max-error."
  ([expected actual max-error]
   `(clojure.test/is (< (abs-error ~expected ~actual) ~max-error)))
  ([expected actual max-error msg]
   `(clojure.test/is (< (abs-error ~expected ~actual) ~max-error) ~msg)))
