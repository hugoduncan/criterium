(ns criterium.test-utils
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.generators :as gen]))

(defn abs-error
  ^double [^double expected ^double actual]
  (Math/abs (- expected actual)))

(defn rel-error
  ^double [^double expected ^double actual]
  (let [e (abs-error expected actual)]
    (if (zero? expected)
      actual
      (/ e  (Math/abs expected)))))

(def ^:private ^:const default-ulps 2)
(def ^:private ^:const default-rel-tolerance 1e-8)

(defn ulp ^double [x]
  (Math/ulp (double x)))

(defn compare-doubles
  "Compare expected and actual doubles.

  Uses both ULP and relative difference.  Numbers are considered equal
  if either criterion matches.  Relative difference is calculated
  relative to expected value.

  Parameters:
    expected - Expected value
    actual   - Actual value to compare against expected
    ulps - Max units in last place difference (default: 2)
    rel-tolerance - Maximum relative difference (default: 1e-8)"
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

(defn element-diffs [expected actual rel-tolerance ulps]
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
                  ~(if ulps ulps default-ulps )))]
     (clojure.test/do-report
      {:type     (if diff# :fail :pass)
       :message  ~msg
       :expected ~expected
       :actual   ~actual
       :diff     diff#})))

(defmacro test-max-error
  ([expected actual max-error]
   `(is (< (abs-error ~expected ~actual) ~max-error)))
  ([expected actual max-error msg]
   `(is (< (abs-error ~expected ~actual) ~max-error) ~msg)))

(defn gen-bounded
  "Generates a long in the range from min-val to max-val inclusive.
  Unlike gen/choose, this is bounded by the generator's `size` parameter,
  starting at min-val."
  [^long min-val ^long max-val]
  (let [r (- max-val min-val)]
    (gen/fmap
     (fn [^long x] (+ min-val x))
     (gen/sized
      (fn [^long size]
        (let [s (min size r)]
          (gen/choose 0 s)))))))

(defn trimmed-lines
  [s]
  (->> s
       str/split-lines
       (mapv str/trim)))

(defn plus-frac ^double [^double x ^double f]
  (+ x (* x f)))

(deftest approximately=-test
  (testing "exact equality"
    (is (nil? (compare-doubles 1.0 1.0)))
    (is (nil? (compare-doubles 0.0 0.0)))
    (is (nil? (compare-doubles -1.0 -1.0))))

  (testing "exact equality, integer args"
    (is (nil? (compare-doubles 1 1)))
    (is (nil? (compare-doubles 0 0)))
    (is (nil? (compare-doubles -1 -1))))

  (testing "ulp-based comparison"
    (let [x (+ 0.1 0.2)]
      (is (nil? (compare-doubles 0.3 x)))))

  (testing "ulp-based failure"
    (let [x (+ 0.1 (* 3 (ulp 0.1)))]
      (is (compare-doubles 0.3 x))))

  (testing "relative tolerance"
    (is (nil? (compare-doubles 100.0 100.1 0.01 2))) ; within 1% tolerance
    (is (some? (compare-doubles 100.0 101.0 0.001 2)))) ; exceeds 0.1% tolerance

  (testing "near zero values"
    (is (nil? (compare-doubles 0.0 1e-12))) ; within default tolerance
    (is (some? (compare-doubles 0.0 1e-6)))) ; exceeds tolerance

  (testing "large numbers"
    (is (nil? (compare-doubles 1e6 (plus-frac 1e6 1e-9)))) ; small rel diff
    (is (some? (compare-doubles 1e6 (plus-frac 1e6 1e-7))))) ; large rel diff

  (testing "small numbers"
    (is (nil? (compare-doubles 1e-6 (plus-frac 1e-6 1e-9)))) ; small rel diff
    (is (some? (compare-doubles 1e-6 (plus-frac 1e-6 1e-7)))))) ;

(comment
  (deftest approx=-test
    (is (approx= 1 2))
    (is (approx= 1 1))
    (is (approx= 1.0 (plus-frac 1.0 0.1)))
    (is (approx= [1.0] [(plus-frac 1.0 0.1)]))
    (is (approx= [1.0] [(plus-frac 1.0 1e-9)]))))
