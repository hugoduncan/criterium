(ns criterium.util.kde-test
  ;; Tests for the KDE (Kernel Density Estimation) module.
  ;; Verifies ISJ bandwidth selection, Gaussian KDE evaluation,
  ;; mode detection, and bootstrap confidence intervals.
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.util.kde :as kde]
   [criterium.util.well :as well]))

(deftest silverman-bandwidth-test
  ;; Tests Silverman's rule of thumb bandwidth selector against
  ;; expected values for known distributions.
  (testing "silverman-bandwidth"
    (testing "returns reasonable bandwidth for uniform data"
      (let [data (range 0 100)
            h (kde/silverman-bandwidth data)]
        (is (pos? h) "bandwidth should be positive")
        (is (< h 50) "bandwidth should be less than half the range")))

    (testing "returns smaller bandwidth for tighter distributions"
      (let [wide-data (range 0 100)
            narrow-data (range 45 55)
            h-wide (kde/silverman-bandwidth wide-data)
            h-narrow (kde/silverman-bandwidth narrow-data)]
        (is (< h-narrow h-wide)
            "narrow distribution should have smaller bandwidth")))))

(deftest isj-bandwidth-test
  ;; Tests the ISJ bandwidth selector, which should produce reasonable
  ;; bandwidths and fall back to Silverman when ISJ gives unreasonable results.
  (testing "isj-bandwidth"
    (testing "returns positive bandwidth"
      (let [data (range 0 100)
            h (kde/isj-bandwidth data)]
        (is (pos? h) "bandwidth should be positive")))

    (testing "falls back to Silverman for problematic data"
      ;; Bimodal data where ISJ might give too large a bandwidth
      (let [bimodal (concat (range 0 10) (range 90 100))
            h (kde/isj-bandwidth bimodal)]
        (is (< h 45) "should not return bandwidth larger than half the range")))))

(deftest gaussian-kde-test
  ;; Tests the Gaussian KDE evaluation function for correct density
  ;; estimation properties.
  (testing "gaussian-kde"
    (testing "density integrates to approximately 1"
      (let [data [1.0 2.0 3.0 4.0 5.0]
            h 1.0
            grid (double-array (range 0.0 6.0 0.1))
            density (kde/gaussian-kde data h grid)
            dx 0.1
            total (* dx (reduce + density))]
        (is (< (Math/abs (- total 1.0)) 0.1)
            "density should integrate to approximately 1")))

    (testing "density is highest near data concentration"
      (let [data (double-array (repeat 10 5.0))
            h 1.0
            grid (double-array [0.0 2.5 5.0 7.5 10.0])
            density (kde/gaussian-kde (vec data) h grid)]
        (is (> (aget density 2) (aget density 0))
            "density at mode should be higher than at edges")
        (is (> (aget density 2) (aget density 4))
            "density at mode should be higher than at edges")))))

(deftest find-modes-test
  ;; Tests mode detection for unimodal and multimodal distributions.
  (testing "find-modes"
    (testing "finds single mode in unimodal density"
      (let [grid (double-array [0 1 2 3 4 5 6])
            ;; Peak at position 3
            density (double-array [0.1 0.2 0.3 0.5 0.3 0.2 0.1])
            modes (kde/find-modes grid density)]
        (is (= 1 (count modes)) "should find exactly one mode")
        (is (= 3.0 (:location (first modes))) "mode should be at position 3")))

    (testing "finds multiple modes in bimodal density"
      (let [grid (double-array [0 1 2 3 4 5 6 7 8])
            ;; Peaks at positions 2 and 6
            density (double-array [0.1 0.2 0.4 0.2 0.1 0.2 0.4 0.2 0.1])
            modes (kde/find-modes grid density)]
        (is (= 2 (count modes)) "should find two modes")
        (is (= #{2.0 6.0} (set (map :location modes)))
            "modes should be at positions 2 and 6")))

    (testing "returns modes sorted by density"
      (let [grid (double-array [0 1 2 3 4 5 6 7 8])
            ;; Higher peak at 2, lower peak at 6
            density (double-array [0.1 0.2 0.5 0.2 0.1 0.2 0.3 0.2 0.1])
            modes (kde/find-modes grid density)]
        (is (= 2.0 (:location (first modes)))
            "first mode should be the highest peak")))))

(deftest kde-confidence-bands-test
  ;; Tests bootstrap confidence bands for KDE density estimates.
  (testing "kde-confidence-bands"
    (testing "returns lower and upper bands of correct size"
      (let [data (range 0 50)
            h 2.0
            grid (double-array (range 0.0 50.0 1.0))
            bands (kde/kde-confidence-bands data h grid {:n-bootstrap 20})
            n-grid (alength grid)]
        (is (= n-grid (alength ^doubles (:lower bands))))
        (is (= n-grid (alength ^doubles (:upper bands))))))

    (testing "lower band <= upper band at all points"
      (let [data (range 0 50)
            h 2.0
            grid (double-array (range 0.0 50.0 2.0))
            bands (kde/kde-confidence-bands data h grid {:n-bootstrap 20})
            lower ^doubles (:lower bands)
            upper ^doubles (:upper bands)
            n (alength lower)]
        (is (every? true?
                    (for [i (range n)]
                      (<= (aget lower i) (aget upper i))))
            "lower band should be <= upper band everywhere")))))

(deftest kde-test
  ;; Tests the main kde function for correct output structure
  ;; and reasonable values.
  (testing "kde"
    (testing "returns correct structure"
      (let [data (range 0 100)
            result (kde/kde data {:n-bootstrap 10 :n-points 32})]
        (is (= :criterium/kde (:type result)))
        (is (number? (:bandwidth result)))
        (is (vector? (:grid result)))
        (is (vector? (:density result)))
        (is (vector? (:lower-band result)))
        (is (vector? (:upper-band result)))
        (is (vector? (:modes result)))
        (is (= 100 (:n result)))))

    (testing "throws on empty data"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"empty"
                            (kde/kde []))))

    (testing "throws on constant data"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"same"
                            (kde/kde (repeat 10 5.0)))))

    (testing "respects custom bandwidth"
      (let [data (range 0 100)
            h 5.0
            result (kde/kde data {:bandwidth h :n-bootstrap 10})]
        (is (= h (:bandwidth result)))))

    (testing "finds mode near data center for unimodal data"
      (let [;; Data concentrated around 50
            data (concat (range 40 60) (range 45 55))
            result (kde/kde data {:n-bootstrap 10 :n-points 64})]
        (when (seq (:modes result))
          (let [mode-loc (:location (first (:modes result)))]
            (is (< 40 mode-loc 60)
                "mode should be near the data concentration")))))))

(deftest dct-ii-test
  ;; Tests DCT-II implementation for basic properties.
  (testing "dct-ii"
    (testing "first coefficient equals sum of inputs"
      (let [data (double-array [1.0 2.0 3.0 4.0])
            result (kde/dct-ii data)]
        (is (< (Math/abs (- (aget result 0) 10.0)) 0.001)
            "DC component should equal sum of inputs")))

    (testing "returns array of same size"
      (let [data (double-array [1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0])
            result (kde/dct-ii data)]
        (is (= 8 (alength result)))))))

(deftest linear-bin-test
  ;; Tests linear binning for correct distribution of weights.
  (testing "linear-bin"
    (testing "weights sum to 1"
      (let [data [1.0 2.0 3.0 4.0 5.0]
            grid (double-array [0.0 2.0 4.0 6.0])
            weights (kde/linear-bin data grid)
            total (reduce + weights)]
        (is (< (Math/abs (- total 1.0)) 0.0001)
            "weights should sum to 1")))

    (testing "data at grid point goes to that bin"
      (let [data [2.0]
            grid (double-array [0.0 2.0 4.0 6.0])
            weights (kde/linear-bin data grid)]
        (is (> (aget weights 1) 0.9)
            "weight should be concentrated at matching grid point")))))
