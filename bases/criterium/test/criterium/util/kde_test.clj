(ns criterium.util.kde-test
  ;; Tests for the KDE (Kernel Density Estimation) module.
  ;; Verifies ISJ bandwidth selection, Gaussian KDE evaluation,
  ;; mode detection, and bootstrap confidence intervals.
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.util.kde :as kde]))

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
  ;; and reasonable values. Note: modes are now computed separately
  ;; via the modes analysis step.
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
        (is (nil? (:modes result)) "modes are computed separately")
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
        (is (= h (:bandwidth result)))))))

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

(deftest count-modes-test
  ;; Tests the count-modes function for mode counting at different bandwidths.
  (testing "count-modes"
    (testing "finds single mode for unimodal data with appropriate bandwidth"
      (let [data (range 0 100)]
        (is (= 1 (kde/count-modes data 20.0 128))
            "large bandwidth should give single mode")))

    (testing "finds multiple modes for bimodal data with small bandwidth"
      (let [bimodal (concat (repeat 50 10.0) (repeat 50 90.0))]
        (is (>= (kde/count-modes bimodal 5.0 128) 2)
            "small bandwidth on bimodal data should find multiple modes")))))

(deftest critical-bandwidth-test
  ;; Tests critical bandwidth computation for mode testing.
  (testing "critical-bandwidth"
    (testing "returns positive bandwidth"
      (let [data (range 0 100)
            h (kde/critical-bandwidth data 1 {})]
        (is (pos? h) "critical bandwidth should be positive")))

    (testing "larger k allows smaller bandwidth"
      (let [data (range 0 100)
            h1 (kde/critical-bandwidth data 1 {})
            h2 (kde/critical-bandwidth data 2 {})]
        (is (<= h2 h1) "more modes allowed means smaller bandwidth OK")))

    (testing "finds appropriate bandwidth for bimodal data"
      (let [bimodal (concat (repeat 50 10.0) (repeat 50 90.0))
            h1 (kde/critical-bandwidth bimodal 1 {})
            h2 (kde/critical-bandwidth bimodal 2 {})]
        (is (> h1 h2) "bandwidth for 1 mode should be larger than for 2")))))

(deftest silverman-test-test
  ;; Tests Silverman's bootstrap test for multimodality.
  (testing "silverman-test"
    (testing "returns correct structure"
      (let [data (range 0 100)
            result (kde/silverman-test data 1 {:n-bootstrap 20})]
        (is (= 1 (:k result)))
        (is (number? (:critical-bandwidth result)))
        (is (number? (:p-value result)))
        (is (<= 0 (:p-value result) 1) "p-value should be between 0 and 1")
        (is (boolean? (:corrected? result)))))

    (testing "applies Hall-York correction for k=1"
      (let [data (range 0 100)
            result (kde/silverman-test data 1 {:n-bootstrap 20})]
        (is (:corrected? result) "k=1 should have correction applied")))

    (testing "does not apply correction for k>1"
      (let [data (range 0 100)
            result (kde/silverman-test data 2 {:n-bootstrap 20})]
        (is (not (:corrected? result)) "k>1 should not have correction")))

    (testing "unimodal Gaussian data should not reject k=1"
      ;; Generate proper Gaussian data using Box-Muller transform
      (let [n 200
            normals (loop [i 0 result []]
                      (if (>= i n)
                        result
                        (let [u1 (max 1e-10 (rand))
                              u2 (rand)
                              z (* (Math/sqrt (* -2.0 (Math/log u1)))
                                   (Math/cos (* 2.0 Math/PI u2)))
                              ;; N(50, 10^2)
                              x (+ 50.0 (* 10.0 z))]
                          (recur (inc i) (conj result x)))))
            result (kde/silverman-test normals 1 {:n-bootstrap 100})]
        ;; p-value should be relatively high (fail to reject H0: <= 1 mode)
        (is (>= (:p-value result) 0.01)
            "Gaussian unimodal data should not strongly reject single mode")))))

(deftest excess-mass-test
  ;; Tests the excess mass statistic (Müller-Sawitzki 1991) for multimodality.
  ;; Verifies the algorithm correctly distinguishes unimodal from multimodal data.
  (testing "excess-mass"
    (testing "returns correct structure"
      (let [data (range 10 110)
            result (kde/excess-mass data 1)]
        (is (= 1 (:k result)))
        (is (= 100 (:n result)))
        (is (number? (:statistic result)))
        (is (>= (:statistic result) 0) "statistic should be non-negative")))

    (testing "handles small data"
      (let [result (kde/excess-mass [1 2 3 4 5] 1)]
        (is (number? (:statistic result)))))

    (testing "rejects insufficient data"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"at least 3"
                            (kde/excess-mass [1 2] 1))))

    (testing "rejects invalid k"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"at least 1"
                            (kde/excess-mass [1 2 3 4 5] 0))))

    (testing "unimodal data has small excess mass for k=1"
      ;; Uniform data is clearly unimodal
      (let [data (range 0 100)
            result (kde/excess-mass data 1)]
        (is (< (:statistic result) 0.2)
            "Uniform data should have small excess mass for k=1")))

    (testing "bimodal data has larger excess mass for k=1"
      ;; Two well-separated random clusters
      (let [cluster1 (repeatedly 50 rand) ;; [0, 1)
            cluster2 (repeatedly 50 #(+ 5.0 (rand))) ;; [5, 6)
            data (concat cluster1 cluster2)
            em-k1 (:statistic (kde/excess-mass data 1))
            em-k2 (:statistic (kde/excess-mass data 2))]
        (is (> em-k1 0.2)
            (str "Bimodal data should have larger excess mass for k=1, got: " em-k1))
        (is (< em-k2 em-k1)
            "Bimodal data should have smaller excess mass for k=2 than k=1")))

    (testing "handles ties in data via jitter"
      ;; Data with many repeated values
      (let [data (concat (repeat 30 1.0) (repeat 30 5.0) (repeat 30 10.0))
            result (kde/excess-mass data 1)]
        (is (number? (:statistic result))
            "Should handle ties without error")))

    (testing "statistic increases with more modes"
      ;; Compare unimodal vs bimodal random data for k=1
      (let [unimodal (repeatedly 100 rand)
            bimodal (concat (repeatedly 50 rand)
                            (repeatedly 50 #(+ 5.0 (rand))))
            em-unimodal (:statistic (kde/excess-mass unimodal 1))
            em-bimodal (:statistic (kde/excess-mass bimodal 1))]
        (is (< em-unimodal em-bimodal)
            "Bimodal data should have larger excess mass than unimodal")))))

(deftest mode-confidence-intervals-test
  ;; Tests mode confidence interval computation.
  (testing "mode-confidence-intervals"
    (testing "returns correct structure"
      (let [data (range 0 100)
            h (kde/isj-bandwidth data)
            grid (double-array (range 0.0 100.0 1.0))
            modes (kde/mode-confidence-intervals data h grid 3
                                                 {:n-bootstrap 20})]
        (is (vector? modes))
        (doseq [mode modes]
          (is (number? (:location mode)))
          (is (number? (:density mode)))
          (is (number? (:ci-lower mode)))
          (is (number? (:ci-upper mode))))))

    (testing "CI lower <= location <= CI upper"
      (let [data (concat (range 40 60) (range 45 55))
            h (kde/isj-bandwidth data)
            grid (double-array (range 0.0 100.0 1.0))
            modes (kde/mode-confidence-intervals data h grid 1
                                                 {:n-bootstrap 30})]
        (doseq [mode modes]
          (is (<= (:ci-lower mode) (:location mode))
              "CI lower should be <= location")
          (is (<= (:location mode) (:ci-upper mode))
              "location should be <= CI upper"))))))
