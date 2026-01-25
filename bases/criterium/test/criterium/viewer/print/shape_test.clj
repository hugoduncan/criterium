(ns criterium.viewer.print.shape-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.test-data :as test-data]
   [criterium.test-utils :refer [trimmed-lines]]
   [criterium.view :as view]
   [criterium.viewer.print.shape]))

;; Tests the print viewer output for shape-stats results.
;; Covers: skewness, kurtosis, and CV with their classifications.

(deftest shape-stats-print-test
  (testing "shape-stats*"
    (testing "prints shape statistics with classifications"
      (let [data-map (test-data/bootstrap-stats-with-shape-map)
            output (with-out-str (view/shape-stats* :print {} data-map))
            lines (trimmed-lines output)]
        (is (some #(str/includes? % "skewness") lines))
        (is (some #(str/includes? % "kurtosis") lines))
        (is (some #(str/includes? % "CV") lines))
        (is (some #(str/includes? % "slightly right-skewed") lines))
        (is (some #(str/includes? % "normal tails") lines))
        (is (some #(str/includes? % "low variability") lines))))
    (testing "handles missing bootstrap data gracefully"
      (let [output (with-out-str (view/shape-stats* :print {} {}))]
        (is (str/blank? output))))
    (testing "uses custom bootstrap-stats-id"
      (let [data-map (test-data/bootstrap-stats-with-shape-map)
            custom-map {:my-bootstrap (:bootstrap-stats data-map)}
            output (with-out-str
                     (view/shape-stats* :print {:bootstrap-stats-id :my-bootstrap}
                                        custom-map))]
        (is (str/includes? output "skewness"))))))
