(ns criterium.viewer.common.regression-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.viewer.common.regression :as regression]))

;;; Log-Log regression view helper tests.
;;; Verifies data preparation functions for log-log charts.

(deftest prepare-log-log-points-test
  ;; Tests prepare-log-log-points which formats log-log regression data for charts.
  ;; Contracts: returns correct structure, handles error bounds, supports multi-impl.
  (testing "prepare-log-log-points"
    (testing "returns points in log space"
      (let [log-log-data {:log-xs [(Math/log 10) (Math/log 20)]
                          :log-ys [(Math/log 100) (Math/log 200)]}
            result (regression/prepare-log-log-points
                    log-log-data
                    {:axis :n})]
        (is (map? result))
        (is (= 2 (count (:points result))))
        (is (= "n" (:axis-name result)))
        (is (= (Math/log 10) (get (first (:points result)) "x")))
        (is (= (Math/log 100) (get (first (:points result)) "y")))))
    (testing "includes error bounds when present"
      (let [log-log-data {:log-xs [(Math/log 10)]
                          :log-ys [(Math/log 100)]
                          :log-lowers [(Math/log 90)]
                          :log-uppers [(Math/log 110)]}
            result (regression/prepare-log-log-points
                    log-log-data
                    {:axis :n})]
        (is (true? (:has-error-bounds? result)))
        (is (= (Math/log 90) (get (first (:points result)) "yLower")))
        (is (= (Math/log 110) (get (first (:points result)) "yUpper")))))
    (testing "returns nil for missing data"
      (is (nil? (regression/prepare-log-log-points nil {:axis :n})))
      (is (nil? (regression/prepare-log-log-points {} {:axis :n}))))
    (testing "handles multi-implementation data"
      (let [log-log-data {:by-impl {:vec {:log-xs [(Math/log 10)]
                                          :log-ys [(Math/log 100)]}
                                    :list {:log-xs [(Math/log 10)]
                                           :log-ys [(Math/log 200)]}}}
            result (regression/prepare-log-log-points
                    log-log-data
                    {:axis :n :impl-axis :impl})]
        (is (= 2 (count (:points result))))
        (is (some #(= "vec" (get % "impl")) (:points result)))
        (is (some #(= "list" (get % "impl")) (:points result)))))))

(deftest prepare-log-log-fit-line-test
  ;; Tests prepare-log-log-fit-line which generates fit line points.
  ;; Uses slope and intercept to compute line: y = slope * x + intercept
  (testing "prepare-log-log-fit-line"
    (testing "generates fit line points"
      (let [log-log-data {:slope 1.0
                          :intercept 0.0
                          :log-xs [(Math/log 10) (Math/log 100)]}
            result (regression/prepare-log-log-fit-line
                    log-log-data
                    {:axis :n})]
        (is (vector? result))
        (is (> (count result) 2))
        ;; Check that y = x (slope 1, intercept 0)
        (is (every? #(< (Math/abs (- (double (get % "y")) (double (get % "x")))) 0.01) result))))
    (testing "returns nil for missing data"
      (is (nil? (regression/prepare-log-log-fit-line nil {:axis :n})))
      (is (nil? (regression/prepare-log-log-fit-line {} {:axis :n}))))
    (testing "handles multi-implementation data"
      (let [log-log-data {:by-impl {:vec {:slope 1.0
                                          :intercept 0.0
                                          :log-xs [(Math/log 10) (Math/log 20)]}
                                    :list {:slope 2.0
                                           :intercept 0.0
                                           :log-xs [(Math/log 10) (Math/log 20)]}}}
            result (regression/prepare-log-log-fit-line
                    log-log-data
                    {:axis :n :impl-axis :impl})]
        (is (vector? result))
        (is (some #(= "vec" (get % "impl")) result))
        (is (some #(= "list" (get % "impl")) result))))))

(deftest prepare-log-log-residuals-test
  ;; Tests prepare-log-log-residuals which formats residual points.
  ;; Residuals are pre-computed in analysis layer.
  (testing "prepare-log-log-residuals"
    (testing "returns residual points"
      (let [log-log-data {:log-xs [(Math/log 10) (Math/log 20)]
                          :residuals [0.01 -0.02]}
            result (regression/prepare-log-log-residuals
                    log-log-data
                    {:axis :n})]
        (is (vector? result))
        (is (= 2 (count result)))
        (is (= 0.01 (get (first result) "residual")))
        (is (= (Math/log 10) (get (first result) "x")))))
    (testing "returns nil for missing data"
      (is (nil? (regression/prepare-log-log-residuals nil {:axis :n})))
      (is (nil? (regression/prepare-log-log-residuals {} {:axis :n}))))
    (testing "handles multi-implementation data"
      (let [log-log-data {:by-impl {:vec {:log-xs [(Math/log 10)]
                                          :residuals [0.01]}
                                    :list {:log-xs [(Math/log 10)]
                                           :residuals [-0.01]}}}
            result (regression/prepare-log-log-residuals
                    log-log-data
                    {:axis :n :impl-axis :impl})]
        (is (= 2 (count result)))
        (is (some #(= "vec" (get % "impl")) result))
        (is (some #(= "list" (get % "impl")) result))))))

(deftest format-log-log-slope-test
  ;; Tests formatting of log-log slope as complexity class estimate.
  ;; The function uses a 0.05 (5%) tolerance for integer rounding.
  (testing "format-log-log-slope"
    (testing "exact integer slopes use simplified form"
      (is (= "O(1)" (regression/format-log-log-slope 0.0)))
      (is (= "O(n)" (regression/format-log-log-slope 1.0)))
      (is (= "O(n²)" (regression/format-log-log-slope 2.0)))
      (is (= "O(n³)" (regression/format-log-log-slope 3.0))))

    (testing "slopes greater than 3 use O(n^k) form"
      (is (= "O(n^4)" (regression/format-log-log-slope 4.0)))
      (is (= "O(n^5)" (regression/format-log-log-slope 5.0))))

    (testing "slopes within 0.05 of integer round to integer form"
      ;; Near 0: tolerance = 0.05
      (is (= "O(1)" (regression/format-log-log-slope 0.04)))
      (is (= "O(1)" (regression/format-log-log-slope -0.04)))
      ;; Near 1
      (is (= "O(n)" (regression/format-log-log-slope 0.96)))
      (is (= "O(n)" (regression/format-log-log-slope 1.04)))
      ;; Near 2
      (is (= "O(n²)" (regression/format-log-log-slope 1.96)))
      (is (= "O(n²)" (regression/format-log-log-slope 2.04)))
      ;; Near 3
      (is (= "O(n³)" (regression/format-log-log-slope 2.96)))
      (is (= "O(n³)" (regression/format-log-log-slope 3.04))))

    (testing "slopes outside 0.05 tolerance show decimal form"
      ;; Just outside the 0.05 threshold (testing boundary)
      (is (= "O(n^0.06)" (regression/format-log-log-slope 0.06)))
      (is (= "O(n^0.94)" (regression/format-log-log-slope 0.94)))
      (is (= "O(n^1.06)" (regression/format-log-log-slope 1.06)))
      (is (= "O(n^1.94)" (regression/format-log-log-slope 1.94)))
      (is (= "O(n^2.06)" (regression/format-log-log-slope 2.06)))
      (is (= "O(n^2.94)" (regression/format-log-log-slope 2.94))))

    (testing "non-integer slopes show two decimal places"
      (is (= "O(n^0.50)" (regression/format-log-log-slope 0.5)))
      (is (= "O(n^1.50)" (regression/format-log-log-slope 1.5)))
      (is (= "O(n^2.50)" (regression/format-log-log-slope 2.5)))
      (is (= "O(n^1.23)" (regression/format-log-log-slope 1.23))))

    (testing "edge cases at exact tolerance boundary"
      ;; The 0.05 threshold uses strict < comparison. Due to floating point
      ;; representation, values like 2.05, 2.95, 3.05 round to integer form
      ;; (their diff from nearest int is 0.04999... < 0.05), while 0.05, 0.95,
      ;; 1.05, 1.95 show decimal (their diff is exactly 0.05 or slightly more).
      (is (= "O(n^0.05)" (regression/format-log-log-slope 0.05)))
      (is (= "O(n^0.95)" (regression/format-log-log-slope 0.95)))
      (is (= "O(n^1.05)" (regression/format-log-log-slope 1.05)))
      (is (= "O(n^1.95)" (regression/format-log-log-slope 1.95)))
      ;; Larger values round to integer due to floating-point representation
      (is (= "O(n²)" (regression/format-log-log-slope 2.05)))
      (is (= "O(n³)" (regression/format-log-log-slope 2.95)))
      (is (= "O(n³)" (regression/format-log-log-slope 3.05)))
      ;; Just inside boundary: 0.049 is within tolerance (< 0.05)
      (is (= "O(1)" (regression/format-log-log-slope 0.049)))
      (is (= "O(n)" (regression/format-log-log-slope 0.951)))
      (is (= "O(n)" (regression/format-log-log-slope 1.049)))
      (is (= "O(n²)" (regression/format-log-log-slope 1.951)))
      (is (= "O(n²)" (regression/format-log-log-slope 2.049)))
      (is (= "O(n³)" (regression/format-log-log-slope 2.951)))
      (is (= "O(n³)" (regression/format-log-log-slope 3.049))))))

;;; Regression model table tests.
;;; Verifies prepare-regression-model-table and prepare-regression-model-table-multi-impl
;;; correctly format model data for table rendering, including AIC and BIC columns.

(deftest prepare-regression-model-table-test
  ;; Tests prepare-regression-model-table which formats model data for table rendering.
  ;; Contracts: returns vector of row maps with :model :r-squared :aic :bic :equation :best-fit keys.
  (testing "prepare-regression-model-table"
    (testing "includes AIC and BIC columns"
      (let [models [{:id :linear
                     :label "O(n)"
                     :equation-str "y = 2.5x + 1"
                     :r-squared 0.95
                     :aic 10.5
                     :bic 12.3}
                    {:id :quadratic
                     :label "O(n²)"
                     :equation-str "y = 0.1x² + 0.5"
                     :r-squared 0.98
                     :aic 8.2
                     :bic 10.1}]
            result (regression/prepare-regression-model-table
                    {:models models :best-fit :quadratic}
                    {})]
        (is (vector? result))
        (is (= 2 (count result)))
        (let [best-row (first (filter #(= "✓" (:best-fit %)) result))
              other-row (first (filter #(= "" (:best-fit %)) result))]
          (is (= "O(n²)" (:model best-row)))
          (is (= "0.9800" (:r-squared best-row)))
          (is (= "8.2" (:aic best-row)))
          (is (= "10.1" (:bic best-row)))
          (is (= "10.5" (:aic other-row)))
          (is (= "12.3" (:bic other-row))))))

    (testing "handles nil AIC/BIC values"
      (let [models [{:id :linear
                     :label "O(n)"
                     :equation-str "y = 2.5x + 1"
                     :r-squared 0.95
                     :aic nil
                     :bic nil}]
            result (regression/prepare-regression-model-table
                    {:models models :best-fit :linear}
                    {})]
        (is (= 1 (count result)))
        (is (nil? (:aic (first result))))
        (is (nil? (:bic (first result))))))

    (testing "handles missing AIC/BIC keys"
      (let [models [{:id :linear
                     :label "O(n)"
                     :equation-str "y = 2.5x + 1"
                     :r-squared 0.95}]
            result (regression/prepare-regression-model-table
                    {:models models :best-fit :linear}
                    {})]
        (is (= 1 (count result)))
        (is (nil? (:aic (first result))))
        (is (nil? (:bic (first result))))))

    (testing "formats negative AIC/BIC values"
      (let [models [{:id :linear
                     :label "O(n)"
                     :equation-str "y = 2.5x + 1"
                     :r-squared 0.95
                     :aic -15.7
                     :bic -12.3}]
            result (regression/prepare-regression-model-table
                    {:models models :best-fit :linear}
                    {})]
        (is (= "-15.7" (:aic (first result))))
        (is (= "-12.3" (:bic (first result))))))

    (testing "returns nil for empty models"
      (is (nil? (regression/prepare-regression-model-table {:models [] :best-fit nil} {}))))

    (testing "sorts models by r-squared descending"
      (let [models [{:id :linear :label "O(n)" :r-squared 0.8 :aic 10.0 :bic 12.0}
                    {:id :quadratic :label "O(n²)" :r-squared 0.95 :aic 8.0 :bic 10.0}
                    {:id :log :label "O(log n)" :r-squared 0.7 :aic 15.0 :bic 17.0}]
            result (regression/prepare-regression-model-table
                    {:models models :best-fit :quadratic}
                    {})]
        (is (= ["O(n²)" "O(n)" "O(log n)"]
               (mapv :model result)))))))

(deftest prepare-regression-model-table-multi-impl-test
  ;; Tests prepare-regression-model-table-multi-impl which formats multi-impl model data.
  ;; Contracts: returns vector with :implementation :model :r-squared :aic :bic :equation :best-fit.
  (testing "prepare-regression-model-table-multi-impl"
    (testing "includes implementation and AIC/BIC columns"
      (let [by-impl {:vec {:models [{:id :linear :label "O(n)" :r-squared 0.95 :aic 10.5 :bic 12.3}]
                           :best-fit :linear}
                     :list {:models [{:id :linear :label "O(n)" :r-squared 0.85 :aic 15.2 :bic 17.0}]
                            :best-fit :linear}}
            impl-keys [:vec :list]
            result (regression/prepare-regression-model-table-multi-impl by-impl impl-keys {})]
        (is (vector? result))
        (is (= 2 (count result)))
        (let [vec-row (first (filter #(= "vec" (:implementation %)) result))
              list-row (first (filter #(= "list" (:implementation %)) result))]
          (is (= "10.5" (:aic vec-row)))
          (is (= "12.3" (:bic vec-row)))
          (is (= "15.2" (:aic list-row)))
          (is (= "17.0" (:bic list-row))))))

    (testing "handles nil AIC/BIC in multi-impl"
      (let [by-impl {:vec {:models [{:id :linear :label "O(n)" :r-squared 0.95}]
                           :best-fit :linear}}
            result (regression/prepare-regression-model-table-multi-impl by-impl [:vec] {})]
        (is (nil? (:aic (first result))))
        (is (nil? (:bic (first result))))))))
