(ns criterium.viewer.common.ascii-chart-test
  ;; Tests for ASCII chart rendering and LTTB downsampling.
  ;; Validates: LTTB correctness, chart dimensions, axis labeling, edge cases.
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.viewer.common.ascii-chart :as ascii-chart]))

;;; LTTB Downsampling Tests

(deftest lttb-downsample-test
  (testing "lttb-downsample"
    (testing "returns points unchanged when count <= target"
      (let [points [[0 0] [1 1] [2 4]]]
        (is (= points (ascii-chart/lttb-downsample points 5)))
        (is (= points (ascii-chart/lttb-downsample points 3)))))

    (testing "returns first and last when target is 2"
      (let [points [[0 0] [1 1] [2 4] [3 9] [4 16]]]
        (is (= [[0 0] [4 16]] (ascii-chart/lttb-downsample points 2)))))

    (testing "preserves first and last points"
      (let [points [[0 0] [1 1] [2 4] [3 9] [4 16] [5 25]]
            result (ascii-chart/lttb-downsample points 4)]
        (is (= [0 0] (first result)))
        (is (= [5 25] (last result)))))

    (testing "returns correct count"
      (let [points (mapv (fn [x] [x (* x x)]) (range 100))]
        (is (= 10 (count (ascii-chart/lttb-downsample points 10))))
        (is (= 20 (count (ascii-chart/lttb-downsample points 20))))
        (is (= 50 (count (ascii-chart/lttb-downsample points 50))))))

    (testing "preserves peaks in data"
      ;; Data with a clear peak at x=5
      (let [points [[0 0] [1 1] [2 2] [3 3] [4 4]
                    [5 100] ; peak
                    [6 4] [7 3] [8 2] [9 1] [10 0]]
            result (ascii-chart/lttb-downsample points 5)
            y-values (mapv second result)]
        ;; The peak should be preserved
        (is (some #(> % 50) y-values))))

    (testing "handles single point"
      (is (= [[5 10]] (ascii-chart/lttb-downsample [[5 10]] 3))))

    (testing "handles two points"
      (is (= [[0 0] [1 1]] (ascii-chart/lttb-downsample [[0 0] [1 1]] 5))))))

;;; Chart Rendering Tests

(deftest render-chart-empty-test
  (testing "render-chart"
    (testing "returns empty vector for empty points"
      (is (= [] (ascii-chart/render-chart [] {}))))

    (testing "returns empty vector for nil points"
      (is (= [] (ascii-chart/render-chart nil {}))))))

(deftest render-chart-dimensions-test
  (testing "render-chart dimensions"
    (testing "respects width and height options"
      (let [points [[0 0] [10 10]]
            result (ascii-chart/render-chart points {:width 40 :height 10})]
        ;; Height should be approximately 10 lines (may vary slightly with labels)
        (is (<= 10 (count result) 12))
        ;; Width of plot lines should not exceed 40
        (is (every? #(<= (count %) 45) result))))

    (testing "uses default dimensions when not specified"
      (let [points [[0 0] [10 10]]
            result (ascii-chart/render-chart points {})]
        ;; Default height is 20
        (is (<= 20 (count result) 22))))))

(deftest render-chart-single-point-test
  (testing "render-chart with single point"
    (let [points [[5 10]]
          result (ascii-chart/render-chart points {:width 40 :height 10})]
      ;; Should render without error
      (is (vector? result))
      (is (pos? (count result)))
      ;; Should contain at least one point character
      (is (some #(str/includes? % "*") result)))))

(deftest render-chart-constant-y-test
  (testing "render-chart with constant y values"
    (let [points [[0 5] [1 5] [2 5] [3 5]]
          result (ascii-chart/render-chart points {:width 40 :height 10})]
      ;; Should render without error
      (is (vector? result))
      (is (pos? (count result))))))

(deftest render-chart-constant-x-test
  (testing "render-chart with constant x values"
    (let [points [[5 0] [5 1] [5 2] [5 3]]
          result (ascii-chart/render-chart points {:width 40 :height 10})]
      ;; Should render without error
      (is (vector? result))
      (is (pos? (count result))))))

(deftest render-chart-axis-labels-test
  (testing "render-chart axis labels"
    (testing "includes y-axis labels"
      (let [points [[0 0] [10 100]]
            result (ascii-chart/render-chart points {:width 60 :height 15})]
        ;; Y-axis should have numeric labels
        (is (some #(re-find #"\d" %) result))))

    (testing "includes x-axis labels on bottom line"
      (let [points [[0 0] [100 100]]
            result (ascii-chart/render-chart points {:width 60 :height 15})
            bottom-lines (take-last 2 result)]
        ;; Bottom should have x-axis labels
        (is (some #(re-find #"\d" %) bottom-lines))))))

(deftest render-chart-si-units-test
  (testing "render-chart with SI units"
    (testing "formats time dimension"
      (let [points [[0 0.001] [1 0.002]] ; values in seconds (1-2ms range)
            result (ascii-chart/render-chart points {:width 60
                                                     :height 15
                                                     :dimension :time})]
        ;; Should include time unit indicator in output
        (is (some #(or (str/includes? % "ms")
                       (str/includes? % "µs")
                       (str/includes? % "s"))
                  result))))))

(deftest render-chart-point-char-test
  (testing "render-chart custom point character"
    (let [points [[0 0] [10 10]]
          result (ascii-chart/render-chart points {:width 40
                                                   :height 10
                                                   :point-char \o})]
      ;; Should contain custom point character
      (is (some #(str/includes? % "o") result)))))

(deftest render-chart-no-line-test
  (testing "render-chart without line interpolation"
    (let [points [[0 0] [5 5] [10 0]]
          result (ascii-chart/render-chart points {:width 40
                                                   :height 10
                                                   :line-char nil})]
      ;; Should render without error
      (is (vector? result))
      ;; Should contain point markers
      (is (some #(str/includes? % "*") result)))))

(deftest render-chart-title-test
  (testing "render-chart with title"
    (let [points [[0 0] [10 10]]
          result (ascii-chart/render-chart points {:width 40
                                                   :height 10
                                                   :title "Test Chart"})]
      ;; Title should be first line
      (is (= "Test Chart" (first result))))))

(deftest render-chart-downsampling-test
  (testing "render-chart automatic downsampling"
    (let [;; 200 points, but width only allows ~30 plot columns
          points (mapv (fn [x] [x (Math/sin (/ x 10.0))]) (range 200))
          result (ascii-chart/render-chart points {:width 40 :height 10})]
      ;; Should render without error
      (is (vector? result))
      (is (pos? (count result))))))

;;; Render Chart Simple Tests

(deftest render-chart-simple-test
  (testing "render-chart-simple"
    (testing "renders with default width"
      (let [points [[0 0] [10 10]]
            result (ascii-chart/render-chart-simple points)]
        (is (vector? result))
        (is (pos? (count result)))))

    (testing "renders with custom width"
      (let [points [[0 0] [10 10]]
            result (ascii-chart/render-chart-simple points 40)]
        (is (vector? result))
        ;; Height should be width/4 = 10
        (is (<= 10 (count result) 12))))))

;;; Edge Cases

(deftest render-chart-negative-values-test
  (testing "render-chart with negative values"
    (let [points [[-10 -5] [0 0] [10 5]]
          result (ascii-chart/render-chart points {:width 40 :height 10})]
      (is (vector? result))
      (is (pos? (count result))))))

(deftest render-chart-large-range-test
  (testing "render-chart with large value range"
    (let [points [[0 0] [1000000 1000000]]
          result (ascii-chart/render-chart points {:width 40 :height 10})]
      (is (vector? result))
      (is (pos? (count result))))))

(deftest render-chart-small-range-test
  (testing "render-chart with small value range"
    (let [points [[0.001 0.001] [0.002 0.002]]
          result (ascii-chart/render-chart points {:width 40 :height 10})]
      (is (vector? result))
      (is (pos? (count result))))))

(deftest render-chart-many-points-test
  (testing "render-chart with many points"
    (let [points (mapv (fn [x] [x (* x x)]) (range 1000))
          result (ascii-chart/render-chart points {:width 80 :height 20})]
      (is (vector? result))
      (is (pos? (count result)))
      ;; Should downsample and still render quickly
      (is (some #(str/includes? % "*") result)))))
