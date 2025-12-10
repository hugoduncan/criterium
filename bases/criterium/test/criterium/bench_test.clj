(ns criterium.bench-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse]
   [criterium.bench :as bench]
   [criterium.bench.impl :as bench-impl]
   [criterium.bench-plans :as bench-plans]
   [criterium.viewer.kindly :as kindly]))

(deftest bench-test
  (testing "bench"
    (bench-impl/last-bench! nil)
    (is (nil? (bench/last-bench)))
    (let [out (with-out-str (bench/bench 1))]
      (testing "outputs the estimated time on stdout"
        (is (re-find
             #"Elapsed Time: [0-9.]+ [mn]s  3σ \[[0-9.-]+ [0-9.]+]  min [0-9.]+"
             out)))))
  (testing "time with stats"
    (let [out (with-out-str (bench/bench 1 :limit-time-s 0.1))]
      (testing "outputs statistics on stdout"
        (is (re-find #"3σ" out)))))
  (testing "time with one-shot"
    (let [out (with-out-str (bench/bench 1 :collect-plan :one-shot))]
      (testing "outputs statistics on stdout"
        (is (not (re-find #"±" out))))))
  (testing "time returns expression-value"
    (with-out-str
      (let [v (bench/bench 1)]
        (is (= 1 v)))))

  (testing "all pipelines"
    (with-out-str
      (let [v (bench/bench
               1
               :limit-time-s 0.1
               :metric-ids [:elapsed-time
                            :memory
                            :thread-allocation
                            :garbage-collector
                            :finalization
                            :compilation
                            :measured-args
                            :class-loader])]
        (is (= 1 v))))))

(deftest kindly-viewer-integration-test
  ;; Integration test for :kindly viewer with actual benchmark execution.
  ;; Verifies that :viewer :kindly produces Kindly-annotated output
  ;; suitable for Clay notebook rendering.
  (testing ":kindly viewer"
    (testing "with one-shot collect plan"
      (reset! kindly/accumulated [])
      (let [result (bench/bench (+ 1 1) :viewer :kindly :collect-plan :one-shot)]
        (is (= 2 result)
            "bench returns expression value"))
      (is (empty? @kindly/accumulated)
          "accumulator is empty after flush"))

    (testing "with full benchmark and log-histogram plan"
      (reset! kindly/accumulated [])
      (bench/bench (+ 1 1)
                   :viewer :kindly
                   :bench-plan bench-plans/log-histogram
                   :limit-time-s 0.5)
      (is (empty? @kindly/accumulated)
          "accumulator is empty after flush - fragment was returned by flush-viewer"))

    (testing "view returns kindly fragment"
      ;; Use view directly to verify fragment is returned
      (let [data-map (:data (do (with-out-str
                                  (bench/bench (+ 1 1)
                                               :viewer :print
                                               :collect-plan :one-shot))
                                (bench/last-bench)))]
        (reset! kindly/accumulated [])
        (let [fragment (bench/view [:metrics :collect-plan] :kindly data-map)]
          (is (= :kind/fragment (:kindly/kind (meta fragment)))
              "view returns kind/fragment")
          (is (pos? (count fragment))
              "fragment contains accumulated views"))))

    (testing "with log-histogram plan produces full output"
      ;; Get benchmark data using :print viewer
      (let [data-map (:data (do (with-out-str
                                  (bench/bench (+ 1 1)
                                               :viewer :print
                                               :bench-plan bench-plans/log-histogram
                                               :limit-time-s 0.5))
                                (bench/last-bench)))]
        ;; View with :kindly - bench/view returns the fragment
        (reset! kindly/accumulated [])
        (let [fragment (bench/view (:view bench-plans/log-histogram) :kindly data-map)]
          (is (= :kind/fragment (:kindly/kind (meta fragment)))
              "view returns kind/fragment for kindly viewer")
          (is (>= (count fragment) 10)
              "fragment contains multiple views (headings, tables, charts)")

          (let [kinds (set (map #(:kindly/kind (meta %)) fragment))]
            (is (contains? kinds :kind/md)
                "contains markdown headings")
            (is (contains? kinds :kind/table)
                "contains tables")
            (is (contains? kinds :kind/vega-lite)
                "contains Vega-Lite charts"))

          (let [tables (filter #(= :kind/table (:kindly/kind (meta %))) fragment)]
            (is (pos? (count tables))
                "has at least one table")
            (is (every? sequential? tables)
                "tables are sequences"))

          (let [charts (filter #(= :kind/vega-lite (:kindly/kind (meta %))) fragment)]
            (is (pos? (count charts))
                "has at least one chart")
            (is (every? #(string? (:$schema %)) charts)
                "charts have Vega-Lite schema")))))))
