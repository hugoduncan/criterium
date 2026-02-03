(ns criterium.viewer.kindly-clay-test
  ;; Integration test verifying Kindly viewer output renders correctly via Clay.
  ;; Uses clay/make! to render benchmark results and verifies the HTML output
  ;; contains expected tables, charts, and structure.
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.bench :as bench]
   [criterium.bench-plans :as bench-plans]
   [scicloj.clay.v2.api :as clay]))

(defmacro with-temp-dir
  "Creates a temporary directory, binds it to dir-sym, cleans up after body."
  [[dir-sym] & body]
  `(let [temp-dir# (io/file (System/getProperty "java.io.tmpdir")
                            (str
                             "criterium-clay-test-"
                             (System/currentTimeMillis)))]
     (.mkdirs temp-dir#)
     (try
       (let [~dir-sym temp-dir#]
         ~@body)
       (finally
         (doseq [^java.io.File file# (reverse (file-seq temp-dir#))]
           (.delete file#))))))

(deftest ^:slow kindly-clay-rendering-test
  ;; Verifies that Kindly viewer output renders correctly through Clay.
  ;; Tests the full pipeline: benchmark -> Kindly fragment -> HTML via Clay.
  (testing "Kindly viewer Clay rendering"
    (testing "renders one-shot benchmark as HTML"
      (with-temp-dir [temp-dir]
        (let [fragment (bench/bench (reduce + (range 100))
                                    :viewer :kindly
                                    :collect-plan :one-shot)]
          (is (= :kind/fragment (:kindly/kind (meta fragment)))
              "bench returns Kindly fragment")

          (clay/make! {:single-value fragment
                       :base-target-path (str temp-dir)
                       :format [:html]
                       :show false
                       :browse false})

          (is (.exists (io/file temp-dir ".clay.html"))
              "Clay creates HTML output")

          (let [html (slurp (io/file temp-dir ".clay.html"))]
            (is (str/includes? html "<table")
                "HTML contains table elements")
            (is (str/includes? html "Elapsed Time")
                "HTML contains elapsed time metric")))))

    (testing "renders histogram benchmark with charts"
      (with-temp-dir [temp-dir]
        (let [fragment (bench/bench (reduce + (range 100))
                                    :viewer :kindly
                                    :bench-plan bench-plans/histogram
                                    :limit-time-s 0.5)]
          (is (= :kind/fragment (:kindly/kind (meta fragment)))
              "bench returns Kindly fragment")

          (clay/make! {:single-value fragment
                       :base-target-path (str temp-dir)
                       :format [:html]
                       :show false
                       :browse false})

          (let [html (slurp (io/file temp-dir ".clay.html"))]
            (is (str/includes? html "<table")
                "HTML contains table elements")
            (is (str/includes? html "Extremes")
                "HTML contains extremes heading")
            (is (str/includes? html "Quantiles")
                "HTML contains quantiles heading")
            (is (str/includes? html "Histogram")
                "HTML contains histogram heading")
            (is (str/includes? html "vegaEmbed")
                "HTML contains Vega-Lite chart embedding")
            (is (str/includes? html "vega-lite")
                "HTML references Vega-Lite")))))))
