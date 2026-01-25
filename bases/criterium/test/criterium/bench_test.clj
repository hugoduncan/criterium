(ns criterium.bench-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse]
   [criterium.bench :as bench]
   [criterium.bench-plans :as bench-plans]
   [criterium.bench.config :as bench-config]
   [criterium.bench.impl :as bench-impl]
   [criterium.viewer.kindly :as kindly]))

(deftest ^:slow bench-test
  (testing "bench"
    (bench-impl/last-bench! nil)
    (is (nil? (bench/last-bench)))
    (let [out (with-out-str (bench/bench 1 :limit-time-s 0.1))]
      (testing "outputs the estimated time on stdout"
        (is (re-find #"Elapsed Time median:" out)))))
  (testing "time with stats"
    (let [out (with-out-str (bench/bench 1 :limit-time-s 0.1))]
      (testing "outputs extremes on stdout"
        (is (re-find #"extremes:" out)))))
  (testing "time with one-shot"
    (let [out (with-out-str (bench/bench 1 :collect-plan :one-shot))]
      (testing "outputs statistics on stdout"
        (is (not (re-find #"±" out))))))
  (testing "time returns expression-value"
    (with-out-str
      (let [v (bench/bench 1 :limit-time-s 0.1)]
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

(deftest ^:slow kindly-viewer-integration-test
  ;; Integration test for :kindly viewer with actual benchmark execution.
  ;; Verifies that :viewer :kindly produces Kindly-annotated output
  ;; suitable for Clay notebook rendering.
  (testing ":kindly viewer"
    (testing "with one-shot collect plan"
      (reset! kindly/accumulated [])
      (let [result (bench/bench (+ 1 1) :viewer :kindly :collect-plan :one-shot)]
        (is (= :kind/fragment (:kindly/kind (meta result)))
            "bench returns kindly fragment")
        (is (sequential? result)
            "result is a sequence of views"))
      (is (empty? @kindly/accumulated)
          "accumulator is empty after flush"))

    (testing "with full benchmark and log-histogram plan"
      (reset! kindly/accumulated [])
      (bench/bench (+ 1 1)
                   :viewer :kindly
                   :bench-plan bench-plans/log-histogram
                   :limit-time-s 0.2)
      (is (empty? @kindly/accumulated)
          "accumulator is empty after flush - fragment was returned by flush-viewer"))

    (testing "view returns kindly fragment"
      ;; Use view directly to verify fragment is returned
      ;; Strip :viewer key since it was added by the previous bench call
      (let [data-map (dissoc
                      (:data (do (with-out-str
                                   (bench/bench (+ 1 1)
                                                :viewer :print
                                                :collect-plan :one-shot))
                                 (bench/last-bench)))
                      :viewer)]
        (reset! kindly/accumulated [])
        (let [fragment (bench/view [:metrics :collect-plan] :kindly data-map)]
          (is (= :kind/fragment (:kindly/kind (meta fragment)))
              "view returns kind/fragment")
          (is (pos? (count fragment))
              "fragment contains accumulated views"))))

    (testing "with log-histogram plan produces full output"
      ;; Get benchmark data using :print viewer
      ;; Strip :viewer key since it was added by the previous bench call
      (let [data-map (dissoc
                      (:data (do (with-out-str
                                   (bench/bench (+ 1 1)
                                                :viewer :print
                                                :bench-plan bench-plans/log-histogram
                                                :limit-time-s 0.2))
                                 (bench/last-bench)))
                      :viewer)]
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
            (is (every? #(or (sequential? %)
                             (and (map? %) (contains? % :row-maps)))
                        tables)
                "tables are sequences or maps with :row-maps"))

          (let [charts (filter #(= :kind/vega-lite (:kindly/kind (meta %))) fragment)]
            (is (pos? (count charts))
                "has at least one chart")
            (is (every? #(string? (:$schema %)) charts)
                "charts have Vega-Lite schema")))))))

(deftest default-viewer-test
  ;; Test default viewer configuration and precedence.
  ;; Verifies that:
  ;; 1. Initial default is :print
  ;; 2. set-default-viewer! changes the default
  ;; 3. Explicit :viewer option overrides the default
  (testing "default-viewer"
    (testing "returns initial default of :print"
      (bench/set-default-viewer! :print)
      (is (= :print (bench/default-viewer))))

    (testing "set-default-viewer! changes the default"
      (let [original (bench/default-viewer)]
        (try
          (bench/set-default-viewer! :kindly)
          (is (= :kindly (bench/default-viewer)))
          (finally
            (bench/set-default-viewer! original)))))

    (testing "config-map uses default viewer when no explicit option"
      (let [original (bench/default-viewer)]
        (try
          (bench/set-default-viewer! :pprint)
          (let [config (bench-config/config-map {})]
            (is (= :pprint (:viewer config))))
          (finally
            (bench/set-default-viewer! original)))))

    (testing "explicit :viewer option overrides default"
      (let [original (bench/default-viewer)]
        (try
          (bench/set-default-viewer! :kindly)
          (let [config (bench-config/config-map {:viewer :portal})]
            (is (= :portal (:viewer config))))
          (finally
            (bench/set-default-viewer! original)))))

    (testing "dynamic var can be bound for local scope"
      (is (= :print (bench/default-viewer)))
      (binding [bench-config/*default-viewer* :kindly]
        (let [config (bench-config/config-map {})]
          (is (= :kindly (:viewer config)))))
      (is (= :print (bench/default-viewer))))))

;; Tests for :with-allocation-trace option.
;; Validates integration of allocation tracing into the bench pipeline.
;; Agent may or may not be attached in test environment.

(deftest ^:slow with-allocation-trace-test
  (testing ":with-allocation-trace option"
    (testing "returns expression value"
      (let [result (with-out-str
                     (bench/bench (+ 1 2)
                                  :with-allocation-trace true
                                  :collect-plan :one-shot))]
        (is (string? result))))

    (testing "with one-shot collect plan"
      (let [out (with-out-str
                  (bench/bench (str "allocate" "strings")
                               :with-allocation-trace true
                               :collect-plan :one-shot))
            data (:data (bench/last-bench))]
        (testing "includes :with-allocation-trace in bench-plan"
          (is (true? (get-in (bench/last-bench) [:bench-plan :with-allocation-trace]))))
        ;; If agent is attached, check allocation data is present
        (when (get-in data [:samples :allocation-trace])
          (testing "collects allocation trace"
            (is (= :criterium/allocation-trace
                   (:type (get-in data [:samples :allocation-trace])))))
          (testing "includes allocation summary"
            (is (= :criterium/allocation-summary
                   (:type (:allocation-summary data)))))
          (testing "includes allocation hotspots"
            (is (= :criterium/allocation-hotspots
                   (:type (:allocation-hotspots data)))))
          (testing "includes allocation by-type"
            (is (= :criterium/allocation-by-type
                   (:type (:allocation-by-type data)))))
          (testing "outputs allocation views"
            (is (re-find #"Allocation Summary" out))))))

    (testing "with warmup collect plan"
      (let [out (with-out-str
                  (bench/bench (str "allocate" "strings")
                               :with-allocation-trace true
                               :limit-time-s 0.1))
            data (:data (bench/last-bench))]
        ;; If agent is attached, check allocation data is present
        (when (get-in data [:samples :allocation-trace])
          (testing "collects allocation trace with warmup"
            (is (= :criterium/allocation-trace
                   (:type (get-in data [:samples :allocation-trace])))))
          (testing "outputs allocation views"
            (is (re-find #"Allocation Summary" out))))))

    (testing "without allocation trace option"
      (with-out-str
        (bench/bench (str "no" "trace")
                     :collect-plan :one-shot))
      (let [data (:data (bench/last-bench))]
        (testing "does not include allocation trace"
          (is (nil? (get-in data [:samples :allocation-trace]))))
        (testing "does not include allocation analysis"
          (is (nil? (:allocation-summary data)))
          (is (nil? (:allocation-hotspots data)))
          (is (nil? (:allocation-by-type data)))))))

  (testing "config-map accepts :with-allocation-trace"
    (let [config (bench-config/config-map {:with-allocation-trace true})]
      (is (true? (:with-allocation-trace config))))))

(deftest ^:slow knuth-histogram-bench-plan-test
  ;; Integration test verifying the knuth-histogram bench plan produces
  ;; correct histogram output with Bayesian optimal binning.
  (testing "knuth-histogram bench plan"
    (testing "produces histogram with Knuth binning"
      (let [result (atom nil)
            out (with-out-str
                  (reset! result
                          (bench/bench (+ 1 1)
                                       :bench-plan bench-plans/knuth-histogram
                                       :limit-time-s 0.1)))
            data (:data (bench/last-bench))
            histogram-data (:histograms data)
            ;; Histogram key is a vector path like [:elapsed-time]
            elapsed-histogram (get-in histogram-data [:histograms [:elapsed-time]])]
        (testing "returns expression value"
          (is (= 2 @result)))
        (testing "produces histogram analysis"
          (is (some? histogram-data)
              "histogram analysis should be present")
          (is (= :criterium/histogram (:type histogram-data))
              "histograms container should have correct type"))
        (testing "histogram has Knuth type"
          (is (= :criterium/histogram-knuth (:type elapsed-histogram))
              "elapsed-time histogram should use Knuth method"))
        (testing "histogram includes optimal-bins"
          (is (pos-int? (:optimal-bins elapsed-histogram))
              "optimal-bins should be a positive integer"))
        (testing "histogram includes log-posterior"
          (is (number? (:log-posterior elapsed-histogram))
              "log-posterior should be a number"))
        (testing "outputs histogram view"
          (is (re-find #"Histogram" out)
              "stdout should contain histogram output"))))))

(deftest ^:slow default-with-warmup-kde-modes-test
  ;; Integration test verifying that default-with-warmup includes KDE and modes
  ;; analysis, and that multimodal-warning view is present in the pipeline.
  ;; This test validates the full pipeline from bench to view output.
  (testing "default-with-warmup bench plan"
    (testing "includes :kde and :modes in analyse plan"
      (is (some #{:kde} (:analyse bench-plans/default-with-warmup))
          ":kde should be in analyse plan")
      (is (some #{:modes} (:analyse bench-plans/default-with-warmup))
          ":modes should be in analyse plan"))

    (testing "includes :multimodal-warning in view plan"
      (is (some #(and (vector? %) (= :multimodal-warning (first %)))
                (:view bench-plans/default-with-warmup))
          "[:multimodal-warning ...] should be in view plan"))

    (testing "produces KDE and modes analysis"
      (let [result (atom nil)
            _      (with-out-str
                     (reset! result (bench/bench (+ 1 1) :limit-time-s 0.1)))
            data   (:data (bench/last-bench))]
        (testing "returns expression value"
          (is (= 2 @result)))
        (testing "produces KDE analysis"
          (is (some? (:kde data))
              "KDE analysis should be present")
          (is (= :criterium/kde (:type (:kde data)))
              "KDE should have correct type"))
        (testing "produces modes analysis"
          (is (some? (:modes data))
              "modes analysis should be present")
          (is (= :criterium/modes (:type (:modes data)))
              "modes should have correct type"))))))
        ;; Note: multimodal-warning only displays when n-modes > 1
        ;; For a simple (+ 1 1) benchmark, distribution should be unimodal
        ;; so we don't test for warning output here

;;; warmup-args-fn option tests
;; Tests for the :warmup-args-fn option in the bench macro.
;; Validates that warmup uses provided warmup-args-fn during JIT warmup
;; while measurement uses the expression's captured arguments.

(deftest ^:slow warmup-args-fn-option-test
  (testing ":warmup-args-fn option"
    (testing "bench macro accepts :warmup-args-fn option"
      (let [warmup-calls (atom 0)
            warmup-args-fn (fn []
                             (swap! warmup-calls inc)
                             [42])]
        (with-out-str
          (bench/bench (identity 1)
                       :warmup-args-fn warmup-args-fn
                       :limit-time-s 0.1))
        (is (pos? @warmup-calls)
            "warmup-args-fn should be called during warmup")))

    (testing "warmup uses warmup-args-fn when provided"
      (let [warmup-calls (atom 0)
            measurement-value 100
            warmup-args-fn (fn []
                             (swap! warmup-calls inc)
                             [999])]
        (with-out-str
          (let [result (bench/bench (identity measurement-value)
                                    :warmup-args-fn warmup-args-fn
                                    :limit-time-s 0.1)]
            (is (= measurement-value result)
                "measurement should use expr's captured args, not warmup args")
            (is (pos? @warmup-calls)
                "warmup-args-fn should be called during warmup")))))

    (testing "without warmup-args-fn, uses default args-fn"
      (with-out-str
        (let [result (bench/bench (identity 42) :limit-time-s 0.1)]
          (is (= 42 result)
              "bench should work without warmup-args-fn"))))))

(deftest warmup-args-fn-config-test
  ;; Tests that :warmup-args-fn is recognized as a valid bench option.
  (testing ":warmup-args-fn in config"
    (testing "config-map accepts :warmup-args-fn"
      (let [warmup-fn (fn [] [1])
            config (bench-config/config-map {:warmup-args-fn warmup-fn})]
        (is (map? config)
            "config-map should accept :warmup-args-fn without error")))

    (testing "unknown options still throw"
      (is (thrown? Exception
                   (bench-config/config-map {:unknown-option true}))))))

(deftest ^:slow tail-analysis-bench-plan-test
  ;; Integration test verifying the tail-analysis bench plan runs correctly.
  ;; Tail analysis requires >30 samples for meaningful results. With short
  ;; time limits, we may not collect enough samples to produce tail-analysis
  ;; output, but the plan should execute without errors and produce standard
  ;; benchmark output.
  (testing "tail-analysis bench plan"
    (testing "executes without error"
      (let [result (atom nil)
            out (with-out-str
                  (reset! result
                          (bench/bench (reduce + (range 100))
                                       :bench-plan bench-plans/tail-analysis
                                       :limit-time-s 0.2)))
            data (:data (bench/last-bench))]
        (testing "returns expression value"
          (is (= 4950 @result) "reduce should return sum"))
        (testing "produces samples"
          (is (some? (:samples data))
              "should have samples in data"))
        (testing "outputs basic timing information"
          (is (re-find #"Elapsed Time" out)
              "stdout should contain elapsed time output"))
        (testing "outputs quantiles"
          (is (re-find #"Quantiles" out)
              "stdout should contain quantiles output"))))))

(deftest tail-analysis-bench-plan-structure-test
  ;; Validates the structure of the tail-analysis bench plan.
  (testing "tail-analysis bench plan structure"
    (testing "includes :tail-analysis in analyse plan"
      (is (some #{:tail-analysis} (:analyse bench-plans/tail-analysis))
          ":tail-analysis should be in analyse plan"))

    (testing "includes tail views in view plan"
      (is (some #{:tail-summary} (:view bench-plans/tail-analysis))
          ":tail-summary should be in view plan")
      (is (some #{:tail-ratios} (:view bench-plans/tail-analysis))
          ":tail-ratios should be in view plan")
      (is (some #{:hill-plot} (:view bench-plans/tail-analysis))
          ":hill-plot should be in view plan"))

    (testing "includes quantiles with tail percentiles"
      (let [quantile-spec (some #(when (and (vector? %) (= :quantiles (first %)))
                                   %)
                                (:analyse bench-plans/tail-analysis))]
        (is (some? quantile-spec)
            "[:quantiles ...] should be in analyse plan")
        (when quantile-spec
          (let [quantiles (get-in quantile-spec [1 :quantiles])]
            (is (contains? (set quantiles) 0.99)
                "quantiles should include 0.99")
            (is (contains? (set quantiles) 0.999)
                "quantiles should include 0.999")))))))
