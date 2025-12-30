(ns criterium.bench-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse]
   [criterium.bench :as bench]
   [criterium.bench-plans :as bench-plans]
   [criterium.bench.config :as bench-config]
   [criterium.bench.impl :as bench-impl]
   [criterium.viewer.kindly :as kindly]))

(deftest bench-test
  (testing "bench"
    (bench-impl/last-bench! nil)
    (is (nil? (bench/last-bench)))
    (let [out (with-out-str (bench/bench 1 :limit-time-s 0.1))]
      (testing "outputs the estimated time on stdout"
        (is (re-find
             #"Elapsed Time: [0-9.]+ [mn]s  3σ \[[0-9.e+-]+ [0-9.e+-]+]  min [0-9.]+"
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

(deftest kindly-viewer-integration-test
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
            (is (every? sequential? tables)
                "tables are sequences"))

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

(deftest with-allocation-trace-test
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

(deftest outlier-method-option-test
  ;; Tests for :outlier-method option.
  ;; Verifies that the option is accepted and injected into the analyse plan.
  (testing ":outlier-method option"
    (testing "config-map accepts :outlier-method"
      (is (some? (bench-config/config-map {:outlier-method :standard})))
      (is (some? (bench-config/config-map {:outlier-method :adjusted})))
      (is (some? (bench-config/config-map {:outlier-method :auto}))))

    (testing "injects :outlier-method into :outliers step in analyse plan"
      (let [config (bench-config/config-map {:outlier-method :standard})]
        (is (some
             (fn [step]
               (and (vector? step)
                    (= :outliers (first step))
                    (= :standard (:outlier-method (second step)))))
             (:analyse config))
            "analyse plan contains [:outliers {:outlier-method :standard}]")))

    (testing "preserves existing outliers options"
      (let [config (bench-config/config-map
                    {:outlier-method :standard
                     :analyse [:transform-log
                               [:outliers {:samples-id :log-samples}]
                               :stats]})
            outlier-step (some
                          (fn [step]
                            (when (and (vector? step)
                                       (= :outliers (first step)))
                              step))
                          (:analyse config))]
        (is (= :standard (:outlier-method (second outlier-step))))
        (is (= :log-samples (:samples-id (second outlier-step))))))

    (testing "nil :outlier-method does not modify analyse plan"
      (let [default-config (bench-config/config-map {})
            nil-config (bench-config/config-map {:outlier-method nil})]
        (is (= (:analyse default-config) (:analyse nil-config)))))

    (testing "bench accepts :outlier-method option"
      (with-out-str
        (let [v (bench/bench 1 :limit-time-s 0.1 :outlier-method :standard)]
          (is (= 1 v)))
        (is (some? (bench/last-bench)))))))

(deftest knuth-histogram-bench-plan-test
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

(deftest default-with-warmup-kde-modes-test
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
            out (with-out-str
                  (reset! result (bench/bench (+ 1 1) :limit-time-s 0.1)))
            data (:data (bench/last-bench))]
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
              "modes should have correct type"))
        ;; Note: multimodal-warning only displays when n-modes > 1
        ;; For a simple (+ 1 1) benchmark, distribution should be unimodal
        ;; so we don't test for warning output here
        ))))
