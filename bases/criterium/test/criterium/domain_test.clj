(ns criterium.domain-test
  "Tests for criterium.domain namespace.

  Tests the domain-expr macro and high-level bench function.
  For tests of sub-namespaces, see:
  - criterium.domain.types-test
  - criterium.domain.builder-test
  - criterium.domain.analysis-test
  - criterium.domain-plans-test"
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.domain :as domain]
   [criterium.domain-plans :as domain-plans]
   [criterium.domain.analysis :as analysis]
   [criterium.domain.test-util :refer [mock-bench-result
                                       mock-bench-result-with-bootstrap
                                       mock-one-shot-result]]
   [criterium.measured :as measured]))

;; Tests for the domain-expr macro.
;; Validates transformation of concise expression syntax into measured functions.

(deftest domain-expr-test
  (testing "domain-expr"
    (testing "with single expression and single axis"
      (testing "uses :default as implementation key"
        (let [spec (domain/domain-expr
                    [n [10 100 1000]]
                    (sort (vec (range n))))]
          (is (= #{:default} (set (keys (:implementations spec)))))))
      (testing "produces correct axes map"
        (let [spec (domain/domain-expr [n [10 100 1000]] (sort (vec (range n))))]
          (is (= {:n [10 100 1000]} (:axes spec)))))
      (testing "produces function that returns Measured"
        (let [spec    (domain/domain-expr [n [10 100 1000]] (sort (vec (range n))))
              impl-fn (get-in spec [:implementations :default])]
          (is (fn? impl-fn))
          (is (measured/measured? (impl-fn {:n 50})))))
      (testing "produces different Measured for different axis values"
        (let [spec    (domain/domain-expr [n [10 100 1000]] (sort (vec (range n))))
              impl-fn (get-in spec [:implementations :default])
              m-10    (impl-fn {:n 10})
              m-100   (impl-fn {:n 100})
              m-1000  (impl-fn {:n 1000})]
          (is (= 10 (count (first ((:args-fn m-10)))))
              "axis value 10 should produce measured with 10 element arg")
          (is (= 100 (count (first ((:args-fn m-100)))))
              "axis value 100 should produce measured with 100 element arg")
          (is (= 1000 (count (first ((:args-fn m-1000)))))
              "axis value 1000 should produce measured with 1000 element arg"))))
    (testing "with multiple implementations"
      (testing "uses map keys as implementation keys"
        (let [spec (domain/domain-expr [n [10 100]]
                                       {:impl-a (sort (vec (range n)))
                                        :impl-b (sort-by identity (vec (range n)))})]
          (is (= #{:impl-a :impl-b} (set (keys (:implementations spec)))))))
      (testing "produces function returning Measured for each implementation"
        (let [spec      (domain/domain-expr [n [10 100]]
                                            {:impl-a (sort (vec (range n)))
                                             :impl-b (sort-by identity (vec (range n)))})
              impl-a-fn (get-in spec [:implementations :impl-a])
              impl-b-fn (get-in spec [:implementations :impl-b])]
          (is (fn? impl-a-fn))
          (is (fn? impl-b-fn))
          (is (measured/measured? (impl-a-fn {:n 10})))
          (is (measured/measured? (impl-b-fn {:n 10}))))))
    (testing "with multiple axes"
      (testing "produces axes map with all bindings"
        (let [spec (domain/domain-expr [n [10 100] m [1 2 3]]
                                       (concat (vec (range n)) (vec (range m))))]
          (is (= {:n [10 100] :m [1 2 3]} (:axes spec)))))
      (testing "impl-fn has access to all axis vars"
        (let [spec    (domain/domain-expr [n [10 100] m [1 2 3]]
                                          (concat (vec (range n)) (vec (range m))))
              impl-fn (get-in spec [:implementations :default])
              m       (impl-fn {:n 5 :m 3})
              args    ((:args-fn m))]
          ;; concat has 2 args: (range n) and (range m)
          (is (= 2 (count args)))
          (is (= 5 (count (first args))))
          (is (= 3 (count (second args))))))
      (testing "produces different Measured for different axis combinations"
        (let [spec       (domain/domain-expr [n [10 100] m [1 2 3]]
                                             (concat (vec (range n)) (vec (range m))))
              impl-fn    (get-in spec [:implementations :default])
              m-10-1     (impl-fn {:n 10 :m 1})
              m-100-3    (impl-fn {:n 100 :m 3})
              args-10-1  ((:args-fn m-10-1))
              args-100-3 ((:args-fn m-100-3))]
          (is (= [10 1] [(count (first args-10-1)) (count (second args-10-1))])
              "axis values n=10, m=1 should produce args with 10 and 1 elements")
          (is (= [100 3] [(count (first args-100-3)) (count (second args-100-3))])
              "axis values n=100, m=3 should produce args with 100 and 3 elements"))))
    (testing "type hints transfer to destructured keys"
      (let [spec    (domain/domain-expr [^long n [10 100]] (+ n 1))
            impl-fn (get-in spec [:implementations :default])]
        (is (fn? impl-fn))
        (is (measured/measured? (impl-fn {:n 10})))))
    (testing "with options"
      (testing "passes :warmup-args-fn to all implementations"
        (let [warmup-fn (fn [] [:warmup-val])
              spec      (domain/domain-expr
                         [n [10 100]]
                         {:impl-a (sort (vec (range n)))
                          :impl-b (sort-by identity (vec (range n)))}
                         {:warmup-args-fn warmup-fn})
              m-a       ((get-in spec [:implementations :impl-a]) {:n 10})
              m-b       ((get-in spec [:implementations :impl-b]) {:n 10})]
          (is (= [:warmup-val] (measured/warmup-args m-a)))
          (is (= [:warmup-val] (measured/warmup-args m-b)))))
      (testing "with single expression applies :warmup-args-fn"
        (let [warmup-fn (fn [] [:single-warmup])
              spec      (domain/domain-expr
                         [n [10 100]]
                         (sort (vec (range n)))
                         {:warmup-args-fn warmup-fn})
              m         ((get-in spec [:implementations :default]) {:n 10})]
          (is (= [:single-warmup] (measured/warmup-args m))))))))

;; Tests for the extract-metrics domain plan.
;; Validates plan structure and integration with analyse-domain.

(deftest extract-metrics-plan-test
  (testing "extract-metrics"
    (testing "has correct structure"
      (is (contains? domain-plans/extract-metrics :analyse))
      (is (contains? domain-plans/extract-metrics :view))
      (is (vector? (:analyse domain-plans/extract-metrics)))
      (is (vector? (:view domain-plans/extract-metrics))))
    (testing "analyse spec uses domain-extract-fn without metric-path"
      (let [[spec-kw spec-opts] (first (:analyse domain-plans/extract-metrics))]
        (is (= :domain-extract-fn spec-kw))
        (is (not (contains? spec-opts :metric-path)))))
    (testing "works with analyse-domain"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-bench-result {:elapsed-time {:mean 1.0}})})
            result (analysis/analyse-domain
                    (assoc domain-plans/extract-metrics :viewer :none)
                    d)]
        (is (map? result))
        (is (= :criterium/domain-extract (:type (:extract result))))))))

;; Tests for viewer option override behavior.
;; Validates that :viewer option properly overrides domain-plan's viewer.

(deftest viewer-option-override-test
  (testing "options->domain-plan"
    (testing "overrides existing :viewer in domain-plan"
      (let [plan-with-viewer (assoc domain-plans/extract-metrics :viewer :print)
            updated-plan (analysis/options->domain-plan
                          plan-with-viewer
                          :viewer :kindly)]
        (is (= :kindly (:viewer updated-plan)))))
    (testing "adds :viewer when domain-plan has none"
      (let [plan-without-viewer (dissoc domain-plans/extract-metrics :viewer)
            updated-plan (analysis/options->domain-plan
                          plan-without-viewer
                          :viewer :portal)]
        (is (= :portal (:viewer updated-plan)))))
    (testing "preserves other domain-plan keys"
      (let [original-plan domain-plans/extract-metrics
            updated-plan (analysis/options->domain-plan original-plan :viewer :none)]
        (is (= (:analyse original-plan) (:analyse updated-plan)))
        (is (= (:view original-plan) (:view updated-plan)))))))

;;; One-shot domain benchmarking integration tests
;; Tests end-to-end functionality with :one-shot collect plan (single sample, no stats).
;; One-shot mode is for measuring infrequently-executed code without JIT warmup.

(deftest one-shot-extract-metrics-test
  ;; Tests extract-metrics plan with one-shot data (samples only, no stats/bootstrap).
  ;; Verifies metric extraction falls back to raw sample values.
  (testing "extract-metrics with one-shot data"
    (testing "extracts metric values from single samples"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-one-shot-result {:elapsed-time 42.5})}
               {:coord {:n 200}
                :data (mock-one-shot-result {:elapsed-time 85.0})})
            plan (analysis/options->domain-plan
                  domain-plans/extract-metrics
                  :viewer :none)
            result (analysis/analyse-domain plan d)
            extract (:extract result)]
        (is (= :criterium/domain-extract (:type extract))
            "returns domain-extract result type")
        (is (contains? (:metrics extract) :elapsed-time)
            "discovers elapsed-time metric from samples")
        (let [et-data (get-in extract [:metrics :elapsed-time])
              values (into {} (map (fn [[c v]] [c (:value v)]) (:data et-data)))]
          (is (= 42.5 (get values {:n 100}))
              "extracts value for n=100")
          (is (= 85.0 (get values {:n 200}))
              "extracts value for n=200"))))
    (testing "has no error bounds (n=1 means no CI)"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-one-shot-result {:elapsed-time 42.5})})
            plan (analysis/options->domain-plan
                  domain-plans/extract-metrics
                  :viewer :none)
            result (analysis/analyse-domain plan d)
            et-data (get-in result [:extract :metrics :elapsed-time])
            [_coord value-map] (first (:data et-data))]
        (is (nil? (:lower value-map))
            "no lower bound for one-shot data")
        (is (nil? (:upper value-map))
            "no upper bound for one-shot data")))))

(deftest one-shot-complexity-analysis-test
  ;; Tests complexity-analysis plan with one-shot data.
  ;; Verifies model fitting works with single-sample data points.
  (testing "complexity-analysis with one-shot data"
    (testing "extracts metrics and fits models"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-one-shot-result {:elapsed-time 100.0})}
               {:coord {:n 200}
                :data (mock-one-shot-result {:elapsed-time 200.0})}
               {:coord {:n 400}
                :data (mock-one-shot-result {:elapsed-time 400.0})})
            plan (analysis/options->domain-plan
                  domain-plans/complexity-analysis
                  :viewer :none)
            result (analysis/analyse-domain plan d)]
        (is (= :criterium/domain-extract (:type (:extract result)))
            "produces extract result")
        (is (= :criterium/domain-regression (:type (:regression result)))
            "produces regression result")
        (let [regression (:regression result)
              et-regression (get-in regression [:regressions :elapsed-time])]
          (is (some? et-regression)
              "has elapsed-time regression data")
          (is (seq (:models et-regression))
              "fits models to elapsed-time data")
          (is (every? #(contains? % :id) (:models et-regression))
              "each fit has a model id"))))))

(deftest one-shot-implementation-comparison-test
  ;; Tests implementation-comparison plan with one-shot data.
  ;; Verifies comparison works with single-sample data per implementation.
  (testing "implementation-comparison with one-shot data"
    (testing "compares implementations"
      (let [d (domain/domain
               {:coord {:impl :fast}
                :data (mock-one-shot-result {:elapsed-time 10.0})}
               {:coord {:impl :slow}
                :data (mock-one-shot-result {:elapsed-time 50.0})}
               {:implementations [:fast :slow]})
            plan (analysis/options->domain-plan
                  domain-plans/implementation-comparison
                  :viewer :none)
            result (analysis/analyse-domain plan d)
            comparison (:comparison result)]
        (is (= :criterium/domain-comparison (:type comparison))
            "returns domain-comparison result type")
        (is (= [:fast :slow] (:implementations comparison))
            "preserves implementation order")
        (let [et-metric (get-in comparison [:metrics :elapsed-time])
              fast-entry (first (get-in et-metric [:data :fast]))
              slow-entry (first (get-in et-metric [:data :slow]))]
          (is (= 10.0 (get-in fast-entry [:value :value]))
              "extracts fast impl value")
          (is (= 50.0 (get-in slow-entry [:value :value]))
              "extracts slow impl value"))))
    (testing "omits error bound keys when bounds are nil"
      ;; Regression test: ensures :lower/:upper keys are absent (not nil)
      ;; when error bounds unavailable. Presence of nil values causes NPE
      ;; in chart rendering (values-have-error-bounds? uses contains?).
      (let [d (domain/domain
               {:coord {:impl :a}
                :data (mock-one-shot-result {:elapsed-time 10.0})}
               {:implementations [:a]})
            plan (analysis/options->domain-plan
                  domain-plans/implementation-comparison
                  :viewer :none)
            result (analysis/analyse-domain plan d)
            et-metric (get-in result [:comparison :metrics :elapsed-time])
            entry (first (get-in et-metric [:data :a]))
            value-map (:value entry)]
        (is (not (contains? value-map :lower))
            "value map should not contain :lower key")
        (is (not (contains? value-map :upper))
            "value map should not contain :upper key")))))

(deftest one-shot-extract-elapsed-time-test
  ;; Tests extract-elapsed-time plan with one-shot data.
  ;; This plan uses explicit :metric-path [:stats :elapsed-time :mean] which
  ;; does NOT fall back to samples - users should use extract-metrics instead.
  (testing "extract-elapsed-time with one-shot data"
    (testing "returns nil values (no stats in one-shot data)"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-one-shot-result {:elapsed-time 42.5})})
            plan (analysis/options->domain-plan
                  domain-plans/extract-elapsed-time
                  :viewer :none)
            result (analysis/analyse-domain plan d)
            et-data (get-in result [:extract :metrics :elapsed-time])
            [_coord value-map] (first (:data et-data))]
        (is (nil? (:value value-map))
            "returns nil - explicit :metric-path bypasses fallback to samples")))))

(deftest with-jit-warmup-regression-test
  ;; Regression test: verifies existing :with-jit-warmup behavior unchanged.
  ;; Uses mock data with stats (normal benchmark results, not one-shot).
  (testing "with-jit-warmup data (regression)"
    (testing "extract-metrics works with stats data"
      (let [d (domain/domain
               {:coord {:n 100}
                :data (mock-bench-result-with-bootstrap
                       {:elapsed-time {:mean 1.0}})}
               {:coord {:n 200}
                :data (mock-bench-result-with-bootstrap
                       {:elapsed-time {:mean 2.0}})})
            plan (analysis/options->domain-plan
                  domain-plans/extract-metrics
                  :viewer :none)
            result (analysis/analyse-domain plan d)
            extract (:extract result)]
        (is (= :criterium/domain-extract (:type extract))
            "returns domain-extract result type")
        (let [et-data (get-in extract [:metrics :elapsed-time])
              [_coord value-map] (first (:data et-data))]
          (is (some? (:value value-map))
              "extracts metric value")
          (is (some? (:lower value-map))
              "has lower bound from bootstrap")
          (is (some? (:upper value-map))
              "has upper bound from bootstrap"))))))

;;; End-to-end integration tests with actual domain/bench
;; Tests full pipeline using real benchmark execution (not mock data).

(deftest ^:slow domain-bench-one-shot-end-to-end-test
  ;; End-to-end test running actual domain/bench with :one-shot collect plan.
  ;; Verifies full pipeline integration from benchmark execution through analysis.
  (testing "domain/bench with :bench-options {:collect-plan :one-shot}"
    (testing "runs benchmarks and extracts metrics from single samples"
      (let [result (domain/bench
                    (domain/domain-expr [n [10 100]] (reduce + (range n)))
                    :domain-plan domain-plans/extract-metrics
                    :bench-options {:collect-plan :one-shot}
                    :reporter nil
                    :viewer :none)
            extract (:extract result)]
        (is (= :criterium/domain-extract (:type extract))
            "produces domain-extract result")
        (is (contains? (:metrics extract) :elapsed-time)
            "discovers elapsed-time metric")
        (let [et-data (get-in extract [:metrics :elapsed-time])
              coords (set (map first (:data et-data)))]
          (is (= #{{:impl :default :n 10} {:impl :default :n 100}} coords)
              "has data for all coordinates")
          (is (every? #(number? (:value (second %))) (:data et-data))
              "all values are numbers"))))
    (testing "produces no error bounds (n=1)"
      (let [result (domain/bench
                    (domain/domain-expr [n [10]] (reduce + (range n)))
                    :domain-plan domain-plans/extract-metrics
                    :bench-options {:collect-plan :one-shot}
                    :reporter nil
                    :viewer :none)
            et-data (get-in result [:extract :metrics :elapsed-time])
            [_coord value-map] (first (:data et-data))]
        (is (nil? (:lower value-map))
            "no lower bound for one-shot")
        (is (nil? (:upper value-map))
            "no upper bound for one-shot")))))

(deftest ^:slow domain-bench-one-shot-impl-comparison-test
  ;; End-to-end test for implementation-comparison with one-shot data.
  ;; Verifies multiple implementations can be compared without NPE.
  (testing "domain/bench with implementation-comparison and :one-shot"
    (testing "compares multiple implementations"
      (let [result (domain/bench
                    (domain/domain-expr
                     [n [100 1000]]
                     {:reduce-range (reduce + (range n))
                      :apply-range  (apply + (range n))})
                    :domain-plan domain-plans/implementation-comparison
                    :bench-options {:collect-plan :one-shot}
                    :reporter nil
                    :viewer :none)
            comparison (:comparison result)]
        (is (= :criterium/domain-comparison (:type comparison))
            "produces domain-comparison result")
        (is (= [:reduce-range :apply-range] (:implementations comparison))
            "preserves implementation order")
        (let [et-metric (get-in comparison [:metrics :elapsed-time])
              reduce-data (get-in et-metric [:data :reduce-range])
              apply-data (get-in et-metric [:data :apply-range])]
          (is (= 2 (count reduce-data))
              "has data for all n values in reduce-range")
          (is (= 2 (count apply-data))
              "has data for all n values in apply-range")
          (is (every? #(number? (get-in % [:value :value])) reduce-data)
              "reduce-range values are numbers")
          (is (every? #(number? (get-in % [:value :value])) apply-data)
              "apply-range values are numbers"))))
    (testing "omits error bound keys from comparison data"
      (let [result (domain/bench
                    (domain/domain-expr
                     [n [10]]
                     {:impl-a (reduce + (range n))})
                    :domain-plan domain-plans/implementation-comparison
                    :bench-options {:collect-plan :one-shot}
                    :reporter nil
                    :viewer :none)
            entry (first (get-in result [:comparison :metrics :elapsed-time
                                         :data :impl-a]))
            value-map (:value entry)]
        (is (not (contains? value-map :lower))
            "value map should not contain :lower key")
        (is (not (contains? value-map :upper))
            "value map should not contain :upper key")))))
