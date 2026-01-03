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
   [criterium.domain.test-util :refer [mock-bench-result]]
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
        (is (measured/measured? (impl-fn {:n 10})))))))

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
