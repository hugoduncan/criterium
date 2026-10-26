(ns criterium.arg-gen-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.generators :as gen]
   [criterium.arg-gen :as arg-gen]
   [criterium.measured :as measured]))

;; TODO test that generate vars are type hinted correctly

(defn invoke
  "Invoke the given Measured.

  Calls the Measured's function with the result of calling the
  Measured's args function."
  ([measured]
   (invoke measured 1))
  ([measured eval-count]
   ((:f measured) ((:args-fn measured)) eval-count)))

(deftest measured-test
  (testing "arg-gen/measured"
    (let [m (arg-gen/measured
             [v (gen/vector gen/small-integer 1 10)
              i (gen/choose 0 (dec (count v)))]
             {:v v :i i})]
      (testing "creates a measured"
        (is (measured/measured? m))
        (testing "with a state fn that generates a tuple of generated values"
          (let [[vv iv] ((:args-fn m))]
            (is (vector? vv))
            (is (every? integer? vv))
            (is (<= (count vv) 10))
            (is (integer? iv))
            (is (<= 0 iv (dec (count vv))))))
        (testing "which passes its state values to its body"
          (let [[elapsed-time val] (invoke m)]
            (is (integer? elapsed-time))
            (is (pos? elapsed-time))
            (is (map? val))
            (is (vector? (:v val)))
            (is (integer? (:i val))))))))
  (testing "arg-gen/measured can specify a test.check size"
    (let [m (arg-gen/measured
             {:size 3} [i gen/small-integer]
             i)]
      (testing
       "which generates a state fn that generates values that respect size"
        (let [[iv] ((:args-fn m))]
          (is (integer? iv))
          (is (<= -3 iv 3))))))
  (testing "arg-gen/measured can specify a test.check seed"
    (let [m (arg-gen/measured
             {:seed 12345} [i gen/small-integer]
             i)]
      (testing "which generates a state fn that generates stable values"
        (let [vs (repeatedly 10 (:args-fn m))]
          (is (every? (comp integer? first) vs))
          (is (= '([0] [0] [0] [3] [4] [5] [-6] [3] [6] [6])
                 vs)))))))

;; Tests for arg-gen/args-fn macro.
;; Validates that args-fn creates zero-arg functions that generate
;; arguments using test.check generators for use as warmup-args-fn.
(deftest args-fn-test
  (testing "arg-gen/args-fn"
    (testing "with single binding"
      (let [f (arg-gen/args-fn [x gen/small-integer] [x])]
        (testing "returns a function"
          (is (fn? f)))
        (testing "generates integer values"
          (let [[x] (f)]
            (is (integer? x))))))

    (testing "with multiple independent bindings"
      (let [f (arg-gen/args-fn
               [a gen/small-integer
                b gen/string-alphanumeric]
               [a b])]
        (testing "generates values for each binding"
          (let [[a b] (f)]
            (is (integer? a))
            (is (string? b))))))

    (testing "with dependent bindings"
      (let [f (arg-gen/args-fn
               [n (gen/choose 5 10)
                v (gen/vector gen/small-integer n)]
               [v])]
        (testing "generates values respecting dependency"
          (let [[v] (f)]
            (is (vector? v))
            (is (<= 5 (count v) 10))
            (is (every? integer? v))))))

    (testing "generates different values on each call"
      (let [f  (arg-gen/args-fn [x gen/large-integer] [x])
            vs (repeatedly 10 f)]
        (testing "values vary across calls"
          (is (< 1 (count (distinct vs)))
              "should generate at least some different values"))))

    (testing "with :size option"
      (let [f (arg-gen/args-fn {:size 3}
                               [i gen/small-integer]
                               [i])]
        (testing "respects size constraint"
          (let [vs (repeatedly 20 f)]
            (is (every? #(<= -3 (first %) 3) vs)
                "values should be bounded by size")))))

    (testing "with :seed option"
      (let [f (arg-gen/args-fn {:seed 12345}
                               [i gen/small-integer]
                               [i])]
        (testing "generates reproducible values"
          (let [vs (vec (repeatedly 10 f))]
            (is (= [[0] [0] [0] [3] [4] [5] [-6] [3] [6] [6]]
                   vs))))))

    (testing "with :size and :seed options"
      (let [f1 (arg-gen/args-fn {:size 50 :seed 999}
                                [x gen/small-integer]
                                [x])
            f2 (arg-gen/args-fn {:size 50 :seed 999}
                                [x gen/small-integer]
                                [x])]
        (testing "same options produce same sequence"
          (is (= (vec (repeatedly 10 f1))
                 (vec (repeatedly 10 f2)))))))

    (testing "without options map"
      (let [f (arg-gen/args-fn [x gen/boolean] [x])]
        (testing "works with bindings vector as first arg"
          (is (fn? f))
          (let [[x] (f)]
            (is (boolean? x))))))

    (testing "independent state per invocation"
      (let [f1 (arg-gen/args-fn {:seed 42} [x gen/small-integer] [x])
            f2 (arg-gen/args-fn {:seed 42} [x gen/small-integer] [x])]
        (testing "separate invocations have independent state"
          (is (= (f1) (f2))
              "first call should match")
          (is (= (f1) (f2))
              "second call should match"))))))

(deftype ArrayComponent [])

(deftest generated-array-test
  ;; Deterministic real generators exercise example metadata and compiled reads.
  (testing "generated arrays retain exact hints, identity, and contents"
    (doseq [[descriptor constructor]
            [["[J" '(long-array [1 2 3])]
             ["[Lcriterium.arg_gen_test.ArrayComponent;"
              '(make-array criterium.arg_gen_test.ArrayComponent 3)]
             ["[[J" '(make-array Long/TYPE 3 2)]
             ["[[Ljava.lang.String;" '(make-array String 3 2)]]]
      (testing descriptor
        (let [generator (list 'clojure.test.check.generators/return constructor)
              tuple-generator (list 'clojure.test.check.generators/tuple generator)
              warnings (java.io.StringWriter.)
              subject (binding [*warn-on-reflection* true
                                *err* warnings]
                        (eval (list 'criterium.arg-gen/measured
                                    ['xs generator]
                                    '[xs (alength xs)])))
              args (measured/args subject)
              original (first args)
              contents (vec original)]
          (is (= [{:tag descriptor}]
                 (arg-gen/arg-metas-from-example tuple-generator)))
          (is (= "" (str warnings)))
          (doseq [n [1 17]]
            (let [[result length] (second (measured/invoke subject args n))]
              (is (identical? original result))
              (is (= 3 length))
              (is (= contents (vec result))))))))))
