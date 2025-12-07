(ns criterium.measured-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.agent :as agent]
   [criterium.jvm :as jvm]
   [criterium.measured :as measured]
   [criterium.measured.impl :as impl]))

(defn- inc-long [x] (inc (long x)))

(defn invoke
  "Invoke the given Measured.

  Calls the Measured's function with the result of calling the
  Measured's state function."
  ([measured]
   (invoke measured 1))
  ([measured eval-count]
   (measured/invoke
    measured
    (measured/args measured)
    eval-count)))

(deftest measured-test
  (let [eval-count (volatile! 0)
        m          (measured/measured
                    (fn [] :arg)
                    (fn [arg ^long n]
                      (vswap! eval-count #(+ n ^long %))
                      [1 [arg arg]])
                    (fn [] ::symbolic))]
    (is (measured/measured? m))
    (is (= ::symbolic (measured/symbolic m)))
    (testing "invoke calls the function with one eval"
      (vreset! eval-count 0)
      (is (= [1 [:arg :arg]] (invoke m)))
      (is (= 1 @eval-count)))
    (testing "invoke with eval-count calls the function with the eval count."
      (vreset! eval-count 0)
      (is (= [1 [:arg :arg]] (invoke m 3)))
      (is (= 3 @eval-count)))))

(deftest expr-test
  (testing "nil expr"
    (let [nil-m (measured/expr nil)]
      (is (nil? (second (invoke nil-m))))))
  (testing "const expr"
    (let [const-m (measured/expr ::const)]
      (is (= ::const (second (invoke const-m))))))
  (testing "function call"
    (let [fncall-m (measured/expr (identity ::value))]
      (is (= ::value (second (invoke fncall-m))))))
  (testing "recursive function call"
    (let [call-count         (volatile! 0)
          f                  (fn [v] (vswap! call-count inc-long) v)
          recursive-fncall-m (measured/expr (f (f ::value)))]
      (is (= ::value (second (invoke recursive-fncall-m))))
      (is (= 2 @call-count))))
  (testing "const expression is lifted"
    (let [const-expr-m (measured/expr (identity (+ 1 2)))]
      (is (= 3 (second (invoke const-expr-m))))
      (is (= [3] ((:args-fn const-expr-m))))))
  (testing "args are type hinted"
    ;; if this gives a reflection warning then it should be treated as
    ;; an error.
    (let [vec-nth-m (measured/expr (.nth [0 1 3] 1))]
      (is (= 1 (second (invoke vec-nth-m))))))
  (testing "accepts time-fn option"
    (let [invokes (volatile! 0)
          f       (fn ^long []
                    (vswap! invokes inc-long)
                    (jvm/thread-cpu-time))
          m       (measured/expr 1 {:time-fn f})]
      (is (= 1 (second (invoke m))))
      (is (= 2 @invokes))))
  (testing "with transduce"
    (let [m (measured/expr
             (transduce (comp (filter odd?) (map inc)) + (range 5)))]
      (is (= 6 (second (invoke m)))))))

(deftest zero-garbage-test
  (testing "return value is zero garbage"
    (let [measured              (measured/measured
                                 (fn [] nil)
                                 (fn [_ _] [1 2]))
          _                     (dotimes [_ 1000]
                                  (measured/invoke measured nil 1))
          [allocations ret]     (agent/with-allocation-tracing
                                  (measured/invoke measured nil 1))
          thread-allocations    (->> allocations
                                     (filterv (agent/allocation-on-thread?)))
          {:keys [freed-bytes]} (-> thread-allocations
                                    agent/allocations-summary)]
      (is (zero? freed-bytes) thread-allocations)
      (when-not (zero? (long freed-bytes))
        (tap> {:zero-garbage-test
               {:allocations (frequencies thread-allocations)}}))
      (is (= [1 2] ret) "hold reference to return value until end of test"))))

;;; Local detection tests
;; Tests for the local binding detection functionality used by measured-expr*.
;; These verify that locals from &env are correctly identified in expressions.

(deftest collect-symbols-test
  (testing "collect-symbols"
    (testing "returns empty set for non-symbol expressions"
      (is (= #{} (impl/collect-symbols 42)))
      (is (= #{} (impl/collect-symbols "string")))
      (is (= #{} (impl/collect-symbols nil))))
    (testing "returns singleton set for symbol"
      (is (= #{'x} (impl/collect-symbols 'x))))
    (testing "collects symbols from list"
      (is (= #{'+ 'x 'y} (impl/collect-symbols '(+ x y)))))
    (testing "collects symbols from nested expressions"
      (is (= #{'+ '* 'a 'b 'c} (impl/collect-symbols '(+ (* a b) c)))))
    (testing "collects symbols from vectors"
      (is (= #{'a 'b} (impl/collect-symbols '[a b]))))
    (testing "collects symbols from maps"
      (is (= #{'val} (impl/collect-symbols '{:key val}))))))

(deftest locals-in-expr-test
  ;; Simulated env maps, structured like Clojure's &env
  (let [env {'x 'local-binding-x 'y 'local-binding-y}]
    (testing "locals-in-expr"
      (testing "returns empty set when env is nil"
        (is (= #{} (impl/locals-in-expr '(+ x y) nil))))
      (testing "returns empty set when no locals match"
        (is (= #{} (impl/locals-in-expr '(+ a b) env))))
      (testing "identifies single local in expression"
        (is (= #{'x} (impl/locals-in-expr '(+ x 1) env))))
      (testing "identifies multiple locals"
        (is (= #{'x 'y} (impl/locals-in-expr '(+ x y) env))))
      (testing "handles nested expressions with locals"
        (is (= #{'x 'y} (impl/locals-in-expr '(foo (bar x) (baz y z)) env)))))))

(deftest local-arg-val-test
  (let [env {'x 'local-binding-x}]
    (testing "local-arg-val?"
      (testing "returns true for symbol in env"
        (is (true? (impl/local-arg-val? 'x env))))
      (testing "returns true for expression containing symbol in env"
        (is (true? (impl/local-arg-val? '(+ x 1) env)))
        (is (true? (impl/local-arg-val? '(foo (bar x)) env))))
      (testing "returns false for symbol not in env"
        (is (false? (impl/local-arg-val? 'z env))))
      (testing "returns false for expression not containing locals"
        (is (false? (impl/local-arg-val? '(+ 1 2) env))))
      (testing "returns false for non-symbol non-expression"
        (is (false? (impl/local-arg-val? 42 env)))
        (is (false? (impl/local-arg-val? "string" env)))))))

(deftest identify-local-args-test
  (let [env      {'x 'local-binding-x 'y 'local-binding-y}
        arg-vals {'arg1 'x         ; local symbol
                  'arg2 '(+ 1 2)   ; constant expression
                  'arg3 'y         ; local symbol
                  'arg4 'z         ; not a local
                  'arg5 '(+ x 1)}] ; expression containing local
    (testing "identify-local-args"
      (testing "returns empty set when env is nil"
        (is (= #{} (impl/identify-local-args arg-vals nil))))
      (testing "identifies arg-syms whose values are or contain locals"
        (is (= #{'arg1 'arg3 'arg5} (impl/identify-local-args arg-vals env))))
      (testing "returns empty set when no arg-vals are locals"
        (is (= #{} (impl/identify-local-args {'a '(+ 1 2)} env)))))))

;;; Local capture integration tests
;; Tests verifying that locals are correctly captured and passed through
;; the measurement pipeline.

(deftest expr-local-capture-test
  (testing "measured/expr with local bindings"
    (testing "captures simple local binding"
      (let [x   42
            m   (measured/expr (+ x 1))
            res (second (invoke m))]
        (is (= 43 res))))
    (testing "captures collection local"
      (let [coll [1 2 3 4 5]
            m    (measured/expr (reduce + coll))
            res  (second (invoke m))]
        (is (= 15 res))))
    (testing "captures multiple locals"
      (let [a 10
            b 20
            m (measured/expr (+ a b))
            res (second (invoke m))]
        (is (= 30 res))))
    (testing "captures local in nested expression"
      (let [x 5
            m (measured/expr (* 2 (+ x 3)))
            res (second (invoke m))]
        (is (= 16 res))))
    (testing "mixes locals with constants"
      (let [x 10
            m (measured/expr (+ x (+ 1 2)))
            res (second (invoke m))]
        (is (= 13 res))))
    (testing "captures local used multiple times"
      (let [x 3
            m (measured/expr (+ x x x))
            res (second (invoke m))]
        (is (= 9 res))))
    (testing "works in loop binding context"
      (let [results (atom [])]
        (doseq [i (range 3)]
          (let [m   (measured/expr (+ i 10))
                res (second (invoke m))]
            (swap! results conj res)))
        (is (= [10 11 12] @results))))))
