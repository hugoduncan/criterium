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
        m (measured/measured
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

(deftest with-args-fn-test
  ;; Tests for with-args-fn which creates a new measured with a replaced args-fn.
  ;; Validates that the measurement function is preserved while args-fn is swapped.
  (testing "with-args-fn"
    (testing "replaces args-fn while preserving measurement function"
      (let [original-m  (measured/measured
                         (fn [] [1])
                         (fn [[x] _n] [0 (* (long x) 10)])
                         (fn [] ::original))
            new-args-fn (fn [] [5])
            modified-m  (measured/with-args-fn original-m new-args-fn)]
        (is (measured/measured? modified-m))
        (is (= [0 50] (invoke modified-m)))))
    (testing "preserves symbolic representation"
      (let [original-m (measured/measured
                        (fn [] nil)
                        (fn [_ _n] [0 nil])
                        (fn [] ::my-symbolic))
            modified-m (measured/with-args-fn original-m (fn [] nil))]
        (is (= ::my-symbolic (measured/symbolic modified-m)))))
    (testing "works with measured/callable"
      (let [f          (fn [coll] (reduce + coll))
            original-m (measured/callable (fn [] [[1 2 3]]) f)
            modified-m (measured/with-args-fn original-m (fn [] [[10 20 30]]))]
        (is (= 60 (second (invoke modified-m))))))
    (testing "original measured is unchanged"
      (let [original-m  (measured/measured
                         (fn [] [1])
                         (fn [[x] _n] [0 x])
                         nil)
            _modified-m (measured/with-args-fn original-m (fn [] [99]))]
        (is (= [0 1] (invoke original-m)))))))

(defn random-seq
  [n]
  (mapv rand-int (repeat n 10000)))

(deftest expr-test
  (testing "expr"
    (testing "with nil returns nil"
      (let [nil-m (measured/expr nil)]
        (is (= [nil] (measured/args nil-m)))
        (is (nil? (second (invoke nil-m))))))
    (testing "with const returns the const"
      (let [const-m (measured/expr ::const)]
        (is (= [::const] (measured/args const-m)))
        (is (= ::const (second (invoke const-m))))))
    (testing "with function call lifts the argument"
      (let [fncall-m (measured/expr (identity ::value))]
        (is (= [::value] (measured/args fncall-m)))
        (is (= ::value (second (invoke fncall-m))))))
    (testing "with recursive function call lifts innermost value and local fn"
      (let [call-count         (volatile! 0)
            f                  (fn [v] (vswap! call-count inc-long) v)
            recursive-fncall-m (measured/expr (f (f ::value)))
            args               (measured/args recursive-fncall-m)]
        ;; Local function f is now captured twice (for both calls) plus ::value
        (is (= 3 (count args)))
        (is (= #{f ::value} (set args)))
        (is (= ::value (second (invoke recursive-fncall-m))))
        (is (= 2 @call-count))))
    (testing "with const expression lifts the evaluated value"
      (let [const-expr-m (measured/expr (identity (+ 1 2)))]
        (is (= [3] (measured/args const-expr-m)))
        (is (= 3 (second (invoke const-expr-m))))))
    (testing "with type-hinted method call lifts the receiver"
      ;; if this gives a reflection warning then it should be treated as
      ;; an error.
      (let [vec-nth-m (measured/expr (.nth [0 1 3] 1))]
        (is (= [[0 1 3] 1] (measured/args vec-nth-m)))
        (is (= 1 (second (invoke vec-nth-m))))))
    (testing "with time-fn option uses the custom time function"
      (let [invokes (volatile! 0)
            f       (fn ^long []
                      (vswap! invokes inc-long)
                      (jvm/thread-cpu-time))
            m       (measured/expr 1 {:time-fn f})]
        (is (= [1] (measured/args m)))
        (is (= 1 (second (invoke m))))
        (is (= 2 @invokes))))
    (testing "with transduce lifts the collection argument"
      (let [m    (measured/expr
                  (transduce (comp (filter odd?) (map inc)) + (range 5)))
            args (measured/args m)
            c    (first args)]
        (is (= [+ '(0 1 2 3 4)] (rest args))) ; can't compare the comp
        (is (= [2 4 6] (into [] c [1 2 3 4 5])))
        (is (= 6 (second (invoke m))))))
    (testing "with nested expr"
      (let [m    (measured/expr
                  (sort (range 10)))
            args (measured/args m)]
        (is (= [(range 10)] args))
        (is (= (range 10) (second (invoke m))))))
    (testing "with nested expr with local fn"
      (let [m    (measured/expr
                  (sort (random-seq 10)))
            args (measured/args m)]
        (is (= 1 (count args)))
        (is (= 10 (count (first args))))
        (is (= 10 (count (second (invoke m)))))))))

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
  (let [env {'x 'local-binding-x 'y 'local-binding-y}
        arg-vals {'arg1 'x ; local symbol
                  'arg2 '(+ 1 2) ; constant expression
                  'arg3 'y ; local symbol
                  'arg4 'z ; not a local
                  'arg5 '(+ x 1)}] ; expression containing local
    (testing "identify-local-args"
      (testing "returns empty set when env is nil"
        (is (= #{} (impl/identify-local-args arg-vals nil))))
      (testing "identifies arg-syms whose values are or contain locals"
        (is (= #{'arg1 'arg3 'arg5} (impl/identify-local-args arg-vals env))))
      (testing "returns empty set when no arg-vals are locals"
        (is (= #{} (impl/identify-local-args {'a '(+ 1 2)} env)))))))

;;; form-print tests
;; Tests verifying form-print handles both regular symbols and gensyms.

(deftest form-print-test
  ;; Tests that form-print correctly handles gensym operators.
  ;; When factor-form captures a local operator, the :op becomes a gensym.
  ;; form-print must use the gensym as-is in the output expression.
  (testing "form-print"
    (testing "returns symbol unchanged"
      (is (= 'x (impl/form-print 'x)))
      (is (= 'my-fn (impl/form-print 'my-fn))))
    (testing "handles FnCallExpr with regular operator"
      (let [fn-call (impl/->FnCallExpr '+ ['a 'b] {'a 1 'b 2} nil)]
        (is (= '(+ a b) (impl/form-print fn-call)))))
    (testing "handles FnCallExpr with gensym operator"
      ;; This is the key test - when operator is a local, it becomes a gensym
      (let [op-sym (gensym "arg")
            fn-call (impl/->FnCallExpr op-sym ['x 'y] {op-sym 'f 'x 1 'y 2} nil)
            result (impl/form-print fn-call)]
        ;; The gensym should appear in operator position
        (is (= op-sym (first result)))
        (is (= '(x y) (rest result)))))
    (testing "preserves metadata on expression"
      (let [fn-call (impl/->FnCallExpr '+ ['a] {'a 1} {:custom :meta})]
        (is (= {:custom :meta} (meta (impl/form-print fn-call))))))))

;;; factor-form tests for local operator handling
;; Tests verifying that local functions in operator position are factored out.

(deftest factor-form-local-operator-test
  ;; Tests that factor-form handles local operators correctly.
  ;; When the operator is a local binding, it should be factored into arg-vals.
  (let [env {'f 'local-binding-f}]
    (testing "factor-form"
      (testing "with local operator factors it into arg-vals"
        (let [{:keys [expr arg-vals]} (impl/factor-form '(f x) env)]
          ;; expr should have a gensym in operator position
          (is (seq? expr))
          (is (symbol? (first expr)))
          (is (not= 'f (first expr)) "operator should be replaced with gensym")
          ;; arg-vals should contain mapping for both operator and argument
          (is (= 2 (count arg-vals)))
          (is (contains? (set (vals arg-vals)) 'f) "f should be in arg-vals")
          (is (contains? (set (vals arg-vals)) 'x) "x should be in arg-vals")))
      (testing "with global operator keeps it unchanged"
        (let [{:keys [expr arg-vals]} (impl/factor-form '(+ x y) nil)]
          ;; expr should have + in operator position (global var)
          (is (= '+ (first expr)))
          ;; arg-vals should only contain x and y, not +
          (is (= 2 (count arg-vals)))
          (is (= #{'x 'y} (set (vals arg-vals))))))
      (testing "with recursive local operator factors at each level"
        ;; factor-form only factors expressions with the SAME operator as top-level
        ;; So (f (f x)) factors both f's, but (f (g x)) only factors outer f
        (let [{:keys [expr arg-vals]} (impl/factor-form '(f (f x)) {'f 'lb-f})]
          ;; f should be factored twice, x once
          (is (= 3 (count arg-vals)))
          ;; f appears twice in vals (for both calls)
          (is (= 2 (count (filter #(= 'f %) (vals arg-vals)))))
          (is (contains? (set (vals arg-vals)) 'x))))
      (testing "with different nested operator only factors outer"
        ;; (f (g x)) - only f is factored since inner has different op
        (let [{:keys [expr arg-vals]} (impl/factor-form '(f (g x)) {'f 'lb-f 'g 'lb-g})]
          ;; Only f is factored, (g x) is stored as a single value
          (is (= 2 (count arg-vals)))
          (is (contains? (set (vals arg-vals)) 'f))
          (is (contains? (set (vals arg-vals)) '(g x)))))
      (testing "with method call operator keeps it unchanged"
        (let [{:keys [expr arg-vals]} (impl/factor-form '(.toString x) nil)]
          ;; .toString is a method, not a local - should stay unchanged
          (is (= '.toString (first expr)))
          (is (= 1 (count arg-vals)))
          (is (= #{'x} (set (vals arg-vals)))))))))

;;; Local capture integration tests
;; Tests verifying that locals are correctly captured and passed through
;; the measurement pipeline.

(deftest expr-local-capture-test
  (testing "measured/expr with local bindings"
    (testing "captures simple local binding"
      (let [x 42
            m (measured/expr (+ x 1))
            res (second (invoke m))]
        (is (= 43 res))))
    (testing "captures collection local"
      (let [coll [1 2 3 4 5]
            m (measured/expr (reduce + coll))
            res (second (invoke m))]
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
            m (measured/expr (+ x 1 2))
            res (second (invoke m))]
        (is (= 13 res))))
    (testing "captures local used multiple times"
      (let [x 3
            m (measured/expr (+ x x x))
            res (second (invoke m))]
        (is (= 9 res))))
    (testing "works in loop binding context"
      (let [results (atom [])]
        (doseq [^long i (range 3)]
          (let [m (measured/expr (+ i 10))
                res (second (invoke m))]
            (swap! results conj res)))
        (is (= [10 11 12] @results))))))

;;; Type hint tests for locals
;; Tests verifying that user-provided type hints on locals are preserved,
;; while unhinted locals work without hints (accepting reflection warnings).

(deftest capture-arg-types-test
  (testing "capture-arg-types"
    (testing "returns nil for unhinted local symbol without env"
      (let [arg-vals {'arg1 'x}
            local-arg-syms #{'arg1}]
        (is (= [nil] (impl/capture-arg-types ['arg1] arg-vals local-arg-syms nil)))))
    (testing "preserves user hint on local symbol"
      (let [arg-vals {'arg1 (with-meta 'x {:tag 'long})}
            local-arg-syms #{'arg1}]
        (is (= [{:tag 'long}]
               (impl/capture-arg-types ['arg1] arg-vals local-arg-syms nil)))))
    (testing "preserves user hint on local with class tag"
      (let [arg-vals {'arg1 (with-meta 'x {:tag 'String})}
            local-arg-syms #{'arg1}]
        (is (= [{:tag 'String}]
               (impl/capture-arg-types ['arg1] arg-vals local-arg-syms nil)))))
    (testing "uses eval for non-local constants"
      (let [arg-vals {'arg1 42}
            local-arg-syms #{}]
        (is (= [{:tag 'long}]
               (impl/capture-arg-types ['arg1] arg-vals local-arg-syms nil)))))
    (testing "handles mixed locals and constants"
      (let [arg-vals {'arg1 (with-meta 'x {:tag 'double})
                      'arg2 "string"
                      'arg3 'y}
            local-arg-syms #{'arg1 'arg3}]
        (is (= [{:tag 'double}
                {:tag 'java.lang.String}
                nil]
               (impl/capture-arg-types ['arg1 'arg2 'arg3] arg-vals local-arg-syms nil)))))))

;;; Acceptance Criteria Tests
;; Tests matching the exact examples from the story acceptance criteria.

(deftest acceptance-criteria-test
  ;; This test verifies the exact acceptance criteria from the story:
  ;; - Simple local binding
  ;; - Collection local
  ;; - Loop binding
  (testing "measured/expr acceptance criteria"
    (testing "simple local binding: (let [x 42] (measured/expr (+ x 1)))"
      (let [x 42
            m (measured/expr (+ x 1))
            res (second (invoke m))]
        (is (= 43 res))))
    (testing "collection local: (let [coll (vec (range 1000))] (measured/expr (reduce + coll)))"
      (let [coll (vec (range 1000))
            m (measured/expr (reduce + coll))
            res (second (invoke m))]
        (is (= 499500 res))))
    (testing "loop binding: (doseq [i (range 3)] (measured/expr (+ i 2)))"
      (let [results (atom [])]
        (doseq [^long i (range 3)]
          (let [m (measured/expr (+ i 2))
                res (second (invoke m))]
            (swap! results conj res)))
        (is (= [2 3 4] @results))))))
