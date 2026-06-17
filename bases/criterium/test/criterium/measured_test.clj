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

(deftest warmup-args-fn-test
  ;; Tests for the warmup-args-fn field in Measured record.
  ;; Validates the 4-arity constructor and warmup-args function behavior.
  (testing "warmup-args-fn"
    (testing "4-arity constructor creates measured with warmup-args-fn"
      (let [m (measured/measured
               (fn [] [:measurement])
               (fn [_args _n] [0 nil])
               nil
               (fn [] [:warmup]))]
        (is (measured/measured? m))
        (is (= [:measurement] (measured/args m)))
        (is (= [:warmup] (measured/warmup-args m)))))
    (testing "warmup-args falls back to args-fn when warmup-args-fn is nil"
      (let [m (measured/measured
               (fn [] [:fallback])
               (fn [_args _n] [0 nil]))]
        (is (= [:fallback] (measured/args m)))
        (is (= [:fallback] (measured/warmup-args m)))))
    (testing "warmup-args-fn can generate varied inputs"
      (let [warmup-call-count (volatile! 0)
            m (measured/measured
               (fn [] [42])
               (fn [[x] _n] [0 x])
               nil
               (fn []
                 (vswap! warmup-call-count inc-long)
                 [(rand-int 1000)]))]
        (is (= [42] (measured/args m)))
        (measured/warmup-args m)
        (measured/warmup-args m)
        (is (= 2 @warmup-call-count))))
    (testing "3-arity constructor creates measured without warmup-args-fn"
      (let [m (measured/measured
               (fn [] [:args])
               (fn [_args _n] [0 nil])
               (fn [] ::expr))]
        (is (= ::expr (measured/symbolic m)))
        (is (= [:args] (measured/warmup-args m)))))))

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
        (is (= [0 1] (invoke original-m)))))
    (testing "preserves warmup-args-fn"
      (let [original-m (measured/measured
                        (fn [] [:original-args])
                        (fn [_args _n] [0 nil])
                        nil
                        (fn [] [:warmup-args]))
            modified-m (measured/with-args-fn original-m (fn [] [:new-args]))]
        (is (= [:new-args] (measured/args modified-m)))
        (is (= [:warmup-args] (measured/warmup-args modified-m)))))))

(deftest callable-test
  ;; measured/callable builds a measured that applies f to the argument list
  ;; returned by the setup function.  The setup function returns the *sequence
  ;; of arguments*; f is called with those arguments spread.  Regression test
  ;; for a bug where only the first argument reached f (multi-arg callables
  ;; failed with an arity error).
  (testing "no setup function (zero-arg f)"
    (let [m (measured/callable (fn [] 41))]
      (is (= 41 (second (invoke m))))))
  (testing "single collection argument"
    (let [f (fn [coll] (reduce + coll))
          m (measured/callable (fn [] [[1 2 3]]) f)]
      (is (= 6 (second (invoke m))))))
  (testing "two arguments are both passed to f"
    (let [f (fn [a b] (+ (long a) (long b)))
          m (measured/callable (fn [] [10 20]) f)]
      (is (= 30 (second (invoke m))))))
  (testing "three arguments are all passed to f"
    (let [f (fn [a b c] (* (long a) (long b) (long c)))
          m (measured/callable (fn [] [2 3 4]) f)]
      (is (= 24 (second (invoke m))))))
  (testing "zero-arg f via empty argument list"
    (let [f (fn [] 42)
          m (measured/callable (fn [] []) f)]
      (is (= 42 (second (invoke m))))))
  (testing "warmup-args-fn arity also spreads all arguments"
    (let [f (fn [a b] (* (long a) (long b)))
          m (measured/callable (fn [] [6 7]) f (fn [] [1 1]))]
      (is (= 42 (second (invoke m)))))))

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

(deftest callable-unrolled-invocation-test
  ;; The callable measurement loop invokes f via an unrolled arity dispatch
  ;; rather than `apply`, so it allocates no argument seq per iteration.
  ;; `apply` over the argument vector would surface as a ChunkedSeq/ArraySeq;
  ;; assert those never appear.  Degrades to a vacuous pass when the native
  ;; agent is not attached (allocations is nil).
  (testing "callable does not allocate an argument seq via apply"
    (let [f                  (fn [a b] (unchecked-add (long a) (long b)))
          ;; values outside the Long cache so any apply seq would be obvious
          mm                 (measured/callable (fn [] [100000 200000]) f)
          st                 (measured/args mm)
          _                  (dotimes [_ 1000] (measured/invoke mm st 1000))
          [allocations ret]  (agent/with-allocation-tracing
                               (measured/invoke mm st 1000))
          thread-allocations (->> allocations
                                  (filterv (agent/allocation-on-thread?)))
          types              (set (map :object-type thread-allocations))]
      (is (not (contains? types "clojure.lang.PersistentVector$ChunkedSeq"))
          thread-allocations)
      (is (not (contains? types "clojure.lang.ArraySeq"))
          thread-allocations)
      (is (= 300000 (second ret))
          "hold reference to return value until end of test"))))

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
        (let [{:keys [_expr arg-vals]} (impl/factor-form '(f (f x)) {'f 'lb-f})]
          ;; f should be factored twice, x once
          (is (= 3 (count arg-vals)))
          ;; f appears twice in vals (for both calls)
          (is (= 2 (count (filter #(= 'f %) (vals arg-vals)))))
          (is (contains? (set (vals arg-vals)) 'x))))
      (testing "with different nested operator only factors outer"
        ;; (f (g x)) - only f is factored since inner has different op
        (let [{:keys [_expr arg-vals]} (impl/factor-form '(f (g x)) {'f 'lb-f 'g 'lb-g})]
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

;;; Local Operator Acceptance Tests
;; Tests verifying that local functions in operator position are dynamically
;; dispatched, not constant-folded. Each test proves dynamic dispatch by
;; substituting a different function via with-args-fn and verifying the
;; result changes.

(deftest local-operator-ac1-test
  ;; AC1: Local function in operator position - verify function is used dynamically.
  ;; Proves the function is not constant-folded by substituting a different
  ;; function via with-args-fn and verifying the result changes.
  (testing "local function in operator position"
    (testing "captures function in args"
      (let [f +
            m (measured/expr (f 1 2))
            args (measured/args m)]
        ;; The local function f should be captured in args
        (is (some #(= + %) args) "function + should be in args")))
    (testing "uses function dynamically (not constant-folded)"
      (let [f +
            m (measured/expr (f 10 5))
            ;; Original invocation with +
            original-result (second (invoke m))
            ;; Create new measured with different function (-)
            args (measured/args m)
            ;; Find position of function in args and replace it
            new-args-fn (fn []
                          (mapv #(if (= + %) - %) args))
            m-with-minus (measured/with-args-fn m new-args-fn)
            new-result (second (invoke m-with-minus))]
        (is (= 15 original-result) "original should use +: 10 + 5 = 15")
        (is (= 5 new-result) "substituted should use -: 10 - 5 = 5")
        (is (not= original-result new-result)
            "results must differ to prove dynamic dispatch")))))

(deftest local-operator-ac2-test
  ;; AC2: Local function with local arguments.
  ;; The function and all arguments should be in args.
  (testing "local function with local arguments"
    (testing "captures function and arguments in args"
      (let [f +
            x 1
            y 2
            m (measured/expr (f x y))
            args (measured/args m)]
        ;; All three locals should be captured
        (is (= 3 (count args)))
        (is (some #(= + %) args) "function + should be in args")
        (is (some #(= 1 %) args) "x=1 should be in args")
        (is (some #(= 2 %) args) "y=2 should be in args")))
    (testing "produces correct result"
      (let [f +
            x 10
            y 20
            m (measured/expr (f x y))]
        (is (= 30 (second (invoke m))))))))

(deftest local-operator-ac3-test
  ;; AC3: Higher-order function pattern using partial.
  ;; The partially applied function should be passed through args-fn.
  (testing "higher-order function pattern"
    (testing "partial function is captured in args"
      (let [f (partial + 10)
            m (measured/expr (f 5))
            args (measured/args m)]
        ;; The partial function and 5 should be in args
        (is (= 2 (count args)))
        (is (some #(= 5 %) args) "argument 5 should be in args")
        (is (some fn? args) "partial function should be in args")))
    (testing "uses partial function dynamically"
      (let [f (partial + 10)
            m (measured/expr (f 5))
            original-result (second (invoke m))
            ;; Substitute with a different partial function
            args (measured/args m)
            new-args-fn (fn []
                          (mapv #(if (fn? %) (partial * 10) %) args))
            m-with-mult (measured/with-args-fn m new-args-fn)
            new-result (second (invoke m-with-mult))]
        (is (= 15 original-result) "original: (+ 10 5) = 15")
        (is (= 50 new-result) "substituted: (* 10 5) = 50")))))

(defn- double-it ^long [^long x] (* x 2))
(defn- triple-it ^long [^long x] (* x 3))
(defn public-add
  "A public function for qualified symbol testing."
  ^long [^long a ^long b]
  (+ a b))

(deftest local-operator-ac4-test
  ;; AC4: Nested local function calls.
  ;; Current implementation: only factors expressions with SAME operator recursively.
  ;; For different nested operators, only the outer is captured; inner is evaluated.
  (testing "nested local function calls"
    (testing "same operator used recursively - both captured"
      ;; (f (f x)) - same operator, so both calls are factored
      (let [f inc
            m (measured/expr (f (f 5)))
            args (measured/args m)]
        ;; f is captured twice, plus the value 5
        (is (= 3 (count args)))
        (is (= 2 (count (filter #(= inc %) args))) "inc should appear twice")
        (is (some #(= 5 %) args) "5 should be in args")))
    (testing "same operator - produces correct result"
      (let [f inc
            m (measured/expr (f (f 5)))]
        ;; inc(inc(5)) = 7
        (is (= 7 (second (invoke m))))))
    (testing "same operator - function is used dynamically"
      (let [f double-it
            m (measured/expr (f (f 2)))
            original-result (second (invoke m))
            ;; Replace double-it with triple-it
            args (measured/args m)
            new-args-fn (fn []
                          (mapv #(if (= double-it %) triple-it %) args))
            m-with-triple (measured/with-args-fn m new-args-fn)
            new-result (second (invoke m-with-triple))]
        ;; Original: double-it(double-it(2)) = double-it(4) = 8
        (is (= 8 original-result))
        ;; Substituted: triple-it(triple-it(2)) = triple-it(6) = 18
        (is (= 18 new-result))
        (is (not= original-result new-result)
            "results must differ to prove function is dynamic")))
    (testing "different operators - outer captured, inner evaluated"
      ;; (f (g x)) - different operators, only f is factored
      ;; Inner (g x) is evaluated at macro expansion time
      (let [f double-it
            g inc
            m (measured/expr (f (g 5)))
            args (measured/args m)]
        ;; Only f (double-it) and the result of (g 5) = 6 are captured
        (is (= 2 (count args)))
        (is (some #(= double-it %) args) "outer function should be in args")
        (is (some #(= 6 %) args) "evaluated inner result (6) should be in args")))
    (testing "different operators - outer function is dynamic"
      (let [f double-it
            g inc
            m (measured/expr (f (g 5)))
            original-result (second (invoke m))
            ;; Replace outer function
            args (measured/args m)
            new-args-fn (fn []
                          (mapv #(if (= double-it %) triple-it %) args))
            m-with-triple (measured/with-args-fn m new-args-fn)
            new-result (second (invoke m-with-triple))]
        ;; Original: double-it((inc 5)) = double-it(6) = 12
        (is (= 12 original-result))
        ;; Substituted: triple-it(6) = 18
        (is (= 18 new-result))
        (is (not= original-result new-result)
            "results must differ to prove outer function is dynamic")))))

;;; Edge Case Tests for Operator Handling
;; Tests verifying that interop calls, keywords in function position,
;; and qualified symbols are handled correctly and not treated as local operators.

(deftest edge-case-tc1-interop-test
  ;; TC1: Interop calls - receiver is argument, not operator.
  ;; Method calls like (.methodName obj) should work correctly.
  ;; The receiver (obj) should be captured in args, not the method name.
  (testing "interop calls"
    (testing "string method call works correctly"
      (let [s "hello"
            m (measured/expr (.toUpperCase s))
            args (measured/args m)]
        ;; The string should be captured as an argument
        (is (= 1 (count args)))
        (is (= "hello" (first args)))
        ;; Result should be the uppercased string
        (is (= "HELLO" (second (invoke m))))))
    (testing "string method with argument works"
      (let [s "hello world"
            m (measured/expr (.substring s 0 5))
            args (measured/args m)]
        ;; String and indices should be captured
        (is (= 3 (count args)))
        (is (some #(= "hello world" %) args))
        (is (= "hello" (second (invoke m))))))
    (testing "local receiver is dynamically used"
      ;; Prove the receiver is not constant-folded by substitution
      (let [s "abc"
            m (measured/expr (.length s))
            original-result (second (invoke m))
            ;; Substitute with different string
            new-args-fn (fn [] ["longer string"])
            m-with-longer (measured/with-args-fn m new-args-fn)
            new-result (second (invoke m-with-longer))]
        (is (= 3 original-result) "original: \"abc\".length() = 3")
        (is (= 13 new-result) "substituted: \"longer string\".length() = 13")
        (is (not= original-result new-result)
            "results must differ to prove receiver is dynamic")))))

(deftest edge-case-tc2-keyword-operator-test
  ;; TC2: Keywords in function position.
  ;; Keywords can be used as functions to look up values in maps.
  ;; The keyword should stay in operator position (not treated as local).
  (testing "keywords in function position"
    (testing "keyword lookup with literal keyword"
      (let [m (measured/expr (:key {:key 42}))]
        (is (= 42 (second (invoke m))))))
    (testing "keyword lookup with local map"
      (let [data {:key 99 :other 1}
            m (measured/expr (:key data))
            args (measured/args m)]
        ;; The map should be captured as an argument
        (is (= 1 (count args)))
        (is (= data (first args)))
        (is (= 99 (second (invoke m))))))
    (testing "local map is dynamically used"
      ;; Prove the map is not constant-folded by substitution
      (let [data {:key 100}
            m (measured/expr (:key data))
            original-result (second (invoke m))
            ;; Substitute with different map
            new-args-fn (fn [] [{:key 200}])
            m-with-different (measured/with-args-fn m new-args-fn)
            new-result (second (invoke m-with-different))]
        (is (= 100 original-result))
        (is (= 200 new-result))
        (is (not= original-result new-result)
            "results must differ to prove map is dynamic")))
    (testing "keyword with default value"
      (let [data {:other 1}
            m (measured/expr (:missing data :default))
            args (measured/args m)]
        ;; Map and default should be captured
        (is (= 2 (count args)))
        (is (= :default (second (invoke m))))))))

;;; Java Method Call Tests with Local Arguments
;; Tests verifying that Java interop calls correctly handle local bindings
;; as receiver and/or arguments. Method names (.methodName) are operators,
;; not locals, while the receiver and arguments should be factored.

(deftest java-interop-local-receiver-test
  ;; Verifies: (let [s "hello"] (measured/expr (.toUpperCase s)))
  ;; - .toUpperCase stays as operator
  ;; - s is factored into arg-vals
  (testing "Java method call with local receiver"
    (testing "receiver is captured in args"
      (let [s "hello"
            m (measured/expr (.toUpperCase s))
            args (measured/args m)]
        (is (= 1 (count args)))
        (is (= "hello" (first args)))))
    (testing "produces correct result"
      (let [s "hello"
            m (measured/expr (.toUpperCase s))]
        (is (= "HELLO" (second (invoke m))))))
    (testing "receiver is used dynamically"
      (let [s "abc"
            m (measured/expr (.toUpperCase s))
            original-result (second (invoke m))
            new-args-fn (fn [] ["xyz"])
            m-with-different (measured/with-args-fn m new-args-fn)
            new-result (second (invoke m-with-different))]
        (is (= "ABC" original-result))
        (is (= "XYZ" new-result))
        (is (not= original-result new-result)
            "results must differ to prove receiver is dynamic")))))

(deftest java-interop-local-argument-test
  ;; Verifies: (let [idx 1] (measured/expr (.nth [0 1 2] idx)))
  ;; - .nth stays as operator
  ;; - Both [0 1 2] and idx are factored
  (testing "Java method call with local argument"
    (testing "receiver and argument are captured in args"
      (let [idx 1
            m (measured/expr (.nth [0 1 2] idx))
            args (measured/args m)]
        (is (= 2 (count args)))
        (is (some #(= [0 1 2] %) args) "vector should be in args")
        (is (some #(= 1 %) args) "idx should be in args")))
    (testing "produces correct result"
      (let [idx 1
            m (measured/expr (.nth [0 1 2] idx))]
        (is (= 1 (second (invoke m))))))
    (testing "argument is used dynamically"
      (let [idx 1
            m (measured/expr (.nth [0 1 2] idx))
            original-result (second (invoke m))
            ;; Replace the index with 2
            args (measured/args m)
            new-args-fn (fn [] (mapv #(if (= 1 %) 2 %) args))
            m-with-different (measured/with-args-fn m new-args-fn)
            new-result (second (invoke m-with-different))]
        (is (= 1 original-result) "original: vec.nth(1) = 1")
        (is (= 2 new-result) "substituted: vec.nth(2) = 2")
        (is (not= original-result new-result)
            "results must differ to prove argument is dynamic")))))

(deftest java-interop-both-local-test
  ;; Verifies: (let [s "hello" n 3] (measured/expr (.substring s n)))
  ;; - .substring stays as operator
  ;; - Both s and n are factored
  (testing "Java method call with both local receiver and argument"
    (testing "both receiver and argument are captured in args"
      (let [s "hello"
            n 3
            m (measured/expr (.substring s n))
            args (measured/args m)]
        (is (= 2 (count args)))
        (is (some #(= "hello" %) args) "string should be in args")
        (is (some #(= 3 %) args) "n should be in args")))
    (testing "produces correct result"
      (let [s "hello"
            n 3
            m (measured/expr (.substring s n))]
        (is (= "lo" (second (invoke m))))))
    (testing "receiver is used dynamically"
      (let [s "hello"
            n 3
            m (measured/expr (.substring s n))
            original-result (second (invoke m))
            ;; Replace the string
            args (measured/args m)
            new-args-fn (fn [] (mapv #(if (= "hello" %) "world!" %) args))
            m-with-different (measured/with-args-fn m new-args-fn)
            new-result (second (invoke m-with-different))]
        (is (= "lo" original-result))
        (is (= "ld!" new-result) "world!.substring(3) = ld!")
        (is (not= original-result new-result)
            "results must differ to prove receiver is dynamic")))
    (testing "argument is used dynamically"
      (let [s "hello"
            n 3
            m (measured/expr (.substring s n))
            original-result (second (invoke m))
            ;; Replace the index
            args (measured/args m)
            new-args-fn (fn [] (mapv #(if (= 3 %) 1 %) args))
            m-with-different (measured/with-args-fn m new-args-fn)
            new-result (second (invoke m-with-different))]
        (is (= "lo" original-result))
        (is (= "ello" new-result) "hello.substring(1) = ello")
        (is (not= original-result new-result)
            "results must differ to prove argument is dynamic")))))

(deftest edge-case-tc3-qualified-symbol-test
  ;; TC3: Qualified symbols (ns/fn) remain global var references.
  ;; Qualified symbols like clojure.core/+ or criterium.measured-test/public-add
  ;; should not be treated as locals - they reference global vars.
  (testing "qualified symbols as operators"
    (testing "clojure.core qualified symbol"
      (let [m (measured/expr (clojure.core/+ 1 2))
            args (measured/args m)]
        ;; Only the arguments should be captured, not the qualified function
        (is (= 2 (count args)))
        (is (= #{1 2} (set args)))
        (is (= 3 (second (invoke m))))))
    (testing "current namespace qualified symbol"
      (let [m (measured/expr (criterium.measured-test/public-add 10 20))
            args (measured/args m)]
        ;; Only the arguments should be captured
        (is (= 2 (count args)))
        (is (= #{10 20} (set args)))
        (is (= 30 (second (invoke m))))))
    (testing "qualified symbols are not captured in args"
      ;; Unlike local operators, qualified symbols should NOT appear in args
      (let [m (measured/expr (clojure.core/+ 5 5))
            args (measured/args m)]
        ;; clojure.core/+ should not be in args
        (is (not (some #(= clojure.core/+ %) args))
            "qualified function should not be captured in args")
        (is (= 10 (second (invoke m))))))
    (testing "arguments to qualified functions are dynamic"
      ;; The arguments should still be dynamically passed
      (let [x 3
            y 4
            m (measured/expr (clojure.core/+ x y))
            original-result (second (invoke m))
            ;; Substitute with different values
            new-args-fn (fn [] [10 20])
            m-with-different (measured/with-args-fn m new-args-fn)
            new-result (second (invoke m-with-different))]
        (is (= 7 original-result))
        (is (= 30 new-result))
        (is (not= original-result new-result)
            "results must differ to prove arguments are dynamic")))))
