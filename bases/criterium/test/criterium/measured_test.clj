(ns criterium.measured-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.agent :as agent]
   [criterium.blackhole :as blackhole]
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
    (testing "with time-fn option over an opaque-eligible argument"
      ;; exercise the custom-timer start#/finish# forms together with a
      ;; live opaque cell: the constant subject above has no cell, so this
      ;; is the only end-to-end check of the two codegen concerns combined.
      (let [f (fn ^long [] (jvm/thread-cpu-time))
            x 41
            m (measured/expr (unchecked-inc ^long x) {:time-fn f})]
        (is (= [41] (measured/args m)))
        (is (= 42 (second (invoke m))))))
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
    (testing "preserves small-primitive user hint on local symbol"
      ;; Task 003 made small-primitive hints load-bearing: each of
      ;; boolean/byte/short/char must reach arg-bindings as its own :tag so
      ;; cell-kind routes it to the dedicated AtomicInteger cell. A
      ;; small-primitive-hinted arg defaulting to :reference mis-routed the
      ;; behaviour and allocation subjects twice; this pins the contract at
      ;; the capture-arg-types boundary.
      (doseq [tag '[boolean byte short char]]
        (let [arg-vals {'arg1 (with-meta 'x {:tag tag})}
              local-arg-syms #{'arg1}]
          (is (= [{:tag tag}]
                 (impl/capture-arg-types ['arg1] arg-vals local-arg-syms nil))
              (str "small-primitive hint " tag " preserved on local symbol")))))
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

;;; Opaque input barrier (#155)

(defn- form-seq [form]
  (tree-seq coll? seq form))

(defn- count-matching [pred form]
  (count (filter pred (form-seq form))))

(defn- get-opaque? [x]
  (and (seq? x) (= '.getOpaque (first x))))

(defn- cell-ctor? [x]
  (and (seq? x) (= 'new (first x))))

(defn- loop-form [fn-form]
  (first (filter #(and (seq? %) (= `loop (first %)))
                 (form-seq fn-form))))

(defn- start-timestamp? [x]
  ;; the `start#` binding value: `(jvm/timestamp)` or
  ;; `(. time-fn invokePrim)`, depending on whether a custom timer is used.
  (and (seq? x)
       (or (= `jvm/timestamp (first x))
           (and (= '. (first x))
                (symbol? (last x))
                (= "invokePrim" (name (last x)))))))

(defn- outer-let-bindings
  "The binding vector of the outer `let` in the generated fn form."
  [fn-form]
  (let [let-form (first (filter #(and (seq? %) (= `let (first %)))
                                (form-seq fn-form)))]
    (second let-form)))

(defn- binding-value-index
  "Index in a let binding vector of the first bound value matching `pred`,
  or nil. Binding vectors alternate name/value, so values are odd indices."
  [bindings pred]
  (->> (map vector (range) bindings)
       (filter (fn [[i v]] (and (odd? i) (pred v))))
       ffirst))

(defn- expr-fn-form
  ([tag] (expr-fn-form tag nil))
  ([tag time-fn]
   (impl/measured-expr-fn
    '[a] '(identity a)
    (cond-> {:arg-metas [(when tag {:tag tag})]}
      time-fn (assoc :time-fn time-fn)))))

(deftest opaque-tag-test
  (testing "opaque-tag?"
    (testing "is true for unboxed primitives, references and no tag"
      (doseq [tag '[long int double float java.lang.String clojure.lang.IFn]]
        (is (impl/opaque-tag? tag) (str tag)))
      (is (impl/opaque-tag? nil)))
    (testing "is true for every tag in both primitive sets"
      (doseq [tag (into impl/PRIMITIVE-CELL-TAGS impl/SMALL-PRIMITIVE-CELL-TAGS)]
        (is (impl/opaque-tag? tag) (str tag))))
    (testing "is false for array tags"
      (doseq [tag (map symbol ["long[]" "double[]" "java.lang.String[]"])]
        (is (not (impl/opaque-tag? tag)) (str tag)))))
  (testing "opaque-cell rejects a fallback tag with a named signal"
    (doseq [tag (map symbol ["long[]" "java.lang.String[]"])]
      (is (thrown-with-msg?
           AssertionError #"opaque-cell requires an opaque-eligible tag"
           (impl/opaque-cell 'x {:tag tag}))
          (str tag))))
  (testing "cell-kind classifies each tag into one kind"
    (testing "unboxed primitives map to their keyword kind"
      (is (= :long (impl/cell-kind 'long)))
      (is (= :int (impl/cell-kind 'int)))
      (is (= :double (impl/cell-kind 'double)))
      (is (= :float (impl/cell-kind 'float))))
    (testing "reference-class symbols and no tag map to :reference"
      (is (= :reference (impl/cell-kind 'java.lang.String)))
      (is (= :reference (impl/cell-kind 'clojure.lang.IFn)))
      (is (= :reference (impl/cell-kind nil))))
    (testing "small primitives map to their keyword kind"
      (doseq [tag impl/SMALL-PRIMITIVE-CELL-TAGS]
        (is (= (keyword tag) (impl/cell-kind tag)) (str tag))))
    (testing "array tags map to :fallback, not :reference"
      ;; Array tags are symbols; pin that they are excluded from the
      ;; generic reference catch regardless of cond clause order (#156).
      (doseq [tag (map symbol ["long[]" "double[]" "java.lang.String[]"])]
        (is (= :fallback (impl/cell-kind tag)) (str tag)))))
  (testing "opaque-eligible-tags-by-kind covers both primitive sets"
    (doseq [tag (into impl/PRIMITIVE-CELL-TAGS impl/SMALL-PRIMITIVE-CELL-TAGS)]
      (is (= tag (get impl/opaque-eligible-tags-by-kind (impl/cell-kind tag)))
          (str tag)))
    (doseq [[kind tag] impl/opaque-eligible-tags-by-kind]
      (is (= kind (impl/cell-kind tag)) (str tag))))
  (testing "arg-bindings is total over the arg-sym⟺meta pairing"
    (testing "a shorter arg-metas binds every arg-sym, none left unbound"
      ;; N syms / (N-1) metas: the trailing argument must still be bound
      ;; (defaulted to :reference, so barriered) rather than silently
      ;; dropped by a truncating pairing (criterion 1).
      (let [{:keys [init read]} (impl/arg-bindings '[a b] [{:tag 'long}])
            init-syms (set (take-nth 2 init))
            read-syms (set (take-nth 2 read))]
        ;; a: long cell + opaque read; b: defaulted reference cell + read.
        (is (= 2 (count (filter symbol? init-syms))))
        (is (contains? read-syms 'a))
        (is (contains? read-syms 'b))))
    (testing "an empty arg-metas (callable path) produces no bindings"
      ;; `[:as args]` with no metas must leave the destructure intact: no
      ;; cell for the whole-args symbols, no fallback binding.
      (is (= {:init [] :read [] :escape []}
             (impl/arg-bindings '[:as args] [])))
      (is (= {:init [] :read [] :escape []}
             (impl/arg-bindings '[:as args] nil))))))

(defn- assert-opaque-structure
  "Assert the once-outside / read-inside / ctor-before-start# invariants
  on a generated opaque expr-fn `form`."
  [form]
  (let [lp (loop-form form)]
    (is (= 1 (count-matching cell-ctor? form))
        "constructs the cell exactly once")
    (is (zero? (count-matching cell-ctor? lp))
        "constructs no cell inside the loop")
    (is (= 1 (count-matching get-opaque? lp))
        "reads the cell opaquely inside the loop")
    (is (= 2 (count-matching get-opaque? form))
        "reads the cell for the primed value and in the loop")
    (is (zero? (count-matching
                #(and (symbol? %)
                      (re-find #"(?i)^\.?set" (name %)))
                lp))
        "stores nothing inside the loop")
    (let [bindings  (outer-let-bindings form)
          ctor-idx  (binding-value-index
                     bindings
                     #(< 0 (long (count-matching cell-ctor? %))))
          start-idx (binding-value-index bindings start-timestamp?)]
      (is (some? ctor-idx)
          "cell ctor is a binding value in the outer let")
      (is (some? start-idx)
          "start# timestamp is a binding value in the outer let")
      (is (and ctor-idx start-idx
               (< (long ctor-idx) (long start-idx)))
          "constructs the cell before start# (outside the timed region)"))))

(deftest opaque-structure-test
  (testing "measured-expr-fn for an opaque-eligible argument"
    ;; Coverage is derived from the `cell-kind` taxonomy (one representative
    ;; tag per non-:fallback kind), not a re-enumerated tag list: adding an
    ;; opaque-eligible kind extends this loop automatically, keeping the
    ;; ctor/read-structure coverage aligned with `opaque-escape-structure-test`.
    (doseq [[kind tag] impl/opaque-eligible-tags-by-kind]
      (testing (str "kind " kind " tagged " tag " (default timer)")
        (assert-opaque-structure (expr-fn-form tag)))
      (testing (str "kind " kind " tagged " tag " (custom time-fn)")
        ;; the custom-timer branch emits a different outer-let shape:
        ;; start# is `(. time-fn invokePrim)`, exercising the
        ;; `invokePrim` arm of `start-timestamp?`.  Confirm the cell
        ;; still builds once, outside the loop, before start#.
        (assert-opaque-structure (expr-fn-form tag 'time-fn))))))

(deftest opaque-structure-multi-arg-test
  (testing "every opaque-eligible argument is read through its own cell"
    ;; criterion 1 says *every* opaque-eligible argument is read through
    ;; an atomic cell.  A single-argument form fixes the counts at N=1, so
    ;; dropping a cell for any argument but the first would still pass the
    ;; one-arg structural test.  Assert ctor / getOpaque counts scale with
    ;; the number of opaque-eligible arguments.
    (doseq [n' [2 3]]
      (testing (str n' " opaque arguments")
        (let [n         (long n')
              arg-syms  (mapv #(symbol (str "a" %)) (range n))
              form      (impl/measured-expr-fn
                         arg-syms (cons '+ (map #(list 'long %) arg-syms))
                         {:arg-metas (repeat n {:tag 'long})})
              lp        (loop-form form)
              bindings  (outer-let-bindings form)
              start-idx (binding-value-index bindings start-timestamp?)]
          (is (= n (count-matching cell-ctor? form))
              "constructs one cell per opaque argument")
          (is (zero? (count-matching cell-ctor? lp))
              "constructs no cell inside the loop")
          (is (= n (count-matching get-opaque? lp))
              "reads every cell opaquely inside the loop")
          (is (= (* 2 n) (count-matching get-opaque? form))
              "reads every cell for the primed value and in the loop")
          ;; each cell ctor must still precede start# (outside the timed
          ;; region), so dropping a cell for any argument but the first is
          ;; caught even when the ctor order is permuted.
          (is (some? start-idx) "start# timestamp is in the outer let")
          (is (= n (->> bindings
                        (map vector (range))
                        (filter (fn [[i v]]
                                  (and (odd? i)
                                       (< 0 (long (count-matching
                                                   cell-ctor? v)))
                                       (< (long i) (long start-idx)))))
                        count))
              "constructs every cell before start#"))))))

(defn- cell-escape?
  "A setup escape of a cell: `(blackhole/consume <cell-sym>)`."
  [x]
  (and (seq? x)
       (= 'criterium.util.blackhole/consume (first x))
       (symbol? (second x))
       (some? (re-find #"-cell\d+$" (name (second x))))))

(defn- cell-syms
  "Cell symbols bound in the outer let (those bound to a cell ctor)."
  [bindings]
  (->> (partition 2 bindings)
       (filter (fn [[_ v]] (cell-ctor? v)))
       (map first)
       set))

(deftest opaque-escape-structure-test
  ;; I-4 / D-5: each cell escapes exactly once, after its init and before
  ;; start#, never inside the loop.
  (testing "every opaque-eligible kind escapes its cell once, at setup"
    ;; Coverage is derived from the `cell-kind` taxonomy (one representative
    ;; tag per non-:fallback kind), not a re-enumerated tag list: adding an
    ;; opaque-eligible kind extends this loop automatically.
    (doseq [[kind tag] impl/opaque-eligible-tags-by-kind
            time-fn     [nil 'time-fn]]
      (testing (str "kind " kind " tagged " tag " time-fn " time-fn)
        (let [form      (expr-fn-form tag time-fn)
              bindings  (outer-let-bindings form)
              cells     (cell-syms bindings)
              escapes   (filter cell-escape? (form-seq form))
              ctor-idx  (binding-value-index bindings cell-ctor?)
              esc-idx   (binding-value-index bindings cell-escape?)
              start-idx (binding-value-index bindings start-timestamp?)]
          (is (= 1 (count cells)))
          (is (= 1 (count escapes)) "escapes exactly once")
          (is (= cells (set (map second escapes)))
              "escapes the cell object itself")
          (is (zero? (count-matching cell-escape? (loop-form form)))
              "no escape inside the loop")
          (is (and ctor-idx esc-idx start-idx
                   (< (long ctor-idx) (long esc-idx) (long start-idx)))
              "escape is after the init and before start#")))))
  (testing "every cell of a multi-argument form escapes once"
    (let [form     (impl/measured-expr-fn
                    '[a b c] '(+ a b c)
                    {:arg-metas [{:tag 'long} {:tag 'double} {}]})
          cells    (cell-syms (outer-let-bindings form))
          escapes  (filter cell-escape? (form-seq form))]
      (is (= 3 (count cells)))
      (is (= cells (set (map second escapes))))
      (is (= 3 (count escapes)))))
  (testing "fallback args and the callable path produce no escape"
    (doseq [tag (map symbol ["long[]" "java.lang.String[]"])]
      (is (zero? (count-matching cell-escape? (expr-fn-form tag)))
          (str tag)))
    (is (zero? (count-matching cell-escape?
                               (impl/measured-callable `(fn [] 1)))))))

(deftest opaque-fallback-test
  (testing "measured-expr-fn falls back to a plain binding"
    (doseq [tag (map symbol ["long[]" "java.lang.String[]"])]
      (testing (str "for tag " tag)
        (let [form (expr-fn-form tag)]
          (is (zero? (count-matching cell-ctor? form)))
          (is (zero? (count-matching get-opaque? form))))))
    (testing "for the callable path"
      (let [form (impl/measured-callable `(fn [] 1))]
        (is (zero? (count-matching get-opaque? form))))
      (let [form (impl/measured-callable `(fn [] [1]) `identity)]
        (is (zero? (count-matching get-opaque? form)))))))

(deftest opaque-mixed-args-test
  (testing "opaque and fallback arguments coexist"
    (let [xs  (long-array [1 2 3])
          n   2
          d   1.5
          m   (measured/expr (+ ^long (aget ^longs xs n) d))
          c   (.charAt "abc" 1) ; primitive char reads through its cell
          a   (measured/expr (str c n))]
      (is (== 4.5 (second (invoke m))))
      (is (== 4.5 (second (invoke m 1000))))
      (is (= "b2" (second (invoke a 1000)))))
    (testing "and primitive values round trip through their cells"
      (let [i (int 7)
            f (float 2.5)
            d -0.0
            l Long/MIN_VALUE]
        (is (= 7 (second (invoke (measured/expr (identity i)) 10))))
        (is (= 2.5 (double (second (invoke (measured/expr (identity f)) 10)))))
        (is (= "-0.0" (str (second (invoke (measured/expr (identity d)) 10)))))
        (is (= Long/MIN_VALUE
               (second (invoke (measured/expr (identity l)) 10))))))
    (testing "and small primitive values round trip exactly through their cells"
      ;; NOTE: value preservation alone does NOT discriminate the
      ;; small-primitive AtomicInteger cell from the AtomicReference fallback
      ;; — both round-trip the value. The small-primitive cell *routing* is
      ;; guarded elsewhere: `capture-arg-types-test` pins hint preservation,
      ;; and `opaque-tag-test` / `opaque-structure-test` pin the eligibility ⟺
      ;; cell invariant. This block only asserts exact round-trip over
      ;; boundary values; read it as a round-trip guard, not a routing guard.
      (doseq [b [true false]]
        (is (= b (second (invoke (measured/expr (identity ^boolean b)) 10)))
            (str b)))
      (doseq [x [Byte/MIN_VALUE (byte -1) (byte 0) Byte/MAX_VALUE]]
        (let [by (byte x)]
          (is (= x (second (invoke (measured/expr (identity ^byte by)) 10)))
              (str x))))
      (doseq [x [Short/MIN_VALUE (short -1) (short 0) Short/MAX_VALUE]]
        (let [sh (short x)]
          (is (= x (second (invoke (measured/expr (identity ^short sh)) 10)))
              (str x))))
      (doseq [x [\u0000 \a \uffff]]
        (let [ch (char x)]
          (is (= x (second (invoke (measured/expr (identity ^char ch)) 10)))
              (str (int x))))))))

(defn- boxing-reflection-warnings
  "Compile `form` and return only its boxed-math/reflection warning lines.
  Filters out unrelated *err* noise (such as the blackhole JVM-17 fallback
  warning) so the signal is just boxing/reflection, not raw stderr."
  [form]
  (let [err (java.io.StringWriter.)]
    (binding [*err*                err
              *warn-on-reflection* true
              *unchecked-math*     :warn-on-boxed
              *ns*                 (the-ns 'criterium.measured.impl)]
      (eval form))
    (->> (str/split-lines (str err))
         (filter #(re-find #"Boxed math warning|Reflection warning" %)))))

(deftest opaque-no-boxing-test
  (testing "primitive cells compile without boxing or reflection"
    (doseq [[tag expr] '[[long (unchecked-inc a)]
                         [int (unchecked-inc-int a)]
                         [double (unchecked-add a 1.0)]
                         [float (unchecked-add a 1.0)]
                         [byte (unchecked-inc a)]
                         [short (unchecked-inc a)]
                         [char (unchecked-inc (int a))]
                         [boolean (if a 1 0)]]]
      (let [form     (impl/measured-expr-fn '[a] expr {:arg-metas [{:tag tag}]})
            warnings (boxing-reflection-warnings form)]
        (is (empty? warnings) (str tag " " (vec warnings)))))))

(defn- thread-allocation-count
  "Count allocations on this thread during `(measured/invoke m args n)`, or
  nil when the native agent is not attached."
  [m args ^long n]
  (let [[allocations _] (agent/with-allocation-tracing
                          (measured/invoke m args n))]
    (when allocations
      (count (filterv (agent/allocation-on-thread?) allocations)))))

;; Eval-counts for the allocation probe: a tiny baseline and a large probe.
;; The gap between them is what gives `max-constant-allocation-noise` its
;; teeth — a single per-iteration allocation would add >= `alloc-probe-count`
;; (one per iteration), far above the noise tolerance.
(def ^:private alloc-baseline-count 1)
(def ^:private alloc-probe-count 10000)

;; Allocation growth below this count between the baseline and probe eval-
;; counts is constant tracing noise (a few objects), not per-iteration
;; allocation.  A single per-iteration allocation would add >=
;; `alloc-probe-count`, far above this bound.
(def ^:private max-constant-allocation-noise 100)

;; Warmup for the allocation probe: compile and JIT the measured fn before
;; tracing, so the traced run sees no first-touch (class-load / JIT) noise.
(def ^:private alloc-warmup-iterations 1000)
(def ^:private alloc-warmup-eval-count 1000)

(deftest opaque-no-per-iteration-allocation-test
  ;; I-2 / I-4: the setup escape and the opaque read add no per-iteration
  ;; allocation for the primitive cell kinds: allocation count does not grow
  ;; with eval-count (beyond a few objects of tracing noise).  Degrades to a vacuous pass without the native agent.
  (testing "primitive cells allocate nothing per iteration"
    (let [l 3
          i (int 3)
          d 3.0
          f (float 3.0)
          b (byte 3)
          s (short 3)
          c \a
          ;; The :boolean read is `(not (zero? (.getOpaque cell)))`, which
          ;; returns a canonical Boolean (TRUE/FALSE), not a fresh object; the
          ;; `(if ^boolean x ...)` subject exercises that read and confirms it
          ;; adds no per-iteration allocation (plan Risks: boxed-boolean read).
          bl true]
      (doseq [[kind m] [[:long (measured/expr (unchecked-inc ^long l))]
                        [:int (measured/expr (unchecked-inc-int ^int i))]
                        [:double (measured/expr (unchecked-add 1.0 ^double d))]
                        [:float (measured/expr (unchecked-add 1.0 ^float f))]
                        ;; Bare hinted symbol direct to the op so the
                        ;; small-primitive cell is on the path; a `(int x)`
                        ;; subform would factor into a `:reference`-routed arg
                        ;; (capture-arg-types only widens Long/Double locals),
                        ;; probing the AtomicReference path, not the cell.
                        [:byte (measured/expr (unchecked-inc-int ^byte b))]
                        [:short (measured/expr (unchecked-inc-int ^short s))]
                        [:char (measured/expr (int ^char c))]
                        [:boolean (measured/expr (if ^boolean bl 1 2))]]]
        (let [args (measured/args m)
              _    (dotimes [_ alloc-warmup-iterations]
                     (measured/invoke m args alloc-warmup-eval-count))
              a1   (thread-allocation-count m args alloc-baseline-count)
              an   (thread-allocation-count m args alloc-probe-count)]
          ;; small constant noise (a few objects) is tolerated; any
          ;; per-iteration allocation would add >= `alloc-probe-count`
          ;; (one per iteration).
          (is (or (nil? an)
                  (< (- (long an) (long a1))
                     (long max-constant-allocation-noise)))
              (str kind " allocation growth n=" alloc-baseline-count "->n="
                   alloc-probe-count " must stay below "
                   max-constant-allocation-noise " (constant noise); got n="
                   alloc-baseline-count " " a1 ", n=" alloc-probe-count
                   " " an)))))))

(deftest opaque-reference-no-reflection-test
  ;; The reference-cell read re-hints the AtomicReference.getOpaque Object
  ;; result with the argument tag (acceptance criterion 4). A subject that
  ;; needs the hint to resolve a method must compile without reflection; a
  ;; dropped re-hint would turn the read reflective and this test would fail.
  (testing "a class-tagged reference read re-hints so the subject resolves"
    (let [form     (impl/measured-expr-fn
                    '[a] '(.length a) {:arg-metas [{:tag 'java.lang.String}]})
          warnings (boxing-reflection-warnings form)]
      (is (empty? warnings) (str (vec warnings)))))
  (testing "and the hinted reference round trips through the cell"
    (let [s "abcde"]
      (is (= 5 (second (invoke (measured/expr (.length ^String s)) 10)))))))

(defn- best-ns-per-call
  "Best per-call ns of `m` over 5 timed runs at count `n`.
  Warms up once internally (the leading `invoke`) before timing, so call
  sites do not add their own warmup."
  [m ^long n]
  (invoke m n)
  (->> (repeatedly 5 #(first (invoke m n)))
       (map #(/ (double %) n))
       (apply min)))

(defrecord OpaqueRec [^long x])

(defrecord DoubleNode [^double v])

;; Shared measurement budget for every behavioural timing test
;; (`opaque-behaviour-test` and `opaque-depth-scaling-test`): a single named
;; eval-count so the sub-tests carry no incidental numeric variation.
(def ^:private behaviour-eval-count 10000000)

;; Minimum depth-3 / depth-1 per-call ratio for the folding oracle to pass.
;; A working setup escape keeps the dependency chain in the loop, so depth 3
;; costs measurably more than depth 1; a folded (hoisted) chain is
;; depth-independent (ratio ~1.0).  Tolerant of machine/noise variation.
(def ^:private depth-scaling-min-ratio 1.5)

;; Minimum subject / control per-call ratio for the hoisting floor in
;; `opaque-behaviour-test`.  A working barrier makes subject ~= control (ratio
;; ~1.0); a hoisted subject still costs the empty-loop/consume cost (~0.3-0.4x
;; control), so this floor fails for a hoisting regression.  See the
;; `opaque-behaviour-test` comment for why 0.5, not 0.1, has teeth.
(def ^:private hoisting-floor-ratio 0.5)

(defn- assert-depth-scales
  "Folding-oracle assertion: the depth-3 subject `d3` must cost at least
  `depth-scaling-min-ratio` times the depth-1 subject `d1` per call."
  [d1 d3 ^long n]
  (let [s1 (best-ns-per-call d1 n)
        s3 (best-ns-per-call d3 n)]
    (is (< (* (double depth-scaling-min-ratio) (double s1)) s3)
        (str "expected depth-3 >= " depth-scaling-min-ratio
             "x depth-1; got depth-1 " s1 " ns, depth-3 " s3 " ns"))))

(defn- assert-not-hoisted
  "Hoisting-floor assertion: `subject` must cost at least
  `hoisting-floor-ratio` times the hoist-resistant `control` per call.  A
  working opaque barrier keeps subject ~= control; a hoisted subject drops
  below the floor."
  [subject control ^long n]
  (let [s (best-ns-per-call subject n)
        c (best-ns-per-call control n)]
    (is (< (* (double hoisting-floor-ratio) (double c)) s)
        (str "expected subject >= " hoisting-floor-ratio
             "x control; got subject " s " ns, control " c " ns"))))

(deftest opaque-depth-scaling-test
  ;; Folding oracle (design 002, acceptance 2/6): a pure subject with an
  ;; internal dependency chain must cost more at depth 3 than at depth 1.
  ;; A depth-independent time means escape analysis proved the cell
  ;; thread-local and folded the chain (missing setup escape, I-4).
  ;; Mode-agnostic: runs unguarded under both the runtime blackhole
  ;; (default suite) and the compiler blackhole (CI `:blackhole` focus).
  (let [n behaviour-eval-count]
    (testing "a nested Math/sqrt chain scales with depth (double cell)"
      (let [x 2.0]
        (assert-depth-scales
         (measured/expr (Math/sqrt x))
         (measured/expr (Math/sqrt (Math/sqrt (Math/sqrt x))))
         n)))
    (testing "a Math/sqrt chain on a field read scales with depth (reference cell)"
      ;; A pure field-read chain does not scale measurably: independent
      ;; iterations overlap the loads (~1 cycle/iteration at depth 1..8 when
      ;; escaped, see task 002 implementation.md).  The dependency op is a
      ;; sqrt chain fed by a field read through the reference cell instead.
      (let [r (->DoubleNode 2.0)]
        (assert-depth-scales
         (measured/expr (Math/sqrt (.-v ^DoubleNode r)))
         (measured/expr
          (Math/sqrt (Math/sqrt (Math/sqrt (.-v ^DoubleNode r)))))
         n)))))

(deftest opaque-behaviour-test
  ;; Guarded on the compiler blackhole: under the runtime blackhole every
  ;; consume costs ~3 ns, so both subject and control are >=1 ns and the
  ;; assertion holds regardless of whether the opaque barrier works. The
  ;; defect (a final-field read hoisted out of the loop) is only observable
  ;; under the compiler blackhole (the `:blackhole` alias). Skip explicitly
  ;; otherwise rather than passing vacuously.
  ;;
  ;; The floor is `hoisting-floor-ratio` (0.5), not 0.1. A working barrier
  ;; makes subject ~= control (ratio ~1.0); a hoisted subject still costs the
  ;; empty-loop/consume cost, measured at ~0.4x control, not zero. At these
  ;; magnitudes a `0.1*c` floor passes for BOTH regimes, so it has no teeth:
  ;; it would not catch a broken barrier. `0.5*c` separates them (measured:
  ;; working ratio ~1.0 passes, hoisted ratio ~0.3-0.4 fails), so a hoisting
  ;; regression fails the assertion.
  ;;
  ;; Every sub-test shares the same arrange/act/assert shape (bind a value
  ;; and a volatile of it, build a subject and a hoist-resistant control
  ;; `measured/expr`, run the floor assertion); `assert-not-hoisted` carries
  ;; that incidental structure, so the sub-tests differ only in subject and
  ;; control.
  (if (blackhole/compiler-blackhole-enabled?)
    (let [n behaviour-eval-count]
      (testing "the opaque barrier keeps work in the loop (criterion 2)"
        (testing "a final field read is not hoisted (reference-cell path)"
          (let [r (->OpaqueRec 1)
                v (volatile! r)]
            (assert-not-hoisted
             (measured/expr (.-x ^OpaqueRec r))
             ;; a volatile read per call can not be hoisted
             (measured/expr (.-x ^OpaqueRec (deref ^clojure.lang.Volatile v)))
             n)))
        (testing "a primitive arithmetic expression is not hoisted (primitive-cell path)"
          ;; Distinct code from the reference path: the `long` argument flows
          ;; through an AtomicLong cell read with getOpaque. A regression that
          ;; re-enabled hoisting for primitive arguments only would pass the
          ;; reference case above but fail here.
          (let [x 1
                v (volatile! x)]
            (assert-not-hoisted
             (measured/expr (unchecked-inc ^long x))
             (measured/expr
              (unchecked-inc ^long (deref ^clojure.lang.Volatile v)))
             n)))
        (testing "a double bit-reinterpretation read is not hoisted (double-cell path)"
          ;; Distinct code from both long and reference: the `double` argument
          ;; flows through an AtomicLong cell whose read is wrapped in
          ;; (Double/longBitsToDouble (.getOpaque cell)). A regression that
          ;; folded that bit-reinterpretation of a now-provably-stable read
          ;; would pass the long case above but fail here.
          (let [x 1.5
                v (volatile! x)]
            (assert-not-hoisted
             (measured/expr (unchecked-add 1.0 ^double x))
             (measured/expr
              (unchecked-add 1.0 ^double (deref ^clojure.lang.Volatile v)))
             n)))
        (testing "an int read is not hoisted (int-cell path)"
          ;; Distinct code from the long path: the `int` argument flows through
          ;; an AtomicInteger cell whose read is re-wrapped (int (.getOpaque
          ;; cell)). A regression that re-enabled hoisting on the narrower
          ;; AtomicInteger path only would pass the long case above but fail
          ;; here.
          (let [x (int 1)
                v (volatile! x)]
            (assert-not-hoisted
             (measured/expr (unchecked-inc-int ^int x))
             (measured/expr
              (unchecked-inc-int ^int (deref ^clojure.lang.Volatile v)))
             n)))
        (testing "a float bit-reinterpretation read is not hoisted (float-cell path)"
          ;; Distinct code from both long and double: the `float` argument
          ;; flows through an AtomicInteger cell whose read is wrapped in
          ;; (Float/intBitsToFloat (.getOpaque cell)) — the only
          ;; AtomicInteger-based bit-reinterpretation path. A regression that
          ;; folded that bit-reinterpretation would pass the long/double cases
          ;; above but fail here.
          (let [x (float 1.5)
                v (volatile! x)]
            (assert-not-hoisted
             (measured/expr (unchecked-add 1.0 ^float x))
             (measured/expr
              (unchecked-add 1.0 ^float (deref ^clojure.lang.Volatile v)))
             n)))
        ;; Small primitives (task 003): each widens into an AtomicInteger
        ;; cell and narrows on read. Without the barrier they are bound once
        ;; before the loop and the subject is hoisted. The subject must put
        ;; the bare hinted symbol directly as an argument of the op so the
        ;; small-primitive cell is on the path: a `(int x)` subform would be
        ;; factored into its own captured arg, and `capture-arg-types` only
        ;; widens `Long`/`Double` locals, so that arg would default to
        ;; `:reference` and route through an `AtomicReference`, giving no
        ;; small-primitive-barrier signal.
        (testing "a byte read is not hoisted (byte-cell path)"
          (let [x (byte 1)
                v (volatile! x)]
            (assert-not-hoisted
             (measured/expr (unchecked-inc-int ^byte x))
             (measured/expr
              (unchecked-inc-int ^byte (deref ^clojure.lang.Volatile v)))
             n)))
        (testing "a short read is not hoisted (short-cell path)"
          (let [x (short 1)
                v (volatile! x)]
            (assert-not-hoisted
             (measured/expr (unchecked-inc-int ^short x))
             (measured/expr
              (unchecked-inc-int ^short (deref ^clojure.lang.Volatile v)))
             n)))
        (testing "a char read is not hoisted (char-cell path)"
          (let [x \a
                v (volatile! x)]
            (assert-not-hoisted
             (measured/expr (int ^char x))
             (measured/expr
              (int ^char (deref ^clojure.lang.Volatile v)))
             n)))
        (testing "a boolean read is not hoisted (boolean-cell path)"
          (let [x true
                v (volatile! x)]
            (assert-not-hoisted
             (measured/expr (if ^boolean x 1 2))
             (measured/expr
              (if ^boolean (deref ^clojure.lang.Volatile v) 1 2))
             n)))))
    (testing "skipped: opaque barrier only observable under compiler blackhole"
      (is (= :runtime (blackhole/mode))
          "expected runtime blackhole when compiler blackhole is disabled"))))
