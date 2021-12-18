(ns criterium.measured-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.agent :as agent]
   [criterium.jvm :as jvm]
   [criterium.measured :as measured]))

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
          f                  (fn [v] (vswap! call-count inc) v)
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
          f       (fn []
                    (vswap! invokes inc)
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
      (when-not (zero? freed-bytes)
        (tap> {:zero-garbage-test
               {:allocations (frequencies thread-allocations)}}))
      (is (= [1 2] ret) "hold reference to return value until end of test"))))
