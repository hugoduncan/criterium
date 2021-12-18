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
  Measured's state function."
  ([measured]
   (invoke measured 1))
  ([measured eval-count]
   ((:f measured) ((:state-fn measured)) eval-count)))

(deftest measured-test
  (testing "arg-gen/measured"
    (let [m (arg-gen/measured
                [v (gen/vector gen/small-integer 1 10)
                 i (gen/choose 0 (dec (count v)))]
                {:v v :i i})]
      (testing "creates a measured"
        (is (measured/measured? m))
        (testing "with a state fn that generates a tuple of generated values"
          (let [[vv iv] ((:state-fn m))]
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
    (let [m
          #_:clj-kondo/ignore
          (arg-gen/measured
              {:size 3} [i gen/small-integer]
            i)]
      (testing "which generates a state fn that generates values that respect size"
        (let [[iv] ((:state-fn m))]
          (is (integer? iv))
          (is (<= -3 iv 3))))))
  (testing "arg-gen/measured can specify a test.check seed"
    (let [m
          #_:clj-kondo/ignore
          (arg-gen/measured
              {:seed 12345} [i gen/small-integer]
            i)]
      (testing "which generates a state fn that generates stable values"
        (let [vs (repeatedly 10 (:state-fn m))]
          (is (every? (comp integer? first) vs))
          (is (= '([0] [0] [0] [3] [4] [5] [-6] [3] [6] [6])
                 vs)))))))
