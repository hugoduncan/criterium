(ns criterium.utils.forms-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.utils.interface :refer [cond*]]))

;; Tests for the cond* macro from criterium.utils.interface.
;; cond* extends standard cond with :let bindings visible to subsequent clauses.

(deftest cond*-test
  (testing "cond*"
    (testing "with basic conditional logic without let"
      (is (= :a (cond* true :a)))
      (is (= :b (cond* false :a true :b)))
      (is (nil? (cond* false :a false :b))))

    (testing "with a single let binding"
      (is (= 15 (cond*
                 :let [a 10]
                 true (+ a 5))))

      (is (= 20 (cond*
                 false :skip
                 :let [b 15]
                 (> b 10) (+ b 5)))))

    (testing "with cascading let bindings"
      (is (= 40 (cond*
                 :let [a 10]
                 :let [b (+ a 20)]
                 true (+ a b))))

      (is (= "15 cats" (cond*
                        :let [x 5]
                        false :wrong
                        :let [y (+ x 10)]
                        (> y 12) (str y " cats")
                        :else :default))))

    (testing "with else clause"
      (is (= 25 (cond*
                 :let [a 20]
                 false :wrong
                 :let [b 5]
                 :else (+ a b))))

      (is (= :default (cond*
                       :let [x 10]
                       (< x 5) :impossible
                       :else :default))))

    (testing "with error conditions"
      (testing "when given invalid :let usage"
        (is (thrown? Exception (eval `(cond* :let "not a vector")))))

      (testing "when given odd number of clauses"
        (is (thrown? Exception (eval `(cond* :let [~'a 1] true))))
        (is (thrown? Exception (eval `(cond* true :a false))))))

    (testing "with edge cases"
      (testing "when containing only let bindings"
        (is (nil? (cond* :let [_ 1] :let [_ 2]))))

      (testing "when early return prevents later binding"
        (is (= 10 (cond*
                   true 10
                   :let [_ (throw (Exception. "Shouldn't execute"))])))))))
