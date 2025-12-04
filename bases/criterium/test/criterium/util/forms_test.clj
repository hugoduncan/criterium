(ns criterium.util.forms-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.util.forms :refer [cond*]]))

(deftest cond*-tests
  (testing "Basic conditional logic without let"
    (is (= :a (cond* true :a)))
    (is (= :b (cond* false :a true :b)))
    (is (nil? (cond* false :a false :b))))

  (testing "Single let binding"
    (is (= 15 (cond*
               :let [a 10]
               true (+ a 5))))

    (is (= 20 (cond*
               false :skip
               :let [b 15]
               (> b 10) (+ b 5)))))

  (testing "Cascading let bindings"
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

  (testing "Else clause behavior"
    (is (= 25 (cond*
               :let [a 20]
               false :wrong
               :let [b 5]
               :else (+ a b))))

    (is (= :default (cond*
                     :let [x 10]
                     (< x 5) :impossible
                     :else :default))))

  (testing "Error conditions"
    (testing "Invalid :let usage"
      (is (thrown? Exception (eval `(cond* :let "not a vector")))))

    (testing "Odd number of clauses"
      (is (thrown? Exception (eval `(cond* :let [~'a 1] true))))
      (is (thrown? Exception (eval `(cond* true :a false))))))

  (testing "Edge cases"
    (testing "Only let bindings"
      (is (nil? (cond* :let [_ 1] :let [_ 2]))))

    (testing "Early return prevents later binding"
      (is (= 10 (cond*
                 true 10
                 :let [_ (throw (Exception. "Shouldn't execute"))]))))))
