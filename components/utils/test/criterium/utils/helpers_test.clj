(ns criterium.utils.helpers-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.utils.interface :as utils]))

;; Tests for the generic utility functions extracted to criterium.utils.interface.
;; These are math utilities, collection helpers, and tree walking functions.

(deftest sqr-test
  (testing "sqr"
    (testing "squares numbers"
      (is (= 4 (utils/sqr 2)))
      (is (= 9 (utils/sqr 3)))
      (is (= 0 (utils/sqr 0)))
      (is (= 4 (utils/sqr -2))))))

(deftest sqrd-test
  (testing "sqrd"
    (testing "squares doubles"
      (is (= 4.0 (utils/sqrd 2.0)))
      (is (= 6.25 (utils/sqrd 2.5))))))

(deftest cubed-test
  (testing "cubed"
    (testing "cubes doubles"
      (is (= 8.0 (utils/cubed 2.0)))
      (is (= -8.0 (utils/cubed -2.0))))))

(deftest trunc-test
  (testing "trunc"
    (testing "rounds towards zero"
      (is (= 2.0 (utils/trunc 2.9)))
      (is (= -2.0 (utils/trunc -2.9)))
      (is (= 0.0 (utils/trunc 0.5)))
      (is (= 0.0 (utils/trunc -0.5))))))

(deftest update-vals-test
  (testing "update-vals"
    (testing "applies function to all values"
      (is (= {:a 2 :b 3} (utils/update-vals {:a 1 :b 2} inc)))
      (is (= {} (utils/update-vals {} inc))))))

(deftest filter-map-test
  (testing "filter-map"
    (testing "filters entries by value predicate"
      (is (= {:a 2 :b 4} (utils/filter-map even? {:a 2 :b 4 :c 3})))
      (is (= {} (utils/filter-map even? {:a 1 :b 3}))))))

(deftest deep-merge-test
  (testing "deep-merge"
    (testing "merges maps recursively"
      (is (= {:a {:b 2 :c 3}}
             (utils/deep-merge {:a {:b 1}} {:a {:b 2 :c 3}})))
      (is (= {:a 1 :b 2}
             (utils/deep-merge {:a 1} {:b 2}))))
    (testing "when merging with nil"
      (is (= {:a 1} (utils/deep-merge nil {:a 1})))
      (is (= {:a 1} (utils/deep-merge {:a 1} nil))))))

(deftest walk-test
  (testing "walk"
    (testing "preserves metadata on lists"
      (let [form (with-meta '(a b c) {:test true})]
        (is (= {:test true}
               (meta (utils/walk identity identity form))))))
    (testing "preserves metadata on vectors"
      (let [form (with-meta [1 2 3] {:test true})]
        (is (= {:test true}
               (meta (utils/walk identity identity form))))))))

(deftest postwalk-test
  (testing "postwalk"
    (testing "applies function to all nodes"
      (is (= [2 [3 4]]
             (utils/postwalk #(if (number? %) (inc (long %)) %) [1 [2 3]]))))
    (testing "preserves structure"
      (is (= {:a [1 2]}
             (utils/postwalk identity {:a [1 2]}))))))

(deftest assoc-tag-test
  (testing "assoc-tag"
    (testing "associates type tag to symbol metadata"
      (let [sym (utils/assoc-tag 'x 'String)]
        (is (= 'String (:tag (meta sym))))))))

(deftest reduce-double-vector-test
  (testing "reduce-double-vector"
    (testing "reduces doubles over a vector"
      (is (= 6.0
             (utils/reduce-double-vector
              (reify clojure.lang.IFn$DOD
                (invokePrim [_ acc x] (+ ^double acc ^double x)))
              0.0
              [1.0 2.0 3.0]))))))
