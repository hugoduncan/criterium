(ns criterium.measured-array-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.measured :as measured]
   [criterium.measured.impl :as impl]))

(deftype ArrayComponent [])

(def array-cases
  ;; Explicit descriptors keep the expected contract independent of tag-meta.
  [["[Z" '(boolean-array 3)]
   ["[B" '(byte-array 3)]
   ["[S" '(short-array 3)]
   ["[C" '(char-array 3)]
   ["[I" '(int-array 3)]
   ["[J" '(long-array 3)]
   ["[F" '(float-array 3)]
   ["[D" '(double-array 3)]
   ["[Ljava.lang.Object;" '(object-array 3)]
   ["[Ljava.lang.String;" '(make-array String 3)]
   ["[Lcriterium.measured_array_test.ArrayComponent;"
    '(make-array criterium.measured_array_test.ArrayComponent 3)]
   ["[[J" '(make-array Long/TYPE 3 2)]
   ["[[Ljava.lang.String;" '(make-array String 3 2)]])

(deftest array-tag-meta-test
  ;; Each inferred array hint retains its component class and dimensions.
  (testing "tag-meta returns exact JVM-name strings for arrays"
    (doseq [[descriptor constructor] array-cases]
      (testing descriptor
        (is (= {:tag descriptor}
               (impl/tag-meta (class (eval constructor)))))))))

(deftest non-array-tag-meta-test
  ;; Array support must not change scalar conversions or reference metadata.
  (testing "tag-meta preserves non-array behavior"
    (is (nil? (impl/tag-meta nil)))
    (doseq [[type expected] [[String 'java.lang.String]
                             [Object 'java.lang.Object]
                             [Long 'long]
                             [Integer 'int]
                             [Double 'double]
                             [Float 'float]
                             [Boolean 'java.lang.Boolean]
                             [Byte 'java.lang.Byte]
                             [Short 'java.lang.Short]
                             [Character 'java.lang.Character]
                             [Long/TYPE 'long]
                             [Double/TYPE 'double]]]
      (is (= {:tag expected} (impl/tag-meta type)) (str type)))))

(deftest inferred-array-local-compilation-test
  ;; Compile inside the test so a regression does not prevent namespace loading.
  (testing "inferred array locals compile and return their length"
    (doseq [[descriptor constructor] array-cases]
      (testing descriptor
        (let [subject (eval (list 'let ['xs constructor]
                            (list 'criterium.measured/expr
                                  '(alength xs))))]
          (is (= 3 (second (measured/invoke
                            subject (measured/args subject) 1)))))))))
