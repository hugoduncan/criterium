(ns criterium.measured-array-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.measured :as measured]
   [criterium.measured.impl :as impl]
   [criterium.measured-test :as measured-test]))

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
          (doseq [n [1 17]]
            (is (= 3 (second (measured/invoke
                              subject (measured/args subject) n))))))))))

(def array-tags
  (into (mapv first array-cases)
        '[booleans bytes shorts chars ints longs floats doubles objects]))

(deftest array-reference-structure-test
  ;; Generated forms establish transport, independently of result correctness.
  (testing "each array uses one reference cell with its exact read hint"
    (doseq [tag array-tags
            timer [nil 'time-fn]]
      (testing (str tag " timer " timer)
        (is (= :reference (impl/cell-kind tag)))
        (is (impl/opaque-tag? tag))
        (let [form (#'measured-test/expr-fn-form tag timer)
              bindings (#'measured-test/outer-let-bindings form)
              ctor (#'measured-test/binding-value-index
                    bindings #'measured-test/cell-ctor?)
              escape (#'measured-test/binding-value-index
                      bindings #'measured-test/cell-escape?)
              start (#'measured-test/binding-value-index
                     bindings #'measured-test/start-timestamp?)
              {:keys [init read]} (impl/opaque-cell 'a {:tag tag})]
          (#'measured-test/assert-opaque-structure form)
          (is (= '(java.util.concurrent.atomic.AtomicReference a)
                 (rest (second init))) "constructor stores the original reference")
          (is (= {:tag tag} (meta (first read))))
          (is (= 1 (#'measured-test/count-matching
                    #'measured-test/cell-escape? form)))
          (is (zero? (#'measured-test/count-matching
                      #'measured-test/cell-escape?
                      (#'measured-test/loop-form form))))
          (is (and ctor escape start (< ctor escape start)))
          (is (= (#'measured-test/cell-syms bindings)
                 (into #{} (comp (filter #'measured-test/cell-escape?)
                                 (map second))
                       (#'measured-test/form-seq form)))))))))

(deftest array-reference-identity-test
  ;; Real opaque reads must preserve the reference and its contents.
  (testing "single and repeated evaluations preserve arrays"
    (doseq [[descriptor constructor] array-cases]
      (testing descriptor
        (let [xs (eval constructor)
              before (vec xs)
              subject (measured/expr (identity xs))]
          (doseq [n [1 17]]
            (is (identical? xs (second (measured/invoke
                                       subject (measured/args subject) n))))
            (is (= before (vec xs))))))))
  (testing "mixed scalar and array transport preserves identity and contents"
    (let [xs (long-array [1 2 3])
          index 1
          subject (measured/expr (do (aget xs index) xs))]
      (doseq [n [1 17]]
        (is (identical? xs (second (measured/invoke
                                   subject (measured/args subject) n))))
        (is (= [1 2 3] (vec xs)))))))

(deftest array-public-compiler-test
  ;; Compile real public expressions while warnings are captured, then invoke
  ;; repeatedly to cover both the initial and loop opaque-read sites.
  (testing "array operations compile without reflection and retain results"
    (doseq [[descriptor constructor] array-cases
            operation (cond-> ['(alength xs)]
                        (contains? #{"[Z" "[B" "[S" "[C" "[I" "[J" "[F" "[D"}
                                   descriptor)
                        (conj '(aget xs 0)))]
      (testing (str descriptor " " operation)
        (let [warnings (java.io.StringWriter.)
              subject (binding [*warn-on-reflection* true
                                *err* warnings]
                        (eval (list 'let [(with-meta 'xs {:tag descriptor}) constructor]
                                    (list 'criterium.measured/expr operation))))
              expected (eval (list 'let ['xs constructor] operation))]
          (is (= "" (str warnings)))
          (doseq [n [1 17]]
            (is (= expected (second (measured/invoke
                                     subject (measured/args subject) n))))))))))

(deftest array-java-parameter-test
  ;; Arrays/equals must resolve its exact primitive or reference overload.
  (testing "statically typed Java array parameters compile without reflection"
    (doseq [constructor ['(long-array [1 2 3])
                         '(double-array [1 2 3])
                         '(into-array String ["a" "b"])]]
      (let [warnings (java.io.StringWriter.)
            subject (binding [*warn-on-reflection* true
                              *err* warnings]
                      (eval (list 'let [(with-meta 'xs {:tag (.getName (class (eval constructor)))}) constructor
                                       (with-meta 'ys {:tag (.getName (class (eval constructor)))}) constructor]
                                  (list 'criterium.measured/expr
                                        '(java.util.Arrays/equals xs ys)))))]
        (is (= "" (str warnings)))
        (doseq [n [1 17]]
          (is (true? (second (measured/invoke
                              subject (measured/args subject) n)))))))))
