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
              cells (#'measured-test/cell-syms bindings)
              escape? (fn [x]
                        (and (seq? x)
                             (= 'criterium.util.blackhole/consume (first x))
                             (contains? cells (second x))))
              ctor (#'measured-test/binding-value-index
                    bindings #'measured-test/cell-ctor?)
              escape (#'measured-test/binding-value-index
                      bindings escape?)
              start (#'measured-test/binding-value-index
                     bindings #'measured-test/start-timestamp?)
              {:keys [init read]} (impl/opaque-cell 'a {:tag tag})]
          (#'measured-test/assert-opaque-structure form)
          (is (= '(java.util.concurrent.atomic.AtomicReference a)
                 (rest (second init))) "constructor stores the original reference")
          (is (= {:tag tag} (meta (first read))))
          (is (= 1 (#'measured-test/count-matching
                    escape? form)))
          (is (zero? (#'measured-test/count-matching
                      escape?
                      (#'measured-test/loop-form form))))
          (is (and ctor escape start (< ctor escape start)))
          (is (= (#'measured-test/cell-syms bindings)
                 (into #{} (comp (filter escape?)
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
            (is (identical? xs (second (measured/invoke subject (measured/args subject) n))))
            (is (= before (vec xs))))))))
  (testing "mixed scalar and array transport preserves identity and contents"
    (let [xs (long-array [1 2 3])
          index 1
          offset 0.5
          subject (measured/expr
                   (measured-test/mixed-array-result xs index offset))]
      (doseq [n [1 17]]
        (let [[result-xs result] (second (measured/invoke subject (measured/args subject) n))]
          (is (identical? xs result-xs))
          (is (== 2.5 result))
          (is (= [1 2 3] (vec xs))))))))

(defmacro mixed-capture
  [subject timer]
  (let [{:keys [expr arg-vals]} (impl/factor-expr subject &env)
        syms (vec (keys arg-vals))
        metas (impl/capture-arg-types
               syms arg-vals (impl/identify-local-args arg-vals &env) &env)]
    (list 'quote
          {:values (mapv arg-vals syms)
           :metas metas
           :form (impl/measured-expr-fn
                  syms expr {:arg-metas metas :time-fn timer})})))

(deftest mixed-array-structure-test
  ;; Inspect the same bare arguments used by the runtime subject, with real
  ;; compiler locals. Counts alone cannot establish which inputs were captured.
  (testing "array, long, and double inputs retain their own opaque cells"
    (let [xs (long-array [1 2 3])
          index 1
          offset 0.5]
      (doseq [{:keys [values metas form]}
              [(mixed-capture
                (measured-test/mixed-array-result xs index offset) nil)
               (mixed-capture
                (measured-test/mixed-array-result xs index offset) time-fn)]]
        (is (= {'xs {:tag "[J"} 'index {:tag 'long} 'offset {:tag 'double}}
               (zipmap values metas)))
        (is (= {'xs :reference 'index :long 'offset :double}
               (zipmap values (mapv #(impl/cell-kind (:tag %)) metas))))
        (let [bindings (#'measured-test/outer-let-bindings form)
              pairs (partition 2 bindings)
              cells (#'measured-test/cell-syms bindings)
              escape? (fn [x]
                        (and (seq? x)
                             (= 'criterium.util.blackhole/consume (first x))
                             (contains? cells (second x))))
              lp (#'measured-test/loop-form form)
              start (#'measured-test/binding-value-index
                     bindings #'measured-test/start-timestamp?)
              nodes (#'measured-test/form-seq form)]
          (is (= 3 (count cells)))
          (is (= 3 (#'measured-test/count-matching
                    #'measured-test/cell-ctor? form)))
          (is (= 3 (#'measured-test/count-matching escape? form)))
          (doseq [[cell ctor] pairs :when (contains? cells cell)]
            (let [ctor-index (#'measured-test/binding-value-index bindings #(= ctor %))
                  escape-index (#'measured-test/binding-value-index bindings #(and (escape? %) (= cell (second %))))]
              (is (and ctor-index escape-index start
                       (< ctor-index escape-index start)))
              (is (= 1 (count (filter #(and (escape? %)
                                            (= cell (second %))) nodes)))))
            (let [read? #(and (#'measured-test/get-opaque? %)
                              (= cell (second %)))]
              (is (= 2 (#'measured-test/count-matching read? form)))
              (is (= 1 (#'measured-test/count-matching read? lp)))))
          (is (zero? (#'measured-test/count-matching
                      #'measured-test/cell-ctor? lp)))
          (is (zero? (#'measured-test/count-matching escape? lp)))
          (is (zero? (#'measured-test/count-matching
                      #(and (symbol? %)
                            (re-find #"(?i)^\.?set" (name %)))
                      form))))))))

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

(deftest explicit-array-hint-test
  ;; Preserve user hints at capture and exercise the real opaque-read path.
  (testing "explicit aliases and descriptors retain metadata and array results"
    (doseq [[tag constructor]
            (into array-cases
                  (map vector
                       '[booleans bytes shorts chars ints longs floats doubles objects]
                       (map second (take 9 array-cases))))]
      (testing (str tag)
        (let [hinted-xs (with-meta 'xs {:tag tag})
              warnings (java.io.StringWriter.)
              subject (binding [*warn-on-reflection* true
                                *err* warnings]
                        (eval (list 'let ['xs constructor]
                                    (list 'criterium.measured/expr
                                          (list 'alength hinted-xs)))))]
          (is (= [{:tag tag}]
                 (impl/capture-arg-types
                  ['arg1] {'arg1 hinted-xs} #{'arg1} nil)))
          (is (= "" (str warnings)))
          (doseq [n [1 17]]
            (is (= 3 (second (measured/invoke
                              subject (measured/args subject) n))))))))))

(def non-local-booleans (boolean-array 3))
(def non-local-bytes (byte-array 3))
(def non-local-shorts (short-array 3))
(def non-local-chars (char-array 3))
(def non-local-ints (int-array 3))
(def non-local-longs (long-array 3))
(def non-local-floats (float-array 3))
(def non-local-doubles (double-array 3))
(def non-local-objects (object-array 3))
(def non-local-strings (make-array String 3))
(def non-local-components (make-array ArrayComponent 3))
(def non-local-long-matrix (make-array Long/TYPE 3 2))
(def non-local-string-matrix (make-array String 3 2))

(deftest inferred-non-local-array-test
  ;; Unhinted Vars require evaluated type capture, not compiler local metadata.
  (testing "non-local arrays retain exact inferred hints and compile without reflection"
    (doseq [[[descriptor _] var-name]
            (map vector array-cases
                 '[non-local-booleans non-local-bytes non-local-shorts
                   non-local-chars non-local-ints non-local-longs
                   non-local-floats non-local-doubles non-local-objects
                   non-local-strings non-local-components
                   non-local-long-matrix non-local-string-matrix])]
      (testing descriptor
        (let [array-sym (symbol "criterium.measured-array-test" (name var-name))
              xs (eval array-sym)
              before (vec xs)]
          (is (= [{:tag descriptor}]
                 (impl/capture-arg-types
                  ['arg1] {'arg1 array-sym} #{} nil)))
          (doseq [[operation expected]
                  (cond-> [[(list 'alength array-sym) 3]
                           [(list 'identity array-sym) xs]]
                    (contains? #{"[Z" "[B" "[S" "[C" "[I" "[J" "[F" "[D"}
                               descriptor)
                    (conj [(list 'aget array-sym 0) (first xs)]))]
            (let [warnings (java.io.StringWriter.)
                  subject (binding [*warn-on-reflection* true
                                    *err* warnings]
                            (eval (list 'criterium.measured/expr operation)))]
              (is (= "" (str warnings)))
              (doseq [n [1 17]]
                (let [result (second (measured/invoke
                                      subject (measured/args subject) n))]
                  (if (identical? expected xs)
                    (is (identical? xs result))
                    (is (= expected result)))
                  (is (= before (vec xs))))))))))))

(deftest inferred-local-array-warning-test
  ;; Constructor return hints supply local types without explicit user hints.
  ;; Dynamic make-array locals infer Object; their exact descriptors are covered
  ;; by the non-local and explicit-hint tests instead.
  (testing "inferred local array operations compile without reflection"
    (doseq [[descriptor constructor] (take 9 array-cases)
            operation (cond-> ['(alength xs)]
                        (not= descriptor "[Ljava.lang.Object;")
                        (conj '(aget xs 0)))]
      (testing (str descriptor " " operation)
        (let [warnings (java.io.StringWriter.)
              subject (binding [*warn-on-reflection* true
                                *err* warnings]
                        (eval (list 'let ['xs constructor]
                                    (list 'criterium.measured/expr operation))))
              expected (eval (list 'let ['xs constructor] operation))]
          (is (= "" (str warnings)))
          (doseq [n [1 17]]
            (is (= expected (second (measured/invoke
                                     subject (measured/args subject) n)))))))))
  (testing "inferred primitive arrays resolve Java array parameter overloads"
    (doseq [constructor ['(long-array [1 2 3])
                         '(double-array [1 2 3])]]
      (let [warnings (java.io.StringWriter.)
            subject (binding [*warn-on-reflection* true
                              *err* warnings]
                      (eval (list 'let ['xs constructor 'ys constructor]
                                  (list 'criterium.measured/expr
                                        '(java.util.Arrays/equals xs ys)))))]
        (is (= "" (str warnings)))
        (doseq [n [1 17]]
          (is (true? (second (measured/invoke
                              subject (measured/args subject) n)))))))))
