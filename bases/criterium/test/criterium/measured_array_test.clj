(ns criterium.measured-array-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.measured :as measured]
   [criterium.measured-test :as measured-test]
   [criterium.measured.impl :as impl]))

(deftype ArrayComponent [])

(defn- array-snapshot
  ;; Copy nested array values so later element changes cannot alter the snapshot.
  [value]
  (if (and value (.isArray (class value)))
    (mapv array-snapshot value)
    value))

(def array-cases
  ;; Explicit descriptors keep the expected contract independent of tag-meta.
  [{:descriptor "[Z"
    :constructor '(boolean-array [true false true])
    :alias 'booleans
    :non-local-var 'non-local-booleans
    :inferred-local? true}
   {:descriptor "[B"
    :constructor '(byte-array [1 2 3])
    :alias 'bytes
    :non-local-var 'non-local-bytes
    :inferred-local? true}
   {:descriptor "[S"
    :constructor '(short-array [4 5 6])
    :alias 'shorts
    :non-local-var 'non-local-shorts
    :inferred-local? true}
   {:descriptor "[C"
    :constructor '(char-array [\a \b \c])
    :alias 'chars
    :non-local-var 'non-local-chars
    :inferred-local? true}
   {:descriptor "[I"
    :constructor '(int-array [7 8 9])
    :alias 'ints
    :non-local-var 'non-local-ints
    :inferred-local? true}
   {:descriptor "[J"
    :constructor '(long-array [1 2 3])
    :alias 'longs
    :non-local-var 'non-local-longs
    :inferred-local? true}
   {:descriptor "[F"
    :constructor '(float-array [1.25 2.5 3.75])
    :alias 'floats
    :non-local-var 'non-local-floats
    :inferred-local? true}
   {:descriptor "[D"
    :constructor '(double-array [1.5 2.5 3.5])
    :alias 'doubles
    :non-local-var 'non-local-doubles
    :inferred-local? true}
   {:descriptor "[Ljava.lang.Object;"
    :constructor '(object-array ["a" 2 :three])
    :alias 'objects
    :non-local-var 'non-local-objects
    :inferred-local? true}
   {:descriptor "[Ljava.lang.String;"
    :constructor '(into-array String ["a" "b" "c"])
    :non-local-var 'non-local-strings
    :inferred-local? false}
   {:descriptor "[Lcriterium.measured_array_test.ArrayComponent;"
    :constructor '(into-array criterium.measured_array_test.ArrayComponent
                              [(criterium.measured_array_test.ArrayComponent.)
                               (criterium.measured_array_test.ArrayComponent.)
                               (criterium.measured_array_test.ArrayComponent.)])
    :non-local-var 'non-local-components
    :inferred-local? false}
   {:descriptor "[[J"
    :constructor '(into-array [(long-array [1 2])
                              (long-array [3 4])
                              (long-array [5 6])])
    :non-local-var 'non-local-long-matrix
    :inferred-local? false}
   {:descriptor "[[Ljava.lang.String;"
    :constructor '(into-array [(into-array String ["a" "b"])
                              (into-array String ["c" "d"])
                              (into-array String ["e" "f"])])
    :non-local-var 'non-local-string-matrix
    :inferred-local? false}])

(deftest array-tag-meta-test
  ;; Each inferred array hint retains its component class and dimensions.
  (testing "tag-meta returns exact JVM-name strings for arrays"
    (doseq [{:keys [descriptor constructor]} array-cases]
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
    (doseq [{:keys [descriptor constructor]} array-cases]
      (testing descriptor
        (let [subject (eval (list 'let ['xs constructor]
                            (list 'criterium.measured/expr
                                  '(alength xs))))]
          (doseq [n [1 17]]
            (is (= 3 (second (measured/invoke
                              subject (measured/args subject) n))))))))))

(def array-tags
  (into (mapv :descriptor array-cases)
        (keep :alias)
        array-cases))

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
    (doseq [{:keys [descriptor constructor]} array-cases]
      (testing descriptor
        (let [xs (eval constructor)
              before (array-snapshot xs)
              subject (measured/expr (identity xs))]
          (doseq [n [1 17]]
            (is (identical? xs (second (measured/invoke subject (measured/args subject) n))))
            (is (= before (array-snapshot xs))))))))
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
          offset 0.5
          time-fn (fn ^long [] (System/nanoTime))]
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
    (doseq [{:keys [descriptor constructor]} array-cases
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
    (doseq [{:keys [descriptor constructor alias]} array-cases
            tag (cond-> [descriptor] alias (conj alias))]
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

(def non-local-booleans (boolean-array [true false true]))
(def non-local-bytes (byte-array [1 2 3]))
(def non-local-shorts (short-array [4 5 6]))
(def non-local-chars (char-array [\a \b \c]))
(def non-local-ints (int-array [7 8 9]))
(def non-local-longs (long-array [1 2 3]))
(def non-local-floats (float-array [1.25 2.5 3.75]))
(def non-local-doubles (double-array [1.5 2.5 3.5]))
(def non-local-objects (object-array ["a" 2 :three]))
(def non-local-strings (into-array String ["a" "b" "c"]))
(def non-local-components (into-array ArrayComponent
                                      [(ArrayComponent.)
                                       (ArrayComponent.)
                                       (ArrayComponent.)]))
(def non-local-long-matrix (into-array [(long-array [1 2])
                                        (long-array [3 4])
                                        (long-array [5 6])]))
(def non-local-string-matrix (into-array [(into-array String ["a" "b"])
                                          (into-array String ["c" "d"])
                                          (into-array String ["e" "f"])]))

(deftest inferred-non-local-array-test
  ;; Unhinted Vars require evaluated type capture, not compiler local metadata.
  (testing "non-local arrays retain exact inferred hints and compile without reflection"
    (doseq [{:keys [descriptor non-local-var]} array-cases]
      (testing descriptor
        (let [array-sym (symbol "criterium.measured-array-test" (name non-local-var))
              xs (eval array-sym)
              before (array-snapshot xs)]
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
                  (is (= before (array-snapshot xs))))))))))))

(deftest inferred-local-array-warning-test
  ;; Constructor return hints supply local types without explicit user hints.
  ;; Dynamic make-array locals infer Object; their exact descriptors are covered
  ;; by the non-local and explicit-hint tests instead.
  (testing "inferred local array operations compile without reflection"
    (doseq [{:keys [descriptor constructor inferred-local?]} array-cases
            :when inferred-local?
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
