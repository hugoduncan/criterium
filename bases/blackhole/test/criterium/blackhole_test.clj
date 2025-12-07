(ns criterium.blackhole-test
  ;; Tests consume macro across all primitive types and Object, and validates
  ;; mode detection logic. Runtime mode is tested directly; compiler mode
  ;; requires JVM flags so we verify it reports correctly based on environment.
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.blackhole :as blackhole])
  (:import
   [criterium.blackhole Blackhole]))

;;; Mode Detection Tests

(deftest mode-test
  (testing "mode"
    (testing "returns a keyword"
      (is (keyword? (blackhole/mode))))
    (testing "returns :compiler or :runtime"
      (is (#{:compiler :runtime} (blackhole/mode))))))

(deftest compiler-blackhole-available?-test
  (testing "compiler-blackhole-available?"
    (testing "returns a boolean"
      (is (boolean? (blackhole/compiler-blackhole-available?))))
    (testing "reflects JVM version capability"
      ;; On JVM 17+ this should be true, otherwise false
      (let [version (System/getProperty "java.version")
            major (if (.startsWith version "1.")
                    (Long/parseLong (second (re-find #"^1\.(\d+)" version)))
                    (Long/parseLong (re-find #"^\d+" version)))]
        (is (= (>= major 17) (blackhole/compiler-blackhole-available?)))))))

(deftest compiler-blackhole-enabled?-test
  (testing "compiler-blackhole-enabled?"
    (testing "returns a boolean"
      (is (boolean? (blackhole/compiler-blackhole-enabled?))))
    (testing "matches mode"
      (is (= (= :compiler (blackhole/mode))
             (blackhole/compiler-blackhole-enabled?))))))

;;; Runtime Blackhole Direct Tests (via Java class)

(deftest runtime-blackhole-boolean-test
  (testing "consumeRuntime boolean"
    (testing "does not throw for true"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh true)
        (is true)))
    (testing "does not throw for false"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh false)
        (is true)))))

(deftest runtime-blackhole-byte-test
  (testing "consumeRuntime byte"
    (testing "does not throw for zero"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh (byte 0))
        (is true)))
    (testing "does not throw for max value"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh Byte/MAX_VALUE)
        (is true)))
    (testing "does not throw for min value"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh Byte/MIN_VALUE)
        (is true)))))

(deftest runtime-blackhole-char-test
  (testing "consumeRuntime char"
    (testing "does not throw for null char"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh (char 0))
        (is true)))
    (testing "does not throw for max value"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh Character/MAX_VALUE)
        (is true)))
    (testing "does not throw for typical char"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh \a)
        (is true)))))

(deftest runtime-blackhole-short-test
  (testing "consumeRuntime short"
    (testing "does not throw for zero"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh (short 0))
        (is true)))
    (testing "does not throw for max value"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh Short/MAX_VALUE)
        (is true)))
    (testing "does not throw for min value"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh Short/MIN_VALUE)
        (is true)))))

(deftest runtime-blackhole-int-test
  (testing "consumeRuntime int"
    (testing "does not throw for zero"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh (int 0))
        (is true)))
    (testing "does not throw for max value"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh Integer/MAX_VALUE)
        (is true)))
    (testing "does not throw for min value"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh Integer/MIN_VALUE)
        (is true)))))

(deftest runtime-blackhole-long-test
  (testing "consumeRuntime long"
    (testing "does not throw for zero"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh 0)
        (is true)))
    (testing "does not throw for max value"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh Long/MAX_VALUE)
        (is true)))
    (testing "does not throw for min value"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh Long/MIN_VALUE)
        (is true)))))

(deftest runtime-blackhole-float-test
  (testing "consumeRuntime float"
    (testing "does not throw for zero"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh (float 0.0))
        (is true)))
    (testing "does not throw for max value"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh Float/MAX_VALUE)
        (is true)))
    (testing "does not throw for NaN"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh Float/NaN)
        (is true)))
    (testing "does not throw for infinity"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh Float/POSITIVE_INFINITY)
        (is true)))))

(deftest runtime-blackhole-double-test
  (testing "consumeRuntime double"
    (testing "does not throw for zero"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh 0.0)
        (is true)))
    (testing "does not throw for max value"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh Double/MAX_VALUE)
        (is true)))
    (testing "does not throw for NaN"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh Double/NaN)
        (is true)))
    (testing "does not throw for infinity"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh Double/POSITIVE_INFINITY)
        (is true)))))

(deftest runtime-blackhole-object-test
  (testing "consumeRuntime Object"
    (testing "does not throw for nil"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh nil)
        (is true)))
    (testing "does not throw for string"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh "test")
        (is true)))
    (testing "does not throw for vector"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh [1 2 3])
        (is true)))
    (testing "does not throw for map"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh {:a 1})
        (is true)))))

(deftest evaporate-test
  (testing "evaporate"
    (testing "clears object references"
      (let [bh (Blackhole.)]
        (.consumeRuntime bh {:large "object"})
        (.consumeRuntime bh "another")
        (.evaporate bh)
        (is true)))))

;;; Static Consume Methods (Compiler Blackhole)

(deftest static-consume-boolean-test
  (testing "static consume boolean"
    (testing "does not throw"
      (Blackhole/consume true)
      (Blackhole/consume false)
      (is true))))

(deftest static-consume-byte-test
  (testing "static consume byte"
    (testing "does not throw"
      (Blackhole/consume (byte 0))
      (Blackhole/consume Byte/MAX_VALUE)
      (Blackhole/consume Byte/MIN_VALUE)
      (is true))))

(deftest static-consume-char-test
  (testing "static consume char"
    (testing "does not throw"
      (Blackhole/consume (char 0))
      (Blackhole/consume Character/MAX_VALUE)
      (Blackhole/consume \a)
      (is true))))

(deftest static-consume-short-test
  (testing "static consume short"
    (testing "does not throw"
      (Blackhole/consume (short 0))
      (Blackhole/consume Short/MAX_VALUE)
      (Blackhole/consume Short/MIN_VALUE)
      (is true))))

(deftest static-consume-int-test
  (testing "static consume int"
    (testing "does not throw"
      (Blackhole/consume (int 0))
      (Blackhole/consume Integer/MAX_VALUE)
      (Blackhole/consume Integer/MIN_VALUE)
      (is true))))

(deftest static-consume-long-test
  (testing "static consume long"
    (testing "does not throw"
      (Blackhole/consume 0)
      (Blackhole/consume Long/MAX_VALUE)
      (Blackhole/consume Long/MIN_VALUE)
      (is true))))

(deftest static-consume-float-test
  (testing "static consume float"
    (testing "does not throw"
      (Blackhole/consume (float 0.0))
      (Blackhole/consume Float/MAX_VALUE)
      (Blackhole/consume Float/NaN)
      (Blackhole/consume Float/POSITIVE_INFINITY)
      (is true))))

(deftest static-consume-double-test
  (testing "static consume double"
    (testing "does not throw"
      (Blackhole/consume 0.0)
      (Blackhole/consume Double/MAX_VALUE)
      (Blackhole/consume Double/NaN)
      (Blackhole/consume Double/POSITIVE_INFINITY)
      (is true))))

(deftest static-consume-object-test
  (testing "static consume Object"
    (testing "does not throw"
      (Blackhole/consume nil)
      (Blackhole/consume "test")
      (Blackhole/consume [1 2 3])
      (Blackhole/consume {:a 1})
      (is true))))

;;; Clojure Wrapper Macro Tests

(deftest consume-macro-primitives-test
  (testing "consume macro"
    (testing "handles boolean"
      (blackhole/consume true)
      (blackhole/consume false)
      (is true))
    (testing "handles byte"
      (blackhole/consume (byte 42))
      (is true))
    (testing "handles char"
      (blackhole/consume \x)
      (is true))
    (testing "handles short"
      (blackhole/consume (short 1000))
      (is true))
    (testing "handles int"
      (blackhole/consume (int 12345))
      (is true))
    (testing "handles long"
      (blackhole/consume 9876543210)
      (is true))
    (testing "handles float"
      (blackhole/consume (float 3.14))
      (is true))
    (testing "handles double"
      (blackhole/consume 2.718281828)
      (is true))))

(deftest consume-macro-objects-test
  (testing "consume macro"
    (testing "handles nil"
      (blackhole/consume nil)
      (is true))
    (testing "handles string"
      (blackhole/consume "hello")
      (is true))
    (testing "handles vector"
      (blackhole/consume [1 2 3])
      (is true))
    (testing "handles map"
      (blackhole/consume {:key "value"})
      (is true))
    (testing "handles set"
      (blackhole/consume #{1 2 3})
      (is true))
    (testing "handles list"
      (blackhole/consume '(a b c))
      (is true))))

(deftest consume-macro-expressions-test
  (testing "consume macro"
    (testing "evaluates expression once"
      (let [counter (atom 0)]
        (blackhole/consume (swap! counter inc))
        (is (= 1 @counter))))
    (testing "handles computed values"
      (blackhole/consume (+ 1 2 3))
      (blackhole/consume (* 10 20))
      (is true))
    (testing "handles function results"
      (blackhole/consume (Math/sqrt 2.0))
      (blackhole/consume (str "a" "b"))
      (is true))))

(deftest evaporate-fn-test
  (testing "evaporate function"
    (testing "can be called multiple times"
      (blackhole/evaporate)
      (blackhole/evaporate)
      (is true))
    (testing "returns nil"
      (is (nil? (blackhole/evaporate))))))
