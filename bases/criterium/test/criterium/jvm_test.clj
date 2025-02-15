(ns criterium.jvm-test
  "Tests for criterium.jvm - JVM monitoring and management interface.

  Tests cover four main categories:
  1. Time Management - timestamp, elapsed time, and wait functionality
  2. Memory Management - GC control and memory usage monitoring
  3. Thread Management - thread identification and resource tracking
  4. JMX Bean Access - JVM runtime metrics and statistics

  Testing approach:
  - Zero garbage validation for sampling functions
  - Thread safety verification where applicable
  - Return type and content validation
  - Edge case handling for critical functions

  Key test principles:
  - Use cleanup fixtures to ensure consistent test environment
  - Separate unit tests from allocation tracking tests
  - Verify both functionality and performance characteristics
  - Document expected behavior and edge cases"
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [criterium.agent :as agent]
   [criterium.jvm :as jvm]))

;; Test fixture for cleanup
(defn cleanup-fixture [f]
  (try
    (f)
    (finally
      (jvm/run-finalization-and-force-gc!))))

(use-fixtures :each cleanup-fixture)

(deftest elapsed-timestamp-test
  (let [ts (jvm/timestamp)]
    (is (zero? (jvm/elapsed-time ts ts)))
    (is (= 1 (jvm/elapsed-time ts (inc ts))))
    (is (= -1 (jvm/elapsed-time (inc ts) ts)))))

;;; Time Management Tests

(deftest wait-test
  (testing "wait delays for specified duration"
    (let [start   (jvm/timestamp)
          _       (jvm/wait 1000000) ; 1ms
          end     (jvm/timestamp)
          elapsed (jvm/elapsed-time start end)]
      (is (>= elapsed 1000000)
          "Should wait at least the specified time")
      (is (< elapsed 10000000)
          "Should not wait significantly longer than specified")))
  (testing "wait returns nil"
    (is (nil? (jvm/wait 1000))
        "Should return nil to avoid allocations")))

;;; Memory Management Tests

(deftest gc-control-test
  (testing "GC control functions execute without errors"
    (is (nil? (jvm/force-gc!)) "force-gc! should return nil")
    (is (nil? (jvm/run-finalization!)) "run-finalization! should return nil")
    (is (nil? (jvm/run-finalization-and-force-gc!))
        "run-finalization-and-force-gc! should return nil")))

;;; JMX Bean Tests

(deftest jmx-bean-tests
  (testing "JIT compilation info"
    (let [compile-time (jvm/compilation-sample)]
      (is (instance? Long compile-time))
      (when (pos? compile-time)
        (let [info (jvm/compilation compile-time)]
          (is (map? info)
              "Should return compilation info map")
          (is (:time-ms info)
              "Should contain compilation time")))))

  (testing "Memory pool operations"
    (let [pools (jvm/memory-pools)]
      (is (map? pools) "Should return memory pools map")
      (is (every? (fn [[k v]]
                    (and (keyword? k)
                         (= #{:committed :init :max :used}
                            (set (keys v)))))
                  pools)
          "Each pool should have type and usage info")))

  (testing "Garbage collector operations"
    (let [gc-names (jvm/garbage-collector-names)
          gc-info  (jvm/garbage-collector)]
      (is (seq gc-names) "Should return GC names")
      (is (map? gc-info) "Should return GC info map")
      (is (every? (fn [[k v]]
                    (and (keyword? k)
                         (number? (:count v))
                         (number? (:time-ms v))))
                  gc-info)
          "Each GC should have name, count and time")))

  (testing "Operating system info"
    (let [os (jvm/os-details)]
      (is (map? os) "Should return OS details map")
      (is (every? os [:name :version :arch :available-processors])
          "Should contain all OS details")))

  (testing "Runtime details"
    (let [rt (jvm/runtime-details)]
      (is (map? rt) "Should return runtime details map")
      (is (every? rt [:name
                      :vm-name :vm-vendor :vm-version
                      :java-version :java-runtime-version
                      :input-arguments])
          "Should contain all runtime details"))))

;;; Thread Management Tests

(deftest thread-management-test
  (testing "Current thread operations"
    (let [thread-id (jvm/current-thread-id)]
      (is (pos? thread-id) "Thread ID should be positive")

      (let [cpu-time (jvm/thread-cpu-time)]
        (is (or (= -1 cpu-time) (not (neg? cpu-time)))
            "CPU time should be -1 if unsupported or non-negative"))

      (let [allocated (jvm/thread-allocated-bytes)]
        (is (or (= -1 allocated) (not (neg? allocated)))
            "Allocated bytes should be -1 if unsupported or non-negative"))

      (let [sample (jvm/thread-sample)]
        (is sample "Thread sample should not be nil")
        (let [info (jvm/thread sample)]
          (is (map? info) "Thread info should be a map")))))

  (testing "Thread allocation change calculation"
    (is (= 100 (jvm/thread-allocated-change 900 1000))
        "Should calculate positive change correctly")
    (is (= -100 (jvm/thread-allocated-change 1000 900))
        "Should calculate negative change correctly")))

;;; Metric Collection Tests

(deftest metric-collection-test
  (testing "Single metric collection"
    (doseq [metric-key [:class-loader-counts :compilation
                        :garbage-collector :memory :memory-pools
                        :thread]]
      (testing (str "Collecting " metric-key)
        (let [metric (jvm/collect-metric metric-key)]
          (is (map? metric)
              (str metric-key " should return a map")))))))

;;; Garbege Free Tests

(def warmup-count 10)

(deftest zero-garbage-test
  (testing "Sampling is zero garbage"
    (testing "for class-loader-counts"
      ;; run once for function initialisation
      (dotimes [_ warmup-count]
        (agent/with-allocation-tracing (jvm/class-loader-counts)))
      (let [[allocations _res]    (agent/with-allocation-tracing
                                    (jvm/class-loader-counts))
            {:keys [freed-bytes]} (->> allocations
                                       (filterv (agent/allocation-on-thread?))
                                       agent/allocations-summary)]
        (is (zero? freed-bytes))))
    (testing "for compilation"
      (dotimes [_ warmup-count]
        (agent/with-allocation-tracing (jvm/compilation-sample)))
      (let [[allocations _res]    (agent/with-allocation-tracing
                                    (jvm/compilation-sample))
            {:keys [freed-bytes]} (->> allocations
                                       (filterv (agent/allocation-on-thread?))
                                       agent/allocations-summary)]
        (is (zero? freed-bytes)
            (->> allocations
                 (filterv (agent/allocation-on-thread?))
                 (filterv agent/allocation-freed?)))))
    (testing "for garbage-collectpr"
      (dotimes [_ warmup-count]
        (agent/with-allocation-tracing (jvm/garbage-collector-sample)))
      (let [[allocations _res]    (agent/with-allocation-tracing
                                    (jvm/garbage-collector-sample))
            {:keys [freed-bytes]} (->> allocations
                                       (filterv (agent/allocation-on-thread?))
                                       agent/allocations-summary)]
        (is (zero? freed-bytes)
            (->> allocations
                 (filterv (agent/allocation-on-thread?))
                 (filterv agent/allocation-freed?)))))
    (testing "for memory"
      (dotimes [_ warmup-count]
        (agent/with-allocation-tracing (jvm/memory-sample)))
      (let [[allocations _res]    (agent/with-allocation-tracing
                                    (jvm/memory-sample))
            {:keys [freed-bytes]} (->> allocations
                                       (filterv (agent/allocation-on-thread?))
                                       agent/allocations-summary)]
        (is (zero? freed-bytes)
            (->> allocations
                 (filterv (agent/allocation-on-thread?))
                 (filterv agent/allocation-freed?)))))
    (testing "for memory-pools"
      (dotimes [_ warmup-count]
        (agent/with-allocation-tracing (jvm/memory-pools-sample)))
      (let [[allocations _res]    (agent/with-allocation-tracing
                                    (jvm/memory-pools-sample))
            {:keys [freed-bytes]} (->> allocations
                                       (filterv (agent/allocation-on-thread?))
                                       agent/allocations-summary)]
        (is (zero? freed-bytes)
            (->> allocations
                 (filterv (agent/allocation-on-thread?))
                 (filterv agent/allocation-freed?)))))

    ;; unfortunately this can not be made garbage free
    (comment
      (testing "for thread"
        (let [[allocations _res]    (agent/with-allocation-tracing
                                      (jvm/thread-sample))
              {:keys [freed-bytes]} (agent/allocations-summary
                                     (filterv (agent/allocation-on-thread?) allocations))]
          (is (zero? freed-bytes)
              (mapv agent/allocation-freed?
                    (filterv (agent/allocation-on-thread?) allocations))))))))

(comment
  (deftest transient-persistent-overhead-test
    (is (= [48 {}]
           (let [x (transient {})]
             (jvm/allocated-bytes (persistent! x)))))))

;; (deftest allocation-test
;;   (testing "compilation-sample does not create garbage"
;;     (let [fc  (jvm/compilation-sample)
;;           _   (jvm/compilation-diff! fc)
;;           fc  (jvm/compilation!)
;;           x   (jvm/current-thread-allocated-bytes)
;;           fcd (jvm/compilation-diff! fc)
;;           d   (- (jvm/current-thread-allocated-bytes) x)]
;;       (is (= 0 d))
;;       (is fcd "keep a refernce until after diff test")))
;;   (testing "finalization! and finalization-diff! do not create garbage"
;;     (let [fc  (jvm/finalization!)
;;           _   (jvm/finalization-diff! fc)
;;           fc  (jvm/finalization!)
;;           x   (jvm/current-thread-allocated-bytes)
;;           fcd (jvm/finalization-diff! fc)
;;           d   (- (jvm/current-thread-allocated-bytes) x)]
;;       (is (= 0 d))
;;       (is fcd "keep a refernce until after diff test")))
;;   (testing "garbage-collector-raw functions do not create garbage"
;;     (let [fc  (jvm/garbage-collector-raw)
;;           x   (jvm/current-thread-allocated-bytes)
;;           _   (agent/with-allocation-tracing
;;                 (jvm/garbage-collector-diff-raw fc))
;;           d   (- (jvm/current-thread-allocated-bytes) x)
;;           res (jvm/garbage-collector-map-raw fc)]
;;       (is (map? res))
;;       (is (= 0 d) "no garbage object genetation")))
;;   ;; (testing "garbage-collector! and garbage-collector-diff! do not create garbage"
;;   ;;   (agent/with-allocation-tracing
;;   ;;     (let [fc  (jvm/garbage-collector!)
;;   ;;           _   (jvm/garbage-collector-diff! fc)
;;   ;;           fc  (jvm/garbage-collector!)
;;   ;;           x   (jvm/current-thread-allocated-bytes)
;;   ;;           fcd (jvm/garbage-collector-diff! fc)
;;   ;;           d   (- (jvm/current-thread-allocated-bytes) x)]
;;   ;;       (is (= 0 d))
;;   ;;       (is fcd "keep a refernce until after diff test"))))
;;   (testing "memory! and memory-diff! do not create garbage"
;;     (let [fc  (jvm/memory!)
;;           x   (jvm/current-thread-allocated-bytes)
;;           _   (agent/with-allocation-tracing
;;                 (jvm/memory-diff! fc))
;;           d   (- (jvm/current-thread-allocated-bytes) x)
;;           res (jvm/memory-diff-map fc)]
;;       (is (map? res))
;;       (is (= 0 d))))
;;   ;; (testing "memory! and memory-diff! do not create garbage"
;;   ;;   (let [fc  (jvm/memory!)
;;   ;;         _   (jvm/memory-diff! fc)
;;   ;;         fc  (jvm/memory!)
;;   ;;         x   (jvm/current-thread-allocated-bytes)
;;   ;;         fcd (jvm/memory-diff! fc)
;;   ;;         d   (- (jvm/current-thread-allocated-bytes) x)]
;;   ;;     (is (= 0 d))
;;   ;;     (is fcd "keep a refernce until after diff test")))
;;   )
