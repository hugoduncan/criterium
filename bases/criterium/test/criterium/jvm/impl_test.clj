(ns criterium.jvm.impl-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.jvm.impl :as impl]))

(defn thread-allocated-bytes-overhead
  ^long []
  (unchecked-subtract
   (impl/thread-allocated-bytes (impl/current-thread-id))
   (impl/thread-allocated-bytes (impl/current-thread-id))))

(deftest zero-overhead-test
  (testing "thread-allocated-bytes has no memopry overhead"
    (is (= 0 (thread-allocated-bytes-overhead)))))
