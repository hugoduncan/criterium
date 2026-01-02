(ns r-validation.r-test
  "Tests for the R connection helper.

  Verifies the R connection helper handles both available and unavailable cases."
  (:require
   [clojure.test :refer [deftest is testing]]
   [r-validation.r :as r]))

(deftest r-connection-test
  ;; Verifies the R connection helper handles both available and unavailable cases.
  (testing "r-connection"
    (testing "reports availability status"
      (let [available? (r/r-available?)]
        (is (boolean? available?)
            "r-available? should return a boolean")
        (when-not available?
          (println "R/Rserve not available:" (r/unavailable-reason-msg)))))))
