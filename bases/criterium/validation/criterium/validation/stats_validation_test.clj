(ns criterium.validation.stats-validation-test
  "Validation tests for criterium.util.stats against R reference implementations.

  Tests skip gracefully when R/Rserve is unavailable."
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.validation.r :as r]))

;; Placeholder test to verify the validation infrastructure works.
;; Subsequent tasks will add actual validation tests.

(deftest r-connection-test
  ;; Verifies the R connection helper handles both available and unavailable cases.
  (testing "r-connection"
    (testing "reports availability status"
      (let [available? (r/r-available?)]
        (is (boolean? available?)
            "r-available? should return a boolean")
        (when-not available?
          (println "R/Rserve not available:" (r/unavailable-reason-msg)))))))
