(ns criterium.viewer.print.modal-test
  ;; Tests the print viewer output for multimodal-warning results.
  ;; Verifies warning display only when n-modes > 1, with aligned output format.
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.collect-plan :as collect-plan]
   [criterium.collector.metrics :as metrics]
   [criterium.test-utils :refer [trimmed-lines]]
   [criterium.view :as view]
   [criterium.viewer.print.modal]))

(deftest multimodal-warning-print-test
  (testing "multimodal-warning*"
    (testing "displays warning when n-modes > 1"
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            modes-data {:type :criterium/modes
                        :metrics-defs metrics-defs
                        :transform collect-plan/identity-transforms
                        :modes {[:elapsed-time]
                                {:modes [{:location 100.0
                                          :density 0.3
                                          :significant? true}
                                         {:location 200.0
                                          :density 0.25
                                          :significant? true}]
                                 :n-modes 2}}}
            output (with-out-str
                     (view/multimodal-warning* :print {} {:modes modes-data}))
            lines (trimmed-lines output)]
        (is (= ["Elapsed Time: Multimodal distribution detected"
                "Mode locations: 100 ns, 200 ns"]
               lines))
        (is
         (str/includes?
          output
          "                                  Mode locations:")
         "Mode locations should be indented to align with label")))

    (testing "does not display when n-modes = 1"
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            modes-data {:type :criterium/modes
                        :metrics-defs metrics-defs
                        :transform collect-plan/identity-transforms
                        :modes {[:elapsed-time]
                                {:modes [{:location 100.0
                                          :density 0.5}]
                                 :n-modes 1}}}
            output (with-out-str
                     (view/multimodal-warning* :print {} {:modes modes-data}))]
        (is (str/blank? output))))

    (testing "does not display when modes data is missing"
      (let [output (with-out-str
                     (view/multimodal-warning* :print {} {}))]
        (is (str/blank? output))))

    (testing "uses custom modes-id"
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            modes-data {:type :criterium/modes
                        :metrics-defs metrics-defs
                        :transform collect-plan/identity-transforms
                        :modes {[:elapsed-time]
                                {:modes [{:location 50.0
                                          :density 0.4}
                                         {:location 150.0
                                          :density 0.35}]
                                 :n-modes 2}}}
            output (with-out-str
                     (view/multimodal-warning* :print {:modes-id :my-modes}
                                               {:my-modes modes-data}))
            lines (trimmed-lines output)]
        (is (= ["Elapsed Time: Multimodal distribution detected"
                "Mode locations: 50.0 ns, 150 ns"]
               lines))
        (is
         (str/includes?
          output
          "                                  Mode locations:"))))))
