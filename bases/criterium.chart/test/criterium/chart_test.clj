(ns criterium.chart-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]
   [criterium.chart :as chart]
   [criterium.view :as view]))


#_(deftest histogram-test
    (testing "histogram"
      (testing "is created by report"
        (io/delete-file "abc.svg" :silent)
        (-> {:metrics [:elapsed-time]
             :sampled {:samples      [{:elapsed-time 1}
                                      {:elapsed-time 1}
                                      {:elapsed-time 3}
                                      {:elapsed-time 1000}]
                       :batch-size   1
                       :eval-count   1
                       :elapsed-time 1}
             :config
             (-> (merge
                  config/default-config
                  {:analysis [{:analysis-type :stats}]
                   :report   [{:report-type :histogram
                               :file        "abc.svg"}]})
                 (config/ensure-full-analysis-config)
                 (config/ensure-full-report-config))}
            analyse/analyse
            view/report)
        (is (.exists (io/file "abc.svg"))))))
