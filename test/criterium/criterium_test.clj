(ns criterium.criterium-test
  (:use clojure.test
	criterium)
  (:require criterium.stats
	    criterium.well))



(deftest outliers-test
  (is (= (outlier-count 0 0 0 0)
	 (outliers [1 2 5 7 8])))
  (is (= (outlier-count 0 0 0 0)
	 (outliers [1 2 2 5 7 8])))
  (is (= (outlier-count 1 0 0 0)
	 (outliers [-100 1 2 5 7 8 9])))
  (is (= (outlier-count 0 1 0 0)
	 (outliers [-10 1 2 5 7 8 9])))
  (is (= (outlier-count 0 0 1 0)
	 (outliers [1 1 2 5 7 8 22])))
  (is (= (outlier-count 0 0 0 1)
	 (outliers [1 1 2 5 7 8 100]))))

(deftest outlier-effect-test
  (is (= :unaffected (outlier-effect 0.009)))
  (is (= :slight (outlier-effect 0.09)))
  (is (= :moderate (outlier-effect 0.49)))
  (is (= :severe (outlier-effect 0.51))))

(deftest bootstrap-test
  (is (= [1 0 [1.0 1.0]]
	 (bootstrap (take 20 (repeatedly (constantly 1)))
		    criterium.stats/mean
		    100
		    criterium.well/well-rng-1024a)))
  (is (=  [ [1 0 [1.0 1.0]] [0 0 [0.0 0.0]]]
	  (bootstrap (take 20 (repeatedly (constantly 1)))
		     (juxt criterium.stats/mean criterium.stats/variance)
		     100
		     criterium.well/well-rng-1024a))))


