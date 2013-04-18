(ns criterium.implementation
  (:use
   perforate.core
   criterium.implementation.array
   criterium.implementation.type))

(defgoal implementation-comparison
  "Speed of implementation choices")

;;; A function to test timings

(defmacro test-case [n]
  `(loop [n# (int ~n)
          r# (int 0)]
     (if (zero? n#)
       r#
       (recur (unchecked-dec n#) (unchecked-inc r#)))))

(defcase implementation-comparison "0" [] (test-case 0))
(defcase implementation-comparison "1" [] (test-case 1))
(defcase implementation-comparison "2" [] (test-case 2))
(defcase implementation-comparison "3" [] (test-case 3))
(defcase implementation-comparison "10" [] (test-case 10))
(defcase implementation-comparison "30" [] (test-case 30))
(defcase implementation-comparison "100" [] (test-case 100))
(defcase implementation-comparison "300" [] (test-case 300))
(defcase implementation-comparison "1000" [] (test-case 1000))
(defcase implementation-comparison "3000" [] (test-case 3000))
(defcase implementation-comparison "10000" [] (test-case 10000))
(defcase implementation-comparison "100000" [] (test-case 100000))
