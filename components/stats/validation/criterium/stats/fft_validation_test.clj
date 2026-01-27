(ns criterium.stats.fft-validation-test
  "Validation tests for criterium.stats.fft functions against R's fft().

  Tests compare our FFT implementation against R as an independent reference.
  Tests skip gracefully when R/Rserve is unavailable."
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.r-validation.r :as r :refer [vec->r-str]]
   [criterium.stats.fft :as fft]
   [criterium.test.assert :refer [approx=]]))

;;; Test data sets
;; Fixed datasets of different sizes for reproducible validation

(def data-8 [1.0 2.3 -0.5 4.1 0.7 -1.2 3.8 2.0])

(def data-32
  [1.2 -0.3 2.1 0.8 -1.5 3.2 0.1 -2.4
   1.8 0.5 -0.9 2.7 -1.1 0.4 1.6 -0.7
   2.3 -1.8 0.2 1.4 -0.6 2.9 -0.1 1.1
   -1.3 0.9 2.5 -0.4 1.7 -2.1 0.6 1.9])

(def data-64
  [0.5 -1.2 2.3 0.1 -0.8 1.9 -0.4 2.7
   1.1 -1.6 0.3 2.1 -0.5 1.4 -1.0 0.8
   2.5 -0.2 1.7 -1.3 0.6 2.0 -0.9 1.5
   -0.1 2.8 -1.4 0.4 1.8 -0.6 2.2 -1.1
   0.7 1.3 -0.7 2.4 -1.5 0.2 1.6 -0.3
   2.6 -1.8 0.9 1.2 -0.4 2.9 -1.0 0.5
   1.4 -0.8 2.1 -0.2 1.0 -1.7 0.3 2.0
   -0.5 1.8 -1.2 0.6 2.3 -0.9 1.1 -1.4])

;;; Helper functions

(defn- extract-interleaved
  "Extract real and imaginary parts from interleaved complex array.
  Returns {:real [...] :imag [...]}."
  [^doubles arr]
  (let [n (quot (alength arr) 2)
        real (double-array n)
        imag (double-array n)]
    (dotimes [i n]
      (aset real i (aget arr (* 2 i)))
      (aset imag i (aget arr (inc (* 2 i)))))
    {:real (vec real) :imag (vec imag)}))

(defn- compare-fft-results
  "Compare criterium FFT result with R FFT result.
  Returns true if all real and imaginary components match within tolerance."
  [clj-result r-real r-imag tolerance]
  (let [{:keys [real imag]} (extract-interleaved clj-result)
        n (count real)]
    (and (= n (count r-real) (count r-imag))
         (every? true?
                 (for [i (range n)]
                   (and (approx= (nth real i) (nth r-real i) tolerance)
                        (approx= (nth imag i) (nth r-imag i) tolerance)))))))

;;; Tests

(deftest fft-validation-test
  ;; Validates criterium.stats.fft/fft against R's fft() function.
  ;; R's fft() computes the discrete Fourier transform.
  ;; Our implementation uses interleaved [re0 im0 re1 im1 ...] format.
  (testing "fft"
    (if-not (r/r-available?)
      (do
        (println "Skipping FFT validation: R/Rserve not available")
        (is true "Skipped - R unavailable"))
      (do
        (testing "with 8-sample real data"
          (let [r-str (vec->r-str data-8)
                r-real (r/r-eval (str "Re(fft(" r-str "))"))
                r-imag (r/r-eval (str "Im(fft(" r-str "))"))
                clj-result (fft/fft (fft/real->complex (double-array data-8)))]
            (is (compare-fft-results clj-result r-real r-imag 1e-10)
                (format "FFT mismatch for 8-sample data\nR real: %s\nR imag: %s\nClj: %s"
                        (vec r-real) (vec r-imag) (extract-interleaved clj-result)))))

        (testing "with 32-sample real data"
          (let [r-str (vec->r-str data-32)
                r-real (r/r-eval (str "Re(fft(" r-str "))"))
                r-imag (r/r-eval (str "Im(fft(" r-str "))"))
                clj-result (fft/fft (fft/real->complex (double-array data-32)))]
            (is (compare-fft-results clj-result r-real r-imag 1e-10)
                (format "FFT mismatch for 32-sample data\nR real: %s\nR imag: %s\nClj: %s"
                        (vec r-real) (vec r-imag) (extract-interleaved clj-result)))))

        (testing "with 64-sample real data"
          (let [r-str (vec->r-str data-64)
                r-real (r/r-eval (str "Re(fft(" r-str "))"))
                r-imag (r/r-eval (str "Im(fft(" r-str "))"))
                clj-result (fft/fft (fft/real->complex (double-array data-64)))]
            (is (compare-fft-results clj-result r-real r-imag 1e-10)
                (format "FFT mismatch for 64-sample data\nR real: %s\nR imag: %s\nClj: %s"
                        (vec r-real) (vec r-imag) (extract-interleaved clj-result)))))))))
