(ns criterium.data.r-validation.medcouple
  "Reference values for medcouple validation.

  Expected values computed using the naive O(n²) algorithm matching
  the definition from Brys, Hubert, and Struyf (2004).

  Note: R's robustbase::mc() uses additional refinements (reflection,
  huberization) that may produce slightly different values for edge cases.
  The naive algorithm values here are correct per the mathematical definition.")

(def test-cases
  "Test cases with sorted data and expected medcouple values.
  Each entry is {:data sorted-vector :expected medcouple-value :description string}"
  [{:data        [1 2 3 4 5 6 7 8 9]
    :expected    0.0
    :description "Symmetric data - medcouple should be zero"}

   {:data        [1 2 2 3 3 3 4 4 5 10 15 20]
    :expected    0.5
    :description "Right-skewed data - positive medcouple"}

   {:data        [-20 -15 -10 -5 -4 -4 -3 -3 -3 -2 -2 -1]
    :expected    -0.5
    :description "Left-skewed data - negative medcouple"}

   {:data        [1 2 3 3 3 3 3 4 5]
    :expected    0.0
    :description "Data with ties at median"}

   {:data        [5 5 5 5 5]
    :expected    0.0
    :description "Constant data"}

   {:data        [1 2 3]
    :expected    0.0
    :description "Minimal data (n=3)"}

   {:data        [1 2]
    :expected    0.0
    :description "Too few elements (n=2) - returns 0"}

   {:data        [-5 -3 -1 0 1]
    :expected    -0.3333333333333333
    :description "Mixed negative/positive, slightly left-skewed"}

   {:data        [1.5 2.3 3.1 4.7 5.2 6.8 7.4 8.9 9.1]
    :expected    0.02631578947368421
    :description "Floating point data, nearly symmetric"}])

(def ozone-data
  "Ozone concentration data from R's airquality dataset.
  Real-world example of right-skewed distribution."
  {:data     (vec (sort [41 36 12 18 28 23 19 8 7 16 11 14 18 14 34 6 30 11 1
                         11 4 32 23 45 115 37 29 71 39 23 21 37 20 12 13 135
                         49 32 64 40 77 97 97 85 10 27 7 48 35 61 79 63 16 80
                         108 20 52 82 50 64 59 39 9 16 78 35 66 122 89 110 44
                         28 65 22 59 23 31 44 21 9 45 168 73 76 118 84 85 96
                         78 73 91 47 32 20 23 21 24 44 21 28 9 13 46 18 13 24
                         16 13 23 36 7 14 30 14 18 20]))
   :expected 0.371794871794872
   :description "Ozone data - right-skewed real-world example"})
