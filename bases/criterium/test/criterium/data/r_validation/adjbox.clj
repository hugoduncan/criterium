(ns criterium.data.r-validation.adjbox
  "Reference values for adjusted boxplot validation.

  Expected fence values computed using the adjusted boxplot formula
  from Hubert & Vandervieren (2008) with coefficients a=-4, b=3.

  The formula produces asymmetric fences based on medcouple:
  - When mc >= 0: lower = q1 - c*exp(a*mc)*iqr, upper = q3 + c*exp(b*mc)*iqr
  - When mc < 0:  lower = q1 - c*exp(-b*mc)*iqr, upper = q3 + c*exp(-a*mc)*iqr

  Using criterium's quartile calculation method for consistency.")

(def test-cases
  "Test cases with sorted data and expected adjusted boxplot fence values.
  Each entry contains:
  - :data - sorted input vector
  - :q1, :q3 - quartiles from criterium.stats.interface/quartiles
  - :mc - medcouple value
  - :expected - [low-severe low-mild high-mild high-severe]
  - :description - test case description"
  [{:data        [1 2 3 4 5 6 7 8 9]
    :q1          2.5
    :q3          7.5
    :mc          0.0
    :expected    [-12.5 -5.0 15.0 22.5]
    :description "Symmetric - same as standard boxplot"}

   {:data        [1 2 2 3 3 3 4 4 5 10 15 20]
    :q1          2.5
    :q3          7.5
    :mc          0.5
    :expected    [0.4699707514508096
                  1.4849853757254048
                  41.112668027535484
                  74.72533605507097]
    :description "Right-skewed - narrower lower, wider upper"}

   {:data        [-20 -15 -10 -5 -4 -4 -3 -3 -3 -2 -2 -1]
    :q1          -7.5
    :q3          -2.5
    :mc          -0.5
    :expected    [-74.72533605507097
                  -41.112668027535484
                  -1.4849853757254048
                  -0.4699707514508096]
    :description "Left-skewed - wider lower, narrower upper"}

   {:data        [1 2 3 3 3 3 3 4 5]
    :q1          2.5
    :q3          3.5
    :mc          0.0
    :expected    [-0.5 1.0 5.0 6.5]
    :description "With ties at median - same as standard boxplot"}])

(def ozone-data
  "Ozone concentration data - right-skewed real-world example."
  {:data     (vec (sort [41 36 12 18 28 23 19 8 7 16 11 14 18 14 34 6 30 11 1
                         11 4 32 23 45 115 37 29 71 39 23 21 37 20 12 13 135
                         49 32 64 40 77 97 97 85 10 27 7 48 35 61 79 63 16 80
                         108 20 52 82 50 64 59 39 9 16 78 35 66 122 89 110 44
                         28 65 22 59 23 31 44 21 9 45 168 73 76 118 84 85 96
                         78 73 91 47 32 20 23 21 24 44 21 28 9 13 46 18 13 24
                         16 13 23 36 7 14 30 14 18 20]))
   :q1       18.0
   :q3       63.5
   :mc       0.3717948717948718
   :expected [-12.850258434436384
              2.574870782781808
              271.7130947903503
              479.9261895807005]
   :description "Ozone data - right-skewed real-world example"})
