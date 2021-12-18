(ns criterium.data.air-quality
  "Mean ozone concentration (in parts per billion) in the air from 13:00 to
  15:00 hours at Roosevelt Island, New York City, from May 1, 1973 to September
  30, 1973. it was obtained from the New York State Department of Conservation.

  Source from airquality data set built into R.

  data(airquality)
  airquality[c(\"Ozone\")]
  ")

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def ozone
  [41 36 12 18 28 23 19 8 7 16 11 14 18 14 34 6 30 11 1 11 4 32 23 45 115 37
   29 71 39 23 21 37 20 12 13 135 49 32 64 40 77 97 97 85 10 27 7 48 35 61 79
   63 16 80 108 20 52 82 50 64 59 39 9 16 78 35 66 122 89 110 44 28 65 22 59
   23 31 44 21 9 45 168 73 76 118 84 85 96 78 73 91 47 32 20 23 21 24 44 21
   28 9 13 46 18 13 24 16 13 23 36 7 14 30 14 18 20])
