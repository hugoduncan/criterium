(ns optimisation.interface
  "Public API for the optimisation component.

  Provides numerical optimisation algorithms:
  - Linear regression for fitting linear models to data"
  (:require
   [optimisation.regression :as regression]))

(defn linear-regression
  "Perform simple linear regression: y = a0 + a1*x.

  Returns a map with:
  - :coeffs [a0 a1] - intercept and slope
  - :variance - residual variance (MSE with n-2 degrees of freedom)
  - :r-sqr - coefficient of determination (R-squared)"
  [xs ys]
  (regression/linear-regression xs ys))

(defn sum-square-delta
  "Sum of squared differences from a mean value."
  ^double [vs ^double mv]
  (regression/sum-square-delta vs mv))
