(ns optimisation.interface
  "Public API for the optimisation component.

  Provides numerical optimisation algorithms:
  - Linear regression for fitting linear models to data"
  (:require
   [optimisation.regression :as regression]))

(def linear-regression
  "Perform simple linear regression: y = a0 + a1*x.

  Returns a map with:
  - :coeffs [a0 a1] - intercept and slope
  - :variance - residual variance (MSE with n-2 degrees of freedom)
  - :r-sqr - coefficient of determination (R-squared)"
  regression/linear-regression)

(def sum-square-delta
  "Sum of squared differences from a mean value."
  regression/sum-square-delta)
