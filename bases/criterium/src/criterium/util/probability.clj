(ns criterium.util.probability
  "Probability functions including log-gamma and error function approximations.

  Re-exports from criterium.stats.interface for backward compatibility."
  (:require
   [criterium.stats.interface :as stats]))

(def polynomial-value
  "Evaluate a polynomial at the given value x, for the coefficients given in
  descending order (so the last element of coefficients is the constant term)."
  stats/polynomial-value)

(def log-gamma
  "Compute the natural logarithm of the gamma function using Lanczos approximation.
  Returns ln(Γ(x)) for x > 0.

  Uses the Lanczos approximation with g=7 and 9 coefficients, providing
  approximately 15 digits of precision. Matches R's lgamma() behavior."
  stats/log-gamma)

(def erf
  "erf polynomial approximation.  Maximum error is 1.5e-7.
  Handbook of Mathematical Functions: with Formulas, Graphs, and Mathematical
  Tables. Milton Abramowitz (Editor), Irene A. Stegun (Editor), 7.1.26"
  stats/erf)

(def normal-cdf
  "Probability p(X<x), for a normal distrubtion.  Uses the polynomial erf
  approximation above, and so is not super accurate."
  stats/normal-cdf)

(def normal-pdf
  "Probability density function for the normal distribution."
  stats/normal-pdf)

(def normal-quantile
  "Normal quantile function. Given a quantile in (0,1), return the normal value
  for that quantile.

  Wichura, MJ. 'Algorithm AS241' The Percentage Points of the Normal
  Distribution. Applied Statistics, 37, 477-484 "
  stats/normal-quantile)
