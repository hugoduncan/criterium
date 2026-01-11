(ns criterium.stats.interface
  "Public API for the stats component.

  Provides statistical functions including:
  - Core stats: min, max, mean, sum, variance, median, quartiles, quantile,
                skewness, kurtosis
  - Outlier detection: boxplot-outlier-thresholds
  - Sampling: uniform-distribution, sample-uniform, sample, confidence-interval
  - Probability: log-gamma, digamma, trigamma, erf, normal-cdf, normal-pdf, normal-quantile
  - Distributions: gamma, weibull, lognormal, inverse-gaussian (PDF and CDF)
  - Model selection: aic, bic, aicc (information criteria)
  - Goodness-of-fit tests: ks-test, cvm-test (Kolmogorov-Smirnov, Cramér-von Mises)
  - Moment matching: parameter estimation, distribution suitability prefilter
  - MLE fitting: gamma-mle, lognormal-mle, inverse-gaussian-mle, weibull-mle
  - Histogram: histogram (Freedman-Diaconis or Knuth Bayesian binning)
  - Knuth: optimal-bins, log-posterior (Bayesian histogram binning)
  - T-digest: streaming quantile estimation
  - Kernel: modal estimation, kernel density estimators
  - KDE: bandwidth selection, Gaussian KDE, mode detection, multimodality tests
  - Bootstrap: resampling, BCa confidence intervals, jacknife"
  (:refer-clojure :exclude [min max])
  (:require
   [criterium.stats.bootstrap :as bootstrap]
   [criterium.stats.core :as core]
   [criterium.stats.histogram :as histogram]
   [criterium.stats.kde :as kde]
   [criterium.stats.kernel :as kernel]
   [criterium.stats.knuth :as knuth]
   [criterium.stats.mle :as mle]
   [criterium.stats.moment-match :as moment-match]
   [criterium.stats.outliers :as outliers]
   [criterium.stats.probability :as probability]
   [criterium.stats.sampling :as sampling]
   [criterium.stats.t-digest :as t-digest]))

;;; Core statistics

(defn transpose
  "Transpose a vector of vectors."
  [data]
  (core/transpose data))

(defn min
  "Minimum value in data."
  ([data] (core/min data))
  ([data count] (core/min data count)))

(defn max
  "Maximum value in data."
  ([data] (core/max data))
  ([data count] (core/max data count)))

(defn unchecked-add-d
  "Unchecked double addition."
  ^double [^double a ^double b]
  (core/unchecked-add-d a b))

(defn mean
  "Arithmetic mean of data."
  ([data] (core/mean data))
  ([data count] (core/mean data count)))

(defn sum
  "Sum of each data point."
  [data]
  (core/sum data))

(defn sum-of-squares
  "Sum of the squares of each data point."
  [data]
  (core/sum-of-squares data))

(defn variance*
  "Variance based on subtracting mean."
  ^double [data ^double mean ^long df]
  (core/variance* data mean df))

(defn variance
  "Return the variance of data.

  By default returns the sample variance with (- (count data) 1) degrees
  of freedom.

  The population variance can be returned using (variance data 0), which uses
  (count data) degrees of freedom."
  ([data] (core/variance data))
  ([data df] (core/variance data df)))

(defn median-value
  "Calculate the median value of a sorted data set.
  Returns just the median value (not the lower/upper partitions).
  Accepts sequences and typed arrays (ITypedArray)."
  ^double [data]
  (core/median-value data))

(defn median
  "Calculate the median of a sorted data set.
  Return [median, [vals less than median] [vals greater than median]]
  For typed arrays, returns [median nil nil] (partitions not supported)."
  [data]
  (core/median data))

(defn quartiles
  "Calculate the quartiles of a sorted data set."
  [data]
  (core/quartiles data))

(defn quantile
  "Calculate the quantile of a sorted data set."
  [^double quantile data]
  (core/quantile quantile data))

(defn central-moment
  "Compute the r-th central moment: (1/n) * Σ(xᵢ - μ)^r"
  ^double [data ^double mean ^long r]
  (core/central-moment data mean r))

(defn skewness
  "Compute sample skewness using one of three methods.

  Type 1: g₁ = m₃ / m₂^(3/2) - typical textbook definition
  Type 2: G₁ = g₁ × √(n(n-1)) / (n-2) - unbiased under normality (SAS/SPSS)
  Type 3: b₁ = g₁ × ((n-1)/n)^(3/2) - used in MINITAB/BMDP

  Default is type 2 (unbiased under normality).

  Reference: Joanes & Gill (1998), Comparing measures of sample skewness
             and kurtosis. The Statistician, 47, 183-189."
  (^double [data] (core/skewness data))
  (^double [data type] (core/skewness data type)))

(defn kurtosis
  "Compute sample excess kurtosis using one of three methods.

  Type 1: g₂ = m₄ / m₂² - 3 - typical textbook definition
  Type 2: G₂ = ((n+1)g₂ + 6)(n-1) / ((n-2)(n-3)) - unbiased under normality (SAS/SPSS)
  Type 3: b₂ = (g₂ + 3)((n-1)/n)² - 3 - used in MINITAB/BMDP

  Default is type 2 (unbiased under normality). Returns excess kurtosis
  (normal distribution has excess kurtosis of 0).

  Reference: Joanes & Gill (1998), Comparing measures of sample skewness
             and kurtosis. The Statistician, 47, 183-189."
  (^double [data] (core/kurtosis data))
  (^double [data type] (core/kurtosis data type)))

(defn cv
  "Coefficient of variation (CV), also known as relative standard deviation.
  Computed as σ/μ (standard deviation divided by mean).

  Returns Double/NaN if mean is zero or data has fewer than 2 elements.
  CV is dimensionless and useful for comparing variability across datasets
  with different units or scales."
  ^double [data]
  (core/cv data))

;;; Outliers

(defn boxplot-outlier-thresholds
  "Outlier thresholds for given quartiles.
  Returns [severe-low mild-low mild-high severe-high]."
  [^double q1 ^double q3]
  (outliers/boxplot-outlier-thresholds q1 q3))

(defn adjusted-boxplot-outlier-thresholds
  "Outlier thresholds for given quartiles adjusted for skewness.
  Uses the adjusted boxplot method from Hubert & Vandervieren (2008)."
  [^double q1 ^double q3 ^double mc]
  (outliers/adjusted-boxplot-outlier-thresholds q1 q3 mc))

(defn medcouple-kernel
  "Compute the medcouple kernel h(x_i, x_j)."
  ^double [^double xi ^double xj ^double med]
  (outliers/medcouple-kernel xi xj med))

(defn medcouple
  "Compute the medcouple, a robust measure of skewness.
  Returns a value in [-1, 1] where positive indicates right-skew
  and negative indicates left-skew."
  ^double [sorted-data]
  (outliers/medcouple sorted-data))

;;; Sampling

(defn uniform-distribution
  "Return uniformly distributed deviates on 0..max-val using the specified rng."
  [^double max-val rng]
  (sampling/uniform-distribution max-val rng))

(defn sample-uniform
  "Provide n samples from a uniform distribution on 0..max-val."
  [n max-val rng]
  (sampling/sample-uniform n max-val rng))

(defn sample
  "Sample with replacement."
  [x rng]
  (sampling/sample x rng))

(defn confidence-interval
  "Find the significance of outliers given bootstrapped mean and variance
   estimates."
  [^double mean ^double variance]
  (sampling/confidence-interval mean variance))

;;; Probability

(defn polynomial-value
  "Evaluate a polynomial at the given value x, for the coefficients given in
  descending order (so the last element of coefficients is the constant term)."
  ^double [^double x ^doubles coefficients]
  (probability/polynomial-value x coefficients))

(defn erf
  "erf polynomial approximation.  Maximum error is 1.5e-7.
  Handbook of Mathematical Functions: with Formulas, Graphs, and Mathematical
  Tables. Milton Abramowitz (Editor), Irene A. Stegun (Editor), 7.1.26"
  ^double [^double x]
  (probability/erf x))

(defn normal-cdf
  "Probability p(X<x), for a normal distrubtion.  Uses the polynomial erf
  approximation above, and so is not super accurate."
  ^double [^double x]
  (probability/normal-cdf x))

(defn normal-pdf
  "Probability density function for the normal distribution."
  [^double mu ^double sigma]
  (probability/normal-pdf mu sigma))

(defn normal-quantile
  "Normal quantile function. Given a quantile in (0,1), return the normal value
  for that quantile.

  Wichura, MJ. 'Algorithm AS241' The Percentage Points of the Normal
  Distribution. Applied Statistics, 37, 477-484 "
  ^double [^double x]
  (probability/normal-quantile x))

(defn log-gamma
  "Compute the natural logarithm of the gamma function using Lanczos approximation.
  Returns ln(Γ(x)) for x > 0.

  Uses the Lanczos approximation with g=7 and 9 coefficients, providing
  approximately 15 digits of precision. Matches R's lgamma() behavior."
  ^double [^double x]
  (probability/log-gamma x))

(defn digamma
  "Compute the digamma function ψ(x) = d/dx ln(Γ(x)) = Γ'(x)/Γ(x).

  Uses the asymptotic expansion for large x and recurrence relation for small x.
  Accurate to ~15 digits for x > 0. Matches R's digamma() behavior."
  ^double [^double x]
  (probability/digamma x))

(defn trigamma
  "Compute the trigamma function ψ'(x) = d²/dx² ln(Γ(x)).

  Uses the asymptotic expansion for large x and recurrence relation for small x.
  Accurate to ~15 digits for x > 0. Matches R's trigamma() behavior."
  ^double [^double x]
  (probability/trigamma x))

(defn regularized-gamma-p
  "Regularized lower incomplete gamma function P(a, x) = γ(a,x) / Γ(a).
  Uses series expansion for small x, continued fraction for large x.
  This is the CDF of the gamma distribution with shape=a and scale=1."
  ^double [^double a ^double x]
  (probability/regularized-gamma-p a x))

;;; Gamma Distribution

(defn gamma-pdf
  "Probability density function for the gamma distribution.
  Returns a function f(x) that computes the density at x."
  [^double shape ^double scale]
  (probability/gamma-pdf shape scale))

(defn gamma-cdf
  "Cumulative distribution function for the gamma distribution.
  Returns a function F(x) that computes P(X ≤ x)."
  [^double shape ^double scale]
  (probability/gamma-cdf shape scale))

;;; Weibull Distribution

(defn weibull-pdf
  "Probability density function for the Weibull distribution.
  Returns a function f(x) that computes the density at x."
  [^double shape ^double scale]
  (probability/weibull-pdf shape scale))

(defn weibull-cdf
  "Cumulative distribution function for the Weibull distribution.
  Returns a function F(x) that computes P(X ≤ x)."
  [^double shape ^double scale]
  (probability/weibull-cdf shape scale))

;;; Log-normal Distribution

(defn lognormal-pdf
  "Probability density function for the log-normal distribution.
  Returns a function f(x) that computes the density at x."
  [^double mu ^double sigma]
  (probability/lognormal-pdf mu sigma))

(defn lognormal-cdf
  "Cumulative distribution function for the log-normal distribution.
  Returns a function F(x) that computes P(X ≤ x)."
  [^double mu ^double sigma]
  (probability/lognormal-cdf mu sigma))

;;; Inverse Gaussian Distribution

(defn inverse-gaussian-pdf
  "Probability density function for the inverse Gaussian distribution.
  Returns a function f(x) that computes the density at x."
  [^double mu ^double lambda]
  (probability/inverse-gaussian-pdf mu lambda))

(defn inverse-gaussian-cdf
  "Cumulative distribution function for the inverse Gaussian distribution.
  Returns a function F(x) that computes P(X ≤ x)."
  [^double mu ^double lambda]
  (probability/inverse-gaussian-cdf mu lambda))

;;; Information Criteria for Model Selection

(defn aic
  "Akaike Information Criterion.

  AIC = 2k - 2·ln(L)

  Parameters:
    k - number of estimated parameters
    log-likelihood - log-likelihood value (log(L))

  Lower AIC indicates better model fit."
  ^double [^long k ^double log-likelihood]
  (probability/aic k log-likelihood))

(defn bic
  "Bayesian Information Criterion (Schwarz criterion).

  BIC = k·ln(n) - 2·ln(L)

  Parameters:
    k - number of estimated parameters
    n - sample size
    log-likelihood - log-likelihood value (log(L))

  Lower BIC indicates better model fit."
  ^double [^long k ^long n ^double log-likelihood]
  (probability/bic k n log-likelihood))

(defn aicc
  "Corrected Akaike Information Criterion for small samples.

  AICc = AIC + (2k² + 2k) / (n - k - 1)

  Parameters:
    k - number of estimated parameters
    n - sample size
    log-likelihood - log-likelihood value (log(L))

  For small samples (n/k < 40), AICc should be used instead of AIC.
  Requires n > k + 1."
  ^double [^long k ^long n ^double log-likelihood]
  (probability/aicc k n log-likelihood))

;;; Goodness-of-Fit Tests

(defn ks-test-statistic
  "Compute the Kolmogorov-Smirnov D statistic.

  D = max|Fₙ(x) - F(x)|

  Parameters:
    samples - sequence of sample values
    cdf-fn - theoretical CDF function (e.g., from gamma-cdf, weibull-cdf)

  Returns the D statistic."
  ^double [samples cdf-fn]
  (probability/ks-test-statistic samples cdf-fn))

(defn ks-pvalue
  "Compute asymptotic p-value for Kolmogorov-Smirnov test.

  Uses the asymptotic distribution with continuity correction.

  Parameters:
    d-statistic - the D statistic from ks-test-statistic
    n - sample size

  Returns the two-sided p-value."
  ^double [^double d-statistic ^long n]
  (probability/ks-pvalue d-statistic n))

(defn ks-test
  "One-sample Kolmogorov-Smirnov goodness-of-fit test.

  Tests whether the sample comes from the specified distribution.

  Parameters:
    samples - sequence of sample values
    cdf-fn - theoretical CDF function (e.g., (gamma-cdf shape scale))

  Returns map with:
    :statistic - the D statistic
    :p-value - asymptotic two-sided p-value
    :n - sample size

  A small p-value suggests the sample does not come from the specified distribution."
  [samples cdf-fn]
  (probability/ks-test samples cdf-fn))

(defn cvm-test-statistic
  "Compute the Cramér-von Mises W² statistic.

  W² = (1/12n) + Σᵢ₌₁ⁿ [F(xᵢ) - (2i-1)/(2n)]²

  Parameters:
    samples - sequence of sample values
    cdf-fn - theoretical CDF function

  Returns the W² statistic."
  ^double [samples cdf-fn]
  (probability/cvm-test-statistic samples cdf-fn))

(defn cvm-pvalue
  "Compute asymptotic p-value for Cramér-von Mises test.

  Parameters:
    w2-statistic - the W² statistic from cvm-test-statistic
    n - sample size

  Returns the p-value."
  ^double [^double w2-statistic ^long n]
  (probability/cvm-pvalue w2-statistic n))

(defn cvm-test
  "One-sample Cramér-von Mises goodness-of-fit test.

  Tests whether the sample comes from the specified distribution.
  W² is more sensitive to differences in the tails than K-S.

  Parameters:
    samples - sequence of sample values
    cdf-fn - theoretical CDF function (e.g., (gamma-cdf shape scale))

  Returns map with:
    :statistic - the W² statistic
    :p-value - asymptotic p-value
    :n - sample size

  A small p-value suggests the sample does not come from the specified distribution."
  [samples cdf-fn]
  (probability/cvm-test samples cdf-fn))

;;; Moment-Based Parameter Estimation

(defn gamma-moment-estimate
  "Estimate gamma distribution parameters using method of moments.

  Parameters (returned):
    shape (k) = mean² / variance
    scale (θ) = variance / mean

  Returns nil if estimates are invalid (non-positive mean or variance)."
  [^double mean ^double variance]
  (moment-match/gamma-moment-estimate mean variance))

(defn lognormal-moment-estimate
  "Estimate log-normal distribution parameters using method of moments.

  Parameters (log-space):
    sigma² = log(1 + variance/mean²)
    mu = log(mean) - sigma²/2

  Returns nil if mean is non-positive or variance is negative."
  [^double mean ^double variance]
  (moment-match/lognormal-moment-estimate mean variance))

(defn inverse-gaussian-moment-estimate
  "Estimate inverse Gaussian distribution parameters using method of moments.

  Parameters:
    mu = mean
    lambda = mean³ / variance

  Returns nil if mean or variance is non-positive."
  [^double mean ^double variance]
  (moment-match/inverse-gaussian-moment-estimate mean variance))

(defn weibull-moment-estimate
  "Estimate Weibull distribution parameters using method of moments.

  Uses the coefficient of variation to estimate shape, then derives scale.
  Returns nil if mean/variance non-positive or CV > 2."
  [^double mean ^double variance]
  (moment-match/weibull-moment-estimate mean variance))

(def all-distributions
  "Set of all distributions supported by the moment-match prefilter."
  moment-match/all-distributions)

(defn moment-match-prefilter
  "Screen distributions for suitability based on sample moments.

  Takes sample mean and variance and returns a map of distributions with
  their moment-based parameter estimates. Distributions where moment
  matching yields invalid parameters are excluded.

  Parameters:
    mean - sample mean
    variance - sample variance
    distributions - (optional) set of distributions to check, defaults to all

  Returns map from distribution keyword to {:params {...} :suitable? true/false}."
  ([^double mean ^double variance]
   (moment-match/moment-match-prefilter mean variance))
  ([^double mean ^double variance distributions]
   (moment-match/moment-match-prefilter mean variance distributions)))

(defn suitable-distributions
  "Return the set of distributions suitable for the given sample statistics.

  Parameters:
    mean - sample mean
    variance - sample variance
    distributions - (optional) set of distributions to check, defaults to all

  Returns set of suitable distribution keywords."
  ([^double mean ^double variance]
   (moment-match/suitable-distributions mean variance))
  ([^double mean ^double variance distributions]
   (moment-match/suitable-distributions mean variance distributions)))

(defn unsuitable-distributions
  "Return the set of distributions unsuitable for the given sample statistics.

  Parameters:
    mean - sample mean
    variance - sample variance
    distributions - (optional) set of distributions to check, defaults to all

  Returns set of unsuitable distribution keywords."
  ([^double mean ^double variance]
   (moment-match/unsuitable-distributions mean variance))
  ([^double mean ^double variance distributions]
   (moment-match/unsuitable-distributions mean variance distributions)))

;;; Maximum Likelihood Estimation

(defn gamma-mle
  "Maximum likelihood estimation for the gamma distribution.

  Uses Minka's fast fixed-point iteration for shape parameter.
  Scale is then: θ = mean(x) / k

  Parameters:
    samples - sequence of positive sample values
    opts - optional map with:
      :max-iter - maximum iterations (default 100)
      :tol - convergence tolerance (default 1e-10)
      :init-shape - initial shape estimate (default: method of moments)

  Returns map with:
    :params {:shape k, :scale θ}
    :log-likelihood - the maximized log-likelihood value
    :iterations - number of iterations used

  Throws if any sample is non-positive."
  ([samples] (mle/gamma-mle samples))
  ([samples opts] (mle/gamma-mle samples opts)))

(defn lognormal-mle
  "Maximum likelihood estimation for the log-normal distribution.

  The MLE for log-normal has a closed-form solution:
    μ = mean(log(x))
    σ = sqrt(variance(log(x)))  ; using population variance

  Parameters:
    samples - sequence of positive sample values

  Returns map with:
    :params {:mu μ, :sigma σ}
    :log-likelihood - the maximized log-likelihood value

  Throws if any sample is non-positive."
  [samples]
  (mle/lognormal-mle samples))

(defn inverse-gaussian-mle
  "Maximum likelihood estimation for the inverse Gaussian distribution.

  The MLE for inverse Gaussian has a closed-form solution:
    μ = mean(x)
    λ = n / Σ(1/xᵢ - 1/μ)

  Parameters:
    samples - sequence of positive sample values

  Returns map with:
    :params {:mu μ, :lambda λ}
    :log-likelihood - the maximized log-likelihood value

  Throws if any sample is non-positive."
  [samples]
  (mle/inverse-gaussian-mle samples))

(defn weibull-mle
  "Maximum likelihood estimation for the Weibull distribution.

  Uses Newton-Raphson iteration to find the shape parameter k that solves:
    1/k + mean(log(x)) - (Σxᵏlog(x))/(Σxᵏ) = 0

  Once k is found, scale is: λ = (Σxᵏ/n)^(1/k)

  Parameters:
    samples - sequence of positive sample values
    opts - optional map with:
      :max-iter - maximum iterations (default 100)
      :tol - convergence tolerance (default 1e-10)
      :init-shape - initial shape estimate (default: method of moments)

  Returns map with:
    :params {:shape k, :scale λ}
    :log-likelihood - the maximized log-likelihood value
    :iterations - number of iterations used

  Throws if any sample is non-positive."
  ([samples] (mle/weibull-mle samples))
  ([samples opts] (mle/weibull-mle samples opts)))

;;; Knuth Bayesian histogram binning

(defn knuth-log-posterior
  "Compute Knuth's log-posterior for M bins given sample count and bin counts.

  F(M|x,I) = n·log(M) + logΓ(M/2) - M·logΓ(1/2) - logΓ((2n+M)/2) + Σₖ₌₁ᴹ logΓ(nₖ + 1/2)

  Parameters:
    n - total sample count
    bin-counts - sequence of counts per bin

  Returns the log-posterior value (higher is better)."
  ^double [^long n bin-counts]
  (knuth/log-posterior n bin-counts))

(defn knuth-optimal-bins
  "Find optimal number of bins using Knuth's Bayesian method.

  Searches M ∈ [1, max-bins] for the value that maximizes the log-posterior.

  Parameters:
    samples - sequence of numeric values
    opts - optional map with:
      :max-bins - maximum M to search (default: 50)
      :min - pre-computed minimum value (avoids redundant scan)
      :max - pre-computed maximum value (avoids redundant scan)

  Returns map with:
    :optimal-bins - the optimal number of bins M
    :log-posterior - the log-posterior value at optimal M"
  ([samples] (knuth/optimal-bins samples))
  ([samples opts] (knuth/optimal-bins samples opts)))

;;; Histogram

(defn histogram
  "Compute histogram from data (sequence or typed array).

  Supports multiple binning methods via the :method option:
  - :freedman-diaconis (default) - Uses IQR-based bin width calculation
  - :knuth - Bayesian optimal bin count selection

  Accepts sequences, vectors, or typed arrays (DoubleArray, LongArray).

  Options:
    :method   - Binning method (:freedman-diaconis or :knuth)
    :iqr      - Pre-computed IQR (only for :freedman-diaconis)
    :max-bins - Maximum bins to search (only for :knuth, default 50)

  Returns map containing:
    :type     - :criterium/histogram-fixed-width or :criterium/histogram-knuth
    :counts   - vector of bin counts
    :centers  - vector of bin centers
    :width    - bin width
    :density  - vector of probability density values
    :n        - total number of samples
    :num-bins - number of bins
    :min      - minimum value
    :max      - maximum value

  Additional keys for :knuth method:
    :optimal-bins  - optimal bin count M
    :log-posterior - log-posterior value at optimal M

  For backward compatibility, second argument can be a number (pre-computed IQR).

  Throws:
    ex-info {:error :histogram/no-values} for empty input
    ex-info {:error :histogram/same-values} when all values are the same"
  ([data]
   (histogram/histogram data))
  ([data opts-or-iqr]
   (histogram/histogram data opts-or-iqr)))

;;; T-digest streaming quantile estimation

(defn digest-new
  "Creates a new t-digest with optional compression factor."
  ([] (t-digest/new-digest))
  ([compression] (t-digest/new-digest compression))
  ([compression buffer-size] (t-digest/new-digest compression buffer-size)))

(defn digest-add-point
  "Add a single value into the digest."
  ([digest value]
   (t-digest/add-point digest value))
  ([digest value weight]
   (t-digest/add-point digest value weight)))

(defn digest-compress
  "Merge any buffered points into the digest."
  [digest]
  (t-digest/compress digest))

(defn digest-quantile
  "Return estimated value at given quantile [0,1].
   Return NaN if digest is empty."
  ^double [digest ^double q]
  (t-digest/quantile digest q))

(defn digest-cdf
  "Return the cumulative probability at x.
   Return NaN if digest is empty."
  ^double [digest ^double x]
  (t-digest/cdf digest x))

(defn digest-sample-count
  "Return the sample count in the digest."
  ^double [digest]
  (t-digest/sample-count digest))

(defn digest-minimum
  "Return the minimum value in the digest."
  ^double [digest]
  (t-digest/minimum digest))

(defn digest-maximum
  "Return the maximum value in the digest."
  ^double [digest]
  (t-digest/maximum digest))

(defn digest-mean
  "Return the mean estimate.
   Return NaN if digest is empty."
  ^double [digest]
  (t-digest/mean digest))

(defn digest-variance
  "Return the variance estimate.
   Return NaN if digest is empty."
  (^double [digest]
   (t-digest/variance digest))
  (^double [digest ^double mean]
   (t-digest/variance digest mean)))

(defn digest-transform
  "Transform digest values using the given function."
  [digest f]
  (t-digest/transform digest f))

(defn digest-centroid-means
  "Return a vector of centroid means."
  [digest]
  (t-digest/centroid-means digest))

(defn digest-histogram
  "Returns a histogram of the digest using centroid centers as bin locations."
  [digest iqr]
  (t-digest/histogram digest iqr))

(defn digest-filter-outliers
  "Filter outliers from the digest."
  [digest outliers]
  (t-digest/filter-outliers digest outliers))

;;; Kernel functions

(defn modal-estimation-constant
  "Kernel function for estimation of multi-modality.
  h-k is the critical bandwidth, sample-variance is the observed sample variance."
  ^double [^double h-k ^double sample-variance]
  (kernel/modal-estimation-constant h-k sample-variance))

(defn smoothed-sample
  "Smoothed estimation function.
  Generates a lazy sequence of smoothed values from data using kernel smoothing."
  [^double c-k ^double h-k data deviates]
  (kernel/smoothed-sample c-k h-k data deviates))

(defn gaussian-weight
  "Weight function for gaussian kernel.
  K(t) = (1/sqrt(2*pi)) * exp(-t^2/2)"
  ^double [^double t]
  (kernel/gaussian-weight t))

(defn kernel-density-estimator
  "Kernel density estimator for x, given n samples X, weights K and width h.
  Computes f(x) = (1/nh) * sum_i K((x - X_i)/h)"
  [h K n X x]
  (kernel/kernel-density-estimator h K n X x))

;;; KDE - Kernel Density Estimation

(defn dct-ii
  "Discrete Cosine Transform Type II.
  Direct O(n²) implementation without FFT dependency."
  ^doubles [^doubles data]
  (kde/dct-ii data))

(defn linear-bin
  "Bin data onto a regular grid using linear interpolation.
  Returns vector of bin weights that sum to 1.0."
  ^doubles [data ^doubles grid]
  (kde/linear-bin data grid))

(defn silverman-bandwidth
  "Silverman's rule of thumb bandwidth selector.
  h = 0.9 * min(σ, IQR/1.34) * n^(-1/5)"
  ^double [data]
  (kde/silverman-bandwidth data))

(defn isj-bandwidth
  "Improved Sheather-Jones bandwidth selector.
  Uses DCT-based algorithm from Botev et al. for optimal bandwidth
  selection that works well for multimodal distributions."
  ^double [data]
  (kde/isj-bandwidth data))

(defn gaussian-kde
  "Compute Gaussian kernel density estimate at grid points.
  Returns vector of density values at each grid point."
  ^doubles [data ^double bandwidth ^doubles grid]
  (kde/gaussian-kde data bandwidth grid))

(defn find-modes
  "Find modes (local maxima) in a density estimate.
  Returns vector of maps with :location and :density for each mode,
  sorted by density (highest first)."
  [^doubles grid ^doubles density]
  (kde/find-modes grid density))

(defn kde-bootstrap-sample
  "Generate a bootstrap sample of KDE density at fixed grid points."
  [data ^double bandwidth ^doubles grid rng]
  (kde/kde-bootstrap-sample data bandwidth grid rng))

(defn kde-confidence-bands
  "Compute bootstrap confidence bands for KDE.
  Returns map with :lower and :upper vectors."
  ([data bandwidth grid]
   (kde/kde-confidence-bands data bandwidth grid))
  ([data bandwidth grid opts]
   (kde/kde-confidence-bands data bandwidth grid opts)))

(defn mode-confidence-intervals
  "Compute bootstrap confidence intervals for mode locations.
  Returns vector of mode CIs, each with :location, :ci-lower, :ci-upper."
  ([data bandwidth grid n-modes]
   (kde/mode-confidence-intervals data bandwidth grid n-modes))
  ([data bandwidth grid n-modes opts]
   (kde/mode-confidence-intervals data bandwidth grid n-modes opts)))

(defn count-modes
  "Count number of modes in KDE with given bandwidth."
  ^long [data ^double bandwidth ^long n-points]
  (kde/count-modes data bandwidth n-points))

(defn critical-bandwidth
  "Find smallest bandwidth giving at most k modes via binary search.
  Returns the critical bandwidth h_k."
  ^double [data ^long k opts]
  (kde/critical-bandwidth data k opts))

(defn locate-modes
  "Find mode and antimode locations using critical bandwidth.
  Returns {:modes [...] :antimodes [...] :critical-bandwidth h_k}"
  [data ^long k opts]
  (kde/locate-modes data k opts))

(defn silverman-bootstrap-sample
  "Generate a smoothed bootstrap sample for Silverman's test."
  [data ^double bandwidth rng]
  (kde/silverman-bootstrap-sample data bandwidth rng))

(defn silverman-test
  "Silverman's bootstrap test for H0: at most k modes.
  Returns map with :k, :critical-bandwidth, :p-value, :corrected?"
  [data ^long k opts]
  (kde/silverman-test data k opts))

(defn acr-test
  "ACR test for H0: at most k modes.
  Combines critical bandwidth and excess mass approaches.
  Returns map with :k, :excess-mass, :critical-bandwidth, :p-value"
  [data ^long k opts]
  (kde/acr-test data k opts))

(defn excess-mass
  "Compute excess mass statistic for testing k modes.
  Returns map with :statistic, :k, :n"
  ([data k] (kde/excess-mass data k))
  ([data k opts] (kde/excess-mass data k opts)))

(defn kde
  "Compute KDE analysis on sample data.
  Returns map with :type, :bandwidth, :grid, :density, :lower-band, :upper-band, :n"
  ([data] (kde/kde data))
  ([data opts] (kde/kde data opts)))

;;; Bootstrap resampling

(defn bootstrap-sample
  "Bootstrap sampling of a statistic, using resampling with replacement.
  Returns transposed results: if statistic returns a vector, returns a vector
  of vectors where each inner vector contains all samples for that statistic."
  [data statistic size rng-factory]
  (bootstrap/bootstrap-sample data statistic size rng-factory))

(defn bootstrap-estimate
  "Mean, variance and confidence interval from bootstrapped samples.
  Returns [mean variance [lower upper]]."
  [sampled-stat]
  (bootstrap/bootstrap-estimate sampled-stat))

(defn drop-at
  "Return coll with element at index n removed."
  [n coll]
  (bootstrap/drop-at n coll))

(defn jacknife
  "Jacknife statistics on data.
  Computes the statistic on each leave-one-out sample of the data."
  [data statistic]
  (bootstrap/jacknife data statistic))

(defn bca-nonparametric-eval
  "Calculate bootstrap values for given estimate and samples."
  [size z-alpha estimate samples jack-samples]
  (bootstrap/bca-nonparametric-eval size z-alpha estimate samples jack-samples))

(defn bca-nonparametric
  "Non-parametric BCa estimate of a statistic on data.
  Size bootstrap samples are used. Confidence values are returned at the
  alpha normal quantiles."
  [data statistic size alpha rng-factory]
  (bootstrap/bca-nonparametric data statistic size alpha rng-factory))

(def ->BcaEstimate
  "Constructor for BcaEstimate record."
  bootstrap/->BcaEstimate)

(def map->BcaEstimate
  "Map constructor for BcaEstimate record."
  bootstrap/map->BcaEstimate)

(defn bootstrap-bca
  "Bootstrap a statistic with BCa confidence intervals.
  Returns a BcaEstimate record with :point-estimate and :estimate-quantiles."
  [data statistic size alpha rng-factory]
  (bootstrap/bootstrap-bca data statistic size alpha rng-factory))

(defn bootstrap
  "Bootstrap a statistic.
  Statistic can produce multiple statistics as a vector.
  Returns [mean variance [lower upper]] for each statistic."
  [data statistic size rng-factory]
  (bootstrap/bootstrap data statistic size rng-factory))

(defn scale-bootstrap-estimate
  "Scale a BcaEstimate by the given scale factor."
  [estimate ^double scale]
  (bootstrap/scale-bootstrap-estimate estimate scale))

(defn scale-bootstrap-stat
  "Scale a bootstrap stat using the given scale function."
  [scale-f stat]
  (bootstrap/scale-bootstrap-stat scale-f stat))

(defn assoc-bootstrap-mean-3-sigma
  "Add :mean-plus-3sigma and :mean-minus-3sigma to stats map."
  [stats]
  (bootstrap/assoc-bootstrap-mean-3-sigma stats))

(defn scale-bootstrap-values
  "Apply function f to all values in stats map."
  [stats f]
  (bootstrap/scale-bootstrap-values stats f))

(def stats-fn-map
  "Map of stat keywords to their corresponding functions."
  bootstrap/stats-fn-map)

(defn stats-fns
  "Build vector of stat functions including quantile functions for given quantiles."
  [quantiles]
  (bootstrap/stats-fns quantiles))

(defn stats-fn
  "Combine multiple stat functions into one that returns a vector of results."
  [fs]
  (bootstrap/stats-fn fs))
