# Validation Tests

This directory contains validation tests that compare criterium's statistics
implementations against GNU R as a reference. These tests ensure numerical
accuracy of the library's statistical algorithms.

## Prerequisites

Validation tests require R and the Rserve package. Tests skip gracefully when
R is unavailable, so this setup is optional for development.

### Installing R

**macOS** (via Homebrew):
```bash
brew install r
```

**Ubuntu/Debian**:
```bash
sudo apt-get install r-base
```

**Windows**: Download from https://cran.r-project.org/bin/windows/base/

### Installing Rserve

Start R and install the Rserve package:

```r
install.packages("Rserve", repos = "http://rforge.net")
```

### Installing Required R Packages

Some validation tests require additional R packages:

```r
# For silverman-test and ACR test validation
install.packages("multimode")
```

### Starting Rserve

Before running validation tests, start Rserve:

```r
library(Rserve)
Rserve()
```

Or from the command line:

```bash
R -e "library(Rserve); Rserve()"
```

Rserve runs as a background process on port 6311 by default.

## Running Validation Tests

From the project root:

```bash
clojure -M:validation
```

This runs all validation tests in `bases/criterium/validation/`. Tests
automatically skip when R/Rserve is unavailable.

## Test Coverage

The validation tests compare the following criterium functions against R:

| Criterium Function | R Reference |
|--------------------|-------------|
| `criterium.stats.core/mean` | `mean()` |
| `criterium.stats.core/variance` | `var()` |
| `criterium.stats.core/median` | `median()` |
| `criterium.stats.core/quantile` | `quantile(..., type=7)` |
| `criterium.stats.core/linear-regression` | `lm()` |
| `criterium.stats.bootstrap/bootstrap-estimate` | `boot::boot()` |
| `criterium.stats.bootstrap/bca-ci` | `boot::boot.ci(..., type="bca")` |
| `criterium.stats.bootstrap/jackknife` | manual jackknife |
| `criterium.stats.kde/silverman-bandwidth` | `bw.nrd0()` |
| `criterium.stats.kde/gaussian-kde` | `density(..., kernel="gaussian")` |
| `criterium.stats.kde/silverman-test` | `multimode::modetest(..., method="SI")` |
| `criterium.stats.kde/acr-test` | `multimode::modetest(..., method="ACR")` |
| `criterium.stats.probability/normal-quantile` | `qnorm()` |
| `criterium.stats.probability/normal-cdf` | `pnorm()` |

## Tolerance Levels

- Basic statistics (mean, variance, median, quantile): < 1e-10 relative error
- Iterative algorithms (KDE, CDF): < 1e-6 relative error
- Bootstrap-based tests: statistical comparison rather than exact matching

## Troubleshooting

**"R/Rserve not available"**: Ensure Rserve is running. Start it with
`R -e "library(Rserve); Rserve()"`.

**"Failed to load clojisr"**: The clojisr dependency may not be on the
classpath. Use the `:validation` alias: `clojure -M:validation`.

**"could not find function 'modetest'"**: Install the multimode R package:
`install.packages("multimode")` in R.
