# Release Notes

Current version is 0.3.0

# 0.3.0

- Remove unused code. Makes criterium work on clojure 1.5.0-master-SNAPSHOT.
  Fixes #11.

- Add :gc-before-sample option
  Passing this option as true will force gc before every sample is taken.

- Fix labelling of quantiles, and drop display of intervals on the estimator
  distributions

- Force the collection of samples before calculating any statistics

- Remove compilation and reflection warnings

- Add tests for bootstrap-estimate and bootstrap-bca


# 0.2.1

## Features

- Add universal reducer function as a default reducer.

- Add os-details and runtime-details to results data

## Bug Fixes

- Fix bug in the calculation of the standard deviation


# 0.2.0

- Use doubles in time calculations to prevent overflows

- Switch to use function, add :reduce-with, and output CI on mean
  This gives more consistent results

- Update to clojure 1.3.0, and project to 0.2.0
