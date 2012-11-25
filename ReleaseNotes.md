# Release Notes

Current version is 0.3.1

# 0.3.1

- Add macro 'benchmark-round-robin' that is like benchmark, but takes a
  sequence of expressions
  The goal is to see whether the results are noticeably different when
  executed 'round robin' instead of doing the same expression over and over
  for an extended period of time (e.g. 1 min).  Instead try 10 different
  expressions for 100 millisec each, for example, and then repeat that 60
  times if :sample-count is 60.

  One needs to be cautious in interpreting the results, e.g. if one
  expression in the sequence tends to leave behind lots of garbage that
  won't be collected until a later expression is being executed.

- Change execute-expr so it has less overhead per execution of the
  benchmarked expression.

- Return options used as part of benchmark* return value

- Add Clojure version and system property sun.arch.data.model to
  runtime-details return value.

- Add the values of system properties java.version and java.runtime.version
  to return value of runtime-details.

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
