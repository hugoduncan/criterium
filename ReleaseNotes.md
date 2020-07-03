# Release Notes

# 0.4.6

- Pass correct gc-before-benchmark parameter value to run-benchmark

# 0.4.5

- Fix benchmark-round-robin

# 0.4.4

- Output WARNING through the warn function

- Fix division by zero exception when warmup-n > warmup-n

- Fix spelling error in progress message

- Fix pre-condition in well rng
  Closes #30

- Make clojure a provided dependency


# 0.4.3

- Push overhead estimation into benchmark*

  This restores the ability to simply use benchmark and quick-benchmark to
  obtain raw benchmark results data.  Closes #23

- Add option to suppress jvm option warnings

- Rename an incorrectly named test
  Rename a test wrongly named with the same name as a previous one that
  caused the former not to run.

# 0.4.2

- Add warning when running with TieredStopAtLevel
  TieredStopAtLevel is used to enable quick startup of lein, for example,
  and disables various levels of JIT compilation, which is probably not what
  you intended if you are benchmarking.

  This commit will print a warning if ths JVM option is in use.

# 0.4.1

- Fix NPE in report-results when there is no overhead in results.

# 0.4.0

- Make warmup and count estimation more robust
  Also splits out debug and warning messages.

  Will now warn if compilation occurs during execution count estimation.

- Compute and subtract the measurement overhead
  Estimate the measurement overhead on first run, and then subtract it from
  subsequent estimates.

  An explicit overhead can be supplied with the :overhead keyword.

- Pass a map instead of varargs
  In all but the top level bench and quick-bench macros, use a map for
  options, rather than varargs.

- Make the final gc run immediately after sampling
  Avoid doing any other memory allocation after the end of sample
  collection.

- Make warmup-for-jit more efficient
  Accumulates runtime in chunks, rather than timing individual calls.

- Use a mutable field for timing loop storage
  Replace the use of an array for the timing loop storage, and remove the
  use of a reducer.  Testing showed no difference in the resulting
  measurements, and this simplifies the code.

- Add benchmarks for timing loop implementations
  In order to quantify the impact of several timing loop implementations,
  adds benchmarks to quantify them.  There seems to be no difference in the
  result between using an array, or a mutable place.

- Use mu for microsecond units. [#14]


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
