# Criterium

Criterium measures the computation time of an expression.  It is
designed to address some of the pitfalls of benchmarking, and benchmarking on
the JVM in particular.

This includes:

  * statistical processing of multiple evaluations
  * inclusion of a warm-up period, designed to allow the JIT compiler to optimise its code
  * purging of gc before testing, to isolate timings from GC state prior
    to testing
  * a final forced GC after testing to estimate impact of cleanup on the
    timing results

## Usage

The top level interface is in `criterium.core`.

    (use 'criterium.core)

Use `bench` to run a benchmark in a simple manner.

    (bench (Thread/sleep 1000) :verbose)

By default bench is quiet about its progress.  Run `with-progress-reporting` to
get progress information on *out*.

    (with-progress-reporting (bench (Thread/sleep 1000) :verbose))
    (with-progress-reporting (quick-bench (Thread/sleep 1000) :verbose))

Lower level functions are available.

    (report-result (benchmark (Thread/sleep 1000)) :verbose)
    (report-result (quick-bench (Thread/sleep 1000)))

Note that results are returned to the user to prevent JIT from recognising that
the results are not used. For functions that are very fast, or return a lot of
data, you may need to supply a function to reduce the results to prevent
excessive memory allocation.

    (bench (rand) :reduce-with +)


## References

[API Documentation](http://hugoduncan.github.com/criterium)

See [Elliptic Group](http://www.ellipticgroup.com/html/benchmarkingArticle.html)
for a Java benchmarking library.  The accompanying article describes many of the
JVM benchmarking pitfalls.

See [Criterion](http://hackage.haskell.org/package/criterion) for a Haskell benchmarking
library that applies many of the same statistical techniques.


## Installation

The library can be installed through
[Leiningen](http://github.com/technomancy/leiningen) or through maven.

## Todo

Serial correlation detection.
Multimodal distribution detection.
Use kernel density estimators?

## License

Licensed under [EPL](http://www.eclipse.org/legal/epl-v10.html)
