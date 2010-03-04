# Criterium

Criterium measures the computation time of an expression.  It is
designed to address some of the pitfalls of benchmarking, and benchmarking on
the JVM in particular.

This includes:
  - statistical processing of multiple evaluations
  - inclusion of a warm-up period, designed to allow the JIT compiler to
    optimise its code
  - purging of gc before testing, to isolate timings from GC state prior
    to testing
  - a final forced GC after testing to estimate impact of cleanup on the
    timing results

## Usage
  (use 'criterium)
  (bench (Thread/sleep 1000) :verbose)
  (with-progress-reporting (bench (Thread/sleep 1000) :verbose))
  (report-result (benchmark (Thread/sleep 1000)) :verbose)
  (report-result (quick-bench (Thread/sleep 1000)))

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

Alternatively, the source code can be compiled into a jar with leiningen.

    lein jar

## Todo

Serial correlation detection.
Multimodal distribution detection.
Use kernel density estimators?

## License

Licensed under [EPL](http://www.eclipse.org/legal/epl-v10.html)

