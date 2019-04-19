# Criterium

Criterium measures the computation time of an expression.  It is
designed to address some of the pitfalls of benchmarking, and benchmarking on
the JVM in particular.

This includes:

  * statistical processing of multiple evaluations
  * inclusion of a warm-up period, designed to allow the JIT compiler to
    optimise its code
  * purging of gc before testing, to isolate timings from GC state prior
    to testing
  * a final forced GC after testing to estimate impact of cleanup on the
    timing results

## Installation

### Leiningen

Add the following to your `:dependencies`:

```clj
[criterium "0.4.5"]
```

### Maven

```xml
<dependency>
  <groupId>criterium</groupId>
  <artifactId>criterium</artifactId>
  <version>0.4.5</version>
</dependency>
```

## Usage

The top level interface is in `criterium.core`.

    (use 'criterium.core)

Use `bench` to run a benchmark in a simple manner.

```
(bench (Thread/sleep 1000))
 =>
                   Execution time mean : 1.000803 sec
          Execution time std-deviation : 328.501853 us
         Execution time lower quantile : 1.000068 sec ( 2.5%)
         Execution time upper quantile : 1.001186 sec (97.5%)
```

By default bench is quiet about its progress.  Run `with-progress-reporting` to
get progress information on `*out*`.

```clj
(with-progress-reporting (bench (Thread/sleep 1000) :verbose))
(with-progress-reporting (quick-bench (Thread/sleep 1000) :verbose))
```

Lower level functions are available, that separate benchmark statistic
generation and reporting.

```clj
(report-result (benchmark (Thread/sleep 1000) {:verbose true}))
(report-result (quick-benchmark (Thread/sleep 1000)))
```

Note that results are returned to the user to prevent JIT from recognising that
the results are not used.

## Measurement Overhead Estimation

Criterium will automatically estimate a time for its measurement
overhead.  The estimate is normally made once per session, and is
available in the `criterium.core/estimated-overhead-cache` var.

If the estimation is made while there is a lot of other processing
going on, then benchmarking quick functions may report small negative
times.  You can force a recalculation of the overhead by calling
`criterium.core/estimated-overhead!`.

If you want consistency across JVM processes, it might be prudent to
explicitly set `criterium.core/estimated-overhead!` to a constant
value.

## References

[API Documentation](http://hugoduncan.github.com/criterium/0.4/api/)
[Annotated Source](http://hugoduncan.github.com/criterium/0.4/uberdoc.html)

See [Elliptic Group](http://www.ellipticgroup.com/html/benchmarkingArticle.html)
for a Java benchmarking library.  The accompanying article describes many of the
JVM benchmarking pitfalls.

See [Criterion](http://hackage.haskell.org/package/criterion) for a Haskell
benchmarking library that applies many of the same statistical techniques.

## Todo

Serial correlation detection.
Multimodal distribution detection.
Use kernel density estimators?


## YourKit

YourKit is kindly supporting open source projects with its full-featured Java
Profiler.

YourKit, LLC is the creator of innovative and intelligent tools for profiling
Java and .NET applications. Take a look at YourKit's leading software products:

* <a href="http://www.yourkit.com/java/profiler/index.jsp">YourKit Java Profiler</a> and
* <a href="http://www.yourkit.com/.net/profiler/index.jsp">YourKit .NET Profiler</a>.

## License

Licensed under [EPL](http://www.eclipse.org/legal/epl-v10.html)
