# Changelog

## [0.5.243-ALPHA] - 2026-04-13

### Bug Fixes

- *(build)* Use :name param directly for lib symbol in deploy (#149)

## [0.5.241-ALPHA] - 2026-04-13

### Features

- *(measured)* Allow use of locals as arguments in expr macro (#63)
- *(viewer)* Add Kindly output viewer for Clay notebook integration (#64)
- *(arg-gen)* Add argument generation using test.check generators
- *(domain)* Add abstraction for working with multiple related benchmark runs
- *(allocation)* Integrate allocation tracing into benchmarking pipeline (#69)
- *(build)* Add ability to build all notebooks
- *(schema)* Replace ad-hoc schema validations with malli-based validation
- Add KDE analysis step for benchmark samples (#74)
- KDE analysis follow-up improvements
- *(validation)* Add validation test tree for stats algorithms against GNU R (#77)
- Add outlier calculation based on adjusted boxplot (#79)
- *(histogram)* Add Knuth's Bayesian optimal binning algorithm (#80)
- *(test)* Add JSON Schema validation for Vega chart specs
- *(analyse)* Add stats analysis for KDE-based summary statistics (#82)
- *(bench)* Update default bench plan to include KDE analysis with multimodal view (#83)
- *(build)* Add dev target to Makefile for building agent and preparing deps
- *(viewer)* Update domain result viewers with adaptive visualizations (#85)
- Add error bars to bar charts and confidence bands to line charts (#90)
- Add median statistics with bootstrapped CI and percentile spread (#91)
- Improve domain complexity analysis (#93)
- Show median, median CI, and quantiles by default (#95)
- *(viewer)* Update domain-compare bar charts to box plots (#94)
- Add function call tracing with call graph display (#96)
- *(stats)* Add distribution fit analysis and visualization (#97)
- *(viewer)* Add domain-apply view for per-run benchmark viewing (#100)
- *(test)* Add kaocha pre-load hook to suppress third-party library warnings (#105)
- *(array)* Use primitive arrays instead of vectors for sample collection (#104)
- *(viewer)* Add legend to multi-point domain comparison chart
- *(plugin)* Add criterium skill for Claude Code
- Add elapsed-time-only collector for memory-efficient benchmarking
- *(domain)* Use bootstrapped median for domain analysis
- Add warmup-args-fn for representative JIT warmup
- Add tail model fitting analysis and views
- Add marketplace.json for Claude plugin installation (#116)
- *(domain)* Enable domain/bench with :one-shot collect plan
- *(collect-plan)* Add :num-warmup-samples option to :one-shot collect-plan (#119)
- *(view)* Add extremes view showing min/max values
- *(bench-plans)* Use :extremes view instead of :log-stats (#121)
- *(test)* Split kindly_test.clj into smaller namespaces
- *(ci)* Add boxed math and reflection warning checks to GH test workflow
- *(stats)* Add autocorrelation analysis to detect sample non-independence
- *(stats)* Add primitive transducers
- *(array)* Define transducer interfaces in Java
- *(view)* Improve print viewer
- *(array)* Add resizable array support (#132)
- *(knuth)* Optimize histogram binning with single resizable array
- *(benchmark)* Enable simple timing of analyse steps
- *(array)* Replace Java interfaces with Clojure definterface+ (#137)
- *(viewer)* Add ASCII plot implementation for :print and :pprint viewers (#139)
- *(viewer)* Implement missing view methods across all viewers
- *(build)* Replace Makefile with bb tasks
- *(notebooks)* Add Quarto documentation structure
- Add line-breaker to enforce 80-character line limits

### Bug Fixes

- *(viewer)* Handle non-numeric metric values in print viewer
- *(viewer)* Handle non-numeric metric values in pprint and portal viewers
- Suppress empty summary stats headings in viewers
- *(test)* Use temp dir for agent javac test instead of production path
- Measured/expr incorrect when expression function position is a local (#84)
- Eliminate boxed math warnings in merging-digest and digest-samples (#87)
- *(viewer)* Fix single point domain comparison regression (#99)
- *(viewer)* Improve single-point domain compare boxplot visibility for tight median CIs (#102)
- :elapsed-time-only should create :elapsed-time metric
- Log-log plot y-axis label to use metric name instead of hardcoded 'time'
- *(clj-kondo)* Add proper hook for domain-expr macro (#117)
- *(stats)* Eliminate boxed math warnings in criterium.stats.tail
- *(warnings)* Eliminate boxed math and reflection warnings
- *(viewer)* Add missing :pprint view implementations (#141)
- *(viewer)* Guard against nil significance in print/pprint/kindly viewers (#142)

### Documentation

- Update installation instructions for 0.5.153-ALPHA
- Update changelog for v0.5.241-ALPHA

### Performance

- *(test)* Speed up test suite and eliminate boxed math warnings (#103)
- *(stats)* Improve KDE calculation speed with FFT-based algorithms
- Improve multi-mode analysis speed (#136)

### Refactor

- Extract polylith components from criterium base (#78)
- *(validation)* Extract validation tests to mirror source structure (#88)
- Cleanup interface functions and remove outlier-method config option (#89)
- *(agent)* Make agent.cpp testable with GoogleTest + GMock (#92)
- Update component namespaces to ensure criterium. prefix
- *(viewer)* Split domain-compare view components into separate table and chart views (#101)
- *(random)* Use mutable deftype for well-rng and ziggurat (#106)
- *(viewer)* Split common-charts into focused sub-namespaces
- *(viewer)* Split print, portal, kindly viewers into sub-namespaces (#128)
- *(stats)* Improve analysis implementations
- *(bench-plans)* Rationalise bench plans

### Testing

- *(well)* Add autocorrelation tests for WELL RNG
- *(ziggurat)* Add autocorrelation tests and extract statistical helpers
- Speed up test suite execution (#76)

### Miscellaneous

- Suppress more dev time boxed warnings (#140)

## [0.5.153-ALPHA] - 2025-12-07

### Bug Fixes

- *(ci)* Generate changelog before version calculation
- Use correct resource path for bundled agent binaries

### Documentation

- Update changelog for v0.5.153-ALPHA

## [0.5.149-ALPHA] - 2025-12-07

### Bug Fixes

- *(ci)* Disable cache in release workflow setup-clojure
- *(test)* Use WELL RNG in sampled-stats variance test
- *(ci)* Rename release assets to avoid filename collision

### Documentation

- Update changelog for v0.5.147-ALPHA
- Update changelog for v0.5.149-ALPHA

## [0.5.144-ALPHA] - 2025-12-07

### Documentation

- Update changelog for v0.5.144-ALPHA

## [0.5.143-ALPHA] - 2025-12-07

### Bug Fixes

- *(build)* Use :name param directly for lib symbol in deploy

### Documentation

- Update changelog for v0.5.143-ALPHA

## [0.5.141-ALPHA] - 2025-12-07

### Bug Fixes

- Provide estimate of execution time to round-robin (#43)
- Update travis build (#46)
- *(ci)* Fix release workflow jar building
- *(ci)* Allow release workflow to run from develop branch (#61)

### Documentation

- Update changelog for v0.5.141-ALPHA

### Miscellaneous

- Add release instructions (#44)


