# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

Criterium is a Clojure benchmarking library designed to provide statistically sound performance measurements while accounting for JVM-specific challenges like JIT compilation, garbage collection, and measurement overhead.

This is currently version 0.5.x (ALPHA) which represents a significant architectural redesign from the stable 0.4.x version. The codebase uses a Polylith monorepo structure.

## Development Commands

### Testing
```bash
# Build agent and prepare dependencies (required once after checkout or deps.edn changes)
make dev

# Run tests with Kaocha (excludes slow tests)
clojure -M:kaocha:dev:test :all --reporter dots

# For agent development: Test with locally-built agent (macOS)
clojure -M:kaocha:dev:test:with-agent-mac --reporter dots

# For agent development: Test with locally-built agent (Linux)
clojure -M:kaocha:dev:test:with-agent-linux --reporter dots

# Run tests for a namespace with Kaocha
clojure -M:kaocha:dev:test --reporter dots --focus the.namespace.name

# Run slow tests (marked with ^:slow metadata, skipped by default)
clojure -M:kaocha:dev:test :slow --reporter dots

# Run validation tests against R (requires R + Rserve installed)
clojure -M:validation
```

### Building and Packaging
```bash
# Build criterium jar
clojure -T:build jar :project :criterium

# Build arg-gen jar (argument generation with test.check)
clojure -T:build jar :project :arg-gen

# Install locally (requires all platform agent binaries)
clojure -T:build install :project :criterium

# Install locally with only current platform's agent (for development)
clojure -T:build install-local

# Deploy to repository
clojure -T:build deploy :project :criterium
clojure -T:build deploy :project :arg-gen

# Clean build artifacts
clojure -T:build clean
```

### Code Quality
```bash
# Lint code
clj-kondo --lint src

# Format code
clojure -M:cljfmt check
clojure -M:cljfmt fix

# Check for outdated dependencies
clojure -M:outdated
```

### C++ Agent Development
```bash
# Build the native agent (from agent-cpp/ directory)
cmake -B build
cmake --build build

# Build with debug symbols and debug output
cmake -B build -DCMAKE_BUILD_TYPE=Debug -DDEBUG=ON
cmake --build build

# Cross-compile for different architectures (macOS)
cmake -B build -DCMAKE_OSX_ARCHITECTURES=arm64
cmake -B build -DCMAKE_OSX_ARCHITECTURES=x86_64

# The agent provides allocation tracking capabilities
```

### REPL Development
```bash
# Start development REPL (agent auto-loads when available)
clojure -M:dev:test

# For agent development: Use locally-built agent (macOS)
clojure -M:dev:test:with-agent-mac

# For agent development: Use locally-built agent (Linux)
clojure -M:dev:test:with-agent-linux

# For JDK 17+ compiler blackhole  (recommended)
clojure -M:dev:test:blackhole
```

### NREPL server

To start an NREPL server:

``` bash
clojure -M:nrepl:dev:test:with-agent-mac:blackhole
```

The command `clj-nrepl-eval` is installed on your path for evaluating Clojure code via nREPL.

**Discover nREPL servers:**

`clj-nrepl-eval --discover-ports`

Only ever use an nREPL server from your own worktree.

**Evaluate code:**

`clj-nrepl-eval -p <port> "<clojure-code>"`

With timeout (milliseconds)

`clj-nrepl-eval -p <port> --timeout 5000 "<clojure-code>"`

The REPL session persists between evaluations - namespaces and state are maintained.
Always use `:reload` when requiring namespaces to pick up changes.

When completing a story, kill any nREPL processes that are running in the story's worktree.


## Architecture

### Polylith Structure
- `bases/criterium/` - Core benchmarking functionality
- `bases/agent/` - JVM agent for allocation tracking
- `bases/blackhole/` - JMH-style Blackhole for preventing dead code elimination
- `bases/arg-gen/` - Argument generation using test.check generators
- `bases/notebooks/` - Computational notebooks and examples
- `components/r-validation/` - R connection helper for validation tests
- `projects/criterium/` - Main criterium JAR (criterium/criterium)
- `projects/arg-gen/` - Argument generation JAR (criterium/arg-gen)
- `development/` - Development environment setup

### Core Components

**Collection Pipeline**: Benchmarking follows a three-stage pipeline:
1. **Collection** - Gather metrics using collectors and collection plans
2. **Analysis** - Apply statistical analysis to raw metrics
3. **Viewing** - Format and present results through viewers

Each of these must be usable independently. e.g the collection can be
replaced by the instrument-fn or sampled-fn results.

**Analysis vs View Separation** (critical design constraint):
- **Analysis** (`criterium.analyse`) contains ALL non-visualization computation. Users must be able to access all criterium analysis results without using viewers. Analysis functions transform data maps and produce computed results (statistics, fits, tests, etc.).
- **View** (`criterium.view`, `criterium.viewer.*`) contains ONLY visualization-specific functionality. Viewers format and display analysis results but must not perform analysis-type computation. Different viewers should render the same pre-computed analysis data.

**Bench Plans** (`criterium.bench-plans`):
- Compose collection, analysis, and view stages into reusable configurations
- Define which collectors to use, which analyses to run, and which views to display
- Users select a bench plan to get a complete benchmarking workflow
- Custom bench plans allow tailored analysis pipelines (e.g., distribution fitting, allocation profiling)

**Key Abstractions**:
- `measured` - Wraps expressions/functions for measurement
- `collector` - Captures various metrics during execution
- `collect-plan` - Defines sampling strategy (warmup, timing, etc.)
- `benchmark` - Coordinates analysis and viewing of collected data

**Argument Generation** (`criterium.arg-gen`):
- Creates `measured` instances using test.check generators for benchmark inputs
- `arg-gen/measured` macro - Define benchmarks with generated arguments
- Supports size and seed options for reproducible benchmarks

**Metrics System**: Supports multiple metric types:
- `:elapsed-time` - Wall clock timing
- `:memory` - Memory usage and GC activity
- `:thread-allocation` - Per-thread allocation tracking
- `:compilation` - JIT compilation events
- `:class-loader` - Class loading statistics

### Primary APIs

**0.5.x (Current Alpha)**:
- `criterium.bench/bench` - Main benchmarking macro
- `criterium.bench/bench-measured` - Function-based benchmarking
- `criterium.bench/last-bench` - Access to last benchmark results

**0.4.x (Legacy)**:
- `criterium.core/bench` - Deprecated but still available
- `criterium.core/quick-bench` - Fast benchmarking variant

### Domain Analysis

Domain analysis enables benchmarking across a parameter space (varying input sizes, comparing implementations) rather than at a single point.

**Main API** (`criterium.domain`):
- `domain-expr` - Macro to define axes and implementations concisely
- `bench` - Run benchmarks across a domain and analyze results
- `domain`, `add-run`, `runs`, `select` - Domain data structure operations

**Domain Plans** (`criterium.domain-plans`):
- `complexity-analysis` - Fit O(log n), O(n), O(n log n), O(n²) models
- `implementation-comparison` - Compare implementations across an :impl axis
- `extract-metrics` - Extract all quantitative metrics from runs

**Analysis Functions** (`criterium.domain.analysis`):
- `extract` - Extract metric values from all runs
- `compare-by` - Compare metrics across an axis dimension
- `group-by-axis` - Partition runs by axis values
- `fit-complexity` - Fit complexity models to extracted data
- `analyse-domain` - Execute a domain plan

**Builder** (`criterium.domain.builder`):
- `domain-builder` - Build domain by running benchmarks across axes
- `log-range`, `linear-range` - Generate coordinate ranges

### Viewers
- `:print` - Default human-readable text output
- `:pprint` - Pretty-printed structured output
- `:portal` - Interactive charts and visualizations (requires Portal)
- `:kindly` - Notebook charts and visualizations using kindly

## Testing Strategy

Tests use Kaocha with the following structure:
- Unit tests in `bases/*/test/` directories
- Integration tests across components
- Performance regression tests
- Test data in `bases/criterium/test/criterium/data/`

### Test Speed Optimizations

The test suite uses several strategies to maintain fast execution:

**Slow Test Marking:**
Tests that take significant time (>30s) are marked with `^:slow` metadata and excluded from default runs via `:skip-meta [:slow]` in `tests.edn`. Run slow tests explicitly using the `:slow` test suite: `clojure -M:kaocha:dev:test :slow`.

**Minimal Iterations for API Tests:**
Tests that validate API behavior (not benchmark accuracy) use reduced time limits:
- `bench_test.clj`: `:limit-time-s 0.1` or `0.2` instead of full benchmark runs
- Property tests in `well_test.clj`: 50 iterations (sufficient for correctness validation)

**Agent Build Cache:**
Agent build tests use a shared CMake build cache at `target/test-agent-build-cache` for faster incremental builds. This cache persists between test runs. Clear it when:
- Switching between major CMake versions
- After changes to `agent-cpp/CMakeLists.txt` that require a clean build
- If you encounter stale build artifacts causing test failures

```bash
rm -rf target/test-agent-build-cache
```

**Vega Chart Schema Validation:**
Tests in `criterium.viewer.schema-validation-test` validate generated Vega and Vega-Lite chart specs against official JSON schemas. Vega-Lite validation requires Node.js 18+ with the vega-lite npm package (auto-installed to `target/npm` on first test run if not present):
- **Node.js 18+:** Required for Vega-Lite spec validation via the vega-lite compiler (uses `structuredClone` API)
- **npm:** Used to install vega-lite package to `target/npm/` (isolated from project package.json)
- **Vega (v5):** Validated via networknt/json-schema-validator (Java, no Node.js needed)
- **Vega-Lite (v6):** Validated via Node.js vega-lite compiler

If you don't have Node.js 18+ installed, Vega-Lite schema validation tests will fail. Install Node.js from https://nodejs.org/ or via your package manager.

### Validation Tests

Validation tests compare criterium's statistics implementations against GNU R as a reference. These tests are in `bases/criterium/validation/` and run separately from the main test suite.

**Running validation tests:**
```bash
clojure -M:validation
```

**Requirements:**
- R installed locally
- Rserve R package: `install.packages("Rserve",,"http://rforge.net")`

Tests skip gracefully when R/Rserve is unavailable - no test failures occur.

**Writing validation tests:**
- Test files go in `bases/criterium/validation/criterium/validation/`
- Use `*_validation_test.clj` naming pattern
- Use `criterium.validation.r` namespace for R interop:
  - `r-available?` - check if R is available
  - `r-eval` - evaluate R expression, return Clojure data
  - `with-r` - execute body only if R available, skip otherwise

## Native Agent

The C++ agent (`agent-cpp/`) provides enhanced allocation tracking and is bundled in the JAR for supported platforms (linux-x64, macos-x64, macos-arm64):

**Normal Usage:**
- Agent automatically loads when available for your platform
- No configuration required - just use `criterium.bench/bench`
- Gracefully degrades on unsupported platforms (logs warning, continues without allocation tracking)

**Agent Development:**
When modifying the C++ agent itself, use the `:with-agent-*` aliases to test local builds:
- Built with CMake in `agent-cpp/` directory
- `:with-agent-mac` alias (macOS) - loads locally-built `agent-cpp/libcriterium.dylib`
- `:with-agent-linux` alias (Linux) - loads locally-built `agent-cpp/libcriterium.so`
- See `docs/contributor/building-agent.md` for updating bundled binaries

## Design Patterns

**Data-Driven Configuration**: Most behavior is controlled through configuration maps rather than global state.

**Functional Pipeline**: Clear separation between collection, analysis, and presentation stages.

**Statistical Rigor**: Bootstrap sampling, outlier detection, and confidence intervals are core features.

**JVM Awareness**: Handles warmup periods, GC interference, and measurement overhead estimation.

## Portal Integration

When using the `:portal` viewer:
1. Ensure Portal is connected to tap>
2. Results include interactive charts and detailed breakdowns
3. Supports histogram visualizations and statistical overlays

## Schema Validation

Criterium uses [malli](https://github.com/metosin/malli) for schema validation during development. Schemas are defined in `development/src/criterium/schema.clj` and instrumentation is available via `development/src/criterium/schema/instrument.clj`.

**Enable instrumentation in REPL:**
```clojure
(require '[criterium.schema.instrument :as inst])

;; Enable validation on criterium.bench public API
(inst/instrument!)

;; Disable validation
(inst/unstrument!)

;; Check if a var is instrumented
(inst/instrumented? #'criterium.bench/bench-measured)
```

**Interpreting validation errors:**

When instrumented, invalid inputs throw exceptions with `:malli.core/invalid-input` type. The error contains:
- `:schema` - The expected schema
- `:args` - The actual arguments passed

```clojure
;; Example error when passing invalid measured
(bench-measured {} {:args-fn (fn [] []) :f +})
;; => ExceptionInfo :malli.core/invalid-input
;;    {:data {:schema [...] :args [...]}}
```

**Tests run with instrumentation enabled** via kaocha hooks defined in `tests.edn`.

**Registry side effect:** Requiring `criterium.schema.instrument` modifies malli's global default registry via `malli.registry/set-default-registry!` to include criterium schemas.

**Load order considerations:**
- If you use malli elsewhere, require `criterium.schema.instrument` AFTER any other code that calls `set-default-registry!`, since it will overwrite the registry.
- To combine criterium schemas with your own, create a composite registry that includes both `criterium.schema/registry` and your schemas, then call `set-default-registry!` with your composite registry AFTER requiring `criterium.schema.instrument`.
- Alternatively, pass explicit registries to malli functions rather than relying on the default registry.

## Development Notes

- The codebase follows the project's Clojure style guide (referenced in global CLAUDE.md)
- Uses 2-space indentation consistently
- Function docstrings precede argument vectors
- Comprehensive namespace documentation required
- Test files mirror source structure with `_test` suffix

- if you see Exception:
    clojure.lang.Compiler$CompilerException: Syntax error macroexpanding at (criterium/agent.clj:144:1)
  then you need to prepare the java libs with:
    cd bases/criterium && clojure -T:deps prep
