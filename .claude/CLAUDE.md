# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

Criterium is a Clojure benchmarking library designed to provide statistically sound performance measurements while accounting for JVM-specific challenges like JIT compilation, garbage collection, and measurement overhead.

This is currently version 0.5.x (ALPHA) which represents a significant architectural redesign from the stable 0.4.x version. The codebase uses a Polylith monorepo structure.

## Development Commands

### Testing
```bash
# Prepare dependencies with Java sources (required once after checkout or deps.edn changes)
cd bases/criterium && clojure -T:deps prep

# Run tests with Kaocha
clojure -M:kaocha:dev:test --reporter dots

# For agent development: Test with locally-built agent (macOS)
clojure -M:kaocha:dev:test:with-agent-mac --reporter dots

# For agent development: Test with locally-built agent (Linux)
clojure -M:kaocha:dev:test:with-agent-linux --reporter dots
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
clojure -M:dev

# For agent development: Use locally-built agent (macOS)
clojure -M:dev:with-agent-mac

# For agent development: Use locally-built agent (Linux)
clojure -M:dev:with-agent-linux

# For exploring with Portal viewer
clojure -M:dev
```

## Architecture

### Polylith Structure
- `bases/criterium/` - Core benchmarking functionality
- `bases/agent/` - JVM agent for allocation tracking
- `bases/blackhole/` - JMH-style Blackhole for preventing dead code elimination
- `bases/arg-gen/` - Argument generation using test.check generators
- `bases/notebooks/` - Computational notebooks and examples
- `projects/criterium/` - Main criterium JAR (criterium/criterium)
- `projects/arg-gen/` - Argument generation JAR (criterium/arg-gen)
- `development/` - Development environment setup

### Core Components

**Collection Pipeline**: Benchmarking follows a three-stage pipeline:
1. **Collection** - Gather metrics using collectors and collection plans
2. **Analysis** - Apply statistical analysis to raw metrics
3. **Viewing** - Format and present results through viewers

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

### Viewers
- `:print` - Default human-readable text output
- `:pprint` - Pretty-printed structured output
- `:portal` - Interactive charts and visualizations (requires Portal)

## Testing Strategy

Tests use Kaocha with the following structure:
- Unit tests in `bases/*/test/` directories
- Integration tests across components
- Performance regression tests
- Test data in `bases/criterium/test/criterium/data/`

Skip slow tests with `:skip-meta [:very-slow]` in test metadata.

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

## Usage Scenarios

See [dev/docs/usage-scenarios.md](../dev/docs/usage-scenarios.md) for documented benchmarking use cases and their requirements.

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
