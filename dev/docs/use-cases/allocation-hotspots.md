# Identify Memory Allocation Hotspots

Locate code paths that produce excessive memory allocation to guide optimization efforts.

## Problem Statement

Memory allocation in the JVM, while fast, is not free. High allocation rates put
pressure on the garbage collector, leading to more frequent GC pauses, increased
CPU overhead, and potential throughput degradation. Without measurement, it's
difficult to know which parts of the code are responsible for the bulk of
allocations. Intuition-based optimization often targets the wrong code paths
while the actual hotspots remain unaddressed.

## Goal

Identify which functions, expressions, or code paths allocate the most memory
per invocation, enabling targeted optimization efforts that will have the
greatest impact on reducing GC pressure and improving overall performance.

## Key Components Required

### Per-Invocation Allocation Measurement

- Track bytes allocated during benchmark execution
- Attribute allocations to specific measured expressions
- Support for both total allocation and allocation rate metrics
- Resolution sufficient to distinguish between similar-allocation code paths

### Comparative Analysis

- Side-by-side comparison of allocation across different implementations
- Ranking of code paths by allocation volume
- Identification of allocation differences between code variants
- Support for relative (ratio) and absolute (bytes) comparisons

### Allocation Source Attribution

- Correlation between allocation measurements and specific code
- Ability to measure allocation of isolated code segments
- Distinction between direct allocations and allocations in called functions
- Support for measuring allocation in nested or composed operations

### Statistical Validity

- Sufficient samples to establish reliable allocation measurements
- Handling of allocation measurement variance
- Confidence intervals or similar uncertainty quantification
- Detection of anomalous measurements (GC interference, JIT changes)

### Integration with Timing

- Combined view of time and allocation metrics
- Identification of cases where allocation reduction trades off against speed
- Support for multi-objective analysis (optimize time AND allocation)
- Correlation analysis between allocation rate and latency

## Logical Dependencies

1. **Allocation tracking capability** - The runtime must provide mechanisms to
   measure memory allocation. This typically requires JVM support through MXBeans,
   JVMTI agents, or similar facilities. Without this, allocation measurement is
   not possible.

2. **Stable measurement environment** - Allocation measurements are affected by
   JIT compilation state, GC activity, and concurrent operations. Measurements
   must account for these factors to produce reliable results.

3. **Isolated measurement scope** - To attribute allocations to specific code,
   the measurement must isolate the code under test from measurement overhead
   and framework allocations.

4. **Representative workloads** - Allocation patterns depend on input data and
   code paths exercised. The benchmark must exercise realistic scenarios to
   identify production-relevant hotspots.

5. **Actionable granularity** - Allocation data must be at a granularity that
   enables optimization. Knowing total allocations is less useful than knowing
   which specific operations allocate the most.

## Decision Criteria

Allocation hotspot identification is successful when it:

- Identifies the code paths responsible for the majority of allocations
- Provides quantitative allocation data (bytes per invocation, allocation rate)
- Enables before/after comparison to validate optimization impact
- Distinguishes between necessary allocations and optimization opportunities
- Produces reproducible measurements across runs

## Common Variations

- **Function-level profiling**: Compare allocation across different functions
- **Implementation comparison**: Measure allocation of alternative approaches
- **Library evaluation**: Assess allocation characteristics of dependencies
- **Refactoring validation**: Verify allocation reduction after optimization
- **Data structure selection**: Compare allocation patterns of different collections
- **Algorithm analysis**: Understand allocation characteristics of different algorithms
- **API design**: Measure allocation impact of different API designs (streaming vs. batch)
