# Optimize Existing Implementation

Iteratively improve performance of an existing implementation through measurement-guided changes.

## Problem Statement

An implementation exists and functions correctly but needs to be faster or use
fewer resources. Without measurement, optimization efforts may target the wrong
areas, introduce regressions, or fail to achieve meaningful improvement. "Premature
optimization is the root of all evil" - but measured, targeted optimization is
essential for performance-critical code.

## Goal

Systematically improve performance by identifying bottlenecks, measuring the
impact of changes, and verifying improvements are real and maintained.

## Key Components Required

### Baseline Measurement
- Establish current performance with statistical confidence
- Capture multiple metrics (time, allocation, GC pressure)
- Document measurement conditions for reproducibility
- Serve as reference point for all subsequent measurements

### Change Impact Measurement
- Measure after each modification
- Compare against baseline with statistical significance testing
- Detect regressions immediately (changes that make things worse)
- Quantify improvement magnitude

### Bottleneck Identification
- Understand where time is spent before optimizing
- Distinguish between algorithmic and implementation inefficiencies
- Identify allocation hotspots contributing to GC pressure
- Recognize JIT compilation behavior affecting measurements

### Iteration Support
- Ability to checkpoint and restore measurement state
- Track cumulative improvement across multiple changes
- Compare any two versions in optimization history
- Maintain measurement consistency across iterations

## Logical Dependencies

1. **Correct baseline** - Optimization requires a stable, reproducible
   baseline. Flaky baselines make it impossible to determine if changes
   helped or hurt.

2. **Incremental changes** - Each optimization should be measured
   independently. Combining multiple changes obscures which ones helped
   and by how much.

3. **Statistical significance** - Small improvements may be noise.
   Changes should only be kept when the improvement exceeds measurement
   variance with confidence.

4. **Consistent conditions** - Comparisons between optimization steps
   must use identical measurement conditions. Changing parameters between
   measurements invalidates comparisons.

5. **Regression detection** - Optimizations can have unintended effects.
   Secondary metrics must be monitored to catch tradeoffs (e.g., faster
   but uses more memory).

## Decision Criteria

An optimization cycle is complete when:

- Target performance is achieved, OR
- Further changes show diminishing returns below measurement noise
- All changes show measurable, statistically significant improvement
- No regressions in secondary metrics (or acceptable tradeoffs documented)

## Common Variations

- **Hot path optimization**: Focus on most frequently executed code
- **Memory optimization**: Reduce allocations to decrease GC pressure
- **Latency optimization**: Reduce worst-case times, not just averages
- **Throughput optimization**: Maximize work per unit time
- **Startup optimization**: Reduce time to first useful result
