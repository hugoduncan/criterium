# Track Performance Over Time

Monitor how performance changes across code versions, releases, or time periods.

## Problem Statement

Software performance evolves as code changes. New features, dependency updates,
refactoring, and bug fixes can all affect performance in ways that are not
immediately apparent. Without systematic tracking, performance degradation
accumulates gradually until it becomes a crisis. Conversely, performance
improvements may go unrecognized, and successful optimizations may be later
undone without notice.

## Goal

Establish a historical record of performance measurements that reveals trends,
enables comparison across arbitrary versions, and provides early warning of
degradation before it impacts users.

## Key Components Required

### Durable Storage

- Persistent storage of benchmark results across sessions
- Association of results with code versions (commit, tag, release)
- Retention policy appropriate to monitoring timeframe
- Storage format that supports efficient querying and comparison

### Consistent Measurement Protocol

- Identical measurement parameters across all tracked runs
- Documented and reproducible benchmark conditions
- Stable benchmark definitions that measure the same thing over time
- Version control of benchmark code itself

### Temporal Association

- Timestamps for all measurements
- Code version identification (git commit SHA, tag, or version number)
- Ability to correlate performance changes with code changes
- Support for both scheduled and ad-hoc measurements

### Trend Analysis

- Visualization of performance over time
- Detection of gradual degradation (regression creep)
- Identification of step changes correlated with specific commits
- Statistical methods to distinguish trends from noise

### Alerting and Notification

- Thresholds for acceptable performance bounds
- Notification when bounds are exceeded
- Configurable sensitivity to avoid alert fatigue
- Integration with existing monitoring or CI systems

## Logical Dependencies

1. **Baseline establishment** - Tracking is only meaningful relative to
   a known starting point. Initial measurements establish the reference
   for detecting change.

2. **Measurement consistency** - Comparisons across time are only valid
   when measurement conditions are equivalent. Hardware changes, JVM
   version updates, or benchmark parameter changes invalidate historical
   comparisons.

3. **Version correlation** - Performance data without associated code
   versions is of limited value. The ability to answer "what changed?"
   requires mapping performance changes to code changes.

4. **Noise filtering** - Normal measurement variance must be distinguished
   from real performance changes. Statistical methods are required to
   avoid false positives (reporting degradation that isn't real) and
   false negatives (missing real degradation hidden in noise).

5. **Long-term stability** - Storage format and benchmark definitions
   must remain stable enough that measurements from months or years ago
   remain comparable to current measurements.

## Decision Criteria

Performance tracking is effective when it enables:

- Detection of degradation within acceptable time bounds (before release, before significant accumulation)
- Attribution of performance changes to specific code changes
- Confident assertions about long-term performance trends
- Historical queries ("when did this get slow?" / "was this always this fast?")

## Common Variations

- **Release-to-release tracking**: Compare performance between tagged releases
- **Continuous tracking**: Measure every commit or merge to main branch
- **Periodic sampling**: Weekly or monthly measurements for long-term trends
- **Multi-environment tracking**: Track performance across different hardware or JVM configurations
- **Capacity trend analysis**: Project future resource needs based on performance trends
