# Criterium Usage Scenarios

Document common usage scenarios for criterium benchmarking. Each scenario
describes WHAT is needed to support a use case, serving as requirements
analysis for LLM agents.

## Core Scenarios

### 1. Compare Implementations
Evaluate alternative implementations to select the most performant.

**Goal:** Determine which implementation to use based on performance.

[Compare implementations](use-cases/compare-implementations.md)

### 2. Optimize an Existing Implementation
Iteratively improve performance of existing code.

**Goal:** Reduce execution time or resource usage through measured changes.

[Optimize existing implementation](use-cases/optimize-implementation.md)

### 3. Document Implementation Characteristics
Establish baseline performance metrics for documentation.

**Goal:** Record authoritative performance data for an implementation.

[Document implementation characteristics](use-cases/document-characteristics.md)

### 4. Track Performance Over Time
Monitor how performance changes across code versions.

**Goal:** Maintain visibility into performance trends.

[Track performance over time](use-cases/track-performance.md)

### 5. Check for Performance Regressions
Validate that changes don't degrade performance.

**Goal:** Prevent unintended performance degradation from reaching production.

[Check for performance regressions](use-cases/check-regressions.md)

## Additional Scenarios

### 6. Tune JVM/GC Settings
Optimize JVM configuration for specific workloads.

**Goal:** Find optimal JVM parameters for a given application profile.

[Tune JVM/GC settings](use-cases/tune-jvm-gc.md)

### 7. Identify Memory Allocation Hotspots
Locate code paths with excessive allocation.

**Goal:** Find allocation-heavy code for optimization.

[Identify memory allocation hotspots](use-cases/allocation-hotspots.md)

### 8. Validate Performance Assumptions During Code Review
Verify performance claims in pull requests.

**Goal:** Make evidence-based decisions during code review.

[Validate performance assumptions](use-cases/validate-assumptions.md)

### 9. Size Infrastructure/Capacity Planning
Estimate resource requirements for deployment.

**Goal:** Make informed infrastructure decisions based on measured performance.

[Size infrastructure/capacity planning](use-cases/capacity-planning.md)

### 10. Benchmark Third-Party Libraries
Evaluate library performance before adoption.

**Goal:** Select libraries based on measured performance characteristics.

[Benchmark third-party libraries](use-cases/benchmark-libraries.md)

### 11. Measure Impact of Dependency Upgrades
Assess performance effects of version changes.

**Goal:** Detect performance changes from dependency updates.

[Measure dependency upgrade impact](use-cases/dependency-upgrades.md)
