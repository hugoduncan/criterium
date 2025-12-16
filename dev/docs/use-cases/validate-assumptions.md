# Validate Performance Assumptions

Verify or refute intuitive beliefs about code performance before committing to implementation decisions.

## Problem Statement

Developers frequently make assumptions about relative performance of different
approaches: "HashMap is faster than TreeMap for our use case," "lazy sequences
are slower than eager evaluation here," "this regex is expensive." These
assumptions influence design decisions, code reviews, and refactoring choices.
However, without measurement, these assumptions may be incorrect due to JIT
optimizations, runtime behavior differences from expectations, or failure to
account for actual usage patterns. Acting on false assumptions leads to
unnecessary complexity, premature optimization, or missed optimization
opportunities.

## Goal

Provide rapid, evidence-based validation of performance hypotheses during code
review or design discussions, enabling confident decisions about implementation
approaches backed by empirical data rather than intuition alone.

## Key Components Required

### Quick Measurement Capability

- Fast feedback cycle suitable for interactive decision-making
- Sufficient statistical rigor to distinguish real differences from noise
- Minimal setup overhead to encourage frequent use
- Results available within seconds to minutes, not hours

### Hypothesis Framing

- Clear articulation of the assumption being tested
- Definition of what outcome would validate vs. refute the assumption
- Identification of the relevant performance dimension (time, memory, throughput)
- Specification of conditions under which the assumption should hold

### Comparative Measurement

- Direct comparison between the assumed-fast and assumed-slow approaches
- Consistent measurement conditions across compared alternatives
- Statistical significance testing or confidence intervals
- Clear presentation of which assumption (if either) the data supports

### Context Sensitivity

- Ability to test assumptions under specific conditions (input sizes, data patterns)
- Support for varying parameters to find where assumptions break down
- Recognition that assumptions may hold in some contexts but not others
- Identification of crossover points where relative performance changes

### Evidence Packaging

- Shareable results suitable for code review discussions
- Clear documentation of measurement conditions and methodology
- Reproducible measurements that others can verify
- Summary conclusions that directly address the original hypothesis

## Logical Dependencies

1. **Comparable implementations** - Both the assumed-fast and assumed-slow
   approaches must be measurable. This may require writing small benchmark
   expressions that isolate the specific behavior in question.

2. **Representative conditions** - The measurement conditions must reflect the
   actual usage context where the assumption matters. Testing with wrong input
   sizes or data patterns can validate assumptions that don't hold in practice.

3. **Statistical discrimination** - The measurement must have sufficient
   precision to distinguish between alternatives. If the difference is smaller
   than measurement noise, the assumption cannot be validated or refuted.

4. **JVM steady-state** - Performance assumptions often apply to warmed-up code.
   Measurements must account for JIT compilation to avoid testing interpreted
   behavior when compiled behavior matters.

5. **Clear decision criteria** - Before measuring, define what performance
   difference would be meaningful. A 5% difference might not matter; a 5x
   difference certainly does.

## Decision Criteria

Performance assumption validation is successful when it:

- Provides a clear answer: assumption supported, refuted, or inconclusive
- Quantifies the actual difference between approaches
- Identifies conditions where the assumption holds or fails
- Enables confident decision-making in code review or design
- Produces evidence that can be shared and discussed with team members
- Can be repeated if assumptions need re-validation after changes

## Common Variations

- **Code review validation**: Quick check of reviewer's performance concerns
- **Design decision support**: Compare approaches before committing to one
- **Optimization targeting**: Verify assumed bottleneck before optimizing
- **Library selection**: Validate assumptions about library performance characteristics
- **Data structure choice**: Test assumptions about collection performance
- **Algorithm comparison**: Verify expected complexity differences materialize
- **Configuration validation**: Test assumptions about JVM flag or setting impacts
- **Concurrency assumptions**: Validate beliefs about thread-safety vs. performance tradeoffs
