(ns warmup
  "Understanding JIT warmup and how criterium options control it."
  (:require
   [criterium.bench :as bench]
   [criterium.notebook.helpers :refer [bench-display]]))

;; # JIT Warmup
;;
;; The JVM doesn't execute your code directly. It interprets bytecode, then
;; compiles frequently-executed paths to native machine code. This process
;; significantly affects benchmark results.

;; ## How the JIT Compiler Works
;;
;; When you run Clojure code, the JVM goes through several compilation stages:
;;
;; | Stage | Description | Typical Speed |
;; |-------|-------------|---------------|
;; | Interpreter | Direct bytecode execution | 1x (baseline) |
;; | C1 (Client) | Quick compilation, basic optimizations | 5-10x faster |
;; | C2 (Server) | Aggressive optimization, slower compilation | 10-100x faster |
;;
;; The JVM uses **tiered compilation** by default, progressing through these
;; stages based on how often code executes.

;; ### Compilation Thresholds
;;
;; The JIT tracks method invocations and loop iterations. When counts exceed
;; thresholds, compilation triggers:
;;
;; - **Tier 1 (C1)**: ~1,500 invocations
;; - **Tier 4 (C2)**: ~10,000 invocations
;;
;; These are approximate—the JVM adjusts based on system load and method
;; complexity.

;; ### Key Optimizations
;;
;; C2 performs aggressive optimizations that dramatically affect performance:
;;
;; **Inlining**: Small methods are copied into callers, eliminating call
;; overhead and enabling further optimizations. Clojure's small functions
;; benefit greatly from inlining.
;;
;; **Escape Analysis**: Objects that don't "escape" their scope can be
;; allocated on the stack or eliminated entirely.
;;
;; **Loop Unrolling**: Small loops are expanded to reduce branch overhead.
;;
;; **Dead Code Elimination**: Unreachable code and unused computations are
;; removed. This is why benchmarks must use their results—otherwise the JIT
;; may optimize away what you're trying to measure.

;; ### On-Stack Replacement (OSR)
;;
;; Long-running loops can be compiled while still executing. The JVM replaces
;; the interpreted loop with compiled code mid-execution. This is crucial for
;; benchmarks that run many iterations in a single method.

;; ### Deoptimization
;;
;; The JIT makes optimistic assumptions. When assumptions are violated, code
;; is "deoptimized" back to interpretation:
;;
;; - A method assumed monomorphic receives a different type
;; - An inlined method is redefined
;; - A guard condition fails
;;
;; Deoptimization causes sudden performance drops and is one reason benchmark
;; results can be unstable.

;; ## Why Warmup Matters
;;
;; Without warmup, you measure a mix of interpreted and compiled code:

^:kindly/hide-code
(bench-display
 (bench/bench (reduce + (range 100))
              :collect-plan :one-shot))

;; This single-shot measurement captures whatever state the JVM happens to be
;; in. The code might be interpreted, partially compiled, or fully optimized.

;; With warmup, criterium ensures you measure optimized code:

^:kindly/hide-code
(bench-display
 (bench/bench (reduce + (range 100))))

;; The default benchmark runs ~150,000 warmup iterations before measuring,
;; giving the JIT time to compile and optimize.

;; ## Criterium Warmup Controls
;;
;; ### No Warmup: `:collect-plan :one-shot`
;;
;; For measuring cold-start or interpreted performance:

^:kindly/hide-code
(bench-display
 (bench/bench (reduce + (range 100))
              :collect-plan :one-shot))

;; Use this when:
;; - Measuring startup/initialization code
;; - Testing code that runs once per JVM lifetime
;; - Simulating cold-cache scenarios
;;
;; The result is a single sample—no statistical analysis, no warmup.

;; ### Time-Limited Benchmarking: `:limit-time-s`
;;
;; Control warmup duration via the total time budget:

^:kindly/hide-code
(bench-display
 (bench/bench (reduce + (range 100))
              :limit-time-s 2))

;; Criterium allocates this time across estimation, warmup, and measurement
;; phases. The default is 10 seconds. Shorter budgets reduce warmup iterations
;; proportionally.
;;
;; **When to reduce time:**
;; - Quick iteration during development
;; - Simple code that optimizes quickly
;; - Time-constrained benchmark suites
;;
;; **When to increase time:**
;; - Complex code with many branches
;; - Polymorphic dispatch (multiple types through same code path)
;; - Code that triggers deoptimization cycles

;; ### Varied Warmup Inputs: `:warmup-args-fn`
;;
;; The JIT optimizes based on observed inputs. If warmup always uses identical
;; data, the JIT may over-specialize. The `:warmup-args-fn` option provides
;; varied inputs during warmup.
;;
;; See the [Warmup notebook](./criterium.warmup_notebook.html) for detailed
;; coverage of varied warmup inputs and integration with test.check generators.

;; ## Matching Warmup to Your Goals
;;
;; Choose warmup based on what you're trying to measure:
;;
;; | Goal | Warmup Setting | Rationale |
;; |------|----------------|-----------|
;; | Hotpath performance | Default (10s budget) | Matches production steady-state |
;; | Startup/one-shot | `:collect-plan :one-shot` | Measures cold performance |
;; | Quick iteration | `:limit-time-s 2` | Fast feedback during development |
;; | Thorough analysis | `:limit-time-s 30` | More warmup for complex code |
;;
;; **Hotpath code**: Functions called millions of times in production deserve
;; full warmup. The JIT will optimize them aggressively in production, so your
;; benchmark should match.
;;
;; **Occasional code**: Functions called infrequently may never reach C2
;; compilation in production. Reduced warmup better reflects their actual
;; performance.
;;
;; **Startup code**: Initialization runs once. Measuring with warmup would
;; misrepresent actual user experience.

;; ## Observing JIT Behavior
;;
;; To see JIT compilation during benchmarking, add these JVM flags:
;;
;; ```
;; -XX:+PrintCompilation
;; -XX:+UnlockDiagnosticVMOptions
;; -XX:+PrintInlining
;; ```
;;
;; You'll see output like:
;;
;; ```
;; 1234  4     3       clojure.core$reduce (...)
;; 1256  4     4       clojure.core$reduce (...)
;; ```
;;
;; The columns show: timestamp, compilation ID, tier level, and method name.
;; Tier 4 entries indicate C2 compilation—your code is fully optimized.

;; ## Summary
;;
;; - The JVM compiles code progressively: interpreter → C1 → C2
;; - Without warmup, benchmarks measure partially-optimized code
;; - Use `:collect-plan :one-shot` for cold-start measurements
;; - Use `:limit-time-s` to control benchmark duration (affects warmup)
;; - Match warmup to how your code runs in production
