# Task: Persistent agent extraction + non-silent error handling

Status: implemented (Part A + Part B)
Area: `bases/agent` (native agent loader / runtime)
Related files:
- `bases/agent/src/clj/criterium/agent/loader.clj`
- `bases/agent/src/clj/criterium/agent/platform.clj`
- `bases/agent/src/clj/criterium/agent/runtime.clj`
- `bases/agent/src/clj/criterium/agent.clj`
- `bases/agent/test/criterium/agent/loader_test.clj`
- `bases/agent/test/criterium/agent/platform_test.clj`
- `bases/agent/test/criterium/agent/degradation_test.clj`
- `bases/agent/test/criterium/agent/integration_test.clj`

## Background / motivation

Two defects in the bundled-agent extraction path were found while debugging a
user report (published jar `org.hugoduncan/criterium {:mvn/version "0.5.245-ALPHA"}`):

> The path reported by `(first (criterium.agent/jvm-opts))` did not exist.

### Defect 1 — extraction is deleted out from under its documented use

`criterium.agent/jvm-opts` → `runtime/agent-path` → `loader/extract-agent`.
`extract-agent` extracts the platform binary to a content-addressed temp file
**and registers a JVM shutdown hook that deletes that file on exit**
(`register-cleanup-hook!` / `extracted-agents` in `loader.clj`).

The documented workflow for `jvm-opts` (see `criterium/agent.clj`) is:

> "use (jvm-opts) to get the correct path, then restart your JVM with that option."

These are incompatible:

1. JVM **A** calls `(jvm-opts)` → extracts `/tmp/criterium-agent-<hash>.dylib`,
   returns `["-agentpath:/tmp/criterium-agent-<hash>.dylib"]`.
2. Caller launches JVM **B** with that `-agentpath`.
3. JVM **A** exits → shutdown hook deletes the file.
4. JVM **B** tries to load `-agentpath` **at launch** (before any Clojure runs,
   so it cannot self-extract) → file missing → failure.

The path is stable (SHA256-named) so it *looks* valid, but the file is ephemeral
and tied to the lifetime of the producing JVM.

Reproduced against the published jar on macos-arm64: the file exists during the
producing JVM's life (full 112432 bytes) and is gone the instant it exits.

### Defect 2 — silent failures make this class of bug undiagnosable

`extract-with-lock` wraps the entire extraction in `(catch Exception _e false)`.
Any failure (locking unsupported, `noexec` tmpdir failing `verify-permissions!`,
disk full, atomic-move across filesystems, permission errors, etc.) is swallowed
and surfaces only as `extract-agent` returning `nil`. Several other stages also
fail silently or ambiguously:

- `platform/detect` → `nil` for unsupported platform (legitimate) is
  indistinguishable from a misdetection bug; no diagnostic about `os.name`/`os.arch`.
- binary-resource lookup `(io/resource resource-path)` → `nil` returns `nil` with
  no message (packaging problem looks identical to "unsupported").
- `read-hash` throws for a missing `.sha256` but the binary-missing case is silent.
- `runtime/agent-path` catches `Exception`, prints a one-line WARNING to stdout,
  and returns `nil`, discarding the cause/stack.
- The SHA256 is only used to *name* the temp file; extracted bytes are never
  verified, so a corrupt/truncated bundled binary extracts "successfully" and
  fails later at load time.

## Goal

A. Make extraction **persistent** so the `jvm-opts` / `-agentpath` workflow works:
   the extracted binary must outlive the producing JVM. Cleanup-on-exit becomes
   opt-in and is not used by the default `agent-path` path.

B. **Eliminate silent failures** at every extraction stage. Every failure must be
   diagnosable: it either throws a structured error or is logged with full context
   (stage + cause + relevant paths). "Unsupported platform" remains a legitimate,
   clearly-signalled non-error.

## Part A — Persistent extraction

### Design

1. Remove the unconditional shutdown-hook deletion from the default path.
   - `extract-agent` (default, used by `runtime/agent-path`) extracts to the
     content-addressed temp file and **does not** register a deletion hook.
   - The SHA256-named path makes the file self-deduplicating across runs and JAR
     versions, so leaving it in place is safe (at most one stale file per
     platform+build in `java.io.tmpdir`).
2. Keep cleanup as an explicit opt-in for genuinely ephemeral, in-process use:
   - Provide an arity/option, e.g. `(extract-agent {:cleanup-on-exit? true})`,
     that registers the existing shutdown-hook behavior.
   - Default is `:cleanup-on-exit? false`.
   - Audit shows current built-in callers (`load-agent!`, `jvm-opts`) do **not**
     need deletion, so both use the persistent default.
3. Keep the existing reuse-if-present and concurrent-safe locking behavior.

### Optional (recommended) follow-up

Consider extracting to a stable per-user cache dir (e.g.
`<user.home>/.cache/criterium/agent/<hash>/…`) instead of `java.io.tmpdir`, with
versioned GC, so the binary survives temp reapers and reboots. Out of scope for
the first cut but note it in the docstring/decision log.

### Acceptance (A)

- After `(criterium.agent/jvm-opts)` returns in JVM A and JVM A exits, the file
  referenced by the returned `-agentpath:` string still exists and is loadable by
  a freshly launched JVM B.
- `load-agent!` still works (loads into the current JVM).
- Repeated calls across processes reuse the same file; no unbounded growth beyond
  one file per platform+build hash.

## Part B — Non-silent error handling at every stage

### Principles

- Distinguish three outcomes explicitly:
  - **success** → absolute path;
  - **unsupported** (platform not in the supported set) → `nil`, with a single
    clear debug/info message including `os.name` and `os.arch`. This is the only
    legitimate `nil`.
  - **failure** (anything that *should* have worked but didn't) → never silent.
    Throw a structured `ex-info`, or log an error with full context and return a
    value the caller can distinguish from "unsupported".
- Never use a bare `(catch Exception _ false)` / `(catch Exception _ nil)` that
  discards the cause anywhere in the extraction path.
- Errors carry structured context: `{:stage <kw> :platform <s> :resource-path <s>
  :target-path <s> :tmpdir <s>}` plus the original throwable as `:cause`.

### Stage-by-stage requirements

1. **platform/detect**
   - On unsupported, expose *why*: which of `os.name`/`os.arch` was unrecognized.
   - Add a helper (e.g. `platform/describe`) returning the raw + canonical values
     for logging, so a misdetected platform is diagnosable, not silent.

2. **read-hash** (`:stage :read-hash`)
   - Keep throwing on missing `.sha256`, but include `resource-path`, the hash
     resource path, and a packaging-fix pointer in `ex-info` data (not just a
     string).

3. **binary resource lookup** (`:stage :resolve-binary`)
   - `(io/resource resource-path)` → `nil` must raise a clear failure: "agent
     binary not found in JAR resources at `<resource-path>`" with `ex-info` data
     and the packaging-doc pointer. Must NOT be confused with "unsupported".

4. **extract-with-lock** — replace the blanket catch with per-step context.
   Each sub-step records its stage so the surfaced error names the failing
   operation. Sub-stages and their `:stage` keys:
   - `:lock-dir-create` — `mkdirs` of lock parent
   - `:open-lock` — `FileChannel/open` of lock file (e.g. locking unsupported on FS)
   - `:acquire-lock` — `.lock`
   - `:create-temp` — `Files/createTempFile`
   - `:copy` — resource → temp copy (e.g. ENOSPC)
   - `:atomic-move` — `Files/move` (e.g. `AtomicMoveNotSupportedException` across FS)
   - `:set-executable` — POSIX perms (only `UnsupportedOperationException` is a
     benign no-op; anything else is a failure, not swallowed)
   - `:verify-permissions` — readable/executable check (e.g. `noexec` tmpdir);
     error message must explicitly mention a possible `noexec` mount
   - `:unlock-cleanup` — lock file deletion failure should warn, not abort
   - Wrap each step so the thrown `ex-info` includes `:stage` and `:cause`.
     Do not collapse to `false`.

5. **integrity verification** (`:stage :verify-hash`) — NEW
   - After extraction, compute SHA256 of the extracted file and compare to the
     expected hash from `read-hash`. On mismatch, fail with both hashes in the
     `ex-info` data and delete the bad file. (Promotes the hash from "naming only"
     to actual integrity checking.)

6. **runtime/agent-path**
   - Replace the lossy `(catch Exception e (println "WARNING:" ...) nil)`:
     - Re-throw or log (via the project's logging mechanism, not bare
       `println` to stdout) with full stage context and stack.
     - Preserve the existing "return `nil` for unsupported platform" contract, but
       only for the genuine unsupported case — failures must be visible.
   - Keep `jvm-opts` returning `[]` only for the unsupported case; failures should
     surface, not masquerade as "no agent".

### Logging

Use the project's existing logging facility if one exists; otherwise emit to
`*err*` (never `*out*`) and include stage + context. Confirm the convention
before implementing and keep it consistent across the loader/runtime.

### Acceptance (B)

- A forced failure at each stage produces a distinct, identifiable error/log
  naming the stage and cause (covered by tests below). No path returns a bare
  `nil`/`false` without a corresponding diagnostic, except genuine "unsupported
  platform", which logs a clear single message.
- A corrupted bundled binary (hash mismatch) is detected and reported, not loaded.
- `noexec` tmpdir produces a message that explicitly suggests the `noexec` cause.

## Tests

Add/extend under `bases/agent/test/criterium/agent/`:

- `loader_test.clj`
  - persistence: extracted file is NOT deleted on simulated shutdown for the
    default path; IS deleted when `:cleanup-on-exit? true`.
  - reuse: second extraction returns the same path without re-copying.
  - hash mismatch: corrupt the extracted/source bytes → `:verify-hash` failure,
    bad file removed.
  - per-stage failures via injection (redef/`with-redefs` the relevant fs ops or
    point at a read-only / `noexec` temp dir): assert each `:stage` surfaces.
  - missing binary resource (hash present, binary absent) → `:resolve-binary`
    failure (currently silent `nil`).
- `degradation_test.clj`
  - unsupported platform still returns `nil` with a clear, asserted log message,
    distinct from failure.
- `integration_test.clj`
  - end-to-end: `jvm-opts` path survives producing-JVM exit (spawn a child JVM
    that loads `-agentpath` from a path produced by a separate process), gated on
    the binary being present like the existing integration guard.

## Out of scope / flag separately

> Note: the resource-path inconsistency below has since been **resolved** — see
> "Follow-ups (now done)" near the end of this document.

- **Resource-path convention inconsistency** (discovered while investigating):
  - `docs/contributor/building-agent.md` and `build/src/build/agent.clj`
    (`resources-base-dir` → `bases/agent/resources/native/{platform}`) document/use
    a `native/{platform}/…` layout.
  - The runtime loader (`platform/resource-path`) and its tests use
    `criterium/agent/{platform}/…`, which is what the published jar actually
    contains.
  - These disagree. The release path works (jar has `criterium/agent/…`), so this
    is latent, but the `native/` references should be reconciled to avoid future
    breakage. Track as a separate cleanup task.

## Definition of done

- [x] Default `extract-agent` does not delete on JVM exit; cleanup is opt-in
      (`{:cleanup-on-exit? true}`).
- [x] `jvm-opts` / `-agentpath` workflow verified across separate JVMs
      (file persists after the producing JVM exits).
- [x] No silent `catch`/`nil`/`false` anywhere in the extraction/runtime path;
      every failure is a structured `ex-info` (carrying `:stage` +
      `:criterium.agent/extraction-failure` + context) or logged-with-context to
      stderr. The `extract-with-lock` blanket `(catch Exception _ false)` is gone.
- [x] SHA256 integrity check on extracted bytes (`verify-hash!`, stage
      `:verify-hash`), bad bytes deleted before publishing.
- [x] Unsupported-platform path remains a `nil`, now with a clear stderr message
      including `os.name`/`os.arch` via `platform/describe`.
- [x] Tests cover persistence, reuse, hash mismatch, missing binary, per-stage
      failures (`:copy`, `:verify-hash`, `:resolve-binary`), and the existing
      cross-JVM integration case.
- [x] Docstrings in `loader.clj`, `runtime.clj`, `agent.clj` updated to reflect
      persistent extraction and the new error contract.

### Implementation notes

- `runtime/agent-path` logs failures richly to stderr (stage + context) and
  still returns `nil`, deliberately preserving the graceful degradation of
  higher-level allocation tracking (`with-allocation-tracing` etc.) rather than
  throwing. The failure is now visible (non-silent) instead of a bare one-line
  stdout WARNING. If callers later want hard failures to surface, `extract-agent`
  itself now throws structured errors that `agent-path` could re-raise.
- Stages implemented in `extract-with-lock`: `:lock-dir-create`, `:open-lock`,
  `:acquire-lock`, `:create-temp`, `:copy`, `:verify-hash`, `:atomic-move`,
  `:set-executable`, `:verify-permissions`. The fragile filename-regex used to
  derive the file extension was replaced with the `platform` argument.
- All agent test namespaces pass; namespaces compile clean under
  `*warn-on-reflection*`.

### Follow-ups (now done)

- Explicit, fail-loud extract API added: `criterium.agent/extract-agent!`
  (delegating to `criterium.agent.runtime/extract-agent!`). It performs the same
  persistent extraction as `agent-path` but *throws* the structured per-stage
  error instead of degrading to nil, for tooling that must fail clearly.
  Accepts the same `{:cleanup-on-exit? bool}` option. Documented with the full
  error/stage contract; covered by `degradation-test/explicit-extract-test`.

- Resource-path convention inconsistency resolved. The canonical location (used
  by `.github/workflows/release.yml`) is
  `bases/criterium/resources/criterium/agent/{platform}/libcriterium.{ext}`,
  which resolves on the classpath to `criterium/agent/{platform}/...` — exactly
  what `platform/resource-path` reads. The stale `native/` references were
  corrected in `build/src/build/agent.clj` (`resources-base-dir`,
  `validate-agent-binaries!`), `.gitignore`, `docs/contributor/building-agent.md`,
  `projects/agent/README.md`, and the `integration_test` namespace comment.
