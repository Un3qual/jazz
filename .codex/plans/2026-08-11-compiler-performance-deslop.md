# Compiler performance branch de-slop follow-up

Status: Active
Branch: `codex/compiler-performance-program`
Base: `codex/typed-core-closure-recursion` at `6e3381f285357b4f9113589b318f8b0feb049ce2`

## Objective

Remove branch-introduced invariant leaks, residual asymptotic work, misleading
benchmark ownership, and incidental abstraction without changing Jazz language
semantics or sacrificing the measured performance wins.

## Constraints

- Preserve diagnostics, source ordering, binder identity, exact artifacts, and
  hosted/runtime behavior.
- Keep one heavyweight Cabal, Jazz, benchmark, profiling, or Nix process active
  at a time, always with bounded jobs.
- Prefer behavior and artifact tests over source-name or implementation-shape
  assertions.
- Do not split modules solely because they are large. Keep the fused resolver,
  vector token stream, prepared recursive-scope product, validation proof,
  lowering indexes, difference-list diagnostics, and explicit benchmark forcing
  boundaries whose responsibilities and performance evidence are concrete.

## Batch 1: proof and runtime representation invariants

- Make `TypedCoreProductionResult` opaque and encode success plus its validated
  proof atomically; keep the raw status and inference result as derived accessors.
- Remove the unused raw-status-only finalizer.
- Make the evaluator constructor view non-forgeable and remove the redundant
  constructor-argument count while retaining append and count as O(1) `Seq`
  operations.
- Move production runtime observations off the compatibility list view where a
  sequence/count observer avoids allocation.

Focused gates: typed-core direct-call, runtime semantics, wide-constructor, and
program-corpus owners.

## Batch 2: inference ownership and asymptotics

- Bind the outer-name projection into `PreparedRecursiveScope` and reject a
  prepared scope consumed under a different visibility projection.
- Replace recursive-group numeric-range enumeration with a source-order interval
  sweep over actual let indices.
- Incrementally extend `TypeEnvFreeVariables` for data constructors instead of
  rescanning the whole environment after each data declaration.
- Deep-force finalized diagnostic values, but not the source AST, before the
  ordinary analyzer pass so error thunks cannot retain the solver state.
- Keep solver updates compositional and remove redundant list-arity scans.

Focused gates: recursive bindings, inference ownership, capability/type-data,
solver, and compiler profiling owners.

## Batch 3: benchmark boundary truthfulness

- Narrow prepared benchmark constructors to only timed inputs; force and discard
  setup-only source/program graphs before returning.
- Replace `rnf . show` Typed Core forcing with an explicit structural force and
  poison-field characterization.
- Give direct analyzer-diagnostic work its own honest benchmark group and stage
  metadata; make validation stages match the work actually timed.
- Make direct-artifact scenario dispatch explicit and total.
- Finalize recorded metadata only for successful benchmark runs.

Focused gates: benchmark stage and profiling specifications plus recorder failure
paths. Physical results remain advisory artifacts, never deterministic timings.

## Batch 4: verification and readability cleanup

- Ensure the repository verification phase never performs a hidden Cabal build;
  pass one prebuilt Jazz executable into example checks and exercise the child
  script with command stubs.
- Remove Template Haskell negative symbol assertions and their dependency; retain
  the positive prepared-scope semantic ownership regressions.
- Make operator-table fields and simple accumulator folds strict.
- Remove dispatch-only wrappers (`AnalysisTarget`, `ExpressionCheck`) and other
  zero-invariant helpers where existing semantic tests own behavior.
- Avoid replacing the CI policy checker with another home-grown parser; add a
  concrete heredoc/scope regression only where the current policy contract needs
  it.

Focused gates: CI policy, main-functional script, parser/operator, analyzer, and
Lowered IR lowering owners.

## Closeout

After the final source change: run affected subsystem gates once, run the single
authoritative full main-functional closeout, obtain an independent whole-diff
review, update this plan with evidence/dispositions, commit, and push the PR
branch.
