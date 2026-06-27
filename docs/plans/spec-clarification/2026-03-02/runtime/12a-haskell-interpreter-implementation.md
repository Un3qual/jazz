# Haskell Interpreter Implementation Plan (Legacy Closure)

> **Legacy closure note (2026-06-24):** this plan is reference-only. The
> original implementation slices target `jazz-hs` runtime, CLI, and test files,
> but `jazz-hs/` is a read-only legacy reference under the current workspace
> policy. New runtime planning and execution work must use
> `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`.

**Closed decision:** active interpreter-backed execution is owned by
`jazz-next`. This legacy plan must not promote `jazz-hs` implementation work,
add a second runtime path, or change the active compile/run/help contract.

**Current authority:** `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`
defines the active runtime pipeline, file owners, closed compile/run/help
baseline, and future-runtime-delta gate.

**Historical goal:** Implement a working Haskell interpreter as the primary
Jazz execution backend, replacing legacy JS codegen as the active runtime path.

**Architecture:** Deliver interpreter functionality in vertical slices: runtime values -> evaluator core -> builtins/environment -> control/data features -> CLI integration. Keep analyzer checks in place and use tests-first progression.

**Historical tech stack:** Haskell (`jazz-hs`), `stack` tests, Nix shell
reproducibility, existing AST/parser/analyzer modules. These remain historical
evidence only.

---

## Progress

- [x] Runtime strategy precondition set (`interpreter-only`) in item #12
- [x] Active runtime architecture replaced this legacy plan with
  `jazz-next` owners
- [x] Legacy `jazz-hs` interpreter implementation slices closed as
  reference-only (2026-06-24)
- [x] Future runtime work routed through the active runtime product blocker
  and concrete `jazz-next` deltas

## Scope Guardrails

Active closure scope:

- preserve this plan as historical runtime-slice evidence,
- keep active interpreter-backed execution routed through `jazz-next`,
- keep future runtime product work blocked until a concrete active delta is
  accepted.

Out of scope:

- editing `jazz-hs/` or `jazz2/`,
- adding a second runtime path,
- changing active compile/run/help semantics,
- promoting legacy runtime implementation phases.

## Superseded Legacy Execution Phases

The phase plan below is preserved as historical context only. Do not execute
these phases as active work. Any future runtime product change must start from
the active `jazz-next` runtime architecture plan and a queue-promoted concrete
delta.

## Phase 0: Baseline + Scope Freeze

- [ ] Confirm parser/analyzer baseline test status before interpreter changes.
- [ ] Freeze first runtime feature slice with explicit included/excluded constructs.
- [ ] Create interpreter test module scaffold.

Create/Modify:
- `jazz-hs/test/InterpreterSpec.hs`
- `jazz-hs/test/Spec.hs`

Commit checkpoint:

```bash
git add jazz-hs/test/InterpreterSpec.hs jazz-hs/test/Spec.hs docs/plans/spec-clarification/2026-03-02/runtime/12a-haskell-interpreter-implementation.md
git commit -m "test(interpreter): add interpreter test scaffold and scope freeze"
```

## Phase 1: Runtime Values + Minimal Evaluator

- [ ] Define runtime value ADT (ints, floats, bools, strings, lists, functions, constructors).
- [ ] Implement minimal expression evaluator for literals/variables/let/block.
- [ ] Add failing tests then pass them.

Modify:
- `jazz-hs/src/Interpreter.hs`
- `jazz-hs/test/InterpreterSpec.hs`

Commit checkpoint:

```bash
git add jazz-hs/src/Interpreter.hs jazz-hs/test/InterpreterSpec.hs
git commit -m "feat(interpreter): implement runtime values and core expression eval"
```

## Phase 2: Functions, Closures, and Builtins

- [ ] Implement lambda evaluation and closure capture.
- [ ] Implement function application semantics.
- [ ] Wire builtin runtime environment (`+`, `-`, `*`, `/`, `==`, `print!`, `map`, `hd`, `tl`) with purity checks from analyzer respected upstream.
- [ ] Add interpreter tests for currying and partial application.

Modify:
- `jazz-hs/src/Interpreter.hs`
- `jazz-hs/src/Types.hs` (only if shared builtin metadata extraction is needed)
- `jazz-hs/test/InterpreterSpec.hs`

Commit checkpoint:

```bash
git add jazz-hs/src/Interpreter.hs jazz-hs/src/Types.hs jazz-hs/test/InterpreterSpec.hs
git commit -m "feat(interpreter): add closures, application, and builtin runtime"
```

(If `Types.hs` is unchanged, omit it.)

## Phase 3: ADT/Pattern/Case Runtime Support

- [ ] Implement constructor value representation and matching semantics.
- [ ] Implement `case` evaluation.
- [ ] Implement lambda pattern-parameter behavior consistent with item #11 core contract.
- [ ] Add exhaustive tests for success/failure pattern-match paths.

Modify:
- `jazz-hs/src/Interpreter.hs`
- `jazz-hs/test/InterpreterSpec.hs`
- `jazz-hs/test/Analyzer/TypeInferenceSpec.hs` (for analyzer/runtime contract alignment)

Commit checkpoint:

```bash
git add jazz-hs/src/Interpreter.hs jazz-hs/test/InterpreterSpec.hs jazz-hs/test/Analyzer/TypeInferenceSpec.hs
git commit -m "feat(interpreter): add case and pattern matching runtime semantics"
```

## Phase 4: CLI Integration and Pipeline Switch

- [ ] Add interpreter entrypoint in library layer.
- [ ] Update CLI to run interpreter path by default.
- [ ] Keep any legacy codegen entrypoint clearly non-default/deprecated until removed.

Modify:
- `jazz-hs/src/Lib.hs`
- `jazz-hs/app/Main.hs`
- `jazz-hs/run.sh`

Commit checkpoint:

```bash
git add jazz-hs/src/Lib.hs jazz-hs/app/Main.hs jazz-hs/run.sh
git commit -m "feat(runtime): switch cli execution to haskell interpreter"
```

## Phase 5: Docs + Verification + Closure

- [ ] Update runtime docs and examples for interpreter-first usage.
- [ ] Verify full tests and example program execution.
- [ ] Record closure in planning trackers.

Modify:
- `README.md`
- `docs/jazz-language-state.md`
- `docs/plans/spec-clarification/2026-03-02/README.md`

Commit checkpoint:

```bash
git add README.md docs/jazz-language-state.md docs/plans/spec-clarification/2026-03-02/README.md
git commit -m "docs(runtime): document interpreter-first execution and close plan"
```

## Nix Verification Commands

```bash
export NIXPKGS_REF='github:NixOS/nixpkgs/68cc97d306d3187c142cfb2378852f28d47bc098'
nix --extra-experimental-features 'nix-command flakes' shell \
  "$NIXPKGS_REF#stack" \
  "$NIXPKGS_REF#ghc" \
  -c bash -lc '
    set -euo pipefail
    cd jazz-hs
    stack test --test-arguments "--match Interpreter"
    stack test
  '
```

## Definition of Done

- [ ] Interpreter executes core language programs through CLI.
- [ ] Interpreter tests cover literals, functions, builtins, ADT/case/pattern behavior.
- [ ] JS/LLVM are not active runtime dependencies.
- [ ] Docs and plans consistently describe interpreter-only strategy.
