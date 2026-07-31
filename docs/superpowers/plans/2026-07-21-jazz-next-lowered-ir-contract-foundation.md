---
id: JN-BOOTSTRAP-LOWERED-IR-CONTRACT-FOUNDATION-001
status: done
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on:
  - JN-BOOTSTRAP-JAZZ-CORE-MODULES-CORPUS-CLOSURE-001
last_verified: 2026-07-21
completed_on: 2026-07-21
plan_section: "Implementation Batch: Lowered IR Contract Foundation"
target_paths:
  - docs/execution/blocker-contracts.md
  - docs/execution/done-archive.md
  - docs/execution/queue.md
  - docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md
  - docs/superpowers/specs/2026-07-21-jazz-next-backend-neutral-lowered-ir-design.md
  - docs/superpowers/plans/2026-07-21-jazz-next-lowered-ir-contract-foundation.md
  - jazz-next/README.md
  - jazz-next/jazz-next.cabal
  - jazz-next/jazz/compiler/LoweredIRTypes.jz
  - jazz-next/jazz/compiler/LoweredIRValidate.jz
  - jazz-next/src/JazzNext/Compiler/LoweredIR.hs
  - jazz-next/src/JazzNext/Compiler/LoweredIR/Validate.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLoweredIRComparison.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs
verification:
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-lowered-ir-contract-spec jazz-core-modules-corpus-closure-spec jazz-core-signatures-declarations-operators-spec jazz-core-control-flow-patterns-spec jazz-core-expression-foundation-spec repository-audit-spec --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal build --project-dir=jazz-next -fdevelopment all
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next all --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c bash -lc 'cd jazz-next && cabal check'
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Establish the permanent backend-neutral lowered-IR contract with matching Haskell and Jazz schemas, complete structured invariant validators, checked canonical comparison, and repeated exact parity over 10 valid and 31 invalid fixed fixtures without adding core lowering or backend behavior."
---

# Jazz-Next Lowered IR Contract Foundation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> `superpowers:executing-plans` to implement this plan task-by-task. Steps use
> checkbox syntax for tracking. Per maintainer direction, this plan records
> responsibilities, stable interfaces, observable tests, commands, and commit
> boundaries without embedding implementation bodies.

**Goal:** Implement the first executable backend-neutral lowered-IR contract
for both the Haskell stage-0 compiler and Jazz-authored compiler path.

**Architecture:** Mirror one reviewed semantic schema in Haskell and ordinary
Jazz ADTs. Both implementations validate complete untrusted IR values into the
same ordered failure model; a checked test adapter renders complete canonical
programs and failures for exact repeated comparison. This child deliberately
stops before typed-core elaboration or core-to-IR lowering.

**UInt64 representation correction (`2026-07-30`):** The original shared
signed-`Int` carrier contradicted the typed-core `UInt64` domain and prevented
ordinary Jazz construction before validation. The Haskell semantic field
remains `Integer`; the corresponding Jazz unsigned-immediate field is
canonical unsigned-decimal `Text`, and the checked adapter owns that
representation bridge. Both validators accept exactly
`0..18446744073709551615` and reject negative, malformed, and overflowing
payloads through the existing structured range failure.

**Tech Stack:** GHC 9.14.1, Haskell 2010 with existing package extensions,
ordinary Jazz `.jz` modules, the stack-safe Jazz interpreter, canonical runtime
values, Cabal test components, and the Nix-pinned development environment.

**Design checkpoint:**
[`2026-07-21-jazz-next-backend-neutral-lowered-ir-design.md`](../specs/2026-07-21-jazz-next-backend-neutral-lowered-ir-design.md)

## Global Constraints

- Modify compiler behavior only under `jazz-next/`; `jazz-hs/` and `jazz2/`
  remain read-only references.
- Keep canonical core and the reference interpreter unchanged.
- Do not add a source/core-to-IR lowering entry point, placeholder or
  otherwise.
- Keep the IR independent of LLVM types, instructions, names, offsets,
  alignments, address spaces, modules, or tool invocation.
- Do not add object generation, linking, a native ABI, allocation, garbage
  collection, bytecode, or a VM.
- Use distinct stable identifier types for functions, blocks, temporaries,
  layouts, and runtime services.
- Preserve program order in all canonical values and validation failures;
  never expose map iteration order.
- Keep temporaries block-local. Values crossing edges must use typed block
  arguments.
- Use structured validation results as the parity contract; rendered prose is
  not canonical evidence.
- Keep the Haskell/Jazz schema mirrors exact. The checked comparison adapter
  must reject unknown or malformed runtime values rather than supplying
  defaults.
- Keep checked-in `.jz` indentation at exactly two spaces.
- Write and run each failing behavior test before its production change.
- Run compiler and test commands through the Nix-pinned environment.
- Do not run opt-in exhaustive parser scale components. Routine Cabal `all`
  may run only the bounded `jazz-parser-scale-spec` component.
- Commit each independently reviewable green milestone.

## File and Responsibility Map

| File | Responsibility in this child |
| --- | --- |
| `jazz-next/src/JazzNext/Compiler/LoweredIR.hs` | Stage-0 identifiers, representations, layouts, call signatures, operands, operations, instructions, terminators, blocks, functions, services, programs, and validation-result data. |
| `jazz-next/src/JazzNext/Compiler/LoweredIR/Validate.hs` | Stable complete Haskell validation without target/backend assumptions. |
| `jazz-next/jazz/compiler/LoweredIRTypes.jz` | Ordinary Jazz mirror of the semantic IR and validation-result schema, with canonical decimal `Text` for the arbitrary-domain unsigned immediate payload. |
| `jazz-next/jazz/compiler/LoweredIRValidate.jz` | Stable complete Jazz validation using ordinary ADTs and list traversal. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLoweredIRComparison.hs` | Checked structural conversion and canonical rendering; no lowering or invariant decisions. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs` | Fixed 10-valid / 31-invalid fixture inventory, manifest audits, Haskell expectations, hosted executions, repetition, and exact assertions. |
| `jazz-next/jazz-next.cabal` | Register production Haskell modules, checked-in Jazz sources, and the focused test component. |
| Coordination/status paths in frontmatter | Promote, document, close, archive, and expose typed-core elaboration as a later design gate. |

## Stable Interfaces

| Owner | Interface |
| --- | --- |
| Haskell IR model | Export opaque identifier wrappers plus complete algebraic data for version, representation, call signature, layout, runtime service, operand, primitive, operation, instruction, terminator, block, function, and program values. |
| Jazz IR model | Export constructor-equivalent `LoweredIRTypes` ADTs using the same field order and ownership; bridge the Haskell unsigned `Integer` payload as canonical decimal `Text`. |
| Haskell validator | `validateLoweredProgram :: LoweredProgram -> [LoweredIRValidationFailure]`. |
| Jazz validator | `validateProgram :: LoweredProgram -> [LoweredIRValidationFailure]`. |
| Validation path | Identify program, layout, runtime-service, function, block, instruction index, or terminator position without source-file paths. |
| Validation detail | Carry the relevant version, identifier, immediate representation, expected/actual representation, or expected/actual arity structurally. |
| Comparison adapter | Convert complete Haskell programs and failure lists to canonical `RuntimeValue`; decode only the top-level hosted validation result needed to reject schema drift. |
| Test contract | Compare complete canonical program values and ordered failure lists twice; never compare Haskell `Show` or implementation text. |

## Fixed Fixture Inventory

The valid family contains exactly these 10 fixtures:

| Fixture | Required coverage |
| --- | --- |
| `scalar-representations` | Unit, boolean, every integer/float width, `Char`, and scalar immediates. |
| `branch-join` | Boolean branch, typed block arguments, join, and return. |
| `product-projection` | Product layout, construction, managed reference, and field projection. |
| `variant-switch` | Tagged variant layout, payload projection, switch cases, and default. |
| `direct-call` | Known function, matching signature, ordinary call result. |
| `direct-tail-call` | Direct tail-call terminator and enclosing result agreement. |
| `closure-call` | Closure environment layout, construction, and matching closure call. |
| `closure-tail-call` | Closure tail-call terminator and matching callable representation. |
| `runtime-service-call` | Declared semantic runtime service and matching ordinary call. |
| `text-list-layouts` | Runtime-managed text/list layouts and deterministic declaration order. |

The invalid family contains exactly these 31 fixtures:

| Group | Fixtures |
| --- | --- |
| Program declarations | `duplicate-layout`, `unknown-layout`, `duplicate-variant-tag`, `duplicate-runtime-service`, `duplicate-function`, `missing-entry-function`. |
| Function/block structure | `duplicate-block`, `missing-entry-block`, `missing-terminator`. |
| Operand scope | `duplicate-temporary`, `use-before-definition`, `cross-block-temporary`, `unknown-parameter`, `unknown-function-call`, `unknown-block-target`. |
| Operation/layout shape | `instruction-result-representation`, `invalid-field-projection`, `invalid-tag-projection`, `closure-environment-layout`. |
| Edge contracts | `jump-argument-arity`, `jump-argument-representation`, `branch-non-boolean`, `switch-duplicate-case-tag`, `switch-target-arguments`, `return-representation`. |
| Call contracts | `direct-call-signature`, `closure-call-signature`, `runtime-call-signature`, `direct-tail-signature`, `closure-tail-signature`, `unknown-runtime-service`. |

Fixture names and ordering are fixed in the test manifest. Tests reject missing,
duplicate, unknown, or reordered cases and assert exact `10`, `31`, and `41`
counts. A separate parity-backed hardening corpus covers later validation gaps
without changing that fixed manifest, including parameterized or capturing
entry functions, jump, branch, or switch edges that target an entry block,
non-scalar character immediates, tag carrier bounds, and exhaustive
no-default switches.

## Implementation Batch: Lowered IR Contract Foundation

### Task 0: Promote the reviewed child

**Files:** this plan, `docs/execution/queue.md`,
`docs/execution/blocker-contracts.md`, and the approved design.

**Produces:** One exact P1/L `Ready Now` row whose plan/frontmatter metadata
match and whose parent blocker names only this child as active.

- [x] Change plan status to `ready` and `autonomous_ready` to `yes` after user
  approval.
- [x] Add the child to `Ready Now` with exact dependency, section, existing
  target paths, deliverable, verification, and date parity. Keep `Next Curation
  Target` empty. Add new source/test paths atomically when their tasks create
  them.
- [x] Update the parent blocker and design status to name the reviewed contract
  foundation as active while keeping typed-core elaboration, LLVM, object/link,
  and native-runtime work unpromoted.
- [x] Run queue/docs validators and `git diff --check`.
- [x] Commit as `docs: promote lowered IR contract foundation`.

### Task 1: Establish the Haskell schema and canonical boundary

**Files:** `LoweredIR.hs`, `CanonicalLoweredIRComparison.hs`,
`JazzLoweredIRContractSpec.hs`, and `jazz-next.cabal`.

**Consumes:** The approved program, representation, layout, call, CFG, stable
identifier, and validation-result contracts.

**Produces:** Constructible complete Haskell IR values plus deterministic
canonical structural rendering, without validation decisions.

- [x] Register `jazz-lowered-ir-contract-spec`, the production module, and the
  shared test adapter; add manifest and canonical-value assertions for the 10
  valid fixtures.
- [x] Run the focused suite and confirm it fails because the new IR module does
  not exist.
- [x] Implement distinct identifiers and the complete Haskell schema, including
  optional block terminators so malformed inputs remain representable.
- [x] Implement checked canonical conversion that preserves constructor fields
  and list order and contains no inferred defaults.
- [x] Run the focused suite twice; require exact stable valid-program values and
  a green manifest audit.
- [x] Commit as `feat: define backend-neutral lowered IR`.

### Task 2: Validate the complete Haskell contract

**Files:** `LoweredIR/Validate.hs` and
`JazzLoweredIRContractSpec.hs`.

**Consumes:** The Haskell schema and the complete 31-case invalid inventory.

**Produces:** Ordered, complete `validateLoweredProgram` findings with exact
paths and details.

- [x] Add all 31 invalid Haskell fixtures and their full expected failure
  lists; add explicit order assertions for programs containing multiple
  independent failures.
- [x] Run the focused suite and confirm invalid cases fail because the
  validator is absent.
- [x] Implement declaration/index validation first, then function/block scope,
  operand use order, layout/operation shape, edges, calls, closure environments,
  and terminators.
- [x] Preserve complete stable traversal order while using internal lookup
  structures only for resolution.
- [x] Run the focused suite twice and require exact valid empties plus all 31
  complete ordered failure values.
- [x] Commit as `feat: validate backend-neutral lowered IR`.

### Task 3: Mirror and validate the contract in Jazz

**Files:** `LoweredIRTypes.jz`, `LoweredIRValidate.jz`,
`CanonicalLoweredIRComparison.hs`, `JazzLoweredIRContractSpec.hs`, and
`jazz-next.cabal`.

**Consumes:** The green Haskell model/validator and the fixed 41-fixture
manifest.

**Produces:** Ordinary Jazz construction and validation with exact canonical
parity to stage 0.

- [x] Register both checked-in Jazz modules and add hosted execution for all 41
  independently constructed Jazz fixtures.
- [x] Run the focused suite and confirm hosted cases fail because the Jazz
  modules do not exist while Haskell expectations remain green.
- [x] Implement the Jazz ADT mirror with matching constructor field order and
  no host-only values. The reviewed UInt64 correction uses canonical decimal
  `Text` for the unsigned immediate payload so ordinary Jazz construction
  reaches validator-owned range checks.
- [x] Implement the Jazz validator through deterministic list traversal and
  compiler-local association-list lookups; aggregate all findings in source
  order.
- [x] Decode hosted results through the checked adapter and reject any unknown
  constructor, arity, field type, or validation-detail shape.
- [x] Run all 41 fixtures twice and require exact Haskell/Jazz program and
  failure-list parity with no compiler/runtime diagnostics.
- [x] Commit as `feat: validate lowered IR in Jazz`.

### Task 4: Harden contract evidence and regressions

**Files:** `CanonicalLoweredIRComparison.hs` and
`JazzLoweredIRContractSpec.hs`.

**Consumes:** Both green validators and the existing hosted canonical-core
suite.

**Produces:** Audited determinism, schema-drift rejection, and focused
regression evidence.

- [x] Add negative adapter cases for unknown constructors, wrong arity, wrong
  field category, and malformed nested values; require explicit checked
  failures rather than crashes or defaults.
- [x] Add repeated batch assertions over all 41 fixtures and require
  byte-identical canonical rendering and exact failure ordering.
- [x] Extend the parity-backed hardening corpus when review finds a real
  contract gap; keep those regressions separate from the fixed 41-fixture
  manifest.
- [x] Run the focused regression command from frontmatter, including all four
  hosted canonical-core suites and repository audit.
- [x] Confirm the new suite contains no synthetic scale generator and enables
  no `full-parser-scale` Cabal flag.
- [x] Commit as `test: prove lowered IR contract parity`.

### Task 5: Verify and close the child

**Files:** all coordination/status paths listed in frontmatter.

**Produces:** Complete routine evidence, archived contract-foundation closure,
and typed-core elaboration returned to a separate design gate.

- [x] Run the exact focused verification command from frontmatter.
- [x] Run the warning-clean development build, routine non-exhaustive Cabal
  `all`, and `cabal check`. Confirm only bounded `jazz-parser-scale-spec` runs;
  do not enable or invoke any `jazz-parser-scale-full-*` component.
- [x] Update the lowered-IR design, bootstrap profile, and `jazz-next/README.md`
  with exact entry points, ownership, fixture counts, and failure behavior.
- [x] Mark this plan done, archive the child with concrete evidence, empty
  `Ready Now`, and leave `Next Curation Target` empty unless a separately
  reviewed typed-core elaboration candidate exists.
- [x] Update the parent blocker to state that the IR contract exists while
  typed-core elaboration, core lowering, LLVM, object/link, and native runtime
  remain unpromoted.
- [x] Run queue/docs validators and `git diff --check`.
- [x] Commit as `docs: close lowered IR contract foundation`.

## Done Criteria

- Haskell and Jazz expose the same complete backend-neutral IR and validation
  data, with no LLVM or host representation leaks.
- Both validators report complete structured failures in stable program order.
- Temporaries remain block-local and all cross-block values use typed block
  arguments.
- All 10 valid and 31 invalid fixtures are explicitly audited, run twice, and
  match exactly across Haskell and Jazz.
- Checked comparison rejects malformed hosted values rather than guessing or
  crashing.
- Canonical core and interpreter behavior remain unchanged.
- Focused tests, warning-clean build, routine Cabal matrix, package check,
  queue/docs validation, and whitespace checks pass.
- No opt-in exhaustive parser scale component is run.
