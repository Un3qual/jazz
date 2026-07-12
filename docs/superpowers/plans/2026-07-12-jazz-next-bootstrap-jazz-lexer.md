---
id: JN-BOOTSTRAP-JAZZ-LEXER-001
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on:
  - JN-BOOTSTRAP-CANONICAL-COMPARISON-001
last_verified: 2026-07-12
plan_section: "Implementation Batch: Jazz-Authored Lexer"
target_paths:
  - docs/execution/blocker-contracts.md
  - docs/execution/done-archive.md
  - docs/execution/queue.md
  - docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md
  - docs/superpowers/plans/2026-07-12-jazz-next-bootstrap-jazz-lexer.md
  - jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs
  - jazz-next/src/JazzNext/Compiler/BundledPrelude.hs
  - jazz-next/src/JazzNext/Compiler/Runtime.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/stdlib/Char.jz
  - jazz-next/stdlib/List.jz
  - jazz-next/stdlib/Text.jz
  - jazz-next/stdlib/Lexer.jz
  - jazz-next/stdlib/LexerTypes.jz
  - jazz-next/test/JazzNext/Compiler/Bootstrap/JazzLexerParitySpec.hs
  - jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - jazz-next/jazz-next.cabal
verification:
  - cabal test --project-dir=jazz-next jazz-lexer-parity-spec builtin-catalog-spec primitive-semantics-spec runtime-semantics-spec loader-spec --test-show-details=failures
  - cabal test --project-dir=jazz-next all --test-show-details=failures
  - bash jazz-next/scripts/check-stdlib-format.sh
  - bash jazz-next/scripts/test-warning-config.sh
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Add permanent generic list construction plus Unicode Char/Text services, implement the lexer state machine in two-space-indented Jazz, and prove exact deterministic parity over the fixed complete corpus and large tail-recursive traversal cases."
---

# Jazz-Next Bootstrap Jazz Lexer Implementation Plan

**Goal:** Implement the first real compiler component in Jazz and prove that it
matches the stage-0 Haskell lexer without introducing a temporary execution or
serialization architecture.

**Architecture:** Extend the backend-neutral builtin catalog only with semantic
collection, scalar, and immutable-text operations that the future native
runtime must implement. Wrap every raw operation in ordinary explicit-import
Jazz stdlib modules. The Jazz lexer is a tail-recursive state machine over
remaining `Text`, line, column, and a reversed token list; it returns the fixed
`LexerTypes` canonical ADTs. The Haskell harness supplies identical normalized
paths/source text to both lexers and compares only ordinary runtime rendering.

**Global constraints:**

- Modify compiler implementation only under `jazz-next/`; legacy trees remain
  read-only.
- Keep all `.jz` module bodies at exactly two spaces per indentation level.
- Do not change the canonical schema fixed by Child 1.
- Do not add a lexer-specific intrinsic, Haskell callback, custom serializer,
  bytecode, VM, lowered IR, LLVM value, object emission, linking, or native
  runtime.
- Preserve Unicode-scalar coordinates, tab stops of eight columns, exact raw
  lexemes, arbitrary-precision decimal payload text, and structured `E0001`
  failure parity.
- Implement behavior test-first and commit each independently reviewable task.

## Implementation Batch: Jazz-Authored Lexer

### Task 1: Add permanent collection and scalar/text kernel services

- [ ] Add RED catalog, type, runtime, invalid-input, Unicode-classification,
  scalar-rejection, list-hint, and text-construction tests.
- [ ] Add one generic `listPrependRaw` intrinsic; `charToUInt32`, checked
  zero-or-one `charFromUInt32Raw`, alpha/alphanumeric/digit/space/hex predicates;
  and immutable `textAppend`/`textAppendChar` intrinsics.
- [ ] Keep every symbol `KernelIntrinsic`, add exact polymorphic or concrete
  inference types, preserve runtime type hints, and produce deterministic
  diagnostics for invalid direct-runtime arguments.
- [ ] Run focused catalog, primitive, and runtime suites GREEN.
- [ ] Commit with `feat: add bootstrap collection and scalar services`.

### Task 2: Expose ordinary Jazz `List`, `Char`, and `Text` APIs

- [ ] Add RED real-loader tests for checked-in module import isolation, generic
  list prepend/reverse/length, checked scalar conversion, classification, and
  text construction.
- [ ] Create `List.jz` and `Char.jz`, extend `Text.jz`, and export only ordinary
  APIs. Raw bridge names remain private implementation details.
- [ ] Implement `listReverse` and `listLength` as tail-recursive Jazz functions;
  wrap zero-or-one scalar conversion in the existing `Maybe` ADT.
- [ ] Run loader, stdlib-format, catalog, primitive, and runtime suites GREEN.
- [ ] Commit with `feat: add bootstrap list char and text modules`.

### Task 3: Implement the Jazz-authored lexer state machine

- [ ] Add a RED focused parity suite that loads checked-in `List`, `Char`,
  `Text`, `Maybe`, `LexerTypes`, and `Lexer` through the real module graph.
- [ ] Implement `Lexer.jz` with explicit scanner result/state ADTs, ignored
  whitespace/comments, identifiers/keywords, arbitrary-size integer spelling,
  punctuation/operator runs, quoted literals, all escapes, Unicode decoding,
  exact spans, and structured failures.
- [ ] Accumulate tokens through `List.listPrepend` and finish with the
  tail-recursive `listReverse`; use no Haskell-side token construction for the
  Jazz result.
- [ ] Cover every token constructor and lexical boundary family in focused
  exact-rendering tests, then run stdlib formatting GREEN.
- [ ] Commit with `feat: implement lexer in Jazz`.

### Task 4: Prove full differential parity and stack safety

- [ ] Compare the Jazz and Haskell rendered canonical results over all manifest
  entries in stable order, including parser-rejected inputs and lexical errors.
- [ ] Evaluate the complete Jazz corpus twice and require byte-identical output
  before comparing it with stage 0.
- [ ] Add large whitespace traversal and large token-list cases that return
  compact counts while proving the shared explicit evaluator does not consume
  one Haskell stack frame per Jazz tail call.
- [ ] Register `jazz-lexer-parity-spec` in Cabal and run the focused lexer,
  catalog, primitive, runtime, and loader suites GREEN.
- [ ] Commit with `test: prove Jazz lexer differential parity`.

### Task 5: Close the hosted lexer milestone

- [ ] Run the full Cabal, stdlib-format, and warning/config gates.
- [ ] Mark this plan done, archive exact evidence, clear its queue row, and
  update the bootstrap parent to record the hosted lexer milestone as complete.
- [ ] Leave backend-neutral lowered IR, LLVM lowering, native runtime, parser
  porting, object emission, and linking as separate later children.
- [ ] Run queue/docs validation and `git diff --check`.
- [ ] Commit with `docs: close Jazz-authored lexer child`.
