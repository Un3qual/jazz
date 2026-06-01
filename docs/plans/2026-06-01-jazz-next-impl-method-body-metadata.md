---
id: JN-ABSTRACTION-IMPL-METHOD-BODY-METADATA-001
status: done
priority: P1
size: M
kind: impl
autonomous_ready: no
depends_on:
  - JN-CLASS-METHOD-SIGNATURE-METADATA-001
last_verified: 2026-06-01
completed_on: 2026-06-01
plan_section: "Batch 1: Impl method body metadata without dispatch"
target_paths:
  - jazz-next/src/JazzNext/Compiler/Parser/AST.hs
  - jazz-next/src/JazzNext/Compiler/Parser.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Lower.hs
  - jazz-next/src/JazzNext/Compiler/AST.hs
  - jazz-next/src/JazzNext/Compiler/Analyzer.hs
  - jazz-next/src/JazzNext/Compiler/Desugar.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/src/JazzNext/Compiler/Runtime.hs
  - jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Accept method binding statements inside concrete impl bodies as inert metadata, reject duplicate impl method names and non-binding body items, and keep method dispatch, dictionaries, runtime evidence, and solver behavior out of scope."
---

# Jazz Next Impl Method Body Metadata

## Source Verification

This child plan narrows `Follow-up: abstraction semantics beyond method
metadata` in the authoritative-syntax plan. The landed class method metadata
batch accepts signature-only method declarations inside `class` bodies. This
child plan owns the first inert concrete `impl` method body metadata contract.

The executor-safe next batch owns the parser/core metadata shape for concrete
`impl` method bodies only. It deliberately does not make methods callable.

## Batch 1: Impl Method Body Metadata Without Dispatch

Completed on `2026-06-01` as
`JN-ABSTRACTION-IMPL-METHOD-BODY-METADATA-001`.

Scope:

- Accept ordinary binding-shaped method entries inside concrete `impl` bodies,
  for example `eq = \(left) -> \(right) -> left == right.`.
- Preserve method names and method expressions through surface AST, core AST,
  lowering, and shared statement walkers.
- Reject duplicate method names within the same `impl` body deterministically.
- Reject method-bearing `impl` bodies whose header target is not concrete.
- Reject body entries that are not ordinary method bindings.
- Preserve existing class method signature metadata and permanent `trait`
  rejection.

Out of scope:

- method lookup or call syntax,
- dictionary passing or runtime evidence values,
- method dispatch/runtime execution semantics,
- default methods,
- superclass semantics,
- inferred constraints,
- broad typeclass/defaulting solver behavior.

Batch target paths:

- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- `jazz-next/src/JazzNext/Compiler/Desugar.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```
