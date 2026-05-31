---
id: JN-PARSER-PARITY-BASELINE-001
status: ready
priority: P2
size: M
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-05-30
plan_section: "Batch 1: Handwritten parser parity baseline"
target_paths:
  - jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
  - jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs
  - jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs
  - jazz-next/test/JazzNext/Compiler/Parser/OperatorSectionSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorSectionSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Add focused parser parity coverage for current handwritten-parser AST shapes, spans, and diagnostics before any future Megaparsec migration, without changing parser technology or broadening accepted syntax."
---

# Jazz Next Parser Parity Baseline

## Source Verification

This child plan narrows `docs/plans/2026-03-03-text-parser-cst-direction.md`
to the first concrete implementation-safe migration precondition. The exact
source section is "Migration Shape", step 1: freeze current behavior with
parser parity tests before introducing a Megaparsec parser over the existing
token stream.

This batch is a test-harness slice only. It does not introduce Megaparsec,
replace the handwritten parser, add a CST, or accept new syntax.

## Batch 1: Handwritten parser parity baseline

Next executor-safe batch:

- Add focused parser parity assertions for current core expression, module,
  operator/fixity, and section behavior.
- Prefer AST shape, source-span, and deterministic diagnostic checks over broad
  golden dumps.
- Keep the parity baseline in existing active `jazz-next` parser suites.
- Leave parser implementation changes out of scope unless an existing behavior
  is not testable without a small harness helper.

Verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorSectionSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```
