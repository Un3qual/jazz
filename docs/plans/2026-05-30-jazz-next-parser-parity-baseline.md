---
id: JN-PARSER-PARITY-BASELINE-001
status: done
priority: P2
size: M
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-05-31
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

Completed on `2026-05-31`:

- Added focused parser parity assertions for current core expression, module,
  operator/fixity, and section behavior.
- Locked block-argument AST/spans and block diagnostic behavior in
  `ParserFoundationSpec.hs`; module/import indented spans and trailing path
  separator diagnostics in `ModuleImportParserSpec.hs`; same-precedence
  arithmetic associativity in `OperatorFixitySpec.hs`; and section application
  precedence plus left-section lowering in `OperatorSectionSpec.hs`.
- Kept the parity baseline in existing active `jazz-next` parser suites with no
  parser implementation changes, parser-technology migration, CST work, or
  broadened accepted syntax.

Verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorSectionSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```
