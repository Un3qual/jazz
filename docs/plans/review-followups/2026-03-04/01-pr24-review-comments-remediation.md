# PR #24 review comment remediation (2026-03-04)

This checklist tracks the implementation/verification batch for the newest `coderabbitai[bot]` comments and the older `chatgpt-codex-connector[bot]` comments on PR #24.

## Checklist

- [x] Pulled latest branch state and re-verified target comments against current code.
- [x] Fixed `if` condition parsing to keep application disabled across infix RHS recursion.
- [x] Restricted strict equality typing to runtime-supported families (`Int`, `Bool`) and added regression coverage.
- [x] Consolidated operator typing classification for binary and section inference paths.
- [x] Added explicit diagnostic for unsupported signature surface forms.
- [x] Added/updated targeted parser and type/inference tests for the reviewed regressions.
- [x] Ran full verification via `bash jazz-next/scripts/test-warning-config.sh`.
- [x] Prepared a single milestone commit bundling code, tests, and checklist updates.

## Evidence

- Parser fix: `jazz-next/src/JazzNext/Compiler/Parser.hs`
- Type-inference fixes: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Tests:
  - `jazz-next/test/IfExpressionParserSpec.hs`
  - `jazz-next/test/PrimitiveSemanticsSpec.hs`
  - `jazz-next/test/BindingSignatureCoherenceSpec.hs`
