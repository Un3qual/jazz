# Copilot Instructions for Jazz

## Repository Overview

Jazz is a functional language compiler inspired by Elixir and Haskell, implemented in Haskell. The repository contains the compiler source, language specification, and documentation.

## Workspace Policy

- **`jazz-hs/`** – Legacy reference implementation. **Read-only.** Do not modify unless the user explicitly requests legacy maintenance.
- **`jazz2/`** – Legacy/experimental reference. **Read-only.** Do not modify unless the user explicitly requests legacy maintenance.
- **`jazz-next/`** – The **only** active target for new compiler implementation work. All new compiler behavior, APIs, runtime, parser, analyzer, and codegen changes must be implemented here.

## Directory Structure

```
jazz-next/
  src/JazzNext/Compiler/   # Compiler modules (warnings, analysis, type inference, driver)
  src/JazzNext/CLI/        # CLI entrypoint
  test/                    # Hspec-style test specs
  scripts/                 # Local test runner scripts
docs/
  spec/                    # Canonical language specification (normative)
  plans/                   # Decision records and RFCs
  feature-status.md        # Feature implementation status matrix
  jazz-language-state.md   # Implementation baseline summary
```

## Build and Test

```bash
# Run all tests from the repository root:
bash jazz-next/scripts/test-warning-config.sh
```

No separate build step is required; the test script compiles and runs all specs.

## Spec Authority and Change Workflow

### Authority hierarchy (highest to lowest)

1. `docs/spec/*` – Normative language rules. When a spec section exists, it is the source of truth.
2. `jazz-hs` behavior and tests – Temporary behavioral authority for areas not yet fully specified in `docs/spec/*`.
3. `README.md` / `docs/jazz-language-state.md` – Descriptive summaries only; not normative.
4. `jazz2` – Reference-only design source; non-normative for current language behavior.

### Semantic language changes

Semantic changes require a decision record or RFC **before** implementation. Each change series must include:
- A decision doc with rationale and migration notes under `docs/plans/`.
- A canonical spec update in `docs/spec/*`.
- Tests and implementation updates in `jazz-next/`.

### Non-semantic / internal changes

Implementation-first is allowed for refactors, tooling, and internal cleanup that does not change language semantics. If observable behavior, docs, or test expectations change, update docs and tests in the same change.

## Code Conventions

- Source language: **Haskell** (GHC).
- Module namespace: `JazzNext.*` for all new code under `jazz-next/src/`.
- Warning categories are defined in `src/JazzNext/Compiler/Warnings.hs`; new categories are added there with a stable `W####` code and a kebab-case token.
- Tests use the internal `JazzNext.TestHarness` helpers (`assertEqual`, `assertLeftContains`, `assertRight`, `runTestSuite`).
- Each test file defines a `main :: IO ()` that calls `runTestSuite`.
- Governance policy details: `docs/spec/governance/spec-authority-policy.md`.
