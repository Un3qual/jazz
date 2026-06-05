---
id: JN-ABSTRACTION-PRELUDE-EQ-BOOL-METHOD-001
status: done
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-06-04
completed_on: 2026-06-04
plan_section: "Batch 1: Bundled prelude Eq(Bool) method body"
target_paths:
  - jazz-next/src/JazzNext/Compiler/BundledPrelude.hs
  - jazz-next/stdlib/Prelude.jz
  - jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Add exactly one bundled-prelude `Eq(Bool).equals` method body through default prelude loading while preserving no-prelude/explicit-prelude determinism and keeping dictionaries, defaults, superclasses, solver behavior, and module method export/import semantics out of scope."
---

# Bundled Prelude Eq(Bool) Method Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend the landed bundled-prelude method-body path from
`Eq(Int).equals` to exactly one Bool concrete method body.

**Architecture:** Keep method bodies as ordinary bundled-prelude source
metadata. Default prelude loading should expose a concrete `Eq(Bool).equals`
body to explicit `Eq::equals` calls while no-prelude mode and explicit minimal
prelude loading remain deterministic. This batch does not introduce dictionary
values, default methods, superclasses, solver behavior, or module method
export/import semantics.

**Tech Stack:** Bundled prelude source generation, checked-in `Prelude.jz`,
active `jazz-next` type/runtime dispatch, focused prelude and runtime `runghc`
specs, Markdown queue metadata.

---

## Source Verification

The completed bundled-prelude `Eq(Int).equals` child proved that ordinary
source-level prelude method bodies can be loaded through the default bundled
prelude and executed through explicit qualified method dispatch. The current
blocker contract narrows the next abstraction child to `Eq(Bool).equals` only
and explicitly excludes dictionaries, defaults, superclasses, solver behavior,
runtime evidence values, and method import/export rules.

## Batch 1: Bundled prelude Eq(Bool) method body

Scope:

- Add exactly one bundled-prelude concrete method body for `impl Eq(Bool)`.
- Implement the method body in ordinary source syntax, delegating to the
  already-implemented Bool equality operator.
- Preserve the existing bundled `Eq(a).equals` class method signature.
- Keep other bundled `Eq` impl method-body work out of scope.
- Verify default bundled-prelude loading exposes Bool `Eq::equals` calls.
- Verify `--no-prelude` / no-prelude compile paths still reject `Eq::equals`
  when no source-local method body exists.
- Verify an explicit minimal prelude does not inherit bundled Bool method
  bodies.
- Keep method bodies out of the builtin catalog and prelude kernel bridge.

Out of scope:

- Adding method bodies for floating types, width-specific numeric types, ADTs,
  tuples, lists, or other classes,
- unqualified overloaded method names,
- dictionaries or runtime evidence values,
- default methods,
- superclass semantics,
- overlap/orphan policy beyond duplicate visible facts,
- inferred constraints or broader solver behavior,
- module method export/import behavior.

Batch target paths:

- `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs`
- `jazz-next/stdlib/Prelude.jz`
- `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

Suggested task order:

- [x] Add focused prelude-loading coverage for default-prelude Bool
  `Eq::equals` execution.
- [x] Add or update no-prelude and explicit-minimal-prelude coverage proving
  bundled Bool method bodies are not accidentally available outside default
  prelude loading.
- [x] Update bundled prelude source generation and checked-in `Prelude.jz` with
  exactly one `Eq(Bool).equals` method body.
- [x] Add or update source-pipeline/runtime coverage proving the bundled Bool
  method body is type-checked and executed through the existing explicit
  qualified dispatch path.
- [x] Run the focused verification commands listed in frontmatter.

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```
