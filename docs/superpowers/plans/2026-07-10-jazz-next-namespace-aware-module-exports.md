---
id: JN-MODULE-NAMESPACE-AWARE-EXPORT-001
status: done
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-07-10
completed_on: 2026-07-10
plan_section: "Implementation Batch: Namespace-Aware Module Exports"
target_paths:
  - jazz-next/src/JazzNext/Compiler/ModuleExports.hs
  - jazz-next/src/JazzNext/Compiler/ModuleGraph.hs
  - jazz-next/src/JazzNext/Compiler/ModuleResolver.hs
  - jazz-next/src/JazzNext/Compiler/Parser/AST.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Declaration.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Lower.hs
  - jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs
  - jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
  - jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs
  - docs/spec/modules/06-explicit-export-lists.md
  - docs/feature-status.md
  - docs/jazz-language-state.md
  - docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md
  - docs/execution/blocker-contracts.md
  - docs/execution/queue.md
  - docs/execution/done-archive.md
  - docs/superpowers/specs/2026-07-10-jazz-next-namespace-aware-module-exports-design.md
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
  - bash jazz-next/scripts/test-warning-config.sh
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Add optional value, constructor, type, and class prefixes to module export allowlists, carry structured selectors through parsing/lowering, and publish exact typed entries while preserving bare-selector and import compatibility."
---

# Jazz-Next Namespace-Aware Module Exports Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add exact namespace selectors to module export allowlists without
breaking existing bare module exports or any import syntax.

**Architecture:** `JazzNext.Compiler.ModuleExports` owns a structured
`ModuleExportSelector`. The parser, surface AST, lowering, and module graph
carry selectors structurally; the resolver validates exact entries and derives
the public typed inventory. Compiler and runtime consumers remain unchanged
because they already consume that inventory.

**Tech Stack:** Haskell 2010, token-stream parsing, `containers` `Map`/`Set`,
the active `jazz-next` parse-once module graph, `runghc` suites, and Markdown
queue/docs validators.

## Global Constraints

- Preserve all existing bare module export lists as compatibility syntax.
- Accept `value`, `constructor`, `type`, and `class` only as contextual export
  prefixes followed by an identifier; do not add global reserved words.
- Preserve omitted-list export-all and `()` export-none behavior.
- Keep explicit import lists text-based and behavior-compatible.
- Preserve `E4015` for unknown or imported-only module exports and preserve
  `E4007` through `E4014` unchanged.
- Do not add re-exports, wildcards, constructor groups, import-side namespace
  prefixes, visibility modifiers, packages, or local-resolution changes.
- Use TDD for each behavior change and commit each independently reviewable
  task.

---

## Implementation Batch: Namespace-Aware Module Exports

### Task 1: Parse and Lower Structured Export Selectors

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/ModuleExports.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleGraph.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Declaration.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs`

**Interfaces:**

- Produces: `ModuleExportSelector` with `Maybe NameNamespace` plus `Text`.
- Produces: `SSModule SourceSpan [Text] (Maybe [ModuleExportSelector])`.
- Produces: `DeclaredModuleExports` carrying `[ModuleExportSelector]`.
- Preserves: bare names, omitted lists, empty lists, and `E0001` syntax errors.

- [x] **Step 1: Write parser and lowering tests**

Add expectations equivalent to:

```haskell
namespaceSelectors =
  [ ModuleExportSelector (Just TypeNamespace) "Box",
    ModuleExportSelector (Just ConstructorNamespace) "Box",
    ModuleExportSelector (Just ValueNamespace) "Box",
    ModuleExportSelector (Just CapabilityNamespace) "Printable",
    ModuleExportSelector Nothing "legacy"
  ]
```

Parse
`module Lib::Box (type Box, constructor Box, value Box, class Printable, legacy) { legacy = 1. }`,
assert the surface `SSModule` carries `namespaceSelectors`, and assert lowering
stores the same selectors in `DeclaredModuleExports`. Add one case that accepts
`type Box, constructor Box` and one that rejects duplicate `type Box` with the
existing `E0001` duplicate diagnostic.

- [x] **Step 2: Run the parser suite and verify RED**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs
```

Expected: compilation fails because `ModuleExportSelector` and the structured
`SSModule` payload do not exist.

- [x] **Step 3: Add the structured selector type**

In `JazzNext.Compiler.ModuleExports`, export and add:

```haskell
data ModuleExportSelector = ModuleExportSelector
  { moduleExportSelectorNamespace :: Maybe NameNamespace,
    moduleExportSelectorName :: Text
  }
  deriving (Eq, Ord, Show)
```

Change `SSModule` and `DeclaredModuleExports` to store selectors rather than
bare `Text` names. Update lowering to carry the structured values unchanged.

- [x] **Step 4: Parse contextual namespace prefixes**

Replace the module-export use of `parseNonEmptyUniqueNameList` with a focused
selector loop. Parse a leading identifier in
`["value", "constructor", "type", "class"]` as a prefix only when the next
token is another `TIdentifier`; otherwise parse it as a bare selector name.
Map prefixes exactly:

```haskell
selectorNamespace "value" = Just ValueNamespace
selectorNamespace "constructor" = Just ConstructorNamespace
selectorNamespace "type" = Just TypeNamespace
selectorNamespace "class" = Just CapabilityNamespace
selectorNamespace _ = Nothing
```

Use the structured selector as the duplicate key so same-text entries in
different explicit namespaces are accepted. Preserve current comma, closing
parenthesis, trailing-comma, and end-of-input diagnostics.

- [x] **Step 5: Run parser verification and commit**

Run the focused parser suite above and `git diff --check`. Expected: all
`ModuleImportParser` tests pass. Commit:

```bash
git add jazz-next/src/JazzNext/Compiler/ModuleExports.hs jazz-next/src/JazzNext/Compiler/ModuleGraph.hs jazz-next/src/JazzNext/Compiler/Parser/AST.hs jazz-next/src/JazzNext/Compiler/Parser/Declaration.hs jazz-next/src/JazzNext/Compiler/Parser/Lower.hs jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs
git commit -m "feat: parse namespace-aware module exports"
```

### Task 2: Select and Validate Exact Typed Exports

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/ModuleExports.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs`

**Interfaces:**

- Produces: `selectModuleExportSelectors` for source declaration selection.
- Preserves: `selectExportNames` as the import-side text selector.
- Produces: exact namespace-aware `E4015` validation.
- Consumes: existing compiler/runtime public-inventory filtering unchanged.

- [x] **Step 1: Write inventory and resolver tests**

Add an inventory test that selects only `TypeNamespace "Box"` from a sample
inventory containing type, constructor, and value entries named `Box`. Retain
the existing bare-selection test proving `ModuleExportSelector Nothing "Box"`
keeps every same-text entry.

Add resolver cases for:

```jazz
module Lib::Box (type Box, value Box) {
  data Box = Box payload.
  Box = 1.
}
```

The resolved public inventory must contain only type `Box` and value `Box`, not
constructor `Box`. Add an `E4015` case where `type Token` is requested but only
`constructor Token` exists.

Add a pipeline case where a module declares both constructor and value `Just`
but exports only `value Just`; runtime exports must contain only
`RuntimeBindingExport (ModuleExport ValueNamespace "Just")`.

- [x] **Step 2: Run inventory, resolver, and pipeline suites and verify RED**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs
```

Expected: exact selection tests fail because the resolver still reduces module
exports to bare names.

- [x] **Step 3: Implement exact inventory selection**

Export and implement:

```haskell
selectModuleExportSelectors ::
  [ModuleExportSelector] ->
  ModuleExportInventory ->
  ModuleExportInventory
selectModuleExportSelectors selectors inventory =
  exportInventory
    [ export
      | export <- Set.toList (exportInventoryEntries inventory),
        any (selectorMatches export) selectors
    ]
```

`selectorMatches` compares names for bare selectors and compares the complete
`ModuleExport namespace name` identity for prefixed selectors.

- [x] **Step 4: Validate selectors against owned declarations**

In `validatePublicExportInventory`, find the first selector that does not match
the local inventory. Bare selectors use `declarationExportNames`; prefixed
selectors use `inventoryHasExport`. Render the selector as `type 'Box'`,
`constructor 'Box'`, `value 'Box'`, or `class 'Eq'` in `E4015`; keep the raw
name as `diagnosticSubject`. On success, call
`selectModuleExportSelectors selectors localInventory`.

- [x] **Step 5: Run focused verification and commit**

Run all three focused suites above plus `LoaderSpec` and `git diff --check`.
Expected: all pass. Commit:

```bash
git add jazz-next/src/JazzNext/Compiler/ModuleExports.hs jazz-next/src/JazzNext/Compiler/ModuleResolver.hs jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs
git commit -m "feat: enforce namespace-aware module exports"
```

### Task 3: Document and Close the Contract

**Files:**

- Modify: `docs/spec/modules/06-explicit-export-lists.md`
- Modify: `docs/feature-status.md`
- Modify: `docs/jazz-language-state.md`
- Modify: `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`
- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/execution/queue.md`
- Modify: `docs/execution/done-archive.md`
- Modify: `docs/superpowers/plans/2026-07-10-jazz-next-namespace-aware-module-exports.md`

**Interfaces:**

- Produces: normative additive syntax documentation.
- Produces: terminal-empty queue state after closing this one child.
- Preserves: all non-goals from the approved design.

- [x] **Step 1: Update normative and status documentation**

Document the four prefixes, exact selection, bare compatibility behavior,
opaque `type` exports, duplicate rules, unchanged imports, and `E4015` wording.
Record the child in the runtime/module umbrella, feature matrix, language-state
summary, and blocker landed evidence.

- [x] **Step 2: Close execution metadata**

Set this plan to `status: done` with `completed_on: 2026-07-10`, remove its
`Ready Now` row, restore the queue's terminal-empty status, and add one concise
`done-archive.md` row with exact verification evidence.

- [x] **Step 3: Run the complete verification ladder**

Run every frontmatter verification command, then:

```bash
git diff --stat "$(git merge-base origin/main HEAD)"..HEAD
rg -n 're-export|wildcard|constructor group|import-side namespace' jazz-next/src/JazzNext/Compiler jazz-next/test/JazzNext/Compiler docs/spec/modules/06-explicit-export-lists.md
git diff --check
```

Expected: focused and full tests, queue/docs checks, and whitespace checks pass;
the scope scan finds only documented non-goals and negative coverage.

- [x] **Step 4: Commit closeout documentation**

```bash
git add docs/spec/modules/06-explicit-export-lists.md docs/feature-status.md docs/jazz-language-state.md docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md docs/execution/blocker-contracts.md docs/execution/queue.md docs/execution/done-archive.md docs/superpowers/plans/2026-07-10-jazz-next-namespace-aware-module-exports.md
git commit -m "docs: close namespace-aware module exports"
```
