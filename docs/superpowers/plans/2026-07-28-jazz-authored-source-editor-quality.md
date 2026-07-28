# Jazz Authored Source and Editor Quality Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make every shipped `.jz` source use the best applicable implemented Jazz syntax, enforce readable data declarations, fix `value` highlighting, and prove aggregate surface-feature coverage.

**Architecture:** Repository audit code discovers the complete authored source roots, parses every file, validates formatting, and folds the parsed surface AST into a closed feature inventory. TextMate checks remain structural JSON assertions because editor scopes are not compiler AST data. Source migrations are semantic and file-reviewed; coverage requires each feature family somewhere in the corpus without injecting decorative syntax into unrelated files.

**Tech Stack:** Jazz source, Haskell repository-audit tests, Aeson JSON inspection, TextMate grammar JSON, Cabal/Nix.

## Global Constraints

- The audit covers `jazz-next/jazz/stdlib`, `jazz-next/jazz/compiler`, `jazz-next/programs`, and `jazz-next/editors/vscode-jazz/fixtures`.
- Every ordinary `value` identifier is renamed by meaning; module export prefixes remain `value`.
- Data declarations exceeding 100 columns use the approved multiline layout.
- Function equations replace lambdas used only to dispatch a `case`.
- `$`, sections, operator values, explicit type application, and pattern forms are used only where they improve the code.
- The authored source set collectively exercises every implemented surface family.
- Feature coverage is AST-based except for syntax intentionally erased by parsing/lowering.
- Invalid and historical Haskell test fixtures are not bulk rewritten.
- `jazz-hs/` and `jazz2/` remain untouched.
- Each task ends with a focused commit.

---

## File Structure

- `jazz-next/test/JazzNext/Repository/AuthoredSources.hs`: discovers all shipped `.jz` sources and assigns source roles.
- `jazz-next/test/JazzNext/Repository/JazzSourceFormat.hs`: enforces data-declaration width and continuation layout.
- `jazz-next/test/JazzNext/Repository/FeatureInventory.hs`: folds parsed AST values into a closed `SurfaceFeature` set.
- `jazz-next/test/JazzNext/Repository/AuditSpec.hs`: owns integration assertions, required-feature policy, and TextMate structural checks.
- `jazz-next/editors/vscode-jazz/syntaxes/jazz.tmLanguage.json`: scopes export modifiers contextually and `value` globally as a keyword.
- `jazz-next/editors/vscode-jazz/fixtures/representative.jz`: executable representative syntax.
- All `.jz` files in the four authored roots are review inputs; the ledger below is the completion checklist.

## File-by-File Review Ledger

### Standard library

- `jazz-next/jazz/stdlib/Char.jz`
- `jazz-next/jazz/stdlib/Dictionary.jz`
- `jazz-next/jazz/stdlib/IO.jz`
- `jazz-next/jazz/stdlib/IOError.jz`
- `jazz-next/jazz/stdlib/List.jz`
- `jazz-next/jazz/stdlib/Map.jz`
- `jazz-next/jazz/stdlib/Maybe.jz`
- `jazz-next/jazz/stdlib/NonEmpty.jz`
- `jazz-next/jazz/stdlib/Prelude.jz`
- `jazz-next/jazz/stdlib/Queue.jz`
- `jazz-next/jazz/stdlib/Result.jz`
- `jazz-next/jazz/stdlib/Set.jz`
- `jazz-next/jazz/stdlib/Text.jz`

### Jazz-authored compiler

- `jazz-next/jazz/compiler/Core.jz`
- `jazz-next/jazz/compiler/CoreLower.jz`
- `jazz-next/jazz/compiler/CoreTypes.jz`
- `jazz-next/jazz/compiler/Lexer.jz`
- `jazz-next/jazz/compiler/LexerTypes.jz`
- `jazz-next/jazz/compiler/LoweredIRTypes.jz`
- `jazz-next/jazz/compiler/LoweredIRValidate.jz`
- `jazz-next/jazz/compiler/Parser.jz`
- `jazz-next/jazz/compiler/ParserContext.jz`
- `jazz-next/jazz/compiler/ParserCore.jz`
- `jazz-next/jazz/compiler/ParserDeclaration.jz`
- `jazz-next/jazz/compiler/ParserExpression.jz`
- `jazz-next/jazz/compiler/ParserOperator.jz`
- `jazz-next/jazz/compiler/ParserPattern.jz`
- `jazz-next/jazz/compiler/ParserProgram.jz`
- `jazz-next/jazz/compiler/ParserSignature.jz`
- `jazz-next/jazz/compiler/ParserToken.jz`
- `jazz-next/jazz/compiler/ParserTypes.jz`
- `jazz-next/jazz/compiler/TypedCoreTypes.jz`
- `jazz-next/jazz/compiler/TypedCoreValidate.jz`

### Existing production-shaped programs

- `jazz-next/programs/capability-workflow/Main.jz`
- `jazz-next/programs/capability-workflow/Workflow.jz`
- `jazz-next/programs/collection-boundaries/Collections.jz`
- `jazz-next/programs/collection-boundaries/Main.jz`
- `jazz-next/programs/dependency-planner/Graph.jz`
- `jazz-next/programs/dependency-planner/Main.jz`
- `jazz-next/programs/expression-evaluator/Expression.jz`
- `jazz-next/programs/expression-evaluator/Main.jz`
- `jazz-next/programs/identifier-classifier/Main.jz`
- `jazz-next/programs/mini-frontend/Analysis.jz`
- `jazz-next/programs/mini-frontend/Evaluation.jz`
- `jazz-next/programs/mini-frontend/Main.jz`
- `jazz-next/programs/mini-frontend/Syntax.jz`
- `jazz-next/programs/mini-frontend/Token.jz`
- `jazz-next/programs/queue-traversal/Main.jz`
- `jazz-next/programs/queue-traversal/Traversal.jz`
- `jazz-next/programs/sorted-index/Index.jz`
- `jazz-next/programs/sorted-index/Main.jz`
- `jazz-next/programs/text-processing/Main.jz`
- `jazz-next/programs/tree-transformations/Main.jz`
- `jazz-next/programs/tree-transformations/Tree.jz`
- `jazz-next/programs/word-frequency/Main.jz`

### Editor fixture

- `jazz-next/editors/vscode-jazz/fixtures/representative.jz`

### Task 1: Discover the Complete Authored Source Set

**Files:**
- Create: `jazz-next/test/JazzNext/Repository/AuthoredSources.hs`
- Modify: `jazz-next/test/JazzNext/Repository/AuditSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`

**Interfaces:**
- Produces: `AuthoredSourceRole`.
- Produces: `AuthoredSource { authoredRelativePath, authoredRole, authoredText, authoredSurface }`.
- Produces: `readAuthoredSources :: FilePath -> IO [AuthoredSource]`.

- [ ] **Step 1: Add failing root and ledger assertions**

Assert all four roots are discovered, paths are unique and sorted, every file ends in `.jz`, and the returned path set exactly equals the checked-in ledger. The test failure must print missing and unexpected paths separately.

- [ ] **Step 2: Run the repository audit and confirm it only reads stdlib/compiler today**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    repository-audit-spec \
    --test-show-details=failures
```

Expected: FAIL because program and editor roots are not part of the current unified source reader.

- [ ] **Step 3: Implement one sorted recursive source reader**

Use:

```haskell
data AuthoredSourceRole
  = StandardLibrarySource
  | CompilerSource
  | ProgramSource
  | EditorFixtureSource
  deriving (Eq, Ord, Show)

data AuthoredSource = AuthoredSource
  { authoredRelativePath :: FilePath,
    authoredRole :: AuthoredSourceRole,
    authoredText :: Text,
    authoredSurface :: SurfaceExpr
  }
```

Parse each source with `parseSurfaceProgram`; report the relative path plus rendered diagnostic on failure. Replace duplicate filesystem walkers in `AuditSpec` with this module.

- [ ] **Step 4: Run the audit**

Run the Step 2 command. Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add jazz-next/test/JazzNext/Repository jazz-next/jazz-next.cabal
git commit -m "test: inventory all authored Jazz sources"
```

### Task 2: Enforce Multiline Data Declaration Formatting

**Files:**
- Modify: `jazz-next/test/JazzNext/Repository/JazzSourceFormat.hs`
- Modify: `jazz-next/test/JazzNext/Repository/AuditSpec.hs`

**Interfaces:**
- Produces: `OverlongDataDeclarationLine FilePath Int Int`.
- Produces: `InvalidDataContinuationIndent FilePath Int`.
- Consumes: valid, parsed authored source text; malformed syntax fixtures remain outside this audit.

- [ ] **Step 1: Add failing unit cases for the 100-column contract**

Add an accepted fixture:

```jazz
module Good {
  data TypedLiteral
    = TypedIntegerLiteral Text
    | TypedFractionalLiteral Text Text Maybe(TypedNumericType)
    | TypedBooleanLiteral Bool.
}
```

Add rejected fixtures for a 101-column data line and a payload continuation not indented two spaces past its constructor:

```jazz
  data TypedFunction
    = TypedFunction
    TypedFunctionId
    [TypedBlock].
```

- [ ] **Step 2: Run the audit and confirm format rules are absent**

Run the repository-audit command from Task 1. Expected: FAIL in the new assertions.

- [ ] **Step 3: Track data-declaration regions and enforce width/indentation**

Implement a line scanner that enters a declaration on a trimmed line starting with `data ` and exits at the terminating `.` outside comments/text. Within that region:

- reject physical lines longer than 100 Unicode scalar columns;
- require `=` and `|` constructor lines at the declaration's body indentation;
- require payload-only continuation lines two spaces deeper than the constructor line.

Do not apply the rule to arbitrary expression lines.

- [ ] **Step 4: Run repository audit**

Expected: the unit fixtures pass and checked-in authored sources report the concrete lines still needing migration.

- [ ] **Step 5: Commit the enforcement before source rewrites**

```bash
git add jazz-next/test/JazzNext/Repository
git commit -m "test: enforce Jazz data declaration layout"
```

### Task 3: Correct TextMate Export and Reserved-Keyword Scopes

**Files:**
- Modify: `jazz-next/editors/vscode-jazz/syntaxes/jazz.tmLanguage.json`
- Modify: `jazz-next/editors/vscode-jazz/fixtures/representative.jz`
- Create: `jazz-next/editors/vscode-jazz/fixtures/reserved-keyword-highlighting.jz.txt`
- Modify: `jazz-next/test/JazzNext/Repository/AuditSpec.hs`

**Interfaces:**
- Produces: `value` with `keyword.other.reserved.jazz` globally.
- Produces: `value`, `type`, `constructor`, and `class` with `storage.modifier.export.jazz` only inside a module export-list region.

- [ ] **Step 1: Add failing JSON-structure assertions**

Assert:

```haskell
keywordMatch == Just "\\bvalue\\b"
keywordScope == Just "keyword.other.reserved.jazz"
```

Assert the root `#exports` pattern begins at the module header's export `(` and ends at its matching `)`, and that the export-modifier match is nested inside it. Assert no root/global export pattern matches `value|constructor|type|class`.

- [ ] **Step 2: Run repository audit and confirm the global export scope fails**

Run the repository-audit command. Expected: FAIL on the current global `storage.modifier.export.jazz` rule.

- [ ] **Step 3: Restructure the grammar and fixtures**

Add `value` to the keyword repository:

```json
{
  "name": "keyword.other.reserved.jazz",
  "match": "\\bvalue\\b"
}
```

Make the export rule a module-header region with the modifier rule nested inside it. Keep grouped type constructor highlighting. Put invalid standalone `value` highlighting text in the `.jz.txt` non-parser fixture; keep `representative.jz` executable.

- [ ] **Step 4: Run audit and JSON validation**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next repository-audit-spec --test-show-details=failures
python3 -m json.tool jazz-next/editors/vscode-jazz/syntaxes/jazz.tmLanguage.json >/dev/null
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add jazz-next/editors/vscode-jazz jazz-next/test/JazzNext/Repository/AuditSpec.hs
git commit -m "fix: scope reserved value highlighting"
```

### Task 4: Migrate Reserved Names and Canonical Data Syntax

**Files:**
- Modify: all ledger `.jz` files containing ordinary `value` identifiers
- Modify: all ledger `.jz` files containing opaque/redundantly grouped constructor field types
- Modify: all ledger `.jz` files reported by the data-format audit

**Interfaces:**
- Consumes: globally reserved `value` and structured `Type(arguments)` grammar.
- Produces: semantically named identifiers and canonical constructor declarations.

- [ ] **Step 1: Generate the concrete migration reports**

Run:

```bash
rg -n '\bvalue\b' \
  jazz-next/jazz/stdlib \
  jazz-next/jazz/compiler \
  jazz-next/programs \
  jazz-next/editors/vscode-jazz/fixtures \
  -g '*.jz'
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next repository-audit-spec --test-show-details=failures
```

Classify every `value` occurrence as export selector, ordinary identifier, comment/text, or diagnostic example. Only export selectors remain bare `value`.

- [ ] **Step 2: Rename identifiers by meaning**

Use these mappings only where semantically accurate:

```text
collection element     -> item
successful computation -> result
constructor contents   -> payload
literal/token contents -> literal, scalar, tokenText
traversal state        -> current
generic type parameter -> a or a domain name
```

Update signatures, bindings, patterns, and uses atomically within each scope. Do not use a repository-wide textual replacement.

- [ ] **Step 3: Canonicalize every data declaration**

Rewrite:

```jazz
data Tree a = Leaf a | Branch (Tree(a)) (Tree(a)).
```

as:

```jazz
data Tree a
  = Leaf a
  | Branch Tree(a) Tree(a).
```

Apply the same `Type(arguments)` and line-breaking rule to all reported declarations.

- [ ] **Step 4: Run parse, audit, stdlib, corpus, and hosted compiler tests**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    repository-audit-spec \
    stdlib-spec \
    program-corpus-spec \
    jazz-lexer-parity-spec \
    jazz-parser-parity-spec \
    canonical-core-comparison-spec \
    --test-show-details=failures \
    --jobs=1
```

Expected: PASS, and `rg` shows ordinary `value` only in deliberate invalid/nonparsed examples.

- [ ] **Step 5: Commit**

```bash
git add jazz-next/jazz jazz-next/programs jazz-next/editors/vscode-jazz/fixtures
git commit -m "refactor: migrate authored Jazz syntax"
```

### Task 5: Convert Dispatch Lambdas into Function Equations

**Files:**
- Modify: applicable ledger `.jz` files in stdlib, compiler, programs, and editor fixture
- Modify: expected outputs only if a discovered existing output was accidentally dependent on formatting text

**Interfaces:**
- Consumes: function equations and grouped head patterns.
- Preserves: exact program stdout and hosted comparison results.

- [ ] **Step 1: Find lambda-plus-case dispatch candidates**

Run:

```bash
rg -n '= \\\\|case ' \
  jazz-next/jazz/stdlib \
  jazz-next/jazz/compiler \
  jazz-next/programs \
  jazz-next/editors/vscode-jazz/fixtures \
  -g '*.jz'
```

Review enclosing definitions. A candidate qualifies only when the lambda's sole purpose is immediate argument dispatch; anonymous callbacks remain lambdas.

- [ ] **Step 2: Convert one representative stdlib family first**

Use:

```jazz
mapMaybe transform Nothing = Nothing.
mapMaybe transform (Just item) = Just (transform item).
```

instead of a named binding to a lambda whose body is only `case`. Preserve clause order and signature adjacency.

- [ ] **Step 3: Run stdlib and semantic suites**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    stdlib-spec \
    function-equation-semantics-spec \
    --test-show-details=failures
```

Expected: PASS.

- [ ] **Step 4: Convert the remaining qualifying compiler and program definitions**

Use grouped constructor patterns such as `(Just item)` and `(left, right)` where required. Leave lambdas in higher-order arguments and closures that capture local state.

- [ ] **Step 5: Run hosted parity and program corpus**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    jazz-parser-parity-spec \
    canonical-core-comparison-spec \
    program-corpus-spec \
    --test-show-details=failures \
    --jobs=1
```

Expected: PASS with unchanged stdout and deterministic statistics.

- [ ] **Step 6: Commit**

```bash
git add jazz-next/jazz jazz-next/programs jazz-next/editors/vscode-jazz/fixtures
git commit -m "refactor: dogfood function equations"
```

### Task 6: Apply the Remaining Idiomatic Jazz Features

**Files:**
- Modify: applicable ledger `.jz` files
- Modify: `jazz-next/programs/corpus.json` budgets only when deterministic semantic counts legitimately change

**Interfaces:**
- Consumes: `$`, compact lambdas, sections, operator values, explicit type application, all pattern families, Prelude/stdlib APIs.
- Preserves: observable output and algorithms.

- [ ] **Step 1: Review application grouping and operator boilerplate**

Replace nesting such as:

```jazz
print! (renderResult (analyze input))
```

with:

```jazz
print! $ renderResult $ analyze input
```

only where the right-associative pipeline reads more clearly. Replace wrappers such as `\(left, right) -> left + right` with `(+)` or a section when the callable shape is identical.

- [ ] **Step 2: Review patterns and explicit type applications**

Prefer constructor/list/tuple/as/or/guarded patterns over manual predicates or projections. Keep explicit type application only where it resolves or documents an actual generalized choice, for example:

```jazz
defaultFlag = Nothing @Bool.
```

Do not add type applications solely to satisfy feature counts.

- [ ] **Step 3: Review kernel boundary use**

Outside the bridge-owning Prelude/stdlib module, replace direct kernel bindings with the public Prelude or standard-library function when an equivalent exists. Do not add a new public fallback or builtin.

- [ ] **Step 4: Run corpus twice and update only measured budgets**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next program-corpus-spec --test-show-details=failures
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next program-corpus-spec --test-show-details=failures
```

Expected: both runs agree completely. If semantic work changed, update ceilings with bounded headroom from the measured observation; never add wall-clock thresholds.

- [ ] **Step 5: Commit**

```bash
git add jazz-next/jazz jazz-next/programs
git commit -m "refactor: use idiomatic Jazz features"
```

### Task 7: Add the Aggregate AST Feature Inventory

**Files:**
- Create: `jazz-next/test/JazzNext/Repository/FeatureInventory.hs`
- Modify: `jazz-next/test/JazzNext/Repository/AuditSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`
- Modify: authored `.jz` sources only if the inventory reveals a genuine uncovered, applicable feature

**Interfaces:**
- Produces: closed `SurfaceFeature`.
- Produces: `inventorySurface :: Text -> SurfaceExpr -> Set SurfaceFeature`.
- Produces: `requiredAuthoredFeatures :: Set SurfaceFeature`.
- Produces: exact authored module-path coverage for every public stdlib family.

- [ ] **Step 1: Add a failing required-feature assertion**

Define the closed inventory:

```haskell
data SurfaceFeature
  = LiteralFeature
  | NumericWidthFeature
  | OrdinaryBindingFeature
  | CompactLambdaFeature
  | FunctionEquationFeature
  | MultiClauseFunctionFeature
  | PartialApplicationFeature
  | ListFeature
  | TupleFeature
  | UnitFeature
  | GenericAdtFeature
  | StructuredConstructorFieldFeature
  | WildcardPatternFeature
  | VariablePatternFeature
  | LiteralPatternFeature
  | ConstructorPatternFeature
  | ListPatternFeature
  | ConsPatternFeature
  | TuplePatternFeature
  | AsPatternFeature
  | OrPatternFeature
  | GuardedCaseFeature
  | ConditionalFeature
  | ApplicationFeature
  | DollarApplicationFeature
  | OperatorValueFeature
  | LeftSectionFeature
  | RightSectionFeature
  | DeclaredOperatorFeature
  | SignatureFeature
  | ConstrainedSignatureFeature
  | ExplicitTypeApplicationFeature
  | ModuleFeature
  | AliasFeature
  | ImportFeature
  | ExplicitImportFeature
  | ValueExportFeature
  | TypeExportFeature
  | ConstructorExportFeature
  | ClassExportFeature
  | ClassFeature
  | ImplFeature
  | QualifiedMethodFeature
  | PureFunctionFeature
  | EffectfulFunctionFeature
  deriving (Bounded, Enum, Eq, Ord, Show)
```

Assert:

```haskell
missing = requiredAuthoredFeatures `Set.difference` observed
```

and print missing constructors on failure.

- [ ] **Step 2: Run repository audit and confirm the inventory is unimplemented**

Run the repository-audit command. Expected: FAIL.

- [ ] **Step 3: Fold every surface node recursively**

Traverse statements, expressions, patterns, signature payloads/types, data fields, class/impl methods, imports, and module selectors. Use parser-surface values wherever the distinction survives. For declared-operator metadata, associativity, purity spelling, and any other parser-erased distinction, inspect token kinds or narrowly bounded source forms in the same parsed file.

Do not assert whole-file substrings for features represented in the AST.

- [ ] **Step 4: Assert every public standard-library family is represented**

Collect module paths from the parsed standard-library role and compare them to:

```haskell
Set.fromList
  [ ["Char"],
    ["Dictionary"],
    ["IO"],
    ["IOError"],
    ["List"],
    ["Map"],
    ["Maybe"],
    ["NonEmpty"],
    ["Queue"],
    ["Result"],
    ["Set"],
    ["Text"]
  ]
```

Treat the ambient `Prelude.jz` as a separately asserted bundled source because
it intentionally has no module wrapper.

- [ ] **Step 5: Close genuine aggregate gaps in representative sources**

If a feature is absent, add it to the most natural stdlib, compiler, program, or editor example. Document the chosen file in the test's `featureWitnesses :: Map SurfaceFeature FilePath`, and assert every witness path is part of `readAuthoredSources`.

- [ ] **Step 6: Run repository audit and full parser check**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    repository-audit-spec \
    parser-foundation-spec \
    --test-show-details=failures
```

Expected: PASS with an empty missing-feature set.

- [ ] **Step 7: Commit**

```bash
git add jazz-next/test/JazzNext/Repository jazz-next/jazz-next.cabal jazz-next/jazz jazz-next/programs jazz-next/editors
git commit -m "test: enforce authored Jazz feature coverage"
```

### Task 8: Verify the Authored-Source and Editor Workstream

**Files:**
- Modify only files required by failures found in this verification task.

**Interfaces:**
- Produces: a green authored-source/editor gate for algorithmic corpus work.

- [ ] **Step 1: Run the naming, formatting, and legacy-scope audits**

Run:

```bash
rg -n '\bvalue\b' \
  jazz-next/jazz/stdlib \
  jazz-next/jazz/compiler \
  jazz-next/programs \
  jazz-next/editors/vscode-jazz/fixtures \
  -g '*.jz'
git diff main...HEAD -- jazz-hs jazz2
git diff --check
```

Expected: each `value` occurrence is an export selector or deliberately documented invalid example, the legacy diff is empty, and diff check passes.

- [ ] **Step 2: Run source-facing suites serially**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    repository-audit-spec \
    stdlib-spec \
    program-corpus-spec \
    jazz-lexer-parity-spec \
    jazz-parser-parity-spec \
    canonical-core-comparison-spec \
    --test-show-details=failures \
    --jobs=1
```

Expected: PASS twice for `program-corpus-spec` if budgets changed.

- [ ] **Step 3: Run build and documentation checks**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal build --project-dir=jazz-next all
bash scripts/check-docs.sh
bash scripts/check-execution-queue.sh
```

Expected: PASS.

- [ ] **Step 4: Commit any verification-only corrections**

If corrections were required:

```bash
git add jazz-next docs
git commit -m "fix: close authored Jazz quality gate"
```

Otherwise, record the passing commands without an empty commit.
