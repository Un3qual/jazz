---
id: JN-BOOTSTRAP-CANONICAL-COMPARISON-001
status: done
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on:
  - JN-BOOTSTRAP-STACK-SAFE-EVALUATION-001
last_verified: 2026-07-12
completed_on: 2026-07-12
plan_section: "Implementation Batch: Canonical Lexer Comparison"
target_paths:
  - docs/execution/blocker-contracts.md
  - docs/execution/done-archive.md
  - docs/execution/queue.md
  - docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md
  - docs/superpowers/specs/2026-07-11-jazz-next-bootstrap-canonical-comparison-design.md
  - docs/superpowers/plans/2026-07-12-jazz-next-bootstrap-canonical-comparison.md
  - jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs
  - jazz-next/stdlib/LexerTypes.jz
  - jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparison.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs
  - jazz-next/test/JazzNext/Compiler/Parser/FixtureCorpus.hs
  - jazz-next/jazz-next.cabal
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs
  - cabal test --project-dir=jazz-next all --test-show-details=failures
  - bash jazz-next/scripts/check-stdlib-format.sh
  - bash jazz-next/scripts/test-warning-config.sh
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Preserve structured stage-0 lexical failures, define the Jazz-owned canonical lexer result ADTs, adapt Haskell tokens and failures into ordinary RuntimeValue trees, normalize logical fixture paths, and establish a deterministic explicit accepted/rejected parser corpus for the stacked Jazz-authored lexer child."
---

# Jazz-Next Bootstrap Canonical Comparison Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the stable reference adapter and parity harness consumed by the
Jazz-authored lexer without adding lexer logic or temporary serialization.

**Architecture:** Preserve structured reasons inside the stage-0 lexer and keep
the existing diagnostic-returning API as a compatibility wrapper. Define the
canonical schema as ordinary Jazz ADTs, mirror it in a test-only Haskell module,
lower the Haskell mirror to `RuntimeValue`, and use `renderRuntimeValue` as the
only textual evidence format. A static fixture manifest records parser inputs
and classifications without runtime instrumentation or test-source parsing.

**Tech Stack:** Jazz `.jz`, Haskell 2010, Megaparsec, `Data.Text`, the existing
Jazz runtime and module graph, the custom `NamedTest` harness, Cabal component
suites, and repository queue/docs validators.

## Global Constraints

- Modify only `jazz-next/` and active documentation; `jazz-hs/` and `jazz2/`
  remain read-only.
- Keep `tokenize :: Text -> Either Diagnostic [Token]` source-compatible.
- Do not reconstruct lexical reasons from rendered `Diagnostic` text.
- Use ordinary Jazz ADTs and the existing `renderRuntimeValue`; do not add JSON,
  a custom serializer, bytecode, a VM, lowered IR, or LLVM-specific values.
- Use logical nonempty relative `/` paths; reject absolute paths and `..`.
- Preserve arbitrary-precision integer values as canonical decimal `Text`.
- Keep the Haskell canonical adapter under `jazz-next/test/`.
- Keep every `.jz` file at exactly two spaces per indentation level.
- Prefer behavioral tests; source inspection is limited to stdlib formatting
  and manifest-maintenance checks.
- Implement behavior test-first and commit each independently reviewable task.

---

## File Ownership Map

| File | Responsibility |
| --- | --- |
| `jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs` | Structured lexical reasons, detailed tokenization result, legacy diagnostic adapter. |
| `jazz-next/stdlib/LexerTypes.jz` | Language-owned canonical result, token, span, path, keyword, punctuation, and error ADTs. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparison.hs` | Test-only Haskell mirror, path normalization, exhaustive token/failure adapter, `RuntimeValue` construction and rendering. |
| `jazz-next/test/JazzNext/Compiler/Parser/FixtureCorpus.hs` | Stable named parser source corpus and accepted/rejected classification. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs` | Behavioral schema, adapter, corpus, determinism, and Jazz/Haskell rendering tests. |
| `jazz-next/jazz-next.cabal` | Focused test component and shared test-module registration. |
| Active docs in frontmatter | Queue promotion, completion evidence, and next stacked child handoff. |

---

## Implementation Batch: Canonical Lexer Comparison

### Task 1: Preserve structured stage-0 lexical failures

**Files:**

- Create: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparison.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Parser/FixtureCorpus.hs`
- Create: `jazz-next/stdlib/LexerTypes.jz`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs`

**Interfaces:**

- Produces:
  `tokenizeDetailed :: Text -> Either LexicalFailure [Token]`,
  `LexicalFailure { lexicalFailureReason :: LexicalFailureReason,
  lexicalFailureSpan :: SourceSpan }`, and the exhaustive
  `LexicalFailureReason` constructors from the accepted design.
- Preserves: `tokenize :: Text -> Either Diagnostic [Token]` and current
  `E0001` summaries.

- [x] **Step 1: Add failing detailed-failure tests.**

Create empty owner modules for the later canonical adapter, fixture corpus, and
Jazz ADTs, then create the focused test executable and register cases asserting
exact values:

```haskell
assertEqual
  "unexpected character"
  (Left (LexicalFailure (UnexpectedCharacter '`') (SourceSpan 1 7)))
  (tokenizeDetailed "value ` 42.")

assertEqual
  "invalid escape"
  (Left (LexicalFailure (InvalidEscape 'x') (SourceSpan 1 1)))
  (tokenizeDetailed "'\\x'")

assertEqual
  "surrogate escape"
  (Left (LexicalFailure (NonScalarUnicodeEscape "D800") (SourceSpan 1 1)))
  (tokenizeDetailed "'\\u{D800}'")
```

Cover empty/multi-scalar character literals, unterminated character/text
literals, raw newlines, malformed Unicode digits, non-scalar Unicode values,
invalid literal characters, and the existing diagnostic wrapper.

- [x] **Step 2: Run the focused test and verify RED.**

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs
```

Expected: compilation fails because `LexicalFailure`,
`LexicalFailureReason`, and `tokenizeDetailed` are not exported.

- [x] **Step 3: Replace text-only custom failures with structured payloads.**

Define the failure types in `Lexer.hs`, change the private Megaparsec custom
component to carry `LexicalFailure`, and make literal helpers construct exact
reason values. Keep one renderer from each reason to the current user-facing
summary. Implement `tokenizeDetailed` over the parser bundle and implement
`tokenize` as:

```haskell
tokenize source =
  case tokenizeDetailed source of
    Right tokens -> Right tokens
    Left failure -> Left (lexicalFailureDiagnostic failure)
```

The diagnostic adapter sets code `E0001`, primary span to
`lexicalFailureSpan`, and preserves the current summary wording without a
second parse of rendered output.

- [x] **Step 4: Run the focused test and existing token parser suite GREEN.**

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/TokenParserSpec.hs
```

Expected: both suites pass and legacy `E0001` diagnostics retain their tested
substrings.

- [x] **Step 5: Commit structured lexical failures.**

```bash
git add jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs
git commit -m "refactor: preserve structured lexical failures"
```

### Task 2: Define and render the canonical schema on both sides

**Files:**

- Create: `jazz-next/stdlib/LexerTypes.jz`
- Create: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparison.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs`

**Interfaces:**

- Produces:
  `normalizeCanonicalSourcePath :: FilePath -> Either Text CanonicalSourcePath`,
  `canonicalizeLexResult :: CanonicalSourcePath -> Either LexicalFailure [Token]
  -> CanonicalLexResult`,
  `canonicalLexResultRuntimeValue :: CanonicalLexResult -> RuntimeValue`, and
  `renderCanonicalLexResult :: CanonicalLexResult -> Text`.
- Consumes: `Token`, `TokenKind`, `LexicalFailure`, and `renderRuntimeValue`.

- [x] **Step 1: Add failing canonical mapping tests.**

Add tests for:

```haskell
normalizeCanonicalSourcePath "fixtures/./parser//basic.jz"
  == Right (CanonicalSourcePath "fixtures/parser/basic.jz")
normalizeCanonicalSourcePath "/tmp/basic.jz"
  == Left "canonical source path must be relative"
normalizeCanonicalSourcePath "fixtures/../basic.jz"
  == Left "canonical source path must not contain '..'"
```

Assert exact rendering for identifiers, every keyword and punctuation,
operators, leading-zero integers, an integer above `Int64`, escaped character
and text payloads, spans, and every structured error reason. Assert a path with
backslashes is rejected rather than platform-normalized.

- [x] **Step 2: Run focused tests and verify RED.**

Run the focused command from Task 1. Expected: compilation fails because the
canonical comparison module and Jazz schema do not exist.

- [x] **Step 3: Add `LexerTypes.jz` with two-space indentation.**

Declare and explicitly export the approved canonical ADTs. Use constructor
payloads exactly as documented, including `IntegerKind Text`; do not add
rendering functions or a schema version.

- [x] **Step 4: Implement the test-only Haskell mirror and total adapters.**

Mirror the Jazz constructor names with Haskell ADTs. Map every `TokenKind` and
`LexicalFailureReason` by exhaustive pattern matching. Build constructor values
with:

```haskell
canonicalConstructor :: Text -> [RuntimeValue] -> RuntimeValue
canonicalConstructor name arguments =
  VConstructor
    (sourceName (mkIdentifier name))
    []
    (sourceName (mkIdentifier name))
    (replicate (length arguments) DataConstructorArgumentOpaque)
    arguments
```

Use `VText`, `VChar`, and `VList` directly. Construct span `Int` values by
evaluating `ELit (LInt value)` through `evaluateRuntimeExpr`; do not export or
duplicate the runtime's private integer metadata. Call only
`renderRuntimeValue` for textual evidence.

- [x] **Step 5: Prove Jazz and Haskell construct identical values.**

Run a small module graph whose entry imports `LexerTypes` and returns a
representative `CanonicalLexSuccess`. Compare its `runOutput` with
`renderCanonicalLexResult` for the equivalent Haskell value. Execute the Jazz
graph twice and require identical output.

- [x] **Step 6: Run focused tests and stdlib formatting GREEN.**

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs
bash jazz-next/scripts/check-stdlib-format.sh
```

- [x] **Step 7: Commit the canonical value contract.**

```bash
git add jazz-next/stdlib/LexerTypes.jz jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparison.hs jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs
git commit -m "feat: add canonical lexer comparison values"
```

### Task 3: Establish the explicit parser fixture corpus

**Files:**

- Create: `jazz-next/test/JazzNext/Compiler/Parser/FixtureCorpus.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`

**Interfaces:**

- Produces:
  `ParserFixtureExpectation = ParserAccepted | ParserRejected`,
  `ParserFixture { parserFixtureName :: Text, parserFixturePath :: FilePath,
  parserFixtureSource :: Text, parserFixtureExpectation ::
  ParserFixtureExpectation }`, and `parserFixtureCorpus :: [ParserFixture]`.
- Consumes: `parseSurfaceProgram`, `tokenizeDetailed`, and the canonical Haskell
  adapter.

- [x] **Step 1: Add failing corpus invariant tests.**

Require nonempty unique names, valid normalized paths, stable manifest order,
both parser classifications, all token constructors, every lexical failure
reason, tabs, LF, CRLF, comments, Unicode literals, leading-zero integers, an
integer above `Int64`, and the current parser source inventory. For each entry,
assert the recorded classification equals `isRight (parseSurfaceProgram
source)` and assert stage-0 adaptation renders deterministically.

- [x] **Step 2: Run the focused suite and verify RED.**

Expected: compilation fails because `FixtureCorpus` does not exist.

- [x] **Step 3: Build the static corpus from actually exercised parser inputs.**

Inventory the parser suites by executing them with a temporary development-only
observer, deduplicate exact source text, classify each source with
`parseSurfaceProgram`, emit stable names and logical paths into
`FixtureCorpus.hs`, then remove the observer before staging. The committed
production lexer and parser must contain no environment lookup, global mutable
registry, file write, or test-capture hook.

Add focused named fixtures not already exercised for CRLF, all escapes, every
lexical reason, leading zeros, and arbitrary-precision integers. Keep the
manifest static Haskell data; do not parse Haskell test source files at test
runtime.

- [x] **Step 4: Register the Cabal test component.**

Add `canonical-lexer-comparison-spec` with the common test stanza and register
`CanonicalLexerComparison` and `FixtureCorpus` as its `other-modules`.

- [x] **Step 5: Run focused and all parser suites GREEN.**

```bash
cabal test --project-dir=jazz-next canonical-lexer-comparison-spec adt-pattern-parser-spec declaration-parser-spec expression-parser-spec if-expression-parser-spec lambda-parser-spec module-import-parser-spec operator-fixity-spec operator-invalid-syntax-spec operator-section-spec parser-foundation-spec pattern-parser-spec token-parser-spec --test-show-details=failures
```

- [x] **Step 6: Commit the explicit corpus and harness component.**

```bash
git add jazz-next/test/JazzNext/Compiler/Parser/FixtureCorpus.hs jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs jazz-next/jazz-next.cabal
git commit -m "test: establish canonical lexer fixture corpus"
```

### Task 4: Close Child 1 and expose the Jazz lexer child

**Files:**

- Modify: `docs/execution/queue.md`
- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/execution/done-archive.md`
- Modify: `docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md`
- Modify: `docs/superpowers/plans/2026-07-12-jazz-next-bootstrap-canonical-comparison.md`

**Interfaces:**

- Produces: completed Child 1 evidence and one source-backed next candidate,
  `JN-BOOTSTRAP-JAZZ-LEXER-001`.

- [x] **Step 1: Run full implementation verification.**

```bash
cabal test --project-dir=jazz-next all --test-show-details=failures
bash jazz-next/scripts/check-stdlib-format.sh
bash jazz-next/scripts/test-warning-config.sh
```

Expected: all commands exit `0`.

- [x] **Step 2: Close queue metadata.**

Mark this plan `done`, set `completed_on` and `last_verified` to `2026-07-12`,
move the queue entry to `done-archive.md` with exact verification evidence, and
make `JN-BOOTSTRAP-JAZZ-LEXER-001` the sole `Next Curation Target`. Its target
paths must name the Jazz lexer modules, permanent `Char`/`Text` support APIs,
focused differential suite, Cabal file, child plan, and queue metadata.

- [x] **Step 3: Run closeout checks.**

```bash
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

- [x] **Step 4: Commit Child 1 closeout.**

```bash
git add docs/execution docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md docs/superpowers/plans/2026-07-12-jazz-next-bootstrap-canonical-comparison.md
git commit -m "docs: close canonical lexer comparison child"
```
