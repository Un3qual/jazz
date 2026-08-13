# Jazz API Reference and Hero Lockup Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Turn every standard-library module page into a function-by-function API reference and make the homepage Jazz mark a deliberate, responsive part of the title lockup.

**Architecture:** Treat `jazz/stdlib/*.jz` export lists and declarations as the API inventory, with a dedicated documentation checker preventing missing headings or stale signatures. Keep the Markdown pages hand-authored so contracts and selective examples remain readable. Recompose the existing hero within `HomepageHeader` and its CSS without changing the site shell or documentation directory.

**Tech Stack:** Docusaurus 3, React 19, TypeScript, CSS modules, Python 3 documentation checks, Node test runner, Jazz Markdown fences, pinned Node 22/Nix website environment.

## Global Constraints

- Every public standard-library value gets its own heading, exact type signature, and concise behavioral contract.
- Every public type and constructor is individually discoverable; private helpers and representations stay undocumented.
- Use short expression examples only when they clarify non-obvious behavior; do not add complete programs or repetitive examples.
- Do not use function inventory tables or bullet lists as substitutes for API entries.
- Preserve the existing visual theme, navbar, documentation directory, code proof, and public library behavior.
- The hero mark must use the canonical SVG, remain decorative to assistive technology, and participate in normal layout flow.
- Desktop and 390px mobile layouts must not overflow horizontally.
- Commit each independently reviewable milestone.

---

### Task 1: Enforce standard-library API documentation coverage

**Files:**

- Create: `scripts/check-stdlib-api-docs.py`
- Create: `scripts/test-check-stdlib-api-docs.py`
- Modify: `scripts/check-docs.sh`

**Interfaces:**

- Consumes: public `module (...)` export lists and top-level `name :: Type.` declarations from `jazz/stdlib/*.jz`.
- Produces: `check_module(root: Path, source: Path, document: Path) -> list[str]` and a zero/nonzero CLI used by `scripts/check-docs.sh`.

- [ ] **Step 1: Write checker regression tests.** Add temporary-fixture tests covering: a complete exported value; a missing value heading; a missing signature fence; a stale signature; a public type; a public constructor; and exclusion of a private helper. Require headings of the form ``## `name` `` or ``### `name` `` and exact two-line Jazz fences:

  ````markdown
  ### `maybeMap`

  ```jazz
  maybeMap :: (a -> b) -> Maybe(a) -> Maybe(b).
  ```
  ````

- [ ] **Step 2: Run the regression test and verify RED.** Run:

  ```bash
  python3 scripts/test-check-stdlib-api-docs.py
  ```

  Expected: failure because `scripts/check-stdlib-api-docs.py` does not exist.

- [ ] **Step 3: Implement the source-backed checker.** Parse the module header through its closing `) {`, collect `value`, `type`, and `constructor` exports, and resolve each public value to its top-level signature. Map `Maybe.jz` through `IOError.jz` to the lowercase/hyphenated Markdown pages. Report module-relative diagnostics for missing headings and signatures. Add an explicit Prelude contract for `Ordering`, its constructors, capability methods, compatibility list helpers, `print!`, and the target-named numeric conversions because Prelude is implicitly loaded rather than expressed through a module export list.

- [ ] **Step 4: Run checker unit tests.** Run:

  ```bash
  python3 scripts/test-check-stdlib-api-docs.py
  ```

  Expected: all checker regressions pass.

- [ ] **Step 5: Wire the checker into the docs gate and verify integrated RED.** Add `python3 scripts/check-stdlib-api-docs.py "$ROOT"` near the other public-documentation checks in `scripts/check-docs.sh`, then run:

  ```bash
  python3 scripts/check-stdlib-api-docs.py .
  ```

  Expected: failures list the module exports not yet represented by individual headings and exact signatures.

- [ ] **Step 6: Commit the checker milestone.** Run:

  ```bash
  git add scripts/check-stdlib-api-docs.py scripts/test-check-stdlib-api-docs.py scripts/check-docs.sh
  git commit -m "test(docs): enforce standard library API coverage"
  ```

### Task 2: Document data modules as API references

**Files:**

- Modify: `docs/standard-library/maybe.md`
- Modify: `docs/standard-library/result.md`
- Modify: `docs/standard-library/nonempty.md`

**Interfaces:**

- Consumes: exported declarations in `Maybe.jz`, `Result.jz`, and `NonEmpty.jz`.
- Produces: individually addressable type, constructor, conversion, transformation, query, and fold entries.

- [ ] **Step 1: Rewrite Maybe.** Document `Maybe`, `Nothing`, and `Just`, followed by individual entries for `maybeMap`, `maybeAndThen`, `maybeWithDefault`, `maybeOrElse`, `maybeFilter`, `maybeIsJust`, `maybeIsNothing`, `maybeToList`, and `maybeFromList`. Include short examples for fallback selection, filtering, and list conversion; state that operations are `O(1)` apart from invoked callbacks.

- [ ] **Step 2: Rewrite Result.** Document `Result`, `Err`, and `Ok`, followed by individual entries for `resultMap`, `resultMapError`, `resultAndThen`, `resultRecover`, `resultWithDefault`, `resultIsOk`, `resultIsErr`, `resultToMaybe`, `resultErrorToMaybe`, and `resultFromMaybe`. Use examples only to distinguish success mapping, error mapping, and recovery.

- [ ] **Step 3: Rewrite NonEmpty.** Document `NonEmpty` and its constructor, followed by individual entries for `nonEmptySingleton`, `nonEmptyFromList`, `nonEmptyToList`, `nonEmptyHead`, `nonEmptyTail`, `nonEmptyLast`, `nonEmptyPrepend`, `nonEmptyAppendList`, `nonEmptyMap`, `nonEmptyLength`, `nonEmptyFoldLeft`, and `nonEmptyFoldRight`. State totality and per-operation complexity next to the affected function.

- [ ] **Step 4: Run focused documentation checks.** Run:

  ```bash
  python3 scripts/check-stdlib-api-docs.py .
  git diff --check -- docs/standard-library/maybe.md docs/standard-library/result.md docs/standard-library/nonempty.md
  ```

  Expected: the three data modules produce no checker diagnostics; remaining failures name only unfinished modules.

- [ ] **Step 5: Commit the data-reference milestone.** Run:

  ```bash
  git add docs/standard-library/maybe.md docs/standard-library/result.md docs/standard-library/nonempty.md
  git commit -m "docs(stdlib): expand data module references"
  ```

### Task 3: Document collection modules as API references

**Files:**

- Modify: `docs/standard-library/list.md`
- Modify: `docs/standard-library/dictionary.md`
- Modify: `docs/standard-library/queue.md`
- Modify: `docs/standard-library/map.md`
- Modify: `docs/standard-library/set.md`

**Interfaces:**

- Consumes: exported declarations and observable behavior in the five collection modules.
- Produces: grouped per-function reference entries with ordering, persistence, empty-input, and complexity contracts.

- [ ] **Step 1: Replace the List operations table.** Add individual signature-and-contract entries for all 39 exports, grouped under Shape, Safe access, Slicing, Combining, Transforming, Folding, Search, Pair views, Normalization, and Ordering. Keep selective expressions for negative counts, `listZip` truncation, the initial value in `listScanLeft`, adjacent grouping, first-occurrence distinctness, and stable sorting.

- [ ] **Step 2: Expand Dictionary.** Document the abstract `Dictionary(k, v)` type and individual entries for `dictionaryEmpty`, `dictionarySingleton`, `dictionaryFromList`, `dictionaryToList`, `dictionarySize`, `dictionaryIsEmpty`, `dictionaryLookup`, `dictionaryGetOr`, `dictionaryContainsKey`, `dictionaryInsert`, `dictionaryReplace`, `dictionaryRemove`, `dictionaryUpdate`, `dictionaryKeys`, `dictionaryValues`, `dictionaryMapValues`, `dictionaryFilter`, `dictionaryFoldLeft`, and `dictionaryFoldRight`. State insertion-order and duplicate-key behavior at the relevant entries.

- [ ] **Step 3: Expand Queue.** Document the abstract `Queue(a)` type and individual entries for `queueEmpty`, `queueSingleton`, `queueFromList`, `queueToList`, `queueSize`, `queueIsEmpty`, `queueEnqueue`, `queueEnqueueAll`, `queuePeek`, `queueDequeue`, `queueMap`, `queueFoldLeft`, and `queueFoldRight`. Keep the persistent normalization boundary attached to `queuePeek` and `queueDequeue`, not in a detached complexity summary.

- [ ] **Step 4: Expand Map.** Document the abstract `Map(k, v)` type and every exported construction, query, update, ordered-boundary, traversal, filter, and fold function from `Map.jz`. Put duplicate-key replacement, ascending traversal, `Nothing` behavior, and logarithmic costs next to the functions they qualify.

- [ ] **Step 5: Expand Set.** Document the abstract `Set(a)` type and every exported construction, query, update, combination, traversal, filter, fold, and map function from `Set.jz`. Put uniqueness, ascending order, capability constraints, and operation-specific complexity next to each relevant entry.

- [ ] **Step 6: Run collection checks.** Run:

  ```bash
  python3 scripts/check-stdlib-api-docs.py .
  python3 scripts/check-public-docs.py .
  git diff --check -- docs/standard-library
  ```

  Expected: data and collection pages produce no API checker diagnostics; unfinished diagnostics are limited to Prelude, Char, Text, IO, and IOError.

- [ ] **Step 7: Commit the collection-reference milestone.** Run:

  ```bash
  git add docs/standard-library/list.md docs/standard-library/dictionary.md docs/standard-library/queue.md docs/standard-library/map.md docs/standard-library/set.md
  git commit -m "docs(stdlib): expand collection module references"
  ```

### Task 4: Document text and system modules as API references

**Files:**

- Modify: `docs/standard-library/char.md`
- Modify: `docs/standard-library/text.md`
- Modify: `docs/standard-library/io.md`
- Modify: `docs/standard-library/io-error.md`

**Interfaces:**

- Consumes: public Char, Text, IO, and IOError declarations.
- Produces: Unicode-scalar, text-processing, host-operation, and recoverable-error API entries.

- [ ] **Step 1: Expand Char.** Add one signature-and-contract entry for `charToUInt32`, `charFromUInt32`, `charIsAlpha`, `charIsAlphaNum`, `charIsDigit`, `charIsSpace`, `charIsHexDigit`, `charIsLower`, `charIsUpper`, `charToLower`, `charToUpper`, and `charIsNewline`. Explain Unicode scalar validity and one-scalar, locale-independent case mapping where those rules apply.

- [ ] **Step 2: Expand Text.** Add one signature-and-contract entry for all 29 exported values. Group them under Constants and shape, Access and slicing, Construction, Conversion and traversal, Search, Splitting, Replacement and cleanup, and Padding. Use selective examples for scalar indexing, negative-count clamping, empty delimiters, line-ending recognition, non-overlapping replacement, and padding width.

- [ ] **Step 3: Expand IO.** Replace the export list with individual entries for `readText!`, `writeText!`, `readStdin!`, `writeStdout!`, `writeStderr!`, `arguments!`, and `exit!`. Keep exact `Result(IOError, value)` signatures, strict UTF-8 behavior, path attachment, host-dependent cost, argument order, and `E3030` exit validation next to the relevant function.

- [ ] **Step 4: Expand IOError.** Add individual headings for `IOErrorCategory`, each of its eight constructors, `IOError`, and the `IOError` constructor. Explain the constructor fields using a Jazz declaration fence and distinguish file-path and stream errors. Keep Result-handling guidance as a related-module note.

- [ ] **Step 5: Run text and system checks.** Run:

  ```bash
  python3 scripts/check-stdlib-api-docs.py .
  python3 scripts/check-public-docs.py .
  git diff --check -- docs/standard-library
  ```

  Expected: the only remaining API checker diagnostics are for Prelude.

- [ ] **Step 6: Commit the text/system-reference milestone.** Run:

  ```bash
  git add docs/standard-library/char.md docs/standard-library/text.md docs/standard-library/io.md docs/standard-library/io-error.md
  git commit -m "docs(stdlib): expand text and system references"
  ```

### Task 5: Document the Prelude as an API reference

**Files:**

- Modify: `docs/standard-library/prelude.md`

**Interfaces:**

- Consumes: capability declarations in `Prelude.jz`, public Prelude-target builtins from the compiler catalog, and existing runtime contracts.
- Produces: one reference entry per public Prelude type, capability method, compatibility helper, effectful value, and conversion.

- [ ] **Step 1: Document Prelude types and capabilities.** Add individual headings and declarations for `Ordering`, `LT`, `EQ`, `GT`, `Eq`, `equals`, `Ord`, `compare`, `Num`, `Integral`, `Fractional`, `Showable`, `show`, `Default`, and `defaultValue`. State the concrete built-in implementations, Unicode-scalar/Text ordering rules, stable rendering, and zero-like defaults without repeating the same implementation list under each method.

- [ ] **Step 2: Document compatibility helpers.** Add individual entries with exact signatures for `map`, `filter`, `hd`, `tl`, and `print!`. Put stable ordering next to map/filter, `E3009` and `E3010` next to the partial accessors, and stub-v1 return behavior next to `print!`.

- [ ] **Step 3: Document conversions.** Add individual headings and target-return signatures for `toInt8`, `toInt16`, `toInt32`, `toInt64`, `toUInt8`, `toUInt16`, `toUInt32`, `toUInt64`, `toFloat16`, `toFloat32`, `toFloat64`, `toInt`, and `toFloat`. Use one shared introductory contract for accepted numeric inputs and separate entry text for target range/format and alias behavior; include one narrowing-failure expression and one alias example.

- [ ] **Step 4: Run the complete API checker and docs formatter.** Run:

  ```bash
  python3 scripts/check-stdlib-api-docs.py .
  nix --extra-experimental-features 'nix-command flakes' develop --command prettier --write docs/standard-library scripts/check-stdlib-api-docs.py scripts/test-check-stdlib-api-docs.py
  python3 scripts/check-stdlib-api-docs.py .
  ```

  Expected: the checker exits zero before and after formatting.

- [ ] **Step 5: Commit the Prelude milestone.** Run:

  ```bash
  git add docs/standard-library/prelude.md
  git commit -m "docs(stdlib): expand Prelude reference"
  ```

### Task 6: Recompose the homepage hero mark

**Files:**

- Modify: `website/scripts/test-experience.mjs`
- Modify: `website/src/components/HomepageHeader.tsx`
- Modify: `website/src/components/BrandMark.tsx`
- Modify: `website/src/pages/index.module.css`

**Interfaces:**

- Consumes: the canonical `/img/jazz-mark-dark.svg` asset and existing `HomepageHeader` two-column layout.
- Produces: `.titleLockup`, `.brandPlane`, and `.brandMark` styles with the mark in normal flow beside the title on desktop and above it on mobile.

- [ ] **Step 1: Add failing hero structure and CSS contracts.** Require `HomepageHeader` to wrap `BrandMark` and the `h1` in `styles.titleLockup`; require `.brandPlane` to have a substantial clamped width and no `position: absolute`, `bottom`, `right`, low opacity, or pointer-positioning declarations; require the mobile rule to stack `.titleLockup` vertically.

- [ ] **Step 2: Run the experience test and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:experience
  ```

  Expected: failure on the missing title lockup and old absolute-positioned mark.

- [ ] **Step 3: Implement the title lockup.** Move `<BrandMark />` from the bottom of `introCopy` into a new `titleLockup` wrapper beside the `h1`. Keep the image alt empty and wrapper `aria-hidden="true"`. Remove positioning responsibilities from `BrandMark` so the parent layout owns placement.

- [ ] **Step 4: Implement responsive sizing.** Make `.titleLockup` a flex row aligned by optical center, size `.brandPlane` with a desktop clamp large enough to balance the title, and remove translucency. At `max-width: 760px`, stack the lockup, align it to the copy edge, and use a mobile clamp that fits within the gutter. Preserve the existing intro/code entrance and reduced-motion behavior.

- [ ] **Step 5: Run focused website checks.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run test:experience
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run typecheck
  nix --extra-experimental-features 'nix-command flakes' develop --command pnpm --dir website run build
  ```

  Expected: all commands exit zero.

- [ ] **Step 6: Visually verify the homepage.** Serve the production build and inspect 1440×1000 and 390×844 viewports. Confirm the mark reads as part of the title, the code column remains balanced, the mobile lockup is deliberate, and `document.body.scrollWidth <= window.innerWidth` at both sizes.

- [ ] **Step 7: Commit the hero milestone.** Run:

  ```bash
  git add website/scripts/test-experience.mjs website/src/components/HomepageHeader.tsx website/src/components/BrandMark.tsx website/src/pages/index.module.css
  git commit -m "fix(website): anchor the hero logo"
  ```

### Task 7: Integrated closeout

**Files:**

- Modify: `.codex/plans/2026-08-12-jazz-api-reference-hero.md` only for completed checkboxes and verification receipts.

**Interfaces:**

- Consumes: Tasks 1-6.
- Produces: a clean committed tree with authoritative documentation, website, and browser-QA receipts.

- [ ] **Step 1: Review the full implementation diff and public copy.** Run:

  ```bash
  git diff 66b81f2b..HEAD -- docs/standard-library scripts website
  rg -n '^\|.*Public values|## Operations$|The public API is' docs/standard-library
  git diff --check
  ```

  Expected: every change belongs to the approved reference/hero scope; the inventory scan and whitespace check return no findings.

- [ ] **Step 2: Run the authoritative documentation gate.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh
  ```

  Expected: every documentation checker, example check, RFC check, link check, authority check, queue check, and formatting check passes.

- [ ] **Step 3: Run the authoritative website gate.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-website.sh
  ```

  Expected: brand tests, experience tests, TypeScript, production build, highlighting, Pages policy, and boundary checks pass.

- [ ] **Step 4: Record receipts and commit closeout.** Mark completed plan steps, add exact successful commands and the date to this task, then run:

  ```bash
  git add .codex/plans/2026-08-12-jazz-api-reference-hero.md
  git commit -m "docs: close API reference redesign"
  ```
