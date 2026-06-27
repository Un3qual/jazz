---
id: JN-RUNTIME-CLI-STDIN-SENTINEL-001
status: ready
priority: P2
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-06-26
plan_section: "Batch 1: Explicit stdin source sentinel"
target_paths:
  - jazz-next/src/JazzNext/CLI/Main.hs
  - jazz-next/test/JazzNext/CLI/CLISpec.hs
  - docs/spec/tooling/cli-source-input.md
  - jazz-next/README.md
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Accept `-` as an explicit stdin source selector for standalone compile and `--run`, while preserving existing no-positional stdin behavior, positional source-file behavior, module-graph rejection, help preflight behavior, and compile/run output contracts."
supersedes:
  - docs/plans/spec-clarification/2026-03-02/runtime/12a-haskell-interpreter-implementation.md
---

# Jazz-Next Runtime CLI Stdin Sentinel Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let standalone `jazz-next` compile and `--run` invocations use `-` as an explicit stdin source selector.

**Architecture:** Keep source selection owned by `JazzNext.CLI.Main`. Treat `-` as a source selector in the existing optional positional source slot, then special-case that selector in `loadCliSource` to read stdin instead of performing a file lookup. This preserves the current no-positional stdin path, one-file path, module-graph rejection path, help preflight path, and compile/run stdout/stderr contracts.

**Tech Stack:** Haskell CLI code in `jazz-next/src/JazzNext/CLI/Main.hs`, runghc CLI product tests in `jazz-next/test/JazzNext/CLI/CLISpec.hs`, and Markdown source-input/quickstart docs.

---

## Queue Safety

This child is a runtime product delta for `JN-RUNTIME-PRODUCTIZE-CLOSURE-PLAN-001`. It must not reopen the closed compile/run/help baseline: successful compile stays diagnostic-only, `--run` keeps the existing rendered-value output, and help flags still preempt ordinary work. It must not add generated artifacts, backend codegen, a second runtime path, a bare `help` command, or any `jazz-hs/` / `jazz2/` changes.

## Source Evidence

- `docs/spec/tooling/cli-source-input.md` defines the active standalone source-selection contract: no positional source reads stdin; one positional `.jz` source file reads that file; module graph execution is selected through `--entry-module` / `--module-root`; compile remains diagnostic-only and `--run` prints the evaluated value.
- `docs/plans/2026-06-24-jazz-next-runtime-cli-product-delta-contract.md` closed the previous product delta as help-only and explicitly preserved source-selection semantics for that child, leaving later runtime product deltas to be accepted separately.
- `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md` names `JazzNext.CLI.Main` and `CLISpec` as the CLI product-surface owners after the compile/run/help baseline closure.
- Current `jazz-next/src/JazzNext/CLI/Main.hs` parses one optional positional source file, rejects unknown `-`-prefixed arguments, and routes `cliSourcePath = Nothing` to stdin in `loadCliSource`.

## target_paths

- `jazz-next/src/JazzNext/CLI/Main.hs`
- `jazz-next/test/JazzNext/CLI/CLISpec.hs`
- `docs/spec/tooling/cli-source-input.md`
- `jazz-next/README.md`

## Deliverable

Accept `-` as an explicit stdin source selector for standalone compile and `--run`, while preserving existing no-positional stdin behavior, positional source-file behavior, module-graph rejection, help preflight behavior, and compile/run output contracts.

## Verification

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

## Batch 1: Explicit stdin source sentinel

### Task 1: Lock the product behavior in CLISpec

**Files:**

- Modify: `jazz-next/test/JazzNext/CLI/CLISpec.hs`

- [ ] **Step 1: Add focused suite entries near the existing source-file tests**

```haskell
    ("parseCliOptions captures explicit stdin sentinel", testParseExplicitStdinSentinel),
    ("parseCliOptions rejects explicit stdin plus source file", testParseExplicitStdinWithSourcePath),
    ("cli explicit stdin sentinel compiles stdin quietly", testCliCompileExplicitStdinSuccess),
    ("cli --run explicit stdin sentinel executes stdin", testCliRunExplicitStdinSuccess),
    ("cli rejects explicit stdin sentinel with entry module before reading source", testCliRejectsExplicitStdinWithEntryModuleBeforeRead),
```

- [ ] **Step 2: Add parser tests beside the other `parseCliOptions` tests**

```haskell
testParseExplicitStdinSentinel :: IO ()
testParseExplicitStdinSentinel = do
  options <-
    case parseCliOptions ["--run", "-"] of
      Left err -> failTest ("parseCliOptions failed: " <> renderDiagnostic err)
      Right parsed -> pure parsed
  assertEqual "run mode" True (cliRunMode options)
  assertEqual "stdin sentinel source selector" (Just "-") (cliSourcePath options)

testParseExplicitStdinWithSourcePath :: IO ()
testParseExplicitStdinWithSourcePath = do
  case parseCliOptions ["-", "first.jz"] of
    Left err ->
      assertContains "stdin plus source path message" "multiple source files are not supported" (renderDiagnostic err)
    Right _ ->
      failTest "expected explicit stdin plus source file to fail option parsing"
  case parseCliOptions ["first.jz", "-"] of
    Left err ->
      assertContains "source path plus stdin message" "multiple source files are not supported" (renderDiagnostic err)
    Right _ ->
      failTest "expected source file plus explicit stdin to fail option parsing"
```

- [ ] **Step 3: Add end-to-end CLI tests beside the current positional source-file tests**

```haskell
testCliCompileExplicitStdinSuccess :: IO ()
testCliCompileExplicitStdinSuccess = do
  lookedUpPaths <- newIORef []
  sourceRead <- newIORef False
  output <-
    runCliWith
      ["-"]
      envLookup
      (recordLookupPath lookedUpPaths fileLookup)
      (recordSourceRead sourceRead)
  didRead <- readIORef sourceRead
  paths <- readIORef lookedUpPaths
  assertEqual "exit code" 0 (cliExitCode output)
  assertEqual "compile stdout stays empty" "" (cliStdout output)
  assertEqual "compile stderr stays empty" "" (cliStderr output)
  assertEqual "stdin source is read" True didRead
  assertEqual "stdin sentinel is not file-looked-up" False ("-" `elem` paths)
  where
    envLookup _ = pure Nothing
    fileLookup _ = pure Nothing

testCliRunExplicitStdinSuccess :: IO ()
testCliRunExplicitStdinSuccess = do
  lookedUpPaths <- newIORef []
  sourceRead <- newIORef False
  output <-
    runCliWith
      ["--run", "-"]
      envLookup
      (recordLookupPath lookedUpPaths fileLookup)
      (recordSourceRead sourceRead)
  didRead <- readIORef sourceRead
  paths <- readIORef lookedUpPaths
  assertEqual "exit code" 0 (cliExitCode output)
  assertEqual "runtime stdout" "42\n" (cliStdout output)
  assertEqual "stderr is empty" "" (cliStderr output)
  assertEqual "stdin source is read" True didRead
  assertEqual "stdin sentinel is not file-looked-up" False ("-" `elem` paths)
  where
    envLookup _ = pure Nothing
    fileLookup _ = pure Nothing

testCliRejectsExplicitStdinWithEntryModuleBeforeRead :: IO ()
testCliRejectsExplicitStdinWithEntryModuleBeforeRead = do
  sourceRead <- newIORef False
  output <- runCliWith ["--entry-module", "App::Main", "-"] envLookup configLookup (recordSourceRead sourceRead)
  didRead <- readIORef sourceRead
  assertEqual "exit code" 2 (cliExitCode output)
  assertContains "stdin plus entry diagnostic" "cannot combine source file with --entry-module" (cliStderr output)
  assertEqual "stdout is suppressed" "" (cliStdout output)
  assertEqual "source should not be read when source selection is invalid" False didRead
  where
    envLookup _ = pure Nothing
    configLookup _ = pure Nothing
```

- [ ] **Step 4: Add a small file-lookup recorder near the existing `recordSourceRead` helper**

```haskell
recordLookupPath :: IORef [FilePath] -> (FilePath -> IO (Maybe Text)) -> FilePath -> IO (Maybe Text)
recordLookupPath paths lookupPath path = do
  writeIORef paths . (path :) =<< readIORef paths
  lookupPath path
```

- [ ] **Step 5: Run the focused suite and confirm RED**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
```

Expected: the suite fails because `-` is still reported as `unknown argument: -` or otherwise treated as an invalid flag.

### Task 2: Implement the stdin sentinel in CLI source selection

**Files:**

- Modify: `jazz-next/src/JazzNext/CLI/Main.hs`

- [ ] **Step 1: Let `-` occupy the existing source selector slot before the unknown-flag branch**

In `parseCliOptions`, update the final `go options (arg : rest)` guards to handle the sentinel before the `"-" \`isPrefixOf\` arg` rejection:

```haskell
    go options (arg : rest)
      | isHelpArg arg =
          go options rest
      | "-W" `isPrefixOf` arg =
          go options {cliWarningFlags = Text.pack arg : cliWarningFlags options} rest
      | arg == "-" && isJust (cliSourcePath options) =
          Left (mkMessageDiagnostic "multiple source files are not supported")
      | arg == "-" =
          go options {cliSourcePath = Just arg} rest
      | "-" `isPrefixOf` arg =
          Left (mkMessageDiagnostic ("unknown argument: " <> Text.pack arg))
      | isJust (cliSourcePath options) =
          Left (mkMessageDiagnostic "multiple source files are not supported")
      | otherwise =
          go options {cliSourcePath = Just arg} rest
```

This intentionally keeps `-W...`, `--help`, `-h`, `--run`, `--entry-module`, `--module-root`, `--prelude`, `--no-prelude`, and `--warnings-config` on their existing paths.

- [ ] **Step 2: Route the sentinel to stdin in `loadCliSource`**

Update `loadCliSource` so `Just "-"` reads from the injected stdin loader and never calls `fileLookup "-"`:

```haskell
loadCliSource options fileLookup loadStdin =
  case cliSourcePath options of
    Nothing -> Right <$> loadStdin
    Just "-" -> Right <$> loadStdin
    Just sourcePath -> do
      sourceContents <- fileLookup sourcePath
      pure $
        case sourceContents of
          Just contents -> Right contents
          Nothing ->
            Left
              (mkMessageDiagnostic ("source file could not be read at '" <> Text.pack sourcePath <> "'"))
```

- [ ] **Step 3: Run the focused suite and confirm GREEN**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
```

Expected: all CLI tests pass.

### Task 3: Document the explicit stdin selector without changing help output

**Files:**

- Modify: `docs/spec/tooling/cli-source-input.md`
- Modify: `jazz-next/README.md`

- [ ] **Step 1: Update the source-selection contract**

In `docs/spec/tooling/cli-source-input.md`, add one source-selection bullet after the no-positional stdin rule:

```markdown
2. With `-` as the positional source selector, standalone CLI compile and
   `--run` read the program from stdin explicitly. `-` counts as the one
   allowed source selector.
```

Then renumber the existing file-source, compile, and run bullets.

- [ ] **Step 2: Update the rejection contract**

In `docs/spec/tooling/cli-source-input.md`, add this rejection rule after the multiple-source rule:

```markdown
2. `-` cannot be combined with a positional `.jz` source file and reports:
   `multiple source files are not supported`.
```

Then renumber the existing entry-module and missing-file rules. Keep the existing entry-module diagnostic unchanged when `-` is combined with `--entry-module`.

- [ ] **Step 3: Add a README stdin example near the first-program run example**

In `jazz-next/README.md`, add a short explicit-stdin example without changing the existing help output contract:

````markdown
Run source from stdin explicitly:

```bash
printf '40 + 2.' | bash jazz-next/scripts/runghc.sh -i./jazz-next/src jazz-next/src/JazzNext/CLI/Main.hs --run -
```
````

- [ ] **Step 4: Run docs and queue verification**

Run:

```bash
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

Expected: all commands pass after the central queue row is added with frontmatter matching this plan.

## Non-goals

- No changes to successful compile stdout/stderr behavior.
- No changes to `--run` rendering or runtime value semantics.
- No changes to explicit source-file reads except rejecting source-file plus `-` as multiple sources.
- No changes to module graph loading beyond preserving the existing source-selector rejection before stdin reads.
- No changes to `--help` / `-h` preflight behavior or help text in this child.
- No generated artifacts, packaging, backend target work, or second runtime pipeline.
- No edits under `jazz-hs/` or `jazz2/`.
