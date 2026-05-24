---
id: JN-FIRST-PROGRAM-CLI-FILE-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-05-23
plan_section: "Batch 1: Positional source file run path"
target_paths:
  - jazz-next/src/JazzNext/CLI/Main.hs
  - jazz-next/test/JazzNext/CLI/CLISpec.hs
  - jazz-next/README.md
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
  - bash jazz-next/scripts/test-warning-config.sh
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "The `jazz-next` CLI accepts one positional `.jz` source file for compile and `--run`, preserves stdin behavior when no file is provided, reports missing source files deterministically, and documents a first-program quickstart."
supersedes:
  - docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md
---

# Jazz Next First Program CLI Flow Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the first small Jazz program runnable from a `.jz` file with a direct `jazz-next` CLI invocation.

**Architecture:** Extend the existing `JazzNext.CLI.Main` option parser with one optional positional source file while preserving stdin as the default source. Reuse the existing injectable file lookup used by CLI tests so the behavior remains testable without shelling out, and document the current `runghc`-based developer command until a packaged executable exists.

**Tech Stack:** Haskell (`jazz-next/src/JazzNext/CLI/Main.hs`), runghc-based CLI tests (`jazz-next/test/JazzNext/CLI/CLISpec.hs`), Markdown quickstart (`jazz-next/README.md`).

---

## Queue Safety

This is a small productization detour. It must not replace or edit the existing `Ready Now` rows for parser-boundary work. The queue change for this plan should only prepend `JN-FIRST-PROGRAM-CLI-FILE-001` ahead of the existing ready rows, leaving their ids, plan links, target paths, and verification commands unchanged.

## Batch 1: Positional source file run path

Next executor-safe batch. It makes the already-working stdin runtime path usable with a file path such as:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src jazz-next/src/JazzNext/CLI/Main.hs --run first.jz
```

### Task 1: Lock positional file CLI behavior

**Files:**
- Modify: `jazz-next/test/JazzNext/CLI/CLISpec.hs`

- [ ] **Step 1: Add tests to the suite list**

Insert these test names near the existing run-mode tests:

```haskell
    ("parseCliOptions captures positional source path", testParseSourcePath),
    ("parseCliOptions rejects multiple positional source paths", testParseMultipleSourcePaths),
    ("cli compiles positional source file quietly", testCliCompileSourceFileSuccess),
    ("cli --run executes positional source file", testCliRunSourceFileSuccess),
    ("cli positional source file reports missing file", testCliSourceFileMissing),
```

- [ ] **Step 2: Add parser-option tests**

Add these tests near the other `parseCliOptions` tests:

```haskell
testParseSourcePath :: IO ()
testParseSourcePath = do
  options <-
    case parseCliOptions ["--run", "first.jz"] of
      Left err -> failTest ("parseCliOptions failed: " <> renderDiagnostic err)
      Right parsed -> pure parsed
  assertEqual "run mode" True (cliRunMode options)
  assertEqual "source path" (Just "first.jz") (cliSourcePath options)

testParseMultipleSourcePaths :: IO ()
testParseMultipleSourcePaths =
  case parseCliOptions ["first.jz", "second.jz"] of
    Left err ->
      assertContains "multiple source path message" "multiple source files are not supported" (renderDiagnostic err)
    Right _ ->
      failTest "expected multiple source paths to fail option parsing"
```

- [ ] **Step 3: Add end-to-end CLI tests**

Add these tests near the other `runCliWith` tests:

```haskell
testCliCompileSourceFileSuccess :: IO ()
testCliCompileSourceFileSuccess = do
  sourceRead <- newIORef False
  output <-
    runCliWith
      ["first.jz"]
      envLookup
      fileLookup
      (recordSourceRead sourceRead)
  didRead <- readIORef sourceRead
  assertEqual "exit code" 0 (cliExitCode output)
  assertEqual "compile stdout stays empty" "" (cliStdout output)
  assertEqual "compile stderr stays empty" "" (cliStderr output)
  assertEqual "stdin source is ignored when source file is present" False didRead
  where
    envLookup _ = pure Nothing
    fileLookup "first.jz" = pure (Just firstProgramSource)
    fileLookup _ = pure Nothing

testCliRunSourceFileSuccess :: IO ()
testCliRunSourceFileSuccess = do
  output <- runCliWith ["--run", "first.jz"] envLookup fileLookup (pure "ignored = 1.")
  assertEqual "exit code" 0 (cliExitCode output)
  assertEqual "runtime stdout" "42\n" (cliStdout output)
  assertEqual "stderr is empty" "" (cliStderr output)
  where
    envLookup _ = pure Nothing
    fileLookup "first.jz" = pure (Just firstProgramSource)
    fileLookup _ = pure Nothing

testCliSourceFileMissing :: IO ()
testCliSourceFileMissing = do
  output <- runCliWith ["--run", "missing.jz"] envLookup fileLookup (pure "ignored = 1.")
  assertEqual "exit code" 2 (cliExitCode output)
  assertContains "missing source diagnostic" "source file could not be read at 'missing.jz'" (cliStderr output)
  assertEqual "stdout is suppressed" "" (cliStdout output)
  where
    envLookup _ = pure Nothing
    fileLookup _ = pure Nothing
```

- [ ] **Step 4: Add the sample source fixture**

Add this fixture near the other source constants:

```haskell
firstProgramSource :: Text
firstProgramSource = "answer = 40 + 2.\nanswer."
```

- [ ] **Step 5: Run the focused suite and confirm RED**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
```

Expected: the suite fails because `cliSourcePath` and positional source-path parsing do not exist yet.

### Task 2: Implement positional source-file loading

**Files:**
- Modify: `jazz-next/src/JazzNext/CLI/Main.hs`

- [ ] **Step 1: Add the source path field**

Extend `CliOptions` with:

```haskell
    cliSourcePath :: Maybe FilePath,
```

Initialize it in the default `CliOptions` value as `Nothing`.

- [ ] **Step 2: Parse one positional source path**

Replace the final unknown-argument branch in `parseCliOptions` with logic that accepts one non-flag argument:

```haskell
      | "-" `isPrefixOf` arg =
          Left (mkMessageDiagnostic ("unknown argument: " <> Text.pack arg))
      | isJust (cliSourcePath options) =
          Left (mkMessageDiagnostic "multiple source files are not supported")
      | otherwise =
          go options {cliSourcePath = Just arg} rest
```

Keep the existing warning-flag branch above this code so `-W...` handling stays unchanged.

- [ ] **Step 3: Reject source path plus module graph mode**

Add this `finalize` guard before accepting module graph options:

```haskell
      | isJust (cliSourcePath options) && isJust (cliEntryModule options) =
          Left (mkMessageDiagnostic "cannot combine source file with --entry-module")
```

- [ ] **Step 4: Load the source from file lookup when present**

Replace the standalone source read in `runCliWith` with a helper:

```haskell
                  sourceResult <- loadCliSource options configLookup loadSource
                  case sourceResult of
                    Left sourceError ->
                      pure
                        CliOutput
                          { cliExitCode = 2,
                            cliStdout = "",
                            cliStderr = "error: " <> renderDiagnostic sourceError <> "\n"
                          }
                    Right source ->
                      if cliRunMode options
                        then runExecute settings preludeSource source
                        else runCompile settings preludeSource source
```

Add the helper near `loadWarningConfig`:

```haskell
loadCliSource ::
  CliOptions ->
  (FilePath -> IO (Maybe Text)) ->
  IO Text ->
  IO (Either Diagnostic Text)
loadCliSource options fileLookup loadStdin =
  case cliSourcePath options of
    Nothing -> Right <$> loadStdin
    Just sourcePath -> do
      sourceContents <- fileLookup sourcePath
      pure $
        case sourceContents of
          Just contents -> Right contents
          Nothing ->
            Left
              (mkMessageDiagnostic ("source file could not be read at '" <> Text.pack sourcePath <> "'"))
```

- [ ] **Step 5: Run the focused suite and confirm GREEN**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
```

Expected: `CLISpec` passes.

### Task 3: Document the first-program flow

**Files:**
- Modify: `jazz-next/README.md`

- [ ] **Step 1: Add a first-program quickstart**

Add this section before `## Run tests`:

````markdown
## Run a first program

Create `first.jz`:

```jazz
answer = 40 + 2.
answer.
```

Compile it:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src jazz-next/src/JazzNext/CLI/Main.hs first.jz
````

Successful compile output is quiet. Run it:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src jazz-next/src/JazzNext/CLI/Main.hs --run first.jz
```

Expected output:

```text
42
```
```

- [ ] **Step 2: Run focused and queue verification**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

Expected: all commands pass; `check-docs.sh` may print the existing Prettier skip warning outside the Nix shell.
