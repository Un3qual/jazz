---
id: JN-RUNTIME-CLI-HELP-001
status: ready
priority: P2
size: S
kind: impl
autonomous_ready: yes
depends_on:
  - JN-RUNTIME-CLI-PRODUCT-DELTA-CONTRACT-001
last_verified: 2026-06-24
plan_section: "Implementation batch: CLI help output"
target_paths:
  - jazz-next/src/JazzNext/CLI/Main.hs
  - jazz-next/test/JazzNext/CLI/CLISpec.hs
  - jazz-next/README.md
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Implement explicit `--help` and `-h` CLI usage output that exits 0, writes usage to stdout, writes no stderr, preempts source/config/prelude/module reads, and leaves compile/run semantics unchanged."
supersedes:
  - docs/plans/spec-clarification/2026-03-02/runtime/12a-haskell-interpreter-implementation.md
---

# Jazz-Next CLI Help Output Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add explicit CLI help output for the active interpreter-first
`jazz-next` product surface.

**Architecture:** Add a CLI preflight path for `--help` and `-h` before normal
argument validation or source/config/prelude loading. Keep help text in
`JazzNext.CLI.Main` so the executable and tests share one stable string, and
cover the behavior with focused `CLISpec` tests.

**Tech Stack:** Haskell CLI module under `jazz-next/src/JazzNext/CLI`,
runghc-based CLI tests under `jazz-next/test/JazzNext/CLI`, README quickstart
docs, and repo-root queue/docs validators.

---

## Implementation batch: CLI help output

Executor-safe scope:

- Recognize `--help` and `-h`.
- Print stable usage text to stdout with exit code `0`.
- Keep stderr empty for help.
- Do not read stdin, source files, warning config files, prelude files, or
  module graph files when help is requested.
- Let help preempt other args in `runCliWith` so `--help --bad-arg` and
  `--help missing.jz` still print usage.
- Do not add a bare `help` command; positional source files already own bare
  non-flag arguments.
- Do not change compile/run/module/prelude/warning semantics outside help
  preemption.

### Task 1: Lock help behavior in CLISpec

**Files:**

- Modify: `jazz-next/test/JazzNext/CLI/CLISpec.hs`

- [ ] **Step 1: Add suite entries**

Add these names near the other CLI argument/product-surface tests:

```haskell
    ("cli --help prints usage without reading inputs", testCliHelpOutput),
    ("cli -h prints usage without reading inputs", testCliShortHelpOutput),
    ("cli help flag preempts invalid args and missing source files", testCliHelpPreemptsOtherArgs),
```

- [ ] **Step 2: Add help assertions**

Add these tests near the other `runCliWith` tests:

```haskell
testCliHelpOutput :: IO ()
testCliHelpOutput = do
  sourceRead <- newIORef False
  configRead <- newIORef False
  output <-
    runCliWith
      ["--help"]
      envLookup
      (recordConfigRead configRead)
      (recordSourceRead sourceRead)
  didReadSource <- readIORef sourceRead
  didReadConfig <- readIORef configRead
  assertHelpOutput "--help" output
  assertEqual "source is not read" False didReadSource
  assertEqual "config/prelude files are not read" False didReadConfig
  where
    envLookup "JAZZ_WARNING_CONFIG" = pure (Just "config/warnings.txt")
    envLookup "JAZZ_PRELUDE" = pure (Just "stdlib/Prelude.jz")
    envLookup _ = pure Nothing

testCliShortHelpOutput :: IO ()
testCliShortHelpOutput = do
  sourceRead <- newIORef False
  configRead <- newIORef False
  output <-
    runCliWith
      ["-h"]
      envLookup
      (recordConfigRead configRead)
      (recordSourceRead sourceRead)
  didReadSource <- readIORef sourceRead
  didReadConfig <- readIORef configRead
  assertHelpOutput "-h" output
  assertEqual "source is not read" False didReadSource
  assertEqual "config/prelude files are not read" False didReadConfig
  where
    envLookup "JAZZ_WARNING_CONFIG" = pure (Just "config/warnings.txt")
    envLookup "JAZZ_PRELUDE" = pure (Just "stdlib/Prelude.jz")
    envLookup _ = pure Nothing

testCliHelpPreemptsOtherArgs :: IO ()
testCliHelpPreemptsOtherArgs = do
  sourceRead <- newIORef False
  configRead <- newIORef False
  invalidOutput <-
    runCliWith
      ["--help", "--bad-arg"]
      envLookup
      (recordConfigRead configRead)
      (recordSourceRead sourceRead)
  missingSourceOutput <-
    runCliWith
      ["--help", "missing.jz"]
      envLookup
      (recordConfigRead configRead)
      (recordSourceRead sourceRead)
  didReadSource <- readIORef sourceRead
  didReadConfig <- readIORef configRead
  assertHelpOutput "invalid arg help" invalidOutput
  assertHelpOutput "missing source help" missingSourceOutput
  assertEqual "source is not read" False didReadSource
  assertEqual "config/prelude files are not read" False didReadConfig
  where
    envLookup "JAZZ_WARNING_CONFIG" = pure (Just "config/warnings.txt")
    envLookup "JAZZ_PRELUDE" = pure (Just "stdlib/Prelude.jz")
    envLookup _ = pure Nothing
```

Add these helpers near the existing assertion helpers:

```haskell
assertHelpOutput :: Text -> CliOutput -> IO ()
assertHelpOutput label output = do
  assertEqual (label <> " exit code") 0 (cliExitCode output)
  assertContains (label <> " usage heading") "Usage: jazz-next" (cliStdout output)
  assertContains (label <> " run flag") "--run" (cliStdout output)
  assertContains (label <> " source file") "source.jz" (cliStdout output)
  assertContains (label <> " entry module") "--entry-module" (cliStdout output)
  assertContains (label <> " module root") "--module-root" (cliStdout output)
  assertContains (label <> " prelude flag") "--prelude" (cliStdout output)
  assertContains (label <> " no prelude flag") "--no-prelude" (cliStdout output)
  assertContains (label <> " warning config") "--warnings-config" (cliStdout output)
  assertContains (label <> " warning flag") "-W<category>" (cliStdout output)
  assertContains (label <> " help flag") "--help" (cliStdout output)
  assertContains (label <> " short help flag") "-h" (cliStdout output)
  assertEqual (label <> " stderr") "" (cliStderr output)
```

Add this helper beside `recordSourceRead`:

```haskell
recordConfigRead :: IORef Bool -> FilePath -> IO (Maybe Text)
recordConfigRead configRead _ = do
  writeIORef configRead True
  pure Nothing
```

- [ ] **Step 3: Run the focused suite and confirm RED**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
```

Expected: the suite fails because `--help` and `-h` are not yet recognized as
successful help output.

### Task 2: Implement CLI help preflight

**Files:**

- Modify: `jazz-next/src/JazzNext/CLI/Main.hs`

- [ ] **Step 1: Add stable help text**

Add this top-level value near the `CliOutput` type:

```haskell
helpUsageText :: Text
helpUsageText =
  Text.unlines
    [ "Usage: jazz-next [--run] [options] [source.jz]",
      "       jazz-next [--run] --entry-module Module::Path [--module-root DIR...] [options]",
      "",
      "Modes:",
      "  compile                         Parse/analyze source; success prints no stdout.",
      "  --run                           Execute source and print the final runtime value.",
      "",
      "Source:",
      "  source.jz                       Read one source file instead of stdin.",
      "  --entry-module Module::Path      Load a module graph entrypoint.",
      "  --module-root DIR                Add a module graph search root.",
      "",
      "Prelude and warnings:",
      "  --prelude PATH                   Use an explicit Prelude source.",
      "  --no-prelude                     Disable the bundled Prelude.",
      "  --warnings-config PATH           Read warning settings from PATH.",
      "  -W<category>                     Enable a warning category.",
      "  -Werror=<category>               Promote a warning category to an error.",
      "",
      "Help:",
      "  --help, -h                       Show this help text."
    ]
```

- [ ] **Step 2: Add the help flag predicate**

Add this helper near `parseCliOptions`:

```haskell
isHelpArg :: String -> Bool
isHelpArg arg =
  arg == "--help" || arg == "-h"
```

- [ ] **Step 3: Return help before normal argument work**

At the start of `runCliWith`, before calling `parseCliOptions`, add a guard:

```haskell
runCliWith args envLookup fileLookup loadSource
  | any isHelpArg args =
      pure
        CliOutput
          { cliExitCode = 0,
            cliStdout = helpUsageText,
            cliStderr = ""
          }
  | otherwise =
      case parseCliOptions args of
        ...
```

Keep the existing `parseCliOptions` branch unchanged inside `otherwise`. This
preflight is intentionally before env, config, source, prelude, and module file
lookups.

- [ ] **Step 4: Recognize help in parseCliOptions for direct parser callers**

Add this branch before the unknown-flag branch:

```haskell
    go options (arg : rest)
      | isHelpArg arg =
          go options rest
      | "-W" `isPrefixOf` arg =
          go options {cliWarningFlags = Text.pack arg : cliWarningFlags options} rest
```

This keeps `parseCliOptions ["--help"]` from reporting an unknown argument
without changing the user-facing preflight behavior in `runCliWith`.

- [ ] **Step 5: Run the focused suite and confirm GREEN**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
```

Expected: all `CLISpec` tests pass.

### Task 3: Document help usage

**Files:**

- Modify: `jazz-next/README.md`

- [ ] **Step 1: Add help command documentation**

In the first-program or CLI usage section, add:

````markdown
Show CLI help:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src jazz-next/src/JazzNext/CLI/Main.hs --help
```

The help path prints usage to stdout and does not read stdin, source files,
warning config files, or Prelude files.
````

- [ ] **Step 2: Run final verification**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

Expected: all commands pass; `check-docs.sh` may print the existing Prettier
skip warning outside the Nix shell.
