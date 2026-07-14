# Jazz-Next GHC 9.14 and Test-Gate Foundation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Move the active `jazz-next` compiler to GHC 9.14.1, make embedded Jazz programs readable with `MultilineStrings`, and replace the warning-named shell mega-gate with Cabal-owned tests plus focused Haskell repository audits.

**Architecture:** Pin GHC 9.14.1 through Nixpkgs 26.05, keep Cabal as the only component inventory and test runner, and fix only the concrete GHC 9.14 compatibility findings established by the preflight. Add one ordinary Cabal test suite whose pure validators own stdlib formatting and private-package policy, migrate authored multiline Jazz literals without rewriting whitespace-sensitive fixtures, then remove the obsolete shell wrappers and update active documentation.

**Tech Stack:** GHC 9.14.1, `base-4.22.0.0`, Cabal 3.16.1.0, Nix flakes, Haskell2010, `MultilineStrings`, the existing `JazzNext.TestHarness`

## Global Constraints

- Implement active compiler and test work only under `jazz-next/`; root Nix and active documentation files may be updated to point at `jazz-next`.
- Do not modify any file under `jazz-hs/` or `jazz2/`.
- Pin GHC exactly to 9.14.1 and target `base >= 4.22 && < 4.23`.
- Keep `default-language: Haskell2010`; do not introduce `GHC2021` or `GHC2024`.
- Use `MultilineStrings` for every hand-written multiline Jazz program embedded in Haskell.
- Keep escaped newline literals or explicit fragments only when a test directly exercises whitespace, indentation, line endings, source spans, or a generated input.
- Let Cabal own test registration, selection, execution, and reporting; do not create a replacement Haskell test launcher.
- Do not mechanically port source-spelling architecture guards into Haskell.
- Keep GHC profiling and runtime instrumentation in Batch 3; this plan must not add profiling flags, cost centres, eventlog markers, or runtime counters.
- Use `apply_patch` for edits and commit after every completed task.

## Resolved Toolchain Facts

The implementation uses the package set verified during planning:

| Component       | Resolved version                                                   | Cabal bound                            |
| --------------- | ------------------------------------------------------------------ | -------------------------------------- |
| GHC             | 9.14.1                                                             | exact Nix compiler attribute `ghc9141` |
| `base`          | 4.22.0.0                                                           | `>= 4.22 && < 4.23`                    |
| `bytestring`    | 0.12.2.0                                                           | `>= 0.12 && < 0.13`                    |
| `containers`    | 0.8                                                                | `>= 0.8 && < 0.9`                      |
| `directory`     | 1.3.10.0                                                           | `>= 1.3 && < 1.4`                      |
| `filepath`      | 1.5.4.0                                                            | `>= 1.5 && < 1.6`                      |
| `megaparsec`    | 9.7.0 in the Nix Haskell package set; 9.8.1 in the Cabal preflight | `>= 9.0 && < 10`                       |
| `text`          | 2.1.3                                                              | `>= 2.1 && < 2.2`                      |
| `transformers`  | 0.6.1.2                                                            | `>= 0.6 && < 0.7`                      |
| `cabal-install` | 3.16.1.0                                                           | supplied by the pinned Nixpkgs input   |

The GHC 9.14.1 preflight compiled and ran all 36 current Cabal suites after temporarily allowing the old bounds and demoting only `unused-imports`, `pattern-namespace-specifier`, and `x-partial`. Therefore this plan contains no open-ended compatibility step.

---

### Task 1: Pin GHC 9.14.1 and make the active compiler warning-clean

**Files:**

- Modify: `flake.nix`
- Modify: `flake.lock`
- Modify: `jazz-next/jazz-next.cabal`
- Modify: `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Analyzer/UnusedBindings.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleCompiler.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Pattern.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/RecursiveBindings.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime/Primitives.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime/ScopePlan.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime/Semantics.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime/Types.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Capabilities.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Pattern.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Scope.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Signature.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Solver.hs`

**Interfaces:**

- Consumes: the existing private Cabal library, executable, 36 test suites, and `development` flag.
- Produces: a Nix development shell and Cabal package that compile warning-clean with GHC 9.14.1; shared test components gain `MultilineStrings` but production components remain `Haskell2010` without that default extension.

- [ ] **Step 1: Record the old gate and old toolchain baseline**

Run before editing:

```bash
ghc --numeric-version
bash jazz-next/scripts/test-warning-config.sh
```

Expected: GHC reports `9.4.8` in the current host environment and the old shell gate exits zero. If the old gate does not pass, stop and diagnose the pre-existing failure before changing the toolchain.

- [ ] **Step 2: Replace the legacy Nix package selection**

Make `flake.nix` use this active structure:

```nix
{
  description = "Jazz development and spec-cleanup checks";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        ghc = pkgs.haskell.compiler.ghc9141;
        hsPkgs = pkgs.haskell.packages.ghc9141;
        jazzNext = hsPkgs.callCabal2nix "jazz-next" ./jazz-next { };
      in {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            ghc
            cabal-install
            ormolu
            hlint
            git
            ripgrep
            nodePackages.prettier
          ];
        };

        checks.jazz-next-test-suite = pkgs.haskell.lib.overrideCabal jazzNext (_: {
          doCheck = true;
        });
      });
}
```

Remove the Stack package, GHC 9.4 package set, `diagnose`/`qbe` legacy overrides, `jazzHs`, and the legacy `checks.jazz-test-suite` definition.

Update the lock:

```bash
nix --extra-experimental-features 'nix-command flakes' flake update nixpkgs
```

Expected: `flake.lock` records `nixos-26.05` and a concrete Nixpkgs revision.

- [ ] **Step 3: Raise Cabal bounds and enable multiline literals only for tests**

Apply these bounds consistently to the library, executable, and shared test stanza where each dependency is present:

```cabal
base >= 4.22 && < 4.23
bytestring >= 0.12 && < 0.13
containers >= 0.8 && < 0.9
directory >= 1.3 && < 1.4
filepath >= 1.5 && < 1.6
megaparsec >= 9.0 && < 10
text >= 2.1 && < 2.2
transformers >= 0.6 && < 0.7
```

Add this field to `common test-common` and nowhere in the production library/executable stanzas:

```cabal
  default-extensions: MultilineStrings
```

- [ ] **Step 4: Verify the new compiler exposes the preflight failures**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command ghc --numeric-version
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build --project-dir=jazz-next all
```

Expected: the first command prints `9.14.1`; the build fails under `-Werror` on the known redundant `foldl'` imports, deprecated `pattern` namespace spelling, and partial list functions. Do not add warning suppressions.

- [ ] **Step 5: Remove imports made redundant by the GHC 9.14 Prelude**

Remove `Data.List (foldl')` imports from `Analyzer.hs`, `Analyzer/UnusedBindings.hs`, `ModuleCompiler.hs`, `Pattern.hs`, `Runtime/ScopePlan.hs`, `TypeInference.hs`, `TypeInference/Pattern.hs`, `TypeInference/Signature.hs`, and `TypeInference/Solver.hs`.

Make the mixed imports exact:

```haskell
-- ModuleResolver.hs
import Data.List (find, sortOn)

-- Runtime.hs
import Data.List (scanl')
```

In `RecursiveBindings.hs`, `TypeInference/Capabilities.hs`, and `TypeInference/Scope.hs`, replace the `foldl'` import with only the `Data.List` helpers introduced in Step 7. All uses of `foldl'` continue through the GHC 9.14 Prelude.

- [ ] **Step 6: Adopt GHC 9.14's explicit data namespace for the pattern synonym**

Add `ExplicitNamespaces` beside `PatternSynonyms` in:

```text
jazz-next/src/JazzNext/Compiler/Runtime.hs
jazz-next/src/JazzNext/Compiler/Runtime/Primitives.hs
jazz-next/src/JazzNext/Compiler/Runtime/Semantics.hs
jazz-next/src/JazzNext/Compiler/Runtime/Types.hs
```

Replace every import or export occurrence:

```haskell
pattern VExplicitResultHints
```

with:

```haskell
data VExplicitResultHints
```

Keep the pattern declaration itself unchanged:

```haskell
pattern VExplicitResultHints :: RuntimeExplicitResultHints -> RuntimeValue -> RuntimeValue
pattern VExplicitResultHints hints innerValue <- VRuntimeExplicitResultHints hints innerValue
```

- [ ] **Step 7: Replace the partial-list sites with explicit non-empty cases**

In `RecursiveBindings.hs`, import `unsnoc` and replace the `last` call:

```haskell
import Data.List (unsnoc)

closestPriorDeclaration declarations =
  case unsnoc (filter (< statementIndex) declarations) of
    Nothing -> Nothing
    Just (_, priorDeclaration) -> Just priorDeclaration
```

In `TypeInference/Capabilities.hs`, import `uncons`, destructure the runtime hints, and use the named first hint:

```haskell
case uncons (constraintRuntimeHintsForDeferred facts state inferredConstraint constraintName maybeMethodKey unresolvedArgumentType) of
  Nothing ->
    addTypeError state (mkAmbiguousDeferredConstraintError inferredConstraint constraintName resolvedArgumentType)
  Just (firstArgumentHint, remainingArgumentHints) ->
    let argumentHints = firstArgumentHint : remainingArgumentHints
        implFactHints =
          filter
            (constraintImplFactExistsForDeferred facts inferredConstraint constraintName)
            argumentHints
        methodBodyHints methodKey =
          filter
            (\argumentHint -> concreteImplMethodBodyExists methodKey argumentHint facts)
            implFactHints
        ambiguousMethodBodyHints methodKey =
          inferredConstraint
            && expressionTypeContainsUncommittedIntegerLiteral unresolvedArgumentType
            && length (methodBodyHints methodKey) > 1
            && not (uniqueExactRuntimeCandidateHint state unresolvedArgumentType (methodBodyHints methodKey))
        renderedImplFactKey =
          constraintName <> "(" <> renderCapabilityType firstArgumentHint <> ")"
     in case maybeMethodKey of
          Nothing
            | not (null implFactHints) ->
                state
            | inferredConstraint
                && inferredEqualityConstraintCanUseStructuralRuntimeEquality state structuralFacts maybeMethodKey constraintName resolvedArgumentType ->
                state
            | otherwise ->
                addTypeError state (mkMissingExplicitConstraintImplFactError renderedImplFactKey)
          Just methodKey
            | null implFactHints ->
                addTypeError state (mkMissingExplicitConstraintImplFactError renderedImplFactKey)
            | ambiguousMethodBodyHints methodKey ->
                addTypeError state (mkAmbiguousQualifiedMethodBodyError methodKey)
            | not (null (methodBodyHints methodKey)) ->
                state
            | otherwise ->
                addTypeError state (mkMissingImplMethodBodyError methodKey)
```

In `TypeInference/Scope.hs`, import `uncons` and `unsnoc`. Replace each guarded `head`/`last` use with named values obtained from those total functions:

```haskell
case Map.lookup statementIndex recursiveGroupsByStatement >>= uncons of
  Just (firstMember, _)
    | statementIndex == firstMember ->
        Map.insert firstMember state groupStartStates
  _ -> groupStartStates
```

Replace `generalizeCompletedRecursiveGroup` with the same body under total boundary matching:

```haskell
generalizeCompletedRecursiveGroup pendingSignatures statementIndex currentEnv groupStartStates state =
  case Map.lookup statementIndex recursiveGroupsByStatement of
    Nothing -> (currentEnv, state)
    Just groupMembers ->
      case (uncons groupMembers, unsnoc groupMembers) of
        (Just (firstMember, _), Just (_, finalMember))
          | statementIndex == finalMember ->
              let groupBindingNames =
                    Set.fromList
                      [ bindingName
                        | memberIndex <- groupMembers,
                          Just bindingName <- [Map.lookup memberIndex bindingNamesByStatement]
                      ]
                  envOutsideGroup =
                    foldl' (flip Map.delete) currentEnv groupBindingNames
                  nextEnv =
                    foldl'
                      (generalizeRecursiveGroupMember pendingSignatures envOutsideGroup state)
                      currentEnv
                      groupMembers
                  groupStartState =
                    Map.findWithDefault state firstMember groupStartStates
                  groupBindings =
                    [ binding
                      | memberIndex <- groupMembers,
                        Just bindingName <- [Map.lookup memberIndex bindingNamesByStatement],
                        Just binding <- [Map.lookup bindingName nextEnv]
                    ]
               in
                ( nextEnv,
                  pruneCapturedInferredClassConstraintsForBindings groupStartState groupBindings state
                )
        _ -> (currentEnv, state)
```

Change `exposeGroup` from guards over `null` and `last` to a total `unsnoc` case, then keep its current preview body:

```haskell
exposeGroup (envAcc, stateAcc) groupMembers =
  case unsnoc groupMembers of
    Nothing ->
      (envAcc, stateAcc)
    Just (_, finalMember)
      | statementIndex `elem` groupMembers ->
          (envAcc, stateAcc)
      | statementIndex > finalMember ->
          (envAcc, stateAcc)
      | any (`Set.member` signedBindingStatements) groupMembers ->
          (envAcc, stateAcc)
      | interleavedBindingFeedsLaterGroup statementIndex groupMembers ->
          (envAcc, stateAcc)
      | laterGroupMemberDependsOnInterveningBinding statementIndex groupMembers ->
          (envAcc, stateAcc)
      | null processedMembers ->
          (envAcc, stateAcc)
      | otherwise ->
          case previewRecursiveGroupState envAcc stateAcc statementIndex groupMembers of
            Nothing ->
              (envAcc, stateAcc)
            Just previewState ->
              let groupBindingNames =
                    Set.fromList
                      [ bindingName
                        | memberIndex <- groupMembers,
                          Just bindingName <- [Map.lookup memberIndex bindingNamesByStatement]
                      ]
                  envOutsideGroup =
                    foldl' (flip Map.delete) envAcc groupBindingNames
                  nextEnv =
                    foldl'
                      (exposeRecursiveGroupMember statementIndex envOutsideGroup previewState)
                      envAcc
                      processedMembers
                  nextState = rollbackPreviewState stateAcc previewState
               in (nextEnv, nextState)
  where
    processedMembers = filter (< statementIndex) groupMembers
```

Do not disable `-Wx-partial` and do not introduce `NonEmpty` only for this migration.

- [ ] **Step 8: Build and run the existing suite without warning exceptions**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build --project-dir=jazz-next all
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test --project-dir=jazz-next all --test-show-details=failures
```

Expected: the build and all 36 existing suites pass under GHC 9.14.1 with the repository's `-Werror` development flag and no `--allow-newer` or `-Wno-error` options.

- [ ] **Step 9: Commit the toolchain foundation**

```bash
git add flake.nix flake.lock jazz-next/jazz-next.cabal jazz-next/src
git commit -m "build: move jazz-next to GHC 9.14"
```

### Task 2: Add the pure Haskell repository audit

**Files:**

- Modify: `jazz-next/jazz-next.cabal`
- Create: `jazz-next/test/JazzNext/Repository/AuditSpec.hs`
- Create: `jazz-next/test/JazzNext/Repository/PackagePolicy.hs`
- Create: `jazz-next/test/JazzNext/Repository/Root.hs`
- Create: `jazz-next/test/JazzNext/Repository/StdlibFormat.hs`

**Interfaces:**

- Consumes: `JazzNext.TestHarness`, the active package root, `jazz-next.cabal`, and current `stdlib/*.jz` files.
- Produces: `validateStdlibModule :: FilePath -> Text -> [StdlibFormatViolation]`, `validatePackagePolicy :: Text -> [PackagePolicyViolation]`, `findJazzNextPackageRoot :: IO (Either Text FilePath)`, and a Cabal component named `repository-audit-spec`.

- [ ] **Step 1: Register the suite and write its failing contract tests**

Add this Cabal component:

```cabal
test-suite repository-audit-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Repository/AuditSpec.hs
  other-modules:
      JazzNext.Repository.PackagePolicy
      JazzNext.Repository.Root
      JazzNext.Repository.StdlibFormat
```

Create `AuditSpec.hs` with named tests for:

```haskell
tests :: [NamedTest]
tests =
  [ ("accepts a valid stdlib module", testValidStdlibModule),
    ("rejects a missing module header", testMissingModuleHeader),
    ("rejects a missing final closing brace", testMissingClosingBrace),
    ("rejects odd or shallow body indentation", testBodyIndentation),
    ("exempts the bundled Prelude source", testPreludeExemption),
    ("accepts only the named private Cabal library", testPrivatePackagePolicy),
    ("rejects an unnamed public Cabal library", testPublicLibraryPolicy),
    ("rejects a private library without private visibility", testMissingPrivateVisibility),
    ("locates the active jazz-next package root", testPackageRoot),
    ("validates all checked-in stdlib modules", testCheckedInStdlib),
    ("validates the checked-in Cabal package policy", testCheckedInPackagePolicy)
  ]
```

Use `MultilineStrings` for the authored fixtures:

```haskell
validStdlibSource :: Text
validStdlibSource =
  """
  module Good {
    value = 1.
  }
  """

validPrivatePackage :: Text
validPrivatePackage =
  """
  library jazz-next-internal
    visibility: private
  """
```

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test --project-dir=jazz-next repository-audit-spec --test-show-details=failures
```

Expected: compilation fails because the three imported repository modules do not exist yet.

- [ ] **Step 2: Implement the pure stdlib validator**

Create `StdlibFormat.hs` with this public model:

```haskell
data StdlibFormatViolation
  = InvalidModuleHeader FilePath
  | MissingFinalClosingBrace FilePath
  | InvalidBodyIndentation FilePath Int
  deriving (Eq, Show)

validateStdlibModule :: FilePath -> Text -> [StdlibFormatViolation]
renderStdlibFormatViolation :: StdlibFormatViolation -> Text
```

Implementation rules:

```haskell
validateStdlibModule path source
  | takeFileName path == "Prelude.jz" = []
  | otherwise = headerViolations <> closingViolations <> indentationViolations
  where
    numberedLines = zip [1 ..] (Text.lines source)
    firstLine = case numberedLines of
      [] -> Nothing
      (_, line) : _ -> Just line
    nonBlankLines = filter (not . Text.all isSpace . snd) numberedLines
    finalNonBlankLine = case reverse nonBlankLines of
      [] -> Nothing
      line : _ -> Just line
    headerViolations =
      case firstLine of
        Just line
          | "module " `Text.isPrefixOf` line,
            "{" `Text.isSuffixOf` line -> []
        _ -> [InvalidModuleHeader path]
    closingViolations =
      case finalNonBlankLine of
        Just (_, "}") -> []
        _ -> [MissingFinalClosingBrace path]
    indentationViolations =
      [ InvalidBodyIndentation path lineNumber
        | (lineNumber, line) <- bodyLines,
          let leadingWhitespace = Text.takeWhile isSpace line,
          not (Text.null (Text.strip line)),
          not
            ( Text.all (== ' ') leadingWhitespace
                && Text.length leadingWhitespace >= 2
                && even (Text.length leadingWhitespace)
            )
      ]
    bodyLines =
      case finalNonBlankLine of
        Nothing -> []
        Just (closingLineNumber, _) ->
          filter (\(lineNumber, _) -> lineNumber > 1 && lineNumber < closingLineNumber) numberedLines
```

Render every violation with its relative path and exact line where available. The missing-header message must say `must be an unindented module header ending in {`; the closing message must say `final non-blank line must be }`; the indentation message must say `must use two-space indentation levels`.

- [ ] **Step 3: Implement the stable package policy validator**

Create `PackagePolicy.hs` with:

```haskell
data PackagePolicyViolation
  = PublicLibraryStanza
  | MissingPrivateLibraryStanza
  | MissingPrivateLibraryVisibility
  deriving (Eq, Show)

validatePackagePolicy :: Text -> [PackagePolicyViolation]
renderPackagePolicyViolation :: PackagePolicyViolation -> Text
```

Treat an unindented line equal to `library` as public. Require an unindented `library jazz-next-internal` stanza, then inspect its indented body up to the next non-blank unindented stanza and require `visibility: private` after trimming. Return all applicable violations in the constructor order above. Do not parse or enumerate test suites.

The core stanza helpers are:

```haskell
isTopLevelLine :: Text -> Bool
isTopLevelLine line =
  case Text.uncons line of
    Nothing -> False
    Just (firstCharacter, _) ->
      not (Text.null (Text.strip line))
        && not (isSpace firstCharacter)

stanzaBody :: Text -> [Text] -> [Text]
stanzaBody header linesValue =
  case dropWhile ((/= header) . Text.stripEnd) linesValue of
    [] -> []
    _ : remaining ->
      takeWhile (\line -> Text.null (Text.strip line) || not (isTopLevelLine line)) remaining
```

`isTopLevelLine` uses `Text.uncons`, so empty and whitespace-only lines remain total under `-Wx-partial`.

- [ ] **Step 4: Implement deterministic package-root discovery**

Create `Root.hs` with:

```haskell
findJazzNextPackageRoot :: IO (Either Text FilePath)
findJazzNextPackageRoot = do
  currentDirectory <- getCurrentDirectory
  search currentDirectory (candidateRoots currentDirectory)
  where
    search startingDirectory candidates =
      case candidates of
        [] ->
          pure
            ( Left
                ( "could not locate jazz-next.cabal from "
                    <> Text.pack startingDirectory
                )
            )
        candidate : remaining -> do
          markerExists <- doesFileExist (candidate </> "jazz-next.cabal")
          if markerExists
            then pure (Right candidate)
            else search startingDirectory remaining

candidateRoots :: FilePath -> [FilePath]
candidateRoots currentDirectory =
  concatMap (\ancestor -> [ancestor, ancestor </> "jazz-next"]) (ancestors currentDirectory)

ancestors :: FilePath -> [FilePath]
ancestors directory =
  let parent = takeDirectory directory
   in directory : if parent == directory then [] else ancestors parent
```

Check each candidate for `candidate </> "jazz-next.cabal"` with `doesFileExist` and return the first match. If there is no match, return a `Left` containing the starting directory and the expected marker name. This supports Cabal execution from the package root, the repository root, or a `dist-newstyle` descendant without an environment variable.

- [ ] **Step 5: Complete the audit suite's real-tree checks**

In `AuditSpec.hs`, resolve the package root through `findJazzNextPackageRoot`. For the stdlib integration test:

```haskell
import qualified Data.Text.IO as TextIO

let stdlibDirectory = packageRoot </> "stdlib"
stdlibExists <- doesDirectoryExist stdlibDirectory
unless stdlibExists (failTest "stdlib audit could not find the stdlib directory")
stdlibEntries <- sort <$> listDirectory stdlibDirectory
let jazzFiles = filter ((== ".jz") . takeExtension) stdlibEntries
when (null jazzFiles) (failTest "stdlib audit found no .jz files")
sources <- forM jazzFiles $ \entry -> do
  source <- TextIO.readFile (stdlibDirectory </> entry)
  pure ("stdlib" </> entry, source)
let violations = concatMap (uncurry validateStdlibModule) sources
unless (null violations) $ failTest (Text.intercalate "\n" (map renderStdlibFormatViolation violations))
```

For the package integration test, read `packageRoot </> "jazz-next.cabal"`, call `validatePackagePolicy`, and fail once with every rendered violation in constructor order. Root discovery failure must call `failTest`; it must never turn into an empty successful scan.

- [ ] **Step 6: Run the new suite and the full Cabal inventory**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test --project-dir=jazz-next repository-audit-spec --test-show-details=failures
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test --project-dir=jazz-next all --test-show-details=failures
```

Expected: all 11 named repository-audit tests pass, the checked-in stdlib is accepted, the private library policy is accepted, and Cabal reports 37 successful suites overall.

- [ ] **Step 7: Commit the repository audit**

```bash
git add jazz-next/jazz-next.cabal jazz-next/test/JazzNext/Repository
git commit -m "test: add Haskell repository audit"
```

### Task 3: Migrate CLI, bootstrap, and module fixtures to `MultilineStrings`

**Files:**

- Modify: `jazz-next/test/JazzNext/CLI/CLISpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzLexerParitySpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/Loader/BasicTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/Loader/CapabilitiesTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/Loader/DiagnosticsTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/Loader/OperatorsTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/Loader/Shared.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/Loader/VisibilityTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`

**Interfaces:**

- Consumes: `MultilineStrings` from `test-common` and the exact source text currently passed to CLI, lexer, module resolver, loader, compiler, and runtime helpers.
- Produces: visually readable authored fixtures with byte-equivalent content wherever the test observes spans or canonical lexer output.

- [ ] **Step 1: Establish the focused baseline**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test --project-dir=jazz-next cli-spec canonical-lexer-comparison-spec jazz-lexer-parity-spec loader-spec module-resolution-spec module-pipeline-contract-spec prelude-loading-spec --test-show-details=failures
```

Expected: all listed suites pass before the literal-only refactor.

- [ ] **Step 2: Rewrite authored fixed programs as native multiline literals**

Convert fixed source such as:

```haskell
"module App::Main {\n  import Lib::Value.\n  value.\n}"
```

to:

```haskell
"""
module App::Main {
  import Lib::Value.
  value.
}
"""
```

Remove `Text.unlines` or list-of-line assembly when all lines are fixed authored Jazz. Remove now-unused qualified or unqualified `Text` imports only when the module no longer needs them.

- [ ] **Step 3: Keep generated and whitespace-observable sources explicit**

For a generated module such as `moduleGraphProjectedSources`, keep an explicit builder because the input is assembled rather than hand-authored as one fixed literal:

```haskell
( "src/Lib/Data.jz",
  Text.intercalate
    "\n"
    [ "module Lib::Data {",
      "  values = [[1, 2], [3]].",
      "  projected = " <> projectedExpr <> ".",
      "}"
    ]
)
```

Keep explicit `"\n"`, `"\r\n"`, or fragments in canonical lexer cases only when the case asserts line endings, leading/trailing blank lines, indentation, or exact spans. Add this comment immediately above each non-obvious retained fixture:

```haskell
-- Explicit escapes are intentional: this case asserts exact whitespace or source spans.
```

Do not add the comment to ordinary diagnostic output strings that are not Jazz source.

- [ ] **Step 4: Run the focused suites and inspect the semantic diff**

Run the Step 1 Cabal command again. Expected: every suite passes with unchanged expected values.

Then run:

```bash
git diff --check
git diff -- jazz-next/test/JazzNext/CLI jazz-next/test/JazzNext/Compiler/Bootstrap jazz-next/test/JazzNext/Compiler/Modules
```

Expected: changes are limited to string representation, unused imports, and explanatory comments; no Jazz tokens or declarations change.

- [ ] **Step 5: Commit the first fixture migration**

```bash
git add jazz-next/test/JazzNext/CLI jazz-next/test/JazzNext/Compiler/Bootstrap jazz-next/test/JazzNext/Compiler/Modules
git commit -m "refactor: use multiline module fixtures"
```

### Task 4: Migrate parser fixtures to `MultilineStrings`

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Parser/DeclarationParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/FixtureCorpus.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/Foundation/ExpressionsTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/Foundation/InvalidSyntaxTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/Foundation/ModulesTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/Foundation/SignaturesTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/OperatorInvalidSyntaxSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/TokenParserSpec.hs`

**Interfaces:**

- Consumes: exact parser source fixtures and expected `SourceSpan` values.
- Produces: readable parser programs while retaining explicit construction for tests whose subject is lexical whitespace, layout, line endings, or coordinates.

- [ ] **Step 1: Establish the parser baseline**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test --project-dir=jazz-next declaration-parser-spec parser-foundation-spec module-import-parser-spec operator-fixity-spec operator-invalid-syntax-spec token-parser-spec --test-show-details=failures
```

Expected: all listed suites pass.

- [ ] **Step 2: Convert fixed multi-statement and module programs**

Use triple-quoted literals for programs whose lines are authored syntax. For example:

```haskell
case parseSurfaceProgram
  """
  module App::Main {
    import Lib::Value.
    value.
  }
  """ of
```

Because `MultilineStrings` removes the delimiter-adjacent newline and shared indentation, align the visible Jazz at its natural indentation and keep the closing delimiter aligned with the Haskell expression.

- [ ] **Step 3: Preserve parser whitespace and span cases explicitly**

Do not convert fixtures that intentionally distinguish:

```haskell
"main = {\n  import Lib::Value.\n  value.\n}."
"line1\r\nline2"
"\nvalue."
"value.\n"
```

Retain explicit syntax when the assertion names a line/column, newline kind, indentation level, leading/trailing blank line, token offset, or whitespace rejection. Add the exact explanatory comment from Task 3 when intent is not obvious from the test name.

In `FixtureCorpus.hs`, preserve byte-exact cases used by canonical lexer comparison when changing representation would change the manifest source value. Convert the remaining fixed multi-line programs.

- [ ] **Step 4: Run parser and canonical lexer verification**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test --project-dir=jazz-next declaration-parser-spec parser-foundation-spec module-import-parser-spec operator-fixity-spec operator-invalid-syntax-spec token-parser-spec canonical-lexer-comparison-spec jazz-lexer-parity-spec --test-show-details=failures
git diff --check
```

Expected: parser ASTs, diagnostics, spans, and canonical lexer results remain unchanged.

- [ ] **Step 5: Commit the parser fixture migration**

```bash
git add jazz-next/test/JazzNext/Compiler/Parser
git commit -m "refactor: use multiline parser fixtures"
```

### Task 5: Migrate semantic and runtime fixtures to `MultilineStrings`

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/BasicsTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/ConstraintsTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/DiagnosticsTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/GeneralizationTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/RecursionTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/Shared.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemantics/EqualityOperator.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemantics/NumericConversions.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemantics/ScalarCollection.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/PuritySemanticsSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/RebindingWarningSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/CapabilitiesTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/NumericTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RecursionTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RenderingTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/Shared.hs`

**Interfaces:**

- Consumes: semantic compiler helpers such as `compileSource`, `runSource`, `parseCoreProgram`, and shared class/implementation fixture text.
- Produces: readable class, implementation, recursion, primitive, and runtime programs with identical compile and evaluation outcomes.

- [ ] **Step 1: Establish the semantic baseline**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test --project-dir=jazz-next binding-signature-coherence-spec builtin-catalog-spec lambda-semantics-spec primitive-semantics-spec purity-semantics-spec rebinding-warning-spec runtime-semantics-spec --test-show-details=failures
```

Expected: all listed suites pass.

- [ ] **Step 2: Convert authored class, implementation, and program fixtures**

Convert shared sources such as:

```haskell
qualifiedEqSource :: Text
qualifiedEqSource =
  """
  class Eq(a) {
    equals :: a -> a -> Bool.
  }.
  impl Eq(Int) {
    equals = \(left) -> \(right) -> left == right.
  }.
  """
```

This task changes Haskell literal syntax only. Do not apply Batch 2's compact-lambda rewrite here; nested Jazz lambdas remain nested until that separately reviewed batch.

- [ ] **Step 3: Retain generated stress inputs and line-ending tests**

Keep `Text.replicate`, numeric-size builders, folds, and concatenation where the test generates a program from a size or injected fragment. In `BuiltinCatalogSpec.hs`, convert the fixed three-line `Eq(Float64)` implementation snippet to `MultilineStrings`, but keep `Text.replace "\n" "\r\n"`, normalization helpers, and direct escaped literals unchanged because those expressions directly exercise line endings.

Keep ordinary Haskell diagnostic strings such as `"FAIL:\n  message"` unchanged; they are not embedded Jazz programs.

- [ ] **Step 4: Run semantic and full verification**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test --project-dir=jazz-next binding-signature-coherence-spec builtin-catalog-spec lambda-semantics-spec primitive-semantics-spec purity-semantics-spec rebinding-warning-spec runtime-semantics-spec --test-show-details=failures
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test --project-dir=jazz-next all --test-show-details=failures
git diff --check
```

Expected: all 37 suites pass and no semantic/runtime output changes.

- [ ] **Step 5: Review remaining explicit multiline candidates**

Run:

```bash
rg -n 'Text\.unlines|T\.unlines|(^|[^[:alnum:]_])unlines[[:space:]]|\\n' jazz-next/src jazz-next/app jazz-next/test --glob '*.hs'
```

Classify every remaining match as one of: whitespace/span test, line-ending test, generated input, warning-config text, or non-Jazz diagnostic text. Convert any remaining hand-written multiline Jazz program. Do not turn this search into a permanent test.

- [ ] **Step 6: Commit the semantic fixture migration**

```bash
git add jazz-next/test/JazzNext/Compiler/Semantics
git commit -m "refactor: use multiline semantic fixtures"
```

### Task 6: Remove the shell mega-gate and update active instructions

**Files:**

- Delete: `jazz-next/scripts/check-stdlib-format.sh`
- Delete: `jazz-next/scripts/runghc.sh`
- Delete: `jazz-next/scripts/test-check-stdlib-format.sh`
- Delete: `jazz-next/scripts/test-warning-config.sh`
- Modify: `jazz-next/README.md`
- Modify: `docs/execution/README.md`
- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/spec/tooling/cli-source-input.md`

**Interfaces:**

- Consumes: the 37-suite Cabal inventory, `repository-audit-spec`, and existing focused behavior suites.
- Produces: one active test command, focused Cabal component commands, `cabal run` CLI instructions, and no active dependency on direct `runghc` or the warning-named gate.

**Old architecture-guard disposition:**

| Removed shell guard                                                                        | Disposition                                                                                                              |
| ------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------ |
| `ExpressionType`, `InferState`, or `TypeScheme` declarations in the `TypeInference` façade | Delete implementation-spelling guard; warning-clean compilation and `binding-signature-coherence-spec` own behavior.     |
| `ModuleReplay`, flattened graph fields, or `__module::` vocabulary                         | Delete spelling guard; `module-resolution-spec`, `module-pipeline-contract-spec`, and `loader-spec` own module behavior. |
| `JazzNext.Compiler.Identifier` references                                                  | Delete module-name guard; `name-semantics-spec` owns structured-name behavior.                                           |
| parallel export record-field names in `ModuleResolver`                                     | Delete field-name guard; `module-exports-spec` and `module-pipeline-contract-spec` own namespace export behavior.        |
| unqualified public `library` stanza                                                        | Preserve as `PackagePolicy` repository audit because package visibility is an external contract.                         |
| exact parser expression `length original - length remaining`                               | Delete implementation-expression guard; declaration and parser suites own behavior and spans.                            |
| `SurfaceConstrainedSignatureType` or `ConstraintSignatureType` names                       | Delete type-name guard; `binding-signature-coherence-spec` owns the unified representation's behavior.                   |

- [ ] **Step 1: Prove Cabal and the audit cover the retained responsibilities**

Run before deleting scripts:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test --project-dir=jazz-next repository-audit-spec module-resolution-spec module-exports-spec module-pipeline-contract-spec loader-spec name-semantics-spec declaration-parser-spec binding-signature-coherence-spec --test-show-details=failures
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test --project-dir=jazz-next all --test-show-details=failures
```

Expected: all named suites and all 37 registered suites pass without invoking a shell test runner.

- [ ] **Step 2: Delete the four compatibility scripts**

Delete all four files listed above. Do not leave forwarding wrappers: Cabal component names are the focused interface, and `cabal test --project-dir=jazz-next all` is the complete test interface.

- [ ] **Step 3: Update active development and execution documentation**

In `jazz-next/README.md`, document:

```bash
nix --extra-experimental-features 'nix-command flakes' develop
cabal build --project-dir=jazz-next all
cabal test --project-dir=jazz-next all --test-show-details=failures
cabal test --project-dir=jazz-next repository-audit-spec --test-show-details=failures
```

State that Cabal discovers every suite and that `repository-audit-spec` owns stdlib formatting and private-package policy. Remove all guidance for `runghc.sh` and `test-warning-config.sh`.

In `docs/execution/README.md` and `docs/execution/blocker-contracts.md`, replace the old whole-repository gate with:

```bash
cabal build --project-dir=jazz-next all
cabal test --project-dir=jazz-next all --test-show-details=failures
```

In `docs/spec/tooling/cli-source-input.md`, replace direct `Main.hs` execution with:

```bash
cabal run --project-dir=jazz-next jazz-next -- first.jz
cabal run --project-dir=jazz-next jazz-next -- --run first.jz
```

Do not edit historical files under `docs/plans/`, prior accepted evidence under `docs/superpowers/`, or `docs/execution/done-archive.md` merely to rewrite commands that were accurate at the time.

- [ ] **Step 4: Verify there are no active wrapper references**

Run:

```bash
rg -n 'test-warning-config\.sh|runghc\.sh|check-stdlib-format\.sh|test-check-stdlib-format\.sh' . --glob '!docs/plans/**' --glob '!docs/superpowers/**' --glob '!docs/execution/done-archive.md' --glob '!jazz-hs/**' --glob '!jazz2/**' --glob '!jazz-next/dist-newstyle/**'
```

Expected: no matches. Matches in historical plans, accepted evidence, and the done archive remain untouched.

- [ ] **Step 5: Run active documentation and test gates**

Run:

```bash
bash scripts/check-docs.sh
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build --project-dir=jazz-next all
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test --project-dir=jazz-next all --test-show-details=failures
git diff --check
```

Expected: all commands exit zero.

- [ ] **Step 6: Commit the gate replacement**

```bash
git add jazz-next/README.md jazz-next/scripts docs/execution/README.md docs/execution/blocker-contracts.md docs/spec/tooling/cli-source-input.md
git commit -m "test: replace shell gate with Cabal"
```

### Task 7: Run the pinned Nix check and close Batch 1

**Files:**

- Modify: `docs/jazz-improvement-backlog.md`
- Modify: `docs/superpowers/specs/2026-07-13-jazz-next-ghc-9-14-test-gate-design.md`

**Interfaces:**

- Consumes: every deliverable and commit from Tasks 1 through 6.
- Produces: fresh release-style evidence and durable completion status for Batch 1 without changing later batch scope.

- [ ] **Step 1: Verify exact tool versions in the pinned shell**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command ghc --numeric-version
nix --extra-experimental-features 'nix-command flakes' develop --command cabal --numeric-version
```

Expected: `9.14.1` and `3.16.1.0`.

- [ ] **Step 2: Run build, tests, and the Nix package check from clean inputs**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal clean --project-dir=jazz-next
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build --project-dir=jazz-next all
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test --project-dir=jazz-next all --test-show-details=failures
nix --extra-experimental-features 'nix-command flakes' flake check --print-build-logs
```

Expected: Cabal rebuilds all active components, all 37 suites pass, and `checks.jazz-next-test-suite` succeeds under GHC 9.14.1.

- [ ] **Step 3: Verify scope and active documentation**

Run:

```bash
bash scripts/check-docs.sh
git diff --check
git diff --name-only 458cbe9..HEAD -- jazz-hs jazz2
git status --short
```

Expected: docs and diff checks pass, the legacy-path command prints nothing, and the worktree contains only the two completion-status documentation edits before the final commit.

- [ ] **Step 4: Mark the approved design and Batch 1 complete**

Change the design status to:

```markdown
Approved and implemented on `2026-07-13`.
```

Add this line immediately below the Batch 1 heading in `docs/jazz-improvement-backlog.md`:

```markdown
Status: completed on 2026-07-13.
```

Do not mark any other batch complete or rewrite its scope.

- [ ] **Step 5: Verify and commit completion status**

Run:

```bash
bash scripts/check-docs.sh
git diff --check
git diff --name-only 458cbe9..HEAD -- jazz-hs jazz2
```

Expected: both checks pass and the legacy-path command prints nothing.

Commit:

```bash
git add docs/jazz-improvement-backlog.md docs/superpowers/specs/2026-07-13-jazz-next-ghc-9-14-test-gate-design.md
git commit -m "docs: close Jazz improvement batch 1"
```

- [ ] **Step 6: Record final evidence**

Run:

```bash
git status --short
git log --oneline -7
```

Expected: the worktree is clean and the log shows one focused commit for each completed task. Report the exact Cabal suite count, Nix check result, and any formatter skip from `scripts/check-docs.sh`; do not claim a skipped check passed.
