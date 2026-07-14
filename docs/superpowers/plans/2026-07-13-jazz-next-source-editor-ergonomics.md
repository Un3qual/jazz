# Jazz-Next Source and Editor Ergonomics Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Move shipped Jazz sources into audited stdlib/compiler trees, adopt compact multi-parameter lambdas throughout ordinary authored Jazz code, and ship a validated TextMate-compatible VS Code extension for `.jz` files.

**Architecture:** The physical source tree becomes `jazz-next/jazz/{stdlib,compiler}` with a parsed repository audit enforcing the one-way compiler-to-stdlib dependency boundary. A shared test-source loader resolves those files from the active Cabal package root, while Cabal owns their source-distribution packaging. The editor deliverable is static JSON plus a valid representative Jazz fixture, validated by the existing Haskell repository-audit suite.

**Tech Stack:** GHC 9.14.1, Haskell 2010, `MultilineStrings`, Cabal 3.x, Megaparsec-backed Jazz parser, `aeson` 2.x for repository-owned JSON validation, Jazz `.jz`, TextMate JSON grammar, VS Code language contributions.

## Global Constraints

- Modify compiler implementation only under `jazz-next/`; never modify `jazz-hs/` or `jazz2/`.
- Use the exact shipped-source root `jazz-next/jazz/{stdlib,compiler}` and remove `jazz-next/stdlib/` without compatibility copies or symlinks.
- Keep Jazz module names and public APIs unchanged during the physical move.
- Permit compiler modules to import stdlib modules; reject stdlib imports of compiler modules.
- Preserve the existing compact-lambda lowering to nested unary core lambdas and its partial-application semantics.
- Retain explicit nested lambdas only in tests whose purpose is nested currying, intermediate closure capture, or compact-versus-nested parser/lowering behavior.
- Keep editor support lexical and dependency-free at runtime; do not add a language server, formatter, semantic tokens, completion, or marketplace publication.
- Use GHC 9.14.1 through the repository Nix environment for all build and test evidence.
- Keep the live execution queue's parser-design curation target unchanged; this improvement batch is tracked in `docs/jazz-improvement-backlog.md`.

---

## File Structure

| Path | Responsibility |
| --- | --- |
| `jazz-next/jazz/stdlib/*.jz` | General user-facing Jazz modules and the checked-in bundled Prelude mirror. |
| `jazz-next/jazz/compiler/*.jz` | Hosted compiler modules, initially `Lexer` and `LexerTypes`. |
| `jazz-next/test/JazzNext/TestSource.hs` | Working-directory-independent checked-in Jazz source loading for tests. |
| `jazz-next/test/JazzNext/Repository/JazzSourceFormat.hs` | Pure format validation for ordinary module sources and the Prelude exemption. |
| `jazz-next/test/JazzNext/Repository/SourceLayout.hs` | Pure parsed-source metadata and stdlib/compiler dependency validation. |
| `jazz-next/test/JazzNext/Repository/AuditSpec.hs` | Real-tree, source-distribution, dependency, and editor-package audit entrypoint. |
| `jazz-next/editors/vscode-jazz/package.json` | VS Code `.jz` language and grammar registration. |
| `jazz-next/editors/vscode-jazz/language-configuration.json` | Comments, brackets, auto-closing, and surrounding pairs. |
| `jazz-next/editors/vscode-jazz/syntaxes/jazz.tmLanguage.json` | Portable `source.jazz` TextMate grammar. |
| `jazz-next/editors/vscode-jazz/fixtures/representative.jz` | Valid active-syntax fixture spanning every required highlighting family. |
| `jazz-next/editors/vscode-jazz/README.md` | Local extension installation and scope documentation. |

---

### Task 1: Lock the new source-tree contract with failing tests

**Files:**
- Modify: `jazz-next/test/JazzNext/Repository/AuditSpec.hs`
- Create after RED: `jazz-next/test/JazzNext/Repository/SourceLayout.hs`
- Modify after RED: `jazz-next/jazz-next.cabal`

**Interfaces:**
- Consumes: `findJazzNextPackageRoot`, `parseSurfaceProgram`, `SurfaceExpr`, and `SurfaceStatement`.
- Produces: `JazzSourceRole`, `JazzSourceModule`, `SourceLayoutViolation`, `sourceModuleFromSurface`, `validateSourceLayering`, and `renderSourceLayoutViolation`.

- [ ] **Step 1: Add failing layout tests to `AuditSpec.hs`.**

Import the future contract:

```haskell
import JazzNext.Repository.SourceLayout
  ( JazzSourceModule (..),
    JazzSourceRole (..),
    SourceLayoutViolation (..),
    renderSourceLayoutViolation,
    sourceModuleFromSurface,
    validateSourceLayering
  )
```

Add these named tests before the checked-in audits:

```haskell
    ("rejects stdlib imports of compiler modules", testRejectsStdlibCompilerImport),
    ("accepts compiler imports of stdlib modules", testAcceptsCompilerStdlibImport),
    ("uses the locked checked-in Jazz source tree", testCheckedInJazzSourceTree),
```

Use parsed in-memory fixtures so the dependency test exercises real Jazz syntax:

```haskell
testRejectsStdlibCompilerImport :: IO ()
testRejectsStdlibCompilerImport = do
  compilerModule <- parsedSourceModule CompilerSource "jazz/compiler/Lexer.jz" "module Lexer { 0. }"
  stdlibModule <- parsedSourceModule StandardLibrarySource "jazz/stdlib/Bad.jz" "module Bad { import Lexer. 0. }"
  assertEqual
    "stdlib compiler dependency"
    [StandardLibraryImportsCompiler "jazz/stdlib/Bad.jz" ["Lexer"]]
    (validateSourceLayering [compilerModule, stdlibModule])

testAcceptsCompilerStdlibImport :: IO ()
testAcceptsCompilerStdlibImport = do
  stdlibModule <- parsedSourceModule StandardLibrarySource "jazz/stdlib/Text.jz" "module Text { 0. }"
  compilerModule <- parsedSourceModule CompilerSource "jazz/compiler/Lexer.jz" "module Lexer { import Text. 0. }"
  assertEqual "compiler stdlib dependency" [] (validateSourceLayering [stdlibModule, compilerModule])

parsedSourceModule :: JazzSourceRole -> FilePath -> Text -> IO JazzSourceModule
parsedSourceModule role path source =
  case parseSurfaceProgram source of
    Left diagnostic -> failTest ("fixture did not parse: " <> renderDiagnostic diagnostic)
    Right surfaceProgram -> pure (sourceModuleFromSurface role path surfaceProgram)

testCheckedInJazzSourceTree :: IO ()
testCheckedInJazzSourceTree =
  withPackageRoot $ \packageRoot -> do
    let jazzRoot = packageRoot </> "jazz"
        stdlibRoot = jazzRoot </> "stdlib"
        compilerRoot = jazzRoot </> "compiler"
        legacyRoot = packageRoot </> "stdlib"
    stdlibExists <- doesDirectoryExist stdlibRoot
    compilerExists <- doesDirectoryExist compilerRoot
    legacyExists <- doesDirectoryExist legacyRoot
    assertEqual "stdlib source root exists" True stdlibExists
    assertEqual "compiler source root exists" True compilerExists
    assertEqual "legacy stdlib root is absent" False legacyExists
```

- [ ] **Step 2: Run the repository audit and observe RED.**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next repository-audit-spec --test-show-details=failures
```

Expected: FAIL because `JazzNext.Repository.SourceLayout` does not exist and the locked source directories have not been created.

- [ ] **Step 3: Implement the pure source-layering contract.**

Create `jazz-next/test/JazzNext/Repository/SourceLayout.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Repository.SourceLayout
  ( JazzSourceModule (..),
    JazzSourceRole (..),
    SourceLayoutViolation (..),
    renderSourceLayoutViolation,
    sourceModuleFromSurface,
    validateSourceLayering
  )
where

import Data.List (find)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceStatement (..)
  )

data JazzSourceRole
  = StandardLibrarySource
  | CompilerSource
  deriving (Eq, Show)

data JazzSourceModule = JazzSourceModule
  { jazzSourceRole :: JazzSourceRole,
    jazzSourcePath :: FilePath,
    jazzModulePath :: Maybe [Text],
    jazzImportedModulePaths :: [[Text]]
  }
  deriving (Eq, Show)

data SourceLayoutViolation
  = StandardLibraryImportsCompiler FilePath [Text]
  deriving (Eq, Show)

sourceModuleFromSurface :: JazzSourceRole -> FilePath -> SurfaceExpr -> JazzSourceModule
sourceModuleFromSurface role path surfaceProgram =
  JazzSourceModule
    { jazzSourceRole = role,
      jazzSourcePath = path,
      jazzModulePath = findModulePath statements,
      jazzImportedModulePaths = [modulePath | SSImport _ modulePath _ _ <- statements]
    }
  where
    statements =
      case surfaceProgram of
        SEBlock values -> values
        _ -> []
    findModulePath values =
      case find isModule values of
        Just (SSModule _ modulePath _) -> Just modulePath
        _ -> Nothing
    isModule statement =
      case statement of
        SSModule _ _ _ -> True
        _ -> False

validateSourceLayering :: [JazzSourceModule] -> [SourceLayoutViolation]
validateSourceLayering modules =
  [ StandardLibraryImportsCompiler (jazzSourcePath sourceModule) importedPath
    | sourceModule <- modules,
      jazzSourceRole sourceModule == StandardLibrarySource,
      importedPath <- jazzImportedModulePaths sourceModule,
      importedPath `Set.member` compilerModules
  ]
  where
    compilerModules :: Set [Text]
    compilerModules =
      Set.fromList
        [ modulePath
          | sourceModule <- modules,
            jazzSourceRole sourceModule == CompilerSource,
            Just modulePath <- [jazzModulePath sourceModule]
        ]

renderSourceLayoutViolation :: SourceLayoutViolation -> Text
renderSourceLayoutViolation violation =
  case violation of
    StandardLibraryImportsCompiler path modulePath ->
      Text.pack path
        <> ": standard-library source must not import compiler module "
        <> Text.intercalate "::" modulePath
```

Register `JazzNext.Repository.SourceLayout` only in the
`repository-audit-spec` `other-modules` list.

- [ ] **Step 4: Run the repository audit and confirm the contract functions are GREEN while the real-tree assertion remains RED.**

Run the same focused Cabal test. Expected: the two in-memory dependency tests pass; `uses the locked checked-in Jazz source tree` fails because `jazz/stdlib` and `jazz/compiler` do not exist and `stdlib` still does.

- [ ] **Step 5: Commit the RED repository contract.**

```sh
git add jazz-next/test/JazzNext/Repository/AuditSpec.hs \
  jazz-next/test/JazzNext/Repository/SourceLayout.hs jazz-next/jazz-next.cabal
git commit -m "test: lock Jazz source layout"
```

---

### Task 2: Move shipped sources and update every active consumer

**Files:**
- Move: `jazz-next/stdlib/{Char,IO,IOError,List,Maybe,Prelude,Result,Text}.jz` -> `jazz-next/jazz/stdlib/`
- Move: `jazz-next/stdlib/{Lexer,LexerTypes}.jz` -> `jazz-next/jazz/compiler/`
- Create: `jazz-next/test/JazzNext/TestSource.hs`
- Rename: `jazz-next/test/JazzNext/Repository/StdlibFormat.hs` -> `jazz-next/test/JazzNext/Repository/JazzSourceFormat.hs`
- Modify: `jazz-next/test/JazzNext/Repository/AuditSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`
- Modify: `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs`
- Modify: `jazz-next/test/JazzNext/CLI/CLISpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzLexerParitySpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs`

**Interfaces:**
- Consumes: `findJazzNextPackageRoot`, `JazzSourceRole`, `sourceModuleFromSurface`, and `validateSourceLayering` from Task 1.
- Produces: `readCheckedInJazzSource :: JazzSourceRole -> FilePath -> IO Text`, one physical source tree, and Cabal-packaged `.jz` assets.

- [ ] **Step 1: Move the ten sources into their locked roles.**

Use `apply_patch` moves so Git records eight files under `jazz/stdlib` and two
under `jazz/compiler`, then remove the empty legacy directory. Do not change
module declarations or bodies in this step.

- [ ] **Step 2: Add one checked-in-source loader for tests.**

Create `jazz-next/test/JazzNext/TestSource.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module JazzNext.TestSource
  ( JazzSourceRole (..),
    checkedInJazzSourcePath,
    readCheckedInJazzSource
  )
where

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import JazzNext.Repository.Root (findJazzNextPackageRoot)
import JazzNext.Repository.SourceLayout (JazzSourceRole (..))
import System.Directory (doesFileExist)
import System.FilePath ((</>))

checkedInJazzSourcePath :: FilePath -> JazzSourceRole -> FilePath -> FilePath
checkedInJazzSourcePath packageRoot role fileName =
  packageRoot </> "jazz" </> roleDirectory role </> fileName
  where
    roleDirectory StandardLibrarySource = "stdlib"
    roleDirectory CompilerSource = "compiler"

readCheckedInJazzSource :: JazzSourceRole -> FilePath -> IO Text.Text
readCheckedInJazzSource role fileName = do
  rootResult <- findJazzNextPackageRoot
  packageRoot <-
    case rootResult of
      Left message -> ioError (userError (Text.unpack message))
      Right root -> pure root
  let path = checkedInJazzSourcePath packageRoot role fileName
  exists <- doesFileExist path
  if exists
    then TextIO.readFile path
    else
      ioError
        ( userError
            ( "could not find checked-in "
                <> roleLabel role
                <> " Jazz source at "
                <> path
            )
        )
  where
    roleLabel StandardLibrarySource = "standard-library"
    roleLabel CompilerSource = "compiler"
```

Move `JazzNext.Repository.Root` and `JazzNext.Repository.SourceLayout` into the
shared `test-common` `other-modules` list, add `JazzNext.TestSource` there, and
remove duplicate declarations from `repository-audit-spec`.

- [ ] **Step 3: Update active path consumers without fallback candidates.**

Set:

```haskell
bundledPreludePath = "jazz-next/jazz/stdlib/Prelude.jz"
```

Replace each local `readStdlibSource`/`readFirstExisting` implementation with
`readCheckedInJazzSource`:

```haskell
lookupSource "src/Maybe.jz" = Just <$> readCheckedInJazzSource StandardLibrarySource "Maybe.jz"
lookupSource "src/Lexer.jz" = Just <$> readCheckedInJazzSource CompilerSource "Lexer.jz"
```

Apply the same role choice to every checked-in module in `LoaderSpec.hs`,
`JazzLexerParitySpec.hs`, and `CanonicalLexerComparisonSpec.hs`. Update
`CLISpec.isBundledPreludePath` to accept only the canonical repository-relative
path and the package-relative `jazz/stdlib/Prelude.jz` form. Keep
`BuiltinCatalogSpec` searching parent directories through
`bundledPreludePath`, since that is the production mirror-path contract.

- [ ] **Step 4: Generalize the source-format audit.**

Rename the module and exported vocabulary from `StdlibFormat` to
`JazzSourceFormat`:

```haskell
data JazzSourceFormatViolation
  = InvalidModuleHeader FilePath
  | MissingFinalClosingBrace FilePath
  | InvalidBodyIndentation FilePath Int
  deriving (Eq, Show)

validateJazzModule :: FilePath -> Text -> [JazzSourceFormatViolation]
renderJazzSourceFormatViolation :: JazzSourceFormatViolation -> Text
```

Preserve the Prelude filename exemption. In `AuditSpec`, recursively enumerate
both `jazz/stdlib` and `jazz/compiler`, sort paths, require at least one `.jz`
file in each role, parse every source through `parseSurfaceProgram`, aggregate
format violations, build `JazzSourceModule` metadata, and aggregate
`validateSourceLayering` violations. Missing roots and parse failures must name
the package-relative path.

Use one deterministic recursive enumerator:

```haskell
listJazzFiles :: FilePath -> IO [FilePath]
listJazzFiles root = sort <$> go root
  where
    go directory = do
      entries <- sort <$> listDirectory directory
      paths <- forM entries $ \entry -> do
        let path = directory </> entry
        isDirectory <- doesDirectoryExist path
        if isDirectory
          then go path
          else pure [path | takeExtension path == ".jz"]
      pure (concat paths)

readSourceRole :: FilePath -> JazzSourceRole -> FilePath -> IO ([JazzSourceFormatViolation], [JazzSourceModule])
readSourceRole packageRoot role relativeDirectory = do
  let sourceRoot = packageRoot </> relativeDirectory
  exists <- doesDirectoryExist sourceRoot
  unless exists (failTest (Text.pack relativeDirectory <> ": source directory does not exist"))
  paths <- listJazzFiles sourceRoot
  when (null paths) (failTest (Text.pack relativeDirectory <> ": contains no .jz files"))
  results <- forM paths $ \path -> do
    source <- TextIO.readFile path
    let relativePath = makeRelative packageRoot path
        formatViolations = validateJazzModule relativePath source
    sourceModule <-
      case parseSurfaceProgram source of
        Left diagnostic ->
          failTest
            ( Text.pack relativePath
                <> ": failed to parse: "
                <> renderDiagnostic diagnostic
            )
        Right surfaceProgram ->
          pure (sourceModuleFromSurface role relativePath surfaceProgram)
    pure (formatViolations, sourceModule)
  pure (concatMap fst results, map snd results)
```

Call it with `"jazz" </> "stdlib"` and `"jazz" </> "compiler"`, concatenate
both metadata lists, and render every format or layering violation before
failing the suite.

- [ ] **Step 5: Package the source tree in Cabal.**

Add near the Cabal package metadata:

```cabal
extra-source-files:
    jazz/stdlib/*.jz
    jazz/compiler/*.jz
```

Do not add editor files yet; Task 4 adds those when they exist.

- [ ] **Step 6: Run focused source consumers and the repository audit.**

```sh
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    repository-audit-spec loader-spec jazz-lexer-parity-spec \
    canonical-lexer-comparison-spec builtin-catalog-spec cli-spec \
    --test-show-details=failures
```

Expected: all six suites pass; no test reads `jazz-next/stdlib` or
package-relative `stdlib`.

- [ ] **Step 7: Verify the source distribution includes only the new tree.**

```sh
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal sdist --project-dir=jazz-next
tar -tf jazz-next/dist-newstyle/sdist/jazz-next-0.1.0.0.tar.gz | \
  rg '(^|/)jazz/(stdlib|compiler)/.*\.jz$|(^|/)stdlib/.*\.jz$'
```

Expected: entries under `jazz/stdlib` and `jazz/compiler`; no entry under a
top-level package `stdlib/` directory.

- [ ] **Step 8: Commit the source-root migration.**

```sh
git add jazz-next/jazz jazz-next/jazz-next.cabal jazz-next/src \
  jazz-next/test
git commit -m "refactor: separate Jazz source trees"
```

---

### Task 3: Migrate authored Jazz code to compact lambdas

**Files:**
- Modify: `jazz-next/jazz/stdlib/*.jz`
- Modify: `jazz-next/jazz/compiler/*.jz`
- Modify: `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs`
- Modify: ordinary authored Jazz programs in `jazz-next/test/**/*.hs`
- Preserve explicit nested coverage in: `jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs`

**Interfaces:**
- Consumes: existing `\(x, y) -> expression` parsing and unary-core lowering.
- Produces: compact authored source with the same observable types, currying, partial application, and runtime results.

- [ ] **Step 1: Record the behavior baseline for compact and nested currying.**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next lambda-parser-spec lambda-semantics-spec \
    --test-show-details=failures
```

Expected: existing tests pass, including multi-parameter surface lowering to
nested unary core lambdas and partial application through
`testClosureCaptureRuntime`.

- [ ] **Step 2: Keep one explicit nested-lambda exception and make its purpose unmistakable.**

Rename the test label and function to describe the retained surface:

```haskell
("explicit nested lambdas capture between curry boundaries", testExplicitNestedLambdaClosureCaptureRuntime)

testExplicitNestedLambdaClosureCaptureRuntime :: IO ()
testExplicitNestedLambdaClosureCaptureRuntime = do
  -- The nested surface is intentional: this test covers closure capture at
  -- each explicit unary lambda boundary rather than compact syntax.
  result <- runSource defaultWarningSettings "makeAdder = \\(x) -> \\(y) -> x + y. add2 = makeAdder 2. add2 3."
  assertEqual "warnings" [] (runWarnings result)
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "5") (runOutput result)
```

- [ ] **Step 3: Rewrite shipped source and generated Prelude chains.**

For every immediate identifier-only chain, combine all parameters:

```jazz
nextTabStop = \(stop, column) -> body.
lexicalFailure = \(reason, line, column) -> body.
scanQuoted = \(mode, literalKind, delimiter, remaining, reversedRaw, reversedDecoded, count, line, startColumn, currentColumn) -> body.
writeText! = \(path, contents) -> body.
```

Update `BundledPrelude.renderEqImpl` to emit:

```haskell
"equals = \\(left, right) -> left == right."
```

Update `BuiltinCatalogSpec` expected source accordingly. The checked-in Prelude
must remain byte-for-byte equal to `bundledPreludeSource` after line-ending
normalization.

- [ ] **Step 4: Rewrite ordinary embedded programs and parser fixtures.**

Change immediate nested identifier lambdas in module capability, operator,
signature, runtime, and parser fixtures to compact parameter lists. Do not
change single-parameter lambdas, pattern-lambda syntax, invalid-syntax cases
whose malformed form is the behavior under test, or the explicit nested
closure-capture case from Step 2.

Examples:

```jazz
equals = \(left, right) -> left == right.
(%%) = \(left, right) -> left + right.
select = \(width, value) -> value.
```

- [ ] **Step 5: Audit the remaining nested spellings.**

```sh
rg -n --glob '*.jz' '\\\([^)]*\) -> \\\(' jazz-next/jazz
rg -n --glob '*.hs' '\\\\+\([^)]*\) -> \\\\+\(' jazz-next/src jazz-next/test
```

Expected: no shipped `.jz` result; Haskell results are limited to the explicit
nested-lambda semantic test and any parser case whose asserted surface is
specifically nested. Inspect every remaining line and add a local explanation
when the test name is not already explicit.

- [ ] **Step 6: Run all directly affected suites.**

```sh
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    lambda-parser-spec lambda-semantics-spec parser-foundation-spec \
    operator-fixity-spec operator-invalid-syntax-spec loader-spec \
    module-pipeline-contract-spec binding-signature-coherence-spec \
    primitive-semantics-spec runtime-semantics-spec builtin-catalog-spec \
    jazz-lexer-parity-spec --test-show-details=failures
```

Expected: all listed suites pass with unchanged runtime and diagnostic results.

- [ ] **Step 7: Commit the compact-lambda migration.**

```sh
git add jazz-next/jazz jazz-next/src jazz-next/test
git commit -m "refactor: use compact Jazz lambdas"
```

---

### Task 4: Add a validated VS Code and TextMate editor package

**Files:**
- Modify first for RED: `jazz-next/test/JazzNext/Repository/AuditSpec.hs`
- Modify first for RED: `jazz-next/jazz-next.cabal`
- Create after RED: `jazz-next/editors/vscode-jazz/package.json`
- Create after RED: `jazz-next/editors/vscode-jazz/language-configuration.json`
- Create after RED: `jazz-next/editors/vscode-jazz/syntaxes/jazz.tmLanguage.json`
- Create after RED: `jazz-next/editors/vscode-jazz/fixtures/representative.jz`
- Create after RED: `jazz-next/editors/vscode-jazz/README.md`

**Interfaces:**
- Consumes: active lexer spellings, contextual declaration keywords, parser syntax, and the repository-audit package root.
- Produces: VS Code language id `jazz`, extension `.jz`, scope `source.jazz`, JSON-validated grammar/configuration, and one parser-valid fixture.

- [ ] **Step 1: Add the audit-only JSON dependency and failing editor tests.**

Add only to `repository-audit-spec`:

```cabal
  build-depends:
      aeson >= 2.2 && < 2.3
```

Add imports for `Data.Aeson`, `Data.Aeson.Key`, `Data.Aeson.KeyMap`, strict
`ByteString`, `Data.Foldable.toList`, `parseSurfaceProgram`, and
`renderDiagnostic`. Add named tests:

```haskell
("validates the Jazz editor package metadata", testEditorPackageMetadata),
("parses the representative editor fixture", testEditorFixtureParses),
```

Use this JSON loader and path accessor:

```haskell
decodeJsonFile :: FilePath -> IO Value
decodeJsonFile path = do
  bytes <- ByteString.readFile path
  case eitherDecodeStrict' bytes of
    Left message -> failTest (Text.pack path <> ": invalid JSON: " <> Text.pack message)
    Right value -> pure value

jsonPath :: [Text] -> Value -> Maybe Value
jsonPath keys value =
  case keys of
    [] -> Just value
    key : remaining ->
      case value of
        Object object -> KeyMap.lookup (Key.fromText key) object >>= jsonPath remaining
        _ -> Nothing

jsonArray :: Value -> [Value]
jsonArray value =
  case value of
    Array values -> toList values
    _ -> []
```

The metadata test loads all three JSON files, asserts
`contributes.languages[0].id == "jazz"`, `.jz` is in its extensions,
`contributes.grammars[0].scopeName == "source.jazz"`, its path is
`./syntaxes/jazz.tmLanguage.json`, the contributed configuration and grammar
paths exist, and the grammar root `scopeName` is `source.jazz`.

The fixture test reads `fixtures/representative.jz`, parses it through
`parseSurfaceProgram`, and checks its source contains the required lexical
families: `#`, `module`, `import`, `data`, `class`, `impl`, `operator`,
`precedence`, `right`, `@{`, `::`, `\(`, `->`, `case`, `if`, `then`, `else`,
single-quoted character, double-quoted text, a Unicode escape, a numeric suffix,
and a purity-marked identifier.

- [ ] **Step 2: Run the repository audit and observe RED.**

Run:

```sh
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next repository-audit-spec --test-show-details=failures
```

Expected: FAIL with a missing `editors/vscode-jazz/package.json` path after
Cabal resolves the audit-only `aeson` dependency.

- [ ] **Step 3: Add the VS Code manifest.**

Create `package.json` with no scripts or dependencies:

```json
{
  "name": "jazz-language",
  "displayName": "Jazz Language",
  "description": "Syntax highlighting for the Jazz programming language.",
  "version": "0.1.0",
  "publisher": "jazz-lang",
  "license": "GPL-3.0-only",
  "engines": {
    "vscode": "^1.85.0"
  },
  "categories": ["Programming Languages"],
  "contributes": {
    "languages": [
      {
        "id": "jazz",
        "aliases": ["Jazz", "jazz"],
        "extensions": [".jz"],
        "configuration": "./language-configuration.json"
      }
    ],
    "grammars": [
      {
        "language": "jazz",
        "scopeName": "source.jazz",
        "path": "./syntaxes/jazz.tmLanguage.json"
      }
    ]
  }
}
```

- [ ] **Step 4: Add the language configuration.**

```json
{
  "comments": {
    "lineComment": "#"
  },
  "brackets": [
    ["{", "}"],
    ["[", "]"],
    ["(", ")"]
  ],
  "autoClosingPairs": [
    {"open": "{", "close": "}"},
    {"open": "[", "close": "]"},
    {"open": "(", "close": ")"},
    {"open": "\"", "close": "\"", "notIn": ["string", "comment"]}
  ],
  "surroundingPairs": [
    ["{", "}"],
    ["[", "]"],
    ["(", ")"],
    ["\"", "\""]
  ]
}
```

Do not auto-pair single quotes because apostrophe is a legal Jazz identifier
continuation.

- [ ] **Step 5: Add the TextMate grammar.**

Create `syntaxes/jazz.tmLanguage.json` with root keys `name: "Jazz"`,
`scopeName: "source.jazz"`, and ordered includes for comments, strings,
characters, numbers, keywords, export namespaces, built-in types,
purity-marked identifiers, constructor/type identifiers, signatures, lambdas,
operators, and punctuation. Use these contract regexes:

```json
{
  "comments": {"patterns": [{"name": "comment.line.number-sign.jazz", "match": "#.*$"}]},
  "escapes": {"patterns": [{"name": "constant.character.escape.jazz", "match": "\\\\(?:[\\\\'\"nrt0]|u\\{[0-9A-Fa-f]{1,6}\\})"}]},
  "strings": {"patterns": [{"name": "string.quoted.double.jazz", "begin": "\"", "end": "\"", "patterns": [{"include": "#escapes"}]}]},
  "characters": {"patterns": [{"name": "string.quoted.single.jazz", "begin": "'", "end": "'", "patterns": [{"include": "#escapes"}]}]},
  "numbers": {"patterns": [{"name": "constant.numeric.jazz", "match": "\\b(?:[0-9]+(?:\\.[0-9]+)?)(?:i8|i16|i32|i64|u8|u16|u32|u64|f16|f32|f64)?\\b"}]},
  "keywords": {"patterns": [{"name": "keyword.control.jazz", "match": "\\b(?:if|then|else|case)\\b"}, {"name": "keyword.declaration.jazz", "match": "\\b(?:module|import|as|data|class|impl|operator|tier|precedence|left|right|nonassoc)\\b"}]},
  "exports": {"patterns": [{"name": "storage.modifier.export.jazz", "match": "\\b(?:value|constructor|type|class)\\b"}]},
  "builtins": {"patterns": [{"name": "support.type.builtin.jazz", "match": "\\b(?:Bool|Char|Text|Int|Float|Int8|Int16|Int32|Int64|UInt8|UInt16|UInt32|UInt64|Float16|Float32|Float64|List)\\b"}, {"name": "constant.language.jazz", "match": "\\b(?:True|False)\\b"}]},
  "purity": {"patterns": [{"name": "entity.name.function.effectful.jazz", "match": "\\b[a-z_][A-Za-z0-9_']*!\\b"}]},
  "types": {"patterns": [{"name": "entity.name.type.jazz", "match": "\\b[A-Z][A-Za-z0-9_']*\\b"}]},
  "signatures": {"patterns": [{"name": "keyword.operator.signature.jazz", "match": "::|@|:"}]},
  "lambdas": {"patterns": [{"name": "keyword.operator.lambda.jazz", "match": "\\\\|->"}]},
  "operators": {"patterns": [{"name": "keyword.operator.jazz", "match": "==|!=|<=|>=|=>|[$+\\-*/|%&?^~<>:=@]+"}]},
  "punctuation": {"patterns": [{"name": "punctuation.separator.jazz", "match": "[.,]"}, {"name": "punctuation.section.group.jazz", "match": "[(){}\\[\\]]"}]}
}
```

Place the repository entries under a root `repository` object and reference
them through the root `patterns` array in the order listed above.

- [ ] **Step 6: Add the valid representative fixture and README.**

The fixture must parse without resolving imports or type checking. Use this
active surface:

```jazz
# Jazz syntax-highlighting fixture.
module Editor::Representative (type Choice, constructor Empty, constructor Chosen, class Matches, value compare!, value sample) {
  import Text as TextTools.

  operator %% precedence 25 right.
  (%%) :: Int -> Int -> Int.
  (%%) = \(left, right) -> left + right.

  data Choice a = Empty | Chosen a.

  class Matches(a) {
    matches :: a -> a -> Bool.
  }.

  impl Matches(Int) {
    matches = \(left, right) -> left == right.
  }.

  compare! :: @{Matches(Int)}: Int -> Int -> Bool.
  compare! = \(left, right) -> if left == right then True else False.

  sample = case Chosen 42i16 {
    | Empty -> (0, '\n', "empty")
    | Chosen value -> (value %% 1, '\u{266B}', "Jazz \\u{1F3B7}")
  }.
}
```

The README states that this is syntax highlighting only, documents the source
location, explains copying or symlinking `vscode-jazz` into the VS Code
extensions directory followed by an editor reload, and notes that future VSIX
packaging needs standard VS Code extension tooling but is not a repository
dependency.

- [ ] **Step 7: Add editor assets to the Cabal source distribution.**

Extend `extra-source-files`:

```cabal
    editors/vscode-jazz/README.md
    editors/vscode-jazz/package.json
    editors/vscode-jazz/language-configuration.json
    editors/vscode-jazz/syntaxes/*.json
    editors/vscode-jazz/fixtures/*.jz
```

- [ ] **Step 8: Run focused validation and inspect the source archive.**

```sh
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next repository-audit-spec \
    parser-foundation-spec --test-show-details=failures
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal sdist --project-dir=jazz-next
tar -tf jazz-next/dist-newstyle/sdist/jazz-next-0.1.0.0.tar.gz | \
  rg 'editors/vscode-jazz/(README|package|language-configuration|syntaxes|fixtures)'
```

Expected: tests pass and all five editor-package asset families are present in
the archive.

- [ ] **Step 9: Commit the editor package.**

```sh
git add jazz-next/editors jazz-next/jazz-next.cabal \
  jazz-next/test/JazzNext/Repository/AuditSpec.hs
git commit -m "feat: add Jazz syntax highlighting"
```

---

### Task 5: Update active docs and close Batch 2

**Files:**
- Modify: `jazz-next/README.md`
- Modify: `docs/jazz-improvement-backlog.md`
- Modify: `docs/jazz-language-state.md`
- Modify: `docs/spec/stdlib-boundary.md`
- Modify: `docs/spec/modules/00-module-clarification-matrix.md`
- Modify: `docs/spec/runtime/text-character-semantics.md`
- Modify: `docs/superpowers/specs/2026-07-13-jazz-next-source-editor-ergonomics-design.md`
- Modify: `docs/superpowers/plans/2026-07-13-jazz-next-source-editor-ergonomics.md`

**Interfaces:**
- Consumes: verified source paths, compact authored syntax, editor installation contract, and source-distribution evidence from Tasks 2-4.
- Produces: accurate active documentation, Batch 2 completion state, and final verification evidence.

- [ ] **Step 1: Update active path and editor instructions.**

In `jazz-next/README.md`, add a `Jazz-authored sources` section describing
`jazz/stdlib`, `jazz/compiler`, dependency direction, and the fact that test and
benchmark fixtures remain outside the shipped-source root. Add an `Editor
support` section linking `editors/vscode-jazz/README.md`.

Change active path references in language-state and normative specs from
`jazz-next/stdlib/...` to `jazz-next/jazz/stdlib/...`. Describe `Lexer` and
`LexerTypes` as compiler sources under `jazz-next/jazz/compiler/`. Do not edit
historical plans, archived closure evidence, or legacy-reference paths.

- [ ] **Step 2: Mark the batch and design implemented.**

Change Batch 2 in `docs/jazz-improvement-backlog.md` to:

```markdown
Status: completed on 2026-07-13.
```

Change the design status to:

```markdown
Approved and implemented on `2026-07-13`.
```

Check every completed implementation-plan checkbox only after its associated
command has passed.

- [ ] **Step 3: Run active documentation gates and stale-path review.**

```sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
rg -n 'jazz-next/stdlib|packageRoot </> "stdlib"|"stdlib/Prelude\.jz"' \
  jazz-next/src jazz-next/test jazz-next/README.md docs/jazz-language-state.md \
  docs/spec
git diff --check
```

Expected: both gates pass, `git diff --check` is silent, and the active-path
search returns no stale old-root consumer. Any match in a historical context is
reviewed rather than mechanically rewritten.

- [ ] **Step 4: Run complete pinned verification.**

```sh
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal build --project-dir=jazz-next all
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next all --test-show-details=failures
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal sdist --project-dir=jazz-next
```

Expected: all components build, all 37 suites pass, and `sdist` succeeds.

- [ ] **Step 5: Inspect the final archive contract.**

```sh
archive=jazz-next/dist-newstyle/sdist/jazz-next-0.1.0.0.tar.gz
tar -tf "$archive" | rg 'jazz/(stdlib|compiler)/.*\.jz$'
tar -tf "$archive" | rg 'editors/vscode-jazz/'
if tar -tf "$archive" | rg '/stdlib/.*\.jz$' | rg -v '/jazz/stdlib/' | rg -q .; then
  exit 1
fi
```

Expected: both source roles and editor assets are present; no legacy package
`stdlib/` tree is present.

- [ ] **Step 6: Commit Batch 2 closeout.**

```sh
git add jazz-next/README.md docs/jazz-improvement-backlog.md \
  docs/jazz-language-state.md docs/spec \
  docs/superpowers/specs/2026-07-13-jazz-next-source-editor-ergonomics-design.md \
  docs/superpowers/plans/2026-07-13-jazz-next-source-editor-ergonomics.md
git commit -m "docs: close Jazz improvement batch 2"
```

---

## Final Review Checklist

- [ ] `git status --short` is clean after the closeout commit.
- [ ] `git diff --check origin/main...HEAD` is silent.
- [ ] `git diff --name-only origin/main...HEAD` contains nothing under `jazz-hs/` or `jazz2/`.
- [ ] `jazz-next/stdlib/` is absent.
- [ ] `jazz-next/jazz/stdlib/` contains eight shipped modules.
- [ ] `jazz-next/jazz/compiler/` contains `Lexer.jz` and `LexerTypes.jz`.
- [ ] Every active checked-in-source consumer uses the new role-aware paths.
- [ ] The explicit nested-lambda exception is documented and all ordinary authored chains are compact.
- [ ] VS Code registers `.jz` as `jazz` with TextMate scope `source.jazz`.
- [ ] All editor JSON parses and the representative fixture parses as Jazz.
- [ ] Cabal `sdist` includes both source roles and all editor assets.
- [ ] GHC 9.14.1 builds all components and all 37 test suites pass.
