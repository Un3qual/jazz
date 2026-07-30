# Jazz Language Surface and Parity Implementation Plan

> **Superseded in part on 2026-07-30:** Tasks and claims for function equations
> are historical. The active surface retains pattern-lambda function heads and
> explicit `case`; see `2026-07-30-jazz-remove-function-equations.md`.

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Reserve `value`, add structured constructor-field types, and add ordered function equations while keeping the Haskell stage-0 compiler and Jazz-authored lexer/parser/core exact mirrors.

**Architecture:** The parser surface gains structured function-clause and constructor-type values, then lowers them into the existing `SLet`, `ELambda`, `EPatternCase`, and `SignatureType` core. No new callable or conditional form crosses into analysis or runtime. The Haskell implementation lands first for each behavior, followed immediately by the equivalent Jazz-authored schema/parser/lowerer and exact comparison adapters.

**Tech Stack:** Haskell 2010, Megaparsec token parsers, Jazz-authored compiler modules, Cabal test suites, Nix development shell.

## Global Constraints

- All compiler implementation changes land under `jazz-next/`.
- `jazz-hs/` and `jazz2/` remain read-only.
- Haskell remains the authoritative stage-0 implementation.
- `value` is globally reserved; `type`, `constructor`, and `class` remain contextual export prefixes.
- `if ... then ... else ...` remains non-strict syntax and is not replaced with a strict function.
- Function clauses are contiguous, ordered, same-name, positive-arity, equal-arity groups.
- Function equations lower completely into existing core forms.
- `Type(arguments)` is the sole named type-application syntax.
- Opaque constructor-field metadata is removed rather than retained as compatibility behavior.
- No typed-core production, core-to-IR lowering, LLVM, native-runtime, object, or linker work is included.
- Each task uses a red-green test cycle and ends with a focused commit.

---

## File Structure

- `jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs`: owns the stage-0 `TValue` token.
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`: owns `SurfaceFunctionClause`, `SSFunction`, and structured constructor field types.
- `jazz-next/src/JazzNext/Compiler/Parser/Pattern.hs`: owns grouped function-head pattern parsing.
- `jazz-next/src/JazzNext/Compiler/Parser/Declaration.hs`: recognizes and groups equations, consumes `TValue` in exports, and parses constructor fields with the signature grammar.
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`: erases function equations into existing core forms and lowers constructor signature types.
- `jazz-next/src/JazzNext/Compiler/AST.hs`: stores constructor fields as `[SignatureType]`.
- `jazz-next/src/JazzNext/Compiler/Name.hs`: owns stable generated function-equation argument identities.
- `jazz-next/src/JazzNext/Compiler/{Force,ModuleGraph,ModuleResolver}.hs`: traverses and resolves structured fields and new surface statements.
- `jazz-next/src/JazzNext/Compiler/TypeInference/{Scope,Signature}.hs`: converts structured constructor signatures into constructor schemes.
- `jazz-next/src/JazzNext/Compiler/Runtime/{Types,Semantics}.hs`: preserves structured runtime constructor metadata and applies nested runtime type hints.
- `jazz-next/jazz/compiler/{LexerTypes,Lexer,ParserTypes,ParserPattern,ParserDeclaration,CoreTypes,CoreLower}.jz`: mirrors the accepted Haskell surface and lowering.
- `jazz-next/test/JazzNext/Compiler/Bootstrap/Canonical{Lexer,Parser,Core}Comparison.hs`: converts both implementations to identical runtime comparison values.
- Existing parser, semantic, module, bootstrap, and profiling tests are updated in place; no parallel test harness is introduced.

### Task 1: Make `value` a Real Stage-0 Keyword

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Declaration.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Failure.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/Foundation/ExpressionsTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/Foundation/SignaturesTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/Foundation/ModulesTests.hs`
- Modify: `jazz-next/test/fixtures/lexer/keyword-operator-inventory.jz`

**Interfaces:**
- Produces: `TValue :: TokenKind`.
- Produces: module exports accepting `TValue TIdentifier` as `ValueNamespace`.
- Preserves: contextual `TIdentifier "type"`, `"constructor"`, and `"class"` export handling.

- [ ] **Step 1: Add failing lexer and parser tests**

Add assertions equivalent to:

```haskell
assertEqual
  "value keyword token"
  (Right [Token TValue "value" (SourceSpan 1 1)])
  (tokenizeDetailed "value")
```

Add table-driven invalid sources for every identifier position:

```haskell
[ "value = 1.",
  "identity value = value.",
  "f = \\value -> value.",
  "f = case x { | value -> value }.",
  "data Box value = Box value.",
  "module value { answer = 1. }.",
  "import Example as value."
]
```

Keep this valid:

```jazz
module Example (value answer) {
  answer = 42.
}
```

- [ ] **Step 2: Run the focused tests and confirm the old contextual behavior fails**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    parser-foundation-spec \
    module-import-parser-spec \
    --test-show-details=failures
```

Expected: failures show `value` is still tokenized and accepted as an identifier.

- [ ] **Step 3: Add `TValue` and route export parsing explicitly**

Implement the token addition and keyword mapping:

```haskell
data TokenKind
  = TIdentifier Text
  | TModule
  | TImport
  | TAs
  | TData
  | TValue
  | TIf
  ...

identifierKind ident =
  case ident of
    ...
    "value" -> TValue
    ...
```

Change `parseModuleExport` so only the value selector consumes the reserved token:

```haskell
parseModuleExport tokens =
  case tokens of
    Token {tokenKind = TValue}
      : Token {tokenKind = TIdentifier exportName, tokenSpan = exportSpan}
      : rest ->
        Right
          (ModuleExportSelector (Just ValueNamespace) exportName, exportSpan, rest)
    Token {tokenKind = TIdentifier prefix}
      : Token {tokenKind = TIdentifier exportName, tokenSpan = exportSpan}
      : rest
      | Just TypeNamespace <- moduleExportNamespacePrefix prefix ->
          parseTypeModuleExport exportName exportSpan rest
      | Just namespace <- moduleExportNamespacePrefix prefix ->
          Right (ModuleExportSelector (Just namespace) exportName, exportSpan, rest)
    ...
```

Remove `"value"` from `moduleExportNamespacePrefix`. Use the existing expected-identifier diagnostic path for `TValue` elsewhere rather than adding identifier exceptions.

- [ ] **Step 4: Run the focused tests and update exact expected diagnostics**

Run the Step 2 command. Expected: PASS, with the primary span on the `value` token for every rejected identifier position.

- [ ] **Step 5: Commit**

```bash
git add jazz-next/src/JazzNext/Compiler/Parser jazz-next/test/JazzNext/Compiler/Parser jazz-next/test/fixtures/lexer
git commit -m "feat: reserve value in the stage-0 grammar"
```

### Task 2: Replace Opaque Constructor Fields with Structured Signature Types

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/AST.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Declaration.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Force.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleGraph.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/AdtPattern/DeclarationsTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/AdtPattern/InvalidSyntaxTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/DeclarationParserSpec.hs`

**Interfaces:**
- Produces: `SurfaceDataConstructor Identifier [SurfaceSignatureType]`.
- Produces: `DataConstructor Name [SignatureType]`.
- Consumes: `parseSignatureTypePrefixDetailed`.
- Removes: `SurfaceDataConstructorArgument` and `DataConstructorArgument`.

- [ ] **Step 1: Add failing structured-field parser and lowering tests**

Add this canonical source:

```jazz
data Tree a
  = Leaf a
  | Branch Tree(a) Tree(a).
data Callback a b
  = Callback (a -> b).
data Forest a
  = Forest [Tree(a)].
```

Assert the first declaration contains:

```haskell
SurfaceDataConstructor "Leaf" [SurfaceTypeVariable "a"]
SurfaceDataConstructor
  "Branch"
  [ SurfaceTypeApplication "Tree" [SurfaceTypeVariable "a"],
    SurfaceTypeApplication "Tree" [SurfaceTypeVariable "a"]
  ]
```

Assert lowering produces `TypeVariable` and `TypeApplication` equivalents. Add invalid cases for an unterminated group, `Tree a`, an empty `Tree()`, and an undeclared lowercase variable nested in `Tree(missing)`.

- [ ] **Step 2: Run the ADT parser tests and confirm the opaque representation fails**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    adt-pattern-parser-spec \
    declaration-parser-spec \
    --test-show-details=failures
```

Expected: the current parser splits `Tree(a)` incorrectly or returns opaque fields.

- [ ] **Step 3: Change the surface/core data shapes and reuse the signature parser**

Use these exact shapes:

```haskell
data SurfaceDataConstructor =
  SurfaceDataConstructor Identifier [SurfaceSignatureType]

data DataConstructor =
  DataConstructor Name [SignatureType]
```

In `parseDataConstructorArguments`, repeatedly call `parseSignatureTypePrefixDetailed`, stop only at top-level `|` or `.`, and validate all `SurfaceTypeVariable` names recursively against the declaration parameter set. Do not consume balanced groups into an opaque sentinel.

Add one recursive validator:

```haskell
surfaceSignatureTypeVariables :: SurfaceSignatureType -> Set Text
```

It must traverse applications, lists, tuples, and both sides of functions. Report the existing declaration diagnostic structure at the field's primary span when the set contains an undeclared name.

- [ ] **Step 4: Lower, force, collect, and resolve every nested signature node**

Replace constructor-argument branches with existing signature helpers:

```haskell
lowerSurfaceDataConstructor
  (SurfaceDataConstructor constructorName fieldTypes) =
    DataConstructor
      (sourceName constructorName)
      (map lowerSurfaceSignatureType fieldTypes)
```

Use `forceSurfaceSignatureType`, `forceSignatureType`, recursive signature-name collection, and `resolveSignatureType` at the existing module boundaries. No field type may be discarded.

- [ ] **Step 5: Run parser, module-resolution, and forcing tests**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    adt-pattern-parser-spec \
    declaration-parser-spec \
    module-resolution-spec \
    profiling-spec \
    --test-show-details=failures
```

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add jazz-next/src/JazzNext/Compiler jazz-next/test/JazzNext/Compiler
git commit -m "feat: structure data constructor field types"
```

### Task 3: Build Complete Constructor Schemes and Runtime Hints

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Scope.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Signature.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime/Types.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime/Semantics.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime/Primitives.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleCompiler.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs`
- Modify: runtime observation and capability test modules that construct `DataConstructor` values

**Interfaces:**
- Consumes: `[SignatureType]` from each `DataConstructor`.
- Produces: complete `ConstructorArgumentType` values through `signatureTypeToExpressionType`.
- Produces: `VConstructor Name [Name] Name [SignatureType] [RuntimeValue]`.

- [ ] **Step 1: Add failing generic recursive constructor tests**

Add a source-level type/runtime test:

```jazz
data Tree a
  = Leaf a
  | Branch Tree(a) Tree(a).

leftmost :: Tree(Bool) -> Bool.
leftmost (Leaf item) = item.
leftmost (Branch left _) = leftmost left.

answer = leftmost (Branch (Leaf True) (Leaf False)).
```

Assert successful inference, `True`, and rejection of `Branch (Leaf 1) (Leaf False)` under `Tree(Bool)`.

- [ ] **Step 2: Run semantic tests and confirm nested fields are not typed**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    adt-pattern-type-spec \
    adt-pattern-runtime-spec \
    --test-show-details=failures
```

Expected: failure because nested constructor applications are still fresh/opaque.

- [ ] **Step 3: Convert constructor signatures through the existing signature type engine**

Replace `constructorArgumentTypes` with:

```haskell
constructorArgumentTypes ::
  [Name] ->
  [SignatureType] ->
  InferState ->
  ([ConstructorArgumentType], InferState)
```

Create one fresh `ExpressionType` per declared type parameter, pass that environment to `signatureTypeToExpressionType`, and wrap each converted result in `ConstructorArgumentMonomorphic`. Preserve `ConstructorArgumentParameter` only if another existing consumer requires that public interface; do not synthesize `ConstructorArgumentFresh` for a parsed supported field.

- [ ] **Step 4: Preserve and substitute complete runtime signature fields**

Change runtime constructor metadata to `[SignatureType]`. Implement:

```haskell
substituteConstructorFieldType ::
  Map Text SignatureType ->
  SignatureType ->
  SignatureType
```

Recurse through `TypeApplication`, `TypeList`, `TypeTuple`, and `TypeFunction`. Use the substituted result in runtime hint attachment and exact/compatible constraint matching. Constructor saturation remains `length fieldTypes == length capturedArgs`.

- [ ] **Step 5: Run semantic, module, runtime, and observation suites**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    adt-pattern-type-spec \
    adt-pattern-runtime-spec \
    module-pipeline-contract-spec \
    runtime-semantics-spec \
    runtime-observation-spec \
    profiling-spec \
    --test-show-details=failures
```

Expected: PASS with no `DataConstructorArgumentOpaque` references under `jazz-next/src`.

- [ ] **Step 6: Commit**

```bash
git add jazz-next/src/JazzNext/Compiler jazz-next/test/JazzNext/Compiler
git commit -m "feat: type structured constructor payloads"
```

### Task 4: Parse Ordered Function Equations and Grouped Head Patterns

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Pattern.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Declaration.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Failure.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Parser/FunctionEquationParserSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`

**Interfaces:**
- Produces: `SurfaceFunctionClause SourceSpan [SurfacePattern] SurfaceExpr`.
- Produces: `SSFunction Identifier SourceSpan (NonEmpty SurfaceFunctionClause)`.
- Produces: `parseFunctionHeadPatternParser :: Parser SurfacePattern`.

- [ ] **Step 1: Add the dedicated failing parser suite**

Register `function-equation-parser-spec` and cover:

```jazz
identity item = item.
constant left right = left.
mapMaybe transform Nothing = Nothing.
mapMaybe transform (Just item) = Just (transform item).
pair (left, right) = left.
headOr fallback [first | _] = first.
headOr fallback [] = fallback.
```

Assert one `SSFunction` per contiguous name group, clause order, per-clause spans, and arity. Add failures for zero head patterns, mixed arity, unterminated grouped patterns, and a signature separated from its function by another statement.

- [ ] **Step 2: Run the new suite and confirm equations are expression errors**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    function-equation-parser-spec \
    --test-show-details=failures
```

Expected: FAIL before a valid function-equation AST exists.

- [ ] **Step 3: Add the surface values and transparent grouped patterns**

Add:

```haskell
data SurfaceFunctionClause =
  SurfaceFunctionClause SourceSpan [SurfacePattern] SurfaceExpr

data SurfaceStatement
  = SSLet Identifier SourceSpan SurfaceExpr
  | SSFunction Identifier SourceSpan (NonEmpty SurfaceFunctionClause)
  ...
```

In `Pattern.hs`, split parenthesized parsing into:

```haskell
parseParenthesizedPattern :: Token -> Parser SurfacePattern
```

Return the inner pattern when the matching `)` follows without a comma; return `SPTuple` only when a comma is present. The grouped form does not add a new AST constructor.

- [ ] **Step 4: Recognize and consume a complete contiguous equation group**

Before the ordinary `name =` case, detect:

```text
TIdentifier functionName, one-or-more head patterns, TEquals
```

Parse one clause body through the existing expression parser and dot terminator, then consume following clauses only while the next statement starts with the same source name. Validate each clause arity against the first and emit a deterministic declaration failure at the first mismatching clause.

Require parentheses for constructor patterns with payloads in a head, which keeps adjacent parameters unambiguous. Bare nullary constructors remain valid head patterns.

- [ ] **Step 5: Run parser and existing pattern/lambda suites**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    function-equation-parser-spec \
    pattern-parser-spec \
    lambda-parser-spec \
    parser-foundation-spec \
    --test-show-details=failures
```

Expected: PASS and no change to tuple-pattern or lambda-pattern behavior.

- [ ] **Step 6: Commit**

```bash
git add jazz-next/src/JazzNext/Compiler/Parser jazz-next/test/JazzNext/Compiler/Parser jazz-next/jazz-next.cabal
git commit -m "feat: parse function equations"
```

### Task 5: Lower Function Equations into the Existing Core

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/Name.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Force.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/FunctionEquationParserSpec.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Semantics/FunctionEquationSemanticsSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`

**Interfaces:**
- Produces: `FunctionEquationArgument Int :: GeneratedNameKind`.
- Produces: one `SLet` with curried `ELambda` values and one ordered `EPatternCase`.
- Preserves: existing analyzer, inference, recursion, and runtime interfaces.

- [ ] **Step 1: Add failing exact-lowering and execution tests**

For:

```jazz
choose Nothing fallback = fallback.
choose (Just item) _ = item.
answer = choose (Just 42) 0.
```

Assert the lowered value has this shape:

```haskell
SLet
  "choose"
  span1
  ( ELambda (GeneratedName (FunctionEquationArgument 1))
      ( ELambda (GeneratedName (FunctionEquationArgument 2))
          ( EPatternCase
              ( ETuple
                  [ EVar (GeneratedName (FunctionEquationArgument 1)),
                    EVar (GeneratedName (FunctionEquationArgument 2))
                  ]
              )
              [ CaseArm (PTuple [PConstructor "Nothing" [], PVariable "fallback"]) Nothing ...,
                CaseArm (PTuple [PConstructor "Just" [PVariable "item"], PWildcard]) Nothing ...
              ]
          )
      )
  )
```

Also test one-argument direct scrutinee lowering, currying, partial application, ordered fallthrough, signature application to the group, recursion, and the existing non-exhaustive match failure.

- [ ] **Step 2: Run the new semantic suite and confirm lowering is missing**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    function-equation-parser-spec \
    function-equation-semantics-spec \
    --test-show-details=failures
```

Expected: FAIL in lowering/semantic assertions.

- [ ] **Step 3: Add stable generated names and one lowering function**

Add:

```haskell
data GeneratedNameKind
  = LambdaPatternArgument Int
  | FunctionEquationArgument Int
  | ...
```

Implement:

```haskell
lowerSurfaceFunction ::
  Identifier ->
  SourceSpan ->
  NonEmpty SurfaceFunctionClause ->
  Statement
```

Generate argument names from `[1..arity]`, build the direct or tuple scrutinee once, map clauses to ordered `CaseArm`s, and fold the argument names into nested unary `ELambda`s. `SSFunction` must be eliminated by `lowerSurfaceStatement`.

- [ ] **Step 4: Extend force and resolver traversal without adding a core statement**

Force every clause span, pattern, and body in the surface tree. Resolve only after lowering, so `ModuleResolver` continues to see `SLet`; its surface-only inventory traversal must count `SSFunction` as a local value binding and collect constructor/type references from each clause.

- [ ] **Step 5: Run parser, semantics, recursion, modules, and warning tests**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    function-equation-parser-spec \
    function-equation-semantics-spec \
    recursive-bindings-spec \
    rebinding-warning-spec \
    module-resolution-spec \
    module-exports-spec \
    --test-show-details=failures
```

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add jazz-next/src/JazzNext/Compiler jazz-next/test/JazzNext/Compiler jazz-next/jazz-next.cabal
git commit -m "feat: lower function equations to canonical core"
```

### Task 6: Mirror the Lexer, Parser Values, and Core Lowering in Jazz

**Files:**
- Modify: `jazz-next/jazz/compiler/LexerTypes.jz`
- Modify: `jazz-next/jazz/compiler/Lexer.jz`
- Modify: `jazz-next/jazz/compiler/ParserTypes.jz`
- Modify: `jazz-next/jazz/compiler/ParserPattern.jz`
- Modify: `jazz-next/jazz/compiler/ParserDeclaration.jz`
- Modify: `jazz-next/jazz/compiler/CoreTypes.jz`
- Modify: `jazz-next/jazz/compiler/CoreLower.jz`
- Modify: other `jazz-next/jazz/compiler/*.jz` files whose ordinary identifier `value` must be renamed for the hosted compiler to parse
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparison.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparison.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalCoreComparison.hs`

**Interfaces:**
- Produces: `ValueKeyword`.
- Produces: `SurfaceFunctionClause`, `FunctionStatement`, structured `[SurfaceSignatureType]` constructor fields.
- Produces: `CoreFunctionEquationArgument Int`.
- Removes: named/opaque constructor argument mirror values.

- [ ] **Step 1: Add failing exact comparison values**

Extend canonical adapters so the authoritative side expects:

```haskell
KeywordKind ValueKeyword
```

and runtime values named:

```text
SurfaceFunctionClause
FunctionStatement
CoreFunctionEquationArgument
```

Change constructor comparison payloads to reuse the existing surface/core signature-type runtime conversion functions.

- [ ] **Step 2: Run the three canonical comparison suites**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    canonical-lexer-comparison-spec \
    canonical-parser-comparison-spec \
    canonical-core-comparison-spec \
    --test-show-details=failures
```

Expected: FAIL because the Jazz-authored schemas do not yet expose the new values.

- [ ] **Step 3: Update Jazz schemas and the lexer**

Use canonical Jazz declarations:

```jazz
data CanonicalKeyword
  = ModuleKeyword
  | ImportKeyword
  | AsKeyword
  | DataKeyword
  | ValueKeyword
  | IfKeyword
  | ThenKeyword
  | ElseKeyword
  | CaseKeyword.

data SurfaceFunctionClause
  = SurfaceFunctionClause CanonicalSpan [SurfacePattern] SurfaceExpr.

data SurfaceDataConstructor
  = SurfaceDataConstructor Text [SurfaceSignatureType].
```

Add `("value", KeywordKind ValueKeyword)` to `keywordKinds`. Rename every ordinary compiler-source `value` binder by meaning (`item`, `result`, `payload`, `current`, or a domain name); module export selectors remain `value`.

- [ ] **Step 4: Port grouped patterns, function grouping, structured fields, and lowering**

Mirror the Haskell algorithms rather than inventing Jazz-only behavior. The hosted lowerer must produce exactly:

```jazz
CoreLetStatement
  name
  span
  (CoreLambdaExpression
    (CoreGeneratedName (CoreFunctionEquationArgument 1))
    ...)
```

Structured constructor fields lower with `lowerSignatureType`; no opaque case remains.

- [ ] **Step 5: Run exact comparison suites**

Run the Step 2 command. Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add jazz-next/jazz/compiler jazz-next/test/JazzNext/Compiler/Bootstrap
git commit -m "feat: mirror language quality syntax in Jazz"
```

### Task 7: Extend Hosted Parity, Failure, and Scale Coverage

**Files:**
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzLexerParitySpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserTypesDeclarationsModulesSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserControlFlowPatternsSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreSignaturesDeclarationsOperatorsSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreControlFlowPatternsSpec.hs`
- Modify: hosted parser scale generators/assertions where declaration forms are enumerated
- Modify: canonical comparison specs that enumerate every constructor name

**Interfaces:**
- Consumes: completed stage-0 and Jazz-authored implementations.
- Produces: exact repeated parity for successful values, failures, spans, and generated-name identity.

- [ ] **Step 1: Add parity cases for every new success and failure boundary**

Include:

```jazz
module Example (value answer) {
  data Tree a
    = Leaf a
    | Branch Tree(a) Tree(a).
  leftmost (Leaf item) = item.
  leftmost (Branch left _) = leftmost left.
  answer = leftmost (Branch (Leaf 1) (Leaf 2)).
}
```

Add paired failures for reserved `value`, unequal clause arity, malformed grouped patterns, invalid `Tree()`, and undeclared nested type variables. Each parity assertion runs twice and compares the complete canonical result.

- [ ] **Step 2: Run parity tests and record any adapter omissions**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    jazz-lexer-parity-spec \
    jazz-parser-types-declarations-modules-spec \
    jazz-parser-control-flow-patterns-spec \
    jazz-core-signatures-declarations-operators-spec \
    jazz-core-control-flow-patterns-spec \
    --test-show-details=failures
```

Expected before fixture updates: FAIL only in the newly added cases.

- [ ] **Step 3: Complete adapters and scale generators**

Update every closed constructor inventory in comparison specs. Extend declaration and control-flow scale generators with deterministic function equations and structured recursive fields while retaining the existing 65-statement smoke and 513-statement optional full tiers.

- [ ] **Step 4: Run parity and smoke-scale suites**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    jazz-lexer-parity-spec \
    jazz-parser-parity-spec \
    jazz-parser-scale-spec \
    jazz-parser-types-declarations-modules-spec \
    jazz-parser-control-flow-patterns-spec \
    jazz-core-signatures-declarations-operators-spec \
    jazz-core-control-flow-patterns-spec \
    --test-show-details=failures
```

Expected: PASS with exact repeated observations.

- [ ] **Step 5: Commit**

```bash
git add jazz-next/test/JazzNext/Compiler/Bootstrap
git commit -m "test: close hosted language parity coverage"
```

### Task 8: Verify the Language Workstream Boundary

**Files:**
- Modify only files required by failures found in this verification task.

**Interfaces:**
- Produces: a green language/parity gate ready for the authored-source audit.

- [ ] **Step 1: Prove obsolete representations and forbidden legacy edits are absent**

Run:

```bash
if rg -n 'SurfaceDataConstructorArgument|DataConstructorArgumentOpaque|CoreOpaqueConstructorArgument' jazz-next/src jazz-next/jazz/compiler; then
  exit 1
fi
git diff main...HEAD -- jazz-hs jazz2
```

Expected: the search and legacy diff are empty.

- [ ] **Step 2: Run the complete parser, semantic, module, and bootstrap matrix serially**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    adt-pattern-parser-spec \
    declaration-parser-spec \
    function-equation-parser-spec \
    function-equation-semantics-spec \
    adt-pattern-type-spec \
    adt-pattern-runtime-spec \
    module-resolution-spec \
    module-exports-spec \
    canonical-lexer-comparison-spec \
    canonical-parser-comparison-spec \
    canonical-core-comparison-spec \
    jazz-lexer-parity-spec \
    jazz-parser-parity-spec \
    jazz-parser-scale-spec \
    jazz-core-signatures-declarations-operators-spec \
    jazz-core-control-flow-patterns-spec \
    --test-show-details=failures \
    --jobs=1
```

Expected: PASS.

- [ ] **Step 3: Run build and diff hygiene**

Run:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal build --project-dir=jazz-next all
git diff --check
```

Expected: PASS.

- [ ] **Step 4: Commit any verification-only corrections**

If verification required corrections:

```bash
git add jazz-next
git commit -m "fix: close language surface parity gate"
```

If no correction was required, record the passing commands in the implementation handoff without creating an empty commit.
