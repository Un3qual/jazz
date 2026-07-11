---
id: JN-BOOTSTRAP-CHAR-TEXT-LITERALS-001
status: done
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-07-10
completed_on: 2026-07-10
plan_section: "Implementation Batch: Bootstrap Char/Text Literals"
target_paths:
  - docs/spec/runtime/text-character-semantics.md
  - docs/spec/runtime/primitive-semantics.md
  - docs/feature-status.md
  - docs/jazz-language-state.md
  - docs/execution/queue.md
  - docs/execution/done-archive.md
  - docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md
  - jazz-next/src/JazzNext/Compiler/AST.hs
  - jazz-next/src/JazzNext/Compiler/Parser/AST.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Expression.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Pattern.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Signature.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Lower.hs
  - jazz-next/src/JazzNext/Compiler/CapabilityFacts.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference/Types.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference/Solver.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference/Capabilities.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference/Diagnostics.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference/Pattern.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference/Scope.hs
  - jazz-next/src/JazzNext/Compiler/Runtime.hs
  - jazz-next/test/JazzNext/Compiler/Parser/TokenParserSpec.hs
  - jazz-next/test/JazzNext/Compiler/Parser/ExpressionParserSpec.hs
  - jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
  - jazz-next/test/JazzNext/Compiler/Parser/Foundation/ExpressionsTests.hs
  - jazz-next/test/JazzNext/Compiler/Parser/Foundation/SignaturesTests.hs
  - jazz-next/test/JazzNext/Compiler/Parser/PatternParserSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/BasicsTests.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RenderingTests.hs
  - jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
  - jazz-next/test/JazzNext/Compiler/Modules/Loader/VisibilityTests.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/TokenParserSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ExpressionParserSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/PatternParserSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
  - bash jazz-next/scripts/test-warning-config.sh
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Add backend-independent Char and Text literal semantics end to end in jazz-next, including validated escapes, signatures, literal patterns, strict equality, runtime rendering, and module transport; keep text traversal, I/O, ordering, bytes, LLVM lowering, and the native runtime in later children."
---

# Jazz-Next Bootstrap Char/Text Literals Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add backend-independent `Char` and `Text` literal values end to end so later Jazz-authored bootstrap modules can represent source characters, lexemes, paths, and diagnostics.

**Architecture:** Parser tokens decode validated Unicode scalar and text literals into surface/core literal nodes. Type inference owns distinct `Char` and `Text` types with strict equality; the stage-0 interpreter stores them as Haskell `Char` and `Data.Text.Text`, while the public semantics stay representation-neutral for the future native runtime ABI. This child does not add a bytecode layer, text traversal APIs, host I/O, or LLVM lowering.

**Tech Stack:** Haskell 2010, Megaparsec over `Text`, the canonical `jazz-next` surface/core AST, split type-inference modules, interpreter runtime, focused `runghc` suites, and repository queue/docs gates.

## Global Constraints

- Modify only `jazz-next/` and active documentation; `jazz-hs/` and `jazz2/` remain read-only.
- LLVM-generated native binaries remain the long-term target. Do not add bytecode, a bytecode VM, LLVM bindings, object generation, native linking, or native runtime code in this child.
- Treat Haskell `Char` and `Text` as stage-0 interpreter representations only. Jazz source semantics, tests, and APIs must not expose Haskell-specific layout or operations.
- `Char` denotes one Unicode scalar value and rejects surrogate code points.
- `Text` is immutable Unicode scalar text. Length, indexing, slicing, traversal, builders, classification, and I/O belong to later bootstrap children.
- Character literals use single quotes. Text literals use double quotes.
- Support `\\`, `\'`, `\"`, `\n`, `\r`, `\t`, `\0`, and `\u{HEX}` escapes. `HEX` contains 1-6 hexadecimal digits and must decode to a Unicode scalar no greater than `0x10FFFF` and outside `0xD800..0xDFFF`.
- Reject raw newline/carriage-return characters, empty or multi-scalar character literals, invalid escapes, invalid scalar escapes, and unterminated literals with deterministic `E0001` diagnostics at the opening quote.
- Preserve raw quoted source in `tokenLexeme`; store only decoded values in `TChar`, `TText`, `SLChar`, `SLText`, `LChar`, and `LText`.
- Support `Char` and `Text` adjacent monomorphic signatures, literal patterns, lists/tuples, strict equality/inequality, operator values, and equality sections.
- Keep `<`, `<=`, `>`, and `>=` for `Char`/`Text` out of this child. Total ordering lands with the later text primitive API so direct, value, and section forms can be added consistently.
- Keep interpolation, multiline text, locale-sensitive classification, grapheme segmentation, normalization, bytes, and implicit Char/Text conversion out of scope.
- Implement behavior test-first and commit each independently reviewable task.

---

## File Map

- `docs/spec/runtime/text-character-semantics.md`: normative scalar, literal, escape, equality, rendering, and staging contract.
- `jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs`: quote-aware tokenization and escape/scalar validation.
- `jazz-next/src/JazzNext/Compiler/Parser/{AST,Expression,Pattern,Signature,Lower}.hs`: surface representation, expression/pattern parsing, type names, and canonical lowering.
- `jazz-next/src/JazzNext/Compiler/AST.hs`: canonical literal and signature constructors.
- `jazz-next/src/JazzNext/Compiler/TypeInference*.hs`: `Char`/`Text` expression types, signatures, unification, equality support, runtime hints, and diagnostics.
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`: stage-0 values, strict equality, literal patterns, runtime type names, and source rendering.
- Parser and semantic specs listed in frontmatter: focused red/green coverage.
- `docs/{feature-status.md,jazz-language-state.md}` and queue/archive files: implemented status and dispatcher closeout.

## Implementation Batch: Bootstrap Char/Text Literals

### Task 1: Lock Literal Semantics and Tokenize Validated Scalars

**Files:**

- Modify: `docs/spec/runtime/text-character-semantics.md`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/TokenParserSpec.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs`

**Interfaces:**

- Consumes: `tokenize :: Text -> Either Diagnostic [Token]`, `Token { tokenKind, tokenLexeme, tokenSpan }`, and parser diagnostic code `E0001`.
- Produces: `TChar Char`, `TText Text`, and tokens whose raw lexemes include their quotes while their token kinds contain decoded values.

- [x] **Step 1: Write lexer tests for simple and escaped literals**

Register these tests in `TokenParserSpec.hs`:

```haskell
    ("tokenizes Char and Text literals", testTokenizesCharAndTextLiterals),
    ("decodes Char and Text escapes", testDecodesCharAndTextEscapes),
    ("preserves quoted literal lexemes and spans", testPreservesQuotedLiteralLexemesAndSpans),
```

Add:

```haskell
testTokenizesCharAndTextLiterals :: IO ()
testTokenizesCharAndTextLiterals = do
  tokens <- lexSource "'a' \"Jazz\"."
  assertEqual
    "Char/Text token kinds"
    [TChar 'a', TText "Jazz", TDot]
    (map tokenKind tokens)

testDecodesCharAndTextEscapes :: IO ()
testDecodesCharAndTextEscapes = do
  tokens <- lexSource "'\\n' \"quote: \\\"; scalar: \\u{1F3B7}\"."
  assertEqual
    "decoded escapes"
    [TChar '\n', TText "quote: \"; scalar: 🎷", TDot]
    (map tokenKind tokens)

testPreservesQuotedLiteralLexemesAndSpans :: IO ()
testPreservesQuotedLiteralLexemesAndSpans = do
  tokens <- lexSource "'\\u{41}' \"a\\n\""
  assertEqual
    "literal lexemes and spans"
    [("'\\u{41}'", SourceSpan 1 1), ("\"a\\n\"", SourceSpan 1 10)]
    [(tokenLexeme token, tokenSpan token) | token <- tokens]
```

Import `SourceSpan (..)` in the test.

- [x] **Step 2: Run the lexer tests and verify they fail**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/TokenParserSpec.hs
```

Expected: compilation fails because `TChar` and `TText` do not exist.

- [x] **Step 3: Add malformed-literal tests**

Register:

```haskell
    ("rejects malformed Char and Text literals", testRejectsMalformedCharAndTextLiterals),
```

Add:

```haskell
testRejectsMalformedCharAndTextLiterals :: IO ()
testRejectsMalformedCharAndTextLiterals = do
  let cases =
        [ ("empty Char", "''", "character literal must contain exactly one Unicode scalar"),
          ("multi-scalar Char", "'ab'", "character literal must contain exactly one Unicode scalar"),
          ("unterminated Char", "'a", "unterminated character literal"),
          ("unterminated Text", "\"abc", "unterminated text literal"),
          ("invalid escape", "'\\x'", "invalid escape '\\x'"),
          ("empty scalar escape", "'\\u{}'", "Unicode escape must contain 1-6 hexadecimal digits"),
          ("surrogate scalar", "'\\u{D800}'", "Unicode escape is not a scalar value"),
          ("large scalar", "'\\u{110000}'", "Unicode escape is not a scalar value"),
          ("raw newline", "\"a\nb\"", "raw newline is not allowed in a text literal")
        ]
  mapM_
    (\(label, source, expected) ->
       case tokenize source of
         Left diagnostic -> do
           assertContains (label <> " code") "E0001" (renderDiagnostic diagnostic)
           assertContains label expected (renderDiagnostic diagnostic)
         Right tokens -> failTest (label <> ": expected failure, got " <> Text.pack (show tokens)))
    cases
```

- [x] **Step 4: Implement quote-aware lexer tokens**

In `Lexer.hs`, add token constructors:

```haskell
  | TChar Char
  | TText Text
```

Dispatch quote-prefixed tokens before integers and identifiers:

```haskell
tokenParser :: LexerParser Token
tokenParser = do
  position <- MP.getSourcePos
  let spanValue = sourcePosSpan position
  charToken spanValue
    <|> textToken spanValue
    <|> intToken spanValue
    <|> identifierToken spanValue
    <|> symbolToken spanValue
```

Add these helpers, using the existing `LexerError` path for all failures:

```haskell
charToken :: SourceSpan -> LexerParser Token
charToken spanValue = do
  (raw, values) <- MP.match (quotedScalars '\'' "character" spanValue)
  case values of
    [value] -> pure Token {tokenKind = TChar value, tokenLexeme = raw, tokenSpan = spanValue}
    _ -> literalFailure spanValue "character literal must contain exactly one Unicode scalar"

textToken :: SourceSpan -> LexerParser Token
textToken spanValue = do
  (raw, values) <- MP.match (quotedScalars '"' "text" spanValue)
  pure Token {tokenKind = TText (Text.pack values), tokenLexeme = raw, tokenSpan = spanValue}

quotedScalars :: Char -> Text -> SourceSpan -> LexerParser [Char]
quotedScalars delimiter label spanValue = do
  void (char delimiter)
  go []
  where
    go reversedValues = do
      atEnd <- MP.atEnd
      if atEnd
        then literalFailure spanValue ("unterminated " <> label <> " literal")
        else do
          next <- MP.lookAhead MP.anySingle
          if next == delimiter
            then void (char delimiter) *> pure (reverse reversedValues)
            else do
              value <- quotedScalar delimiter label spanValue
              go (value : reversedValues)

quotedScalar :: Char -> Text -> SourceSpan -> LexerParser Char
quotedScalar delimiter label spanValue =
  escapedScalar spanValue
    <|> MP.satisfy
      (\value ->
         value /= delimiter
           && value /= '\\'
           && value /= '\n'
           && value /= '\r'
           && unicodeScalar value)
    <|> do
      value <- MP.lookAhead MP.anySingle
      if value == '\n' || value == '\r'
        then literalFailure spanValue ("raw newline is not allowed in a " <> label <> " literal")
        else literalFailure spanValue ("invalid " <> label <> " literal character")

escapedScalar :: SourceSpan -> LexerParser Char
escapedScalar spanValue = do
  void (char '\\')
  escape <- MP.anySingle
  case escape of
    '\\' -> pure '\\'
    '\'' -> pure '\''
    '"' -> pure '"'
    'n' -> pure '\n'
    'r' -> pure '\r'
    't' -> pure '\t'
    '0' -> pure '\0'
    'u' -> unicodeScalarEscape spanValue
    _ -> literalFailure spanValue ("invalid escape '\\" <> Text.singleton escape <> "'")

unicodeScalarEscape :: SourceSpan -> LexerParser Char
unicodeScalarEscape spanValue = do
  void (char '{')
  digits <- MP.takeWhileP (Just "Unicode scalar body") (/= '}')
  maybeClose <- MP.optional (char '}')
  if maybeClose == Nothing
    then literalFailure spanValue "unterminated Unicode escape"
    else if Text.length digits < 1 || Text.length digits > 6 || not (Text.all isHexDigit digits)
    then literalFailure spanValue "Unicode escape must contain 1-6 hexadecimal digits"
    else
      case TextRead.hexadecimal digits :: Either String (Integer, Text) of
        Right (value, trailing)
          | Text.null trailing,
            value <= 0x10FFFF,
            not (value >= 0xD800 && value <= 0xDFFF) -> pure (chr (fromInteger value))
        _ -> literalFailure spanValue "Unicode escape is not a scalar value"

unicodeScalar :: Char -> Bool
unicodeScalar value =
  let scalar = ord value
   in scalar < 0xD800 || scalar > 0xDFFF

literalFailure :: SourceSpan -> Text -> LexerParser a
literalFailure spanValue message =
  MP.customFailure (LexerError (message <> " at " <> renderSpanValue spanValue))
```

Import `chr`, `isHexDigit`, and `ord` from `Data.Char`.

- [x] **Step 5: Verify the normative semantics document against the implementation**

Keep `docs/spec/runtime/text-character-semantics.md` aligned with these locked rules:

```markdown
# Text and Character Semantics

Status: active bootstrap contract; Char/Text literal and equality child

`Char` is one Unicode scalar value. `Text` is an immutable sequence of Unicode
scalar values. Character literals use single quotes; text literals use double
quotes. Supported escapes are `\\`, `\'`, `\"`, `\n`, `\r`, `\t`, `\0`, and
`\u{HEX}` with 1-6 digits. Surrogates, values above `0x10FFFF`, raw newlines,
invalid escapes, empty/multi-scalar character literals, and unterminated quoted
literals reject with `E0001` at the opening quote.

`Char` and `Text` support `==` and `!=` only in this child. Ordering,
classification, traversal, indexing, slicing, concatenation, builders, bytes,
and I/O are later bootstrap contracts. The Haskell interpreter representation
is non-normative; future LLVM-generated binaries implement the same semantics
through the native runtime ABI.
```

- [x] **Step 6: Run focused verification and commit**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/TokenParserSpec.hs
git diff --check
```

Expected: all TokenParser tests pass and `git diff --check` exits 0.

Commit:

```bash
git add docs/spec/runtime/text-character-semantics.md jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs jazz-next/test/JazzNext/Compiler/Parser/TokenParserSpec.hs
git commit -m "feat: tokenize Jazz Char and Text literals"
```

### Task 2: Carry Char/Text Through Parsing, Patterns, Signatures, and Lowering

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/AST.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Expression.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Pattern.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Signature.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/ExpressionParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/PatternParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/Foundation/ExpressionsTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/Foundation/SignaturesTests.hs`

**Interfaces:**

- Consumes: `TChar Char` and `TText Text` from Task 1.
- Produces: surface/core literals `SLChar`/`SLText` and `LChar`/`LText`; signature constructors `SurfaceTypeChar`/`SurfaceTypeText` and `TypeChar`/`TypeText`; expression and literal-pattern parsing.

- [x] **Step 1: Add failing expression and lowering tests**

Register in `ExpressionParserSpec.hs`:

```haskell
    ("parses Char and Text expressions", testParsesCharAndTextExpressions),
```

Add:

```haskell
testParsesCharAndTextExpressions :: IO ()
testParsesCharAndTextExpressions = do
  tokens <- lexSource "pair 'a' \"Jazz\"."
  assertExpression
    "Char/Text application"
    (SEApply (SEApply (SEVar "pair") (SELit (SLChar 'a'))) (SELit (SLText "Jazz")))
    [TDot]
    (parseExpressionTokens Set.empty [] tokens)
```

In `Foundation/ExpressionsTests.hs`, register and add:

```haskell
testLowersCharAndTextLiterals :: IO ()
testLowersCharAndTextLiterals = do
  assertEqual "lower Char" (ELit (LChar 'a')) (lowerSurfaceExpr (SELit (SLChar 'a')))
  assertEqual "lower Text" (ELit (LText "Jazz")) (lowerSurfaceExpr (SELit (SLText "Jazz")))
```

- [x] **Step 2: Add failing literal-pattern and signature tests**

In `PatternParserSpec.hs`, register and add:

```haskell
testParsesCharAndTextLiteralPatterns :: IO ()
testParsesCharAndTextLiteralPatterns = do
  charTokens <- lexSource "'a' -> body"
  assertEqual
    "Char literal pattern"
    (Right (SPLiteral (SLChar 'a'), [TArrow, TIdentifier "body"]))
    (fmap (fmap tokenKinds) (parseCaseArmPatternTokens charTokens))
  textTokens <- lexSource "\"Jazz\" -> body"
  assertEqual
    "Text literal pattern"
    (Right (SPLiteral (SLText "Jazz"), [TArrow, TIdentifier "body"]))
    (fmap (fmap tokenKinds) (parseCaseArmPatternTokens textTokens))
```

In `Foundation/SignaturesTests.hs`, register and add:

```haskell
testParsesCharAndTextSignatures :: IO ()
testParsesCharAndTextSignatures =
  assertEqual
    "Char/Text signatures"
    ( Right
        ( SEBlock
            [ SSSignature "character" (SourceSpan 1 1) (SurfaceSignatureType SurfaceTypeChar),
              SSSignature "message" (SourceSpan 2 1) (SurfaceSignatureType SurfaceTypeText),
              SSSignature
                "render"
                (SourceSpan 3 1)
                (SurfaceSignatureType (SurfaceTypeFunction SurfaceTypeChar SurfaceTypeText))
            ]
        )
    )
    (parseSurfaceProgram "character :: Char.\nmessage :: Text.\nrender :: Char -> Text.")
```

- [x] **Step 3: Run parser suites and verify red failures**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ExpressionParserSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/PatternParserSpec.hs
```

Expected: compilation fails on missing Char/Text surface/core constructors.

- [x] **Step 4: Add surface and core constructors**

In `Parser/AST.hs`:

```haskell
data SurfaceLiteral
  = SLInt Integer
  | SLFloat Double FractionalLiteralSource (Maybe SurfaceNumericType)
  | SLBool Bool
  | SLChar Char
  | SLText Text
  deriving (Eq, Show)

data SurfaceSignatureType
  = SurfaceTypeInt
  | SurfaceTypeFloat
  | SurfaceTypeNumeric SurfaceNumericType
  | SurfaceTypeBool
  | SurfaceTypeChar
  | SurfaceTypeText
  | SurfaceTypeList SurfaceSignatureType
  | SurfaceTypeTuple [SurfaceSignatureType]
  | SurfaceTypeFunction SurfaceSignatureType SurfaceSignatureType
  deriving (Eq, Show)
```

In `AST.hs`:

```haskell
data Literal
  = LInt Integer
  | LFloat Double FractionalLiteralSource (Maybe NumericType)
  | LBool Bool
  | LChar Char
  | LText Text
  deriving (Eq, Show)

data SignatureType
  = TypeInt
  | TypeFloat
  | TypeNumeric NumericType
  | TypeBool
  | TypeChar
  | TypeText
  | TypeList SignatureType
  | TypeTuple [SignatureType]
  | TypeFunction SignatureType SignatureType
  deriving (Eq, Show)
```

- [x] **Step 5: Parse expressions, literal patterns, and signature names**

In `Expression.parsePrimaryExpr`, add before identifier cases:

```haskell
        TChar value -> pure (SELit (SLChar value))
        TText value -> pure (SELit (SLText value))
```

In `Pattern.hs`, extend the atomic literal branch:

```haskell
        TChar value -> pure (SPLiteral (SLChar value))
        TText value -> pure (SPLiteral (SLText value))
```

Do not add fractional literal patterns or new pattern syntax.

In `Signature.parseSurfacePrimitiveType` add:

```haskell
    "Char" -> Just SurfaceTypeChar
    "Text" -> Just SurfaceTypeText
```

- [x] **Step 6: Lower the new values and types**

Extend `lowerSurfaceLiteral`:

```haskell
    SLChar value -> LChar value
    SLText value -> LText value
```

Extend `lowerSurfaceSignatureType`:

```haskell
    SurfaceTypeChar -> TypeChar
    SurfaceTypeText -> TypeText
```

The existing `SPLiteral -> PLiteral` and expression lowering paths then carry the new values without another representation.

- [x] **Step 7: Run parser verification and commit**

Run the three parser suites from Step 3. Expected: all pass.

Commit:

```bash
git add jazz-next/src/JazzNext/Compiler/AST.hs jazz-next/src/JazzNext/Compiler/Parser jazz-next/test/JazzNext/Compiler/Parser
git commit -m "feat: parse and lower Jazz Char and Text literals"
```

### Task 3: Type Char/Text Values, Signatures, Patterns, and Equality

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/CapabilityFacts.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Types.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Solver.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Capabilities.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Diagnostics.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Pattern.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Scope.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/BasicsTests.hs`

**Interfaces:**

- Consumes: `LChar`, `LText`, `TypeChar`, and `TypeText` from Task 2.
- Produces: `TCharType`, `TTextType`, signature conversion, literal-pattern typing, equality support, and deterministic `Char`/`Text` diagnostics.

- [x] **Step 1: Add failing source type/equality tests**

Register and add in `PrimitiveSemanticsSpec.hs`:

```haskell
testSourcePipelineAcceptsCharTextEquality :: IO ()
testSourcePipelineAcceptsCharTextEquality = do
  assertCompiles "same = 'a' == 'a'. different = 'a' != 'b'."
  assertCompiles "same = \"Jazz\" == \"Jazz\". different = \"Jazz\" != \"jazz\"."

testSourcePipelineRejectsCharTextMismatch :: IO ()
testSourcePipelineRejectsCharTextMismatch = do
  result <- compileSource defaultWarningSettings "bad = 'a' == \"a\"."
  assertSingleDiagnosticContains "Char/Text mismatch" "E2004" (compileErrors result)

testSourcePipelineAcceptsCharTextEqualityValuesAndSections :: IO ()
testSourcePipelineAcceptsCharTextEqualityValuesAndSections =
  assertCompiles
    "eq = (==). char = eq 'a' 'a'. text = (\"Jazz\" ==) \"Jazz\". other = (!= \"Jazz\") \"jazz\"."
```

Register and add this test to `BindingSignature/BasicsTests.hs`; it is already
included by `BindingSignatureCoherenceSpec.hs`:

```haskell
testSourceAcceptsCharTextSignatures :: IO ()
testSourceAcceptsCharTextSignatures =
  assertSourceOk
    "character :: Char.\ncharacter = 'a'.\nmessage :: Text.\nmessage = \"Jazz\".\n(message, character)."
```

Expected: no compile diagnostics.

- [x] **Step 2: Add failing pattern type tests**

Add source tests to `PrimitiveSemanticsSpec.hs`:

```haskell
testSourcePipelineTypesCharTextPatterns :: IO ()
testSourcePipelineTypesCharTextPatterns = do
  assertCompiles "x = case 'a' { | 'a' -> True | _ -> False }."
  assertCompiles "x = case \"Jazz\" { | \"Jazz\" -> True | _ -> False }."
  result <- compileSource defaultWarningSettings "x = case 'a' { | \"a\" -> True | _ -> False }."
  assertSingleDiagnosticContains "Char/Text pattern mismatch" "E2011" (compileErrors result)
```

- [x] **Step 3: Run semantic suites and verify red failures**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
```

Expected: compilation fails because `TCharType` and `TTextType` do not exist.

- [x] **Step 4: Add expression types and unification**

In `TypeInference/Types.hs`:

```haskell
data ExpressionType
  = TIntType
  | TIntegerLiteralType IntegerLiteralRange
  | TFloatType
  | TNumericType NumericType
  | TBoolType
  | TCharType
  | TTextType
  | TListType ExpressionType
  | TTupleType [ExpressionType]
  | TDataType Name [ExpressionType]
  | TFunctionType ExpressionType ExpressionType
  | TVarType Int
  deriving (Eq, Show)
```

In both `TypeInference.literalExpressionType` and `TypeInference.Pattern.literalExpressionType` add:

```haskell
    LChar _ -> TCharType
    LText _ -> TTextType
```

In `Solver.resolveType`, `Solver.unifyTypes`, `Solver.typeContainsFunction`, free-variable traversals, and other closed primitive-type cases add terminal `TCharType`/`TTextType` branches matching `TBoolType` behavior. The unifier cases are:

```haskell
        (TCharType, TCharType) -> Just state
        (TTextType, TTextType) -> Just state
```

Add to `supportsRuntimeEqualityTypeWith`:

```haskell
    TCharType -> True
    TTextType -> True
```

- [x] **Step 5: Convert signatures and runtime hints**

In `TypeInference.Capabilities.signatureTypeToExpressionType` add:

```haskell
    TypeChar -> TCharType
    TypeText -> TTextType
```

In `CapabilityFacts.signatureTypeToConstraintSignatureType` add:

```haskell
    TypeChar -> ConstraintTypeName "Char"
    TypeText -> ConstraintTypeName "Text"
```

Where expression types become constraint/runtime hints, add:

```haskell
    TCharType -> Just (ConstraintTypeName "Char")
    TTextType -> Just (ConstraintTypeName "Text")
```

Where `ConstraintTypeName` becomes an expression type, recognize:

```haskell
    "Char" -> Just TCharType
    "Text" -> Just TTextType
```

In `TypeInference.Scope`, `Capabilities.freeTypeVariables`, `Capabilities.replaceTypeVariables`, literal defaulting, and type-scheme traversal, treat `TCharType` and `TTextType` as closed nonnumeric types exactly as `TBoolType`, without adding numeric or class constraints.

- [x] **Step 6: Render deterministic type names and diagnostics**

In `TypeInference.Diagnostics` add:

```haskell
    TCharType -> "Char"
    TTextType -> "Text"
```

and:

```haskell
    TypeChar -> "Char"
    TypeText -> "Text"
```

Update the `E2004` unsupported-equality message to list `Char` and `Text` among supported primitive types.

- [x] **Step 7: Guard exhaustiveness mechanically**

Run:

```bash
rg -n "TBoolType|TypeBool|LBool" jazz-next/src/JazzNext/Compiler/TypeInference.hs jazz-next/src/JazzNext/Compiler/TypeInference
```

For every closed primitive-type case, add `Char`/`Text` branches. Do not add them to numeric constraints, numeric defaulting, conversion, or arithmetic cases.

- [x] **Step 8: Run semantic verification and commit**

Run the two suites from Step 3. Expected: all pass.

Commit:

```bash
git add jazz-next/src/JazzNext/Compiler/CapabilityFacts.hs jazz-next/src/JazzNext/Compiler/TypeInference.hs jazz-next/src/JazzNext/Compiler/TypeInference jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature
git commit -m "feat: type Jazz Char and Text values"
```

### Task 4: Execute, Compare, Match, and Render Char/Text Values

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RenderingTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

**Interfaces:**

- Consumes: typed `LChar`/`LText` core literals and `ConstraintTypeName "Char"`/`"Text"` hints.
- Produces: `VChar Char`, `VText Text`, source-style rendering, strict equality/inequality, literal-pattern matching, and runtime type diagnostics.

- [x] **Step 1: Add failing runtime value and rendering tests**

Register in `Runtime/RenderingTests.hs`:

```haskell
    ("Char and Text literals evaluate and render", testCharTextLiteralRendering),
    ("Char and Text strict equality evaluates", testCharTextStrictEquality),
    ("Char and Text literal patterns match", testCharTextLiteralPatterns),
```

Add:

```haskell
testCharTextLiteralRendering :: IO ()
testCharTextLiteralRendering = do
  result <- runSource defaultWarningSettings "('a', '\\n', \"Jazz\", \"a\\n\\\"b\")."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "('a', '\\n', \"Jazz\", \"a\\n\\\"b\")") (runOutput result)

testCharTextStrictEquality :: IO ()
testCharTextStrictEquality = do
  result <- runSource defaultWarningSettings "('a' == 'a', 'a' != 'b', \"Jazz\" == \"Jazz\", \"Jazz\" != \"jazz\")."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, True, True, True)") (runOutput result)

testCharTextLiteralPatterns :: IO ()
testCharTextLiteralPatterns = do
  result <- runSource defaultWarningSettings "(case 'a' { | 'a' -> 1 | _ -> 0 }, case \"Jazz\" { | \"Jazz\" -> 1 | _ -> 0 })."
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(1, 1)") (runOutput result)
```

- [x] **Step 2: Run runtime tests and verify red failures**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
```

Expected: compilation fails because `VChar` and `VText` do not exist.

- [x] **Step 3: Add runtime values and source escaping**

Add constructors:

```haskell
  | VChar Char
  | VText Text
```

Extend `Eq RuntimeValue`, `Show RuntimeValue`, and `literalRuntimeValue`:

```haskell
      (VChar leftChar, VChar rightChar) -> leftChar == rightChar
      (VText leftText, VText rightText) -> leftText == rightText
```

```haskell
      VChar value -> "VChar " <> show value
      VText value -> "VText " <> show value
```

```haskell
    LChar value -> VChar value
    LText value -> VText value
```

Add one shared source escape helper:

```haskell
renderQuotedScalar :: Char -> Text
renderQuotedScalar value =
  case value of
    '\\' -> "\\\\"
    '\'' -> "\\'"
    '"' -> "\\\""
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    '\0' -> "\\0"
    _
      | isControl value ->
          "\\u{" <> Text.pack (map toUpper (showHex (ord value) "")) <> "}"
    _ -> Text.singleton value
```

Import `isControl`, `ord`, and `toUpper` from `Data.Char` and `showHex` from
`Numeric`.

Use it in `renderRuntimeValue`:

```haskell
    VChar value -> "'" <> renderQuotedScalar value <> "'"
    VText value -> "\"" <> Text.concatMap renderQuotedScalar value <> "\""
```

- [x] **Step 4: Implement strict equality and pattern equality**

In `evalBinary` add:

```haskell
    ("==", VChar left, VChar right) -> Right (VBool (left == right))
    ("!=", VChar left, VChar right) -> Right (VBool (left /= right))
    ("==", VText left, VText right) -> Right (VBool (left == right))
    ("!=", VText left, VText right) -> Right (VBool (left /= right))
```

In `runtimeStructuralEquality` add:

```haskell
    (VChar left, VChar right) -> Just (left == right)
    (VText left, VText right) -> Just (left == right)
```

The existing `PLiteral` matcher calls `literalRuntimeValue`, so these branches also make literal patterns executable.

- [x] **Step 5: Support runtime type hints and diagnostics**

In `runtimeValueCanAcceptTypeHint` and `applyRuntimeTypeHint`, accept only exact pairs:

```haskell
        (ConstraintTypeName "Char", VChar {}) -> True
        (ConstraintTypeName "Text", VText {}) -> True
```

Use `identifierText` if the matched type name is a structured `Name`.

In `renderRuntimeType` add:

```haskell
    VChar {} -> "Char"
    VText {} -> "Text"
```

Do not accept numeric conversions, implicit `Char -> Text`, or text ordering.

- [x] **Step 6: Guard runtime exhaustiveness mechanically**

Run:

```bash
rg -n "VBool|LBool" jazz-next/src/JazzNext/Compiler/Runtime.hs
```

Add terminal `VChar`/`VText` cases to closed value traversals such as default-literal attachment and function detection. They must remain unchanged values and must never be treated as callable, numeric, structural containers, or targeted numeric values.

- [x] **Step 7: Run runtime verification and commit**

Run the runtime suite from Step 2. Expected: all tests pass.

Commit:

```bash
git add jazz-next/src/JazzNext/Compiler/Runtime.hs jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs jazz-next/test/JazzNext/Compiler/Semantics/Runtime
git commit -m "feat: execute Jazz Char and Text values"
```

### Task 5: Prove Module Transport, Update Status, and Close the Queue Child

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/Loader/VisibilityTests.hs`
- Modify: `docs/spec/runtime/primitive-semantics.md`
- Modify: `docs/feature-status.md`
- Modify: `docs/jazz-language-state.md`
- Modify: `docs/execution/queue.md`
- Modify: `docs/execution/done-archive.md`
- Modify: `docs/superpowers/plans/2026-07-10-jazz-next-bootstrap-char-text-literals.md`

**Interfaces:**

- Consumes: end-to-end Char/Text behavior from Tasks 1-4.
- Produces: cross-module evidence, current documentation, archived closure evidence, and an empty or correctly advanced dispatcher.

- [x] **Step 1: Add a failing module transport regression**

Register and add to `Loader/VisibilityTests.hs`:

```haskell
    ("run module graph transports Char/Text values", testRunModuleGraphTransportsCharTextValues),
```

```haskell
testRunModuleGraphTransportsCharTextValues :: IO ()
testRunModuleGraphTransportsCharTextValues = do
  result <-
    runModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just "(True, True)") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/Lib/TextValues.jz", "module Lib::TextValues (value letter, value message) { letter :: Char. letter = 'J'. message :: Text. message = \"Jazz\". }"),
          ("src/App/Main.jz", "module App::Main { import Lib::TextValues (letter, message). (letter == 'J', message == \"Jazz\"). }")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)
```

- [x] **Step 2: Run the loader suite**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
```

Expected: the registered module transport test passes using the implementation
from Tasks 1-4. Any required fix stays within module interface/runtime hint
transport for `Char`/`Text`; import syntax remains unchanged.

- [x] **Step 3: Update authoritative status docs**

Update `primitive-semantics.md` with strict Char/Text equality and the explicit ordering deferral.

Add `Char and Text literals` to `feature-status.md` as `Implemented Today`, citing parser AST/lowering, inference, runtime, token/parser suites, primitive/runtime suites, and loader coverage.

Update `jazz-language-state.md` to list:

- single-line Char/Text literals and supported escapes;
- `Char`/`Text` monomorphic signatures;
- literal patterns and strict equality;
- Haskell representation as stage-0-only; and
- traversal, I/O, ordering, bytes, lowered IR, and LLVM generation as later bootstrap/backend children.

- [x] **Step 4: Run the full verification matrix**

Run every frontmatter verification command in order. Expected: each exits 0. Record focused suite pass counts and the full script result in the done-archive evidence.

- [x] **Step 5: Close queue and plan metadata**

After verification succeeds:

1. remove `JN-BOOTSTRAP-CHAR-TEXT-LITERALS-001` from `Ready Now`;
2. add one `done-archive.md` row with the exact behavior and verification evidence;
3. set this plan's `status: done`, `completed_on: 2026-07-10`, and refreshed `last_verified`;
4. leave `Next Curation Target` empty unless a separately accepted plan for text traversal/classification has been written; and
5. make the queue executor status say the Char/Text child landed and that no later bootstrap child is promoted unless one actually exists.

- [x] **Step 6: Verify metadata and commit closeout**

Run:

```bash
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

Expected: all checks exit 0.

Commit:

```bash
git add docs jazz-next/test/JazzNext/Compiler/Modules
git commit -m "docs: close Jazz Char and Text bootstrap child"
```

## Plan Self-Review Checklist

- The child produces one end-to-end behavior: Char/Text literals, signatures, patterns, equality, rendering, and module transport.
- It does not implement traversal, classification, I/O, generic named-type applications, stack-safe evaluation, the Jazz lexer, lowered backend IR, LLVM lowering, or the native runtime.
- Stage-0 Haskell representations are private and the semantics are reusable by the future LLVM native runtime.
- No bytecode format or bytecode VM is introduced.
- Every production change begins with a focused failing test and ends with an independently meaningful commit.
