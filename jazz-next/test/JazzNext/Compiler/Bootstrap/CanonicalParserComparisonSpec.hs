{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.CanonicalLexerComparison
  ( CanonicalSourcePath,
    normalizeCanonicalSourcePath,
  )
import JazzNext.Compiler.Bootstrap.CanonicalParserComparison
  ( canonicalizeParserResult,
    canonicalizeSourceResult,
    parserFailureRuntimeValue,
    renderCanonicalParserResult,
    renderCanonicalSourceResult,
    surfaceExprRuntimeValue,
  )
import JazzNext.Compiler.DiagnosticCatalog
  ( ErrorCode (E0001),
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runModuleGraph,
    runRuntimeErrors,
  )
import JazzNext.Compiler.FractionalLiteral
  ( mkFractionalLiteralSource,
  )
import JazzNext.Compiler.ModuleExports
  ( LocatedModuleExportName (..),
    ModuleExportSelector (..),
    ModuleTypeConstructorSelector (..),
  )
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
  )
import JazzNext.Compiler.Name
  ( NameNamespace (..),
  )
import JazzNext.Compiler.Parser
  ( parseSurfaceProgramTokensDetailed,
  )
import JazzNext.Compiler.Parser.AST
import JazzNext.Compiler.Parser.Failure
import JazzNext.Compiler.Parser.FixtureCorpus
  ( ParserFixture (..),
    parserFixtureCorpus,
  )
import JazzNext.Compiler.Parser.Lexer
  ( LexicalFailure,
    TokenKind (..),
    tokenizeDetailed,
  )
import JazzNext.Compiler.Runtime
  ( renderRuntimeValue,
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
    runTestSuite,
  )
import JazzNext.TestSource
  ( JazzSourceRole (CompilerSource),
    readCheckedInJazzProjectModuleSource,
    readCheckedInJazzSource,
  )

main :: IO ()
main = runTestSuite "CanonicalParserComparison" tests

tests :: [NamedTest]
tests =
  [ ("loads the checked-in parser schema", testLoadsParserSchema),
    ("constructs the schema through the real Jazz module graph", testJazzSchemaRendering),
    ("canonicalizes the complete surface inventory", testSurfaceInventory),
    ("preserves source-exact numeric values", testNumericFidelity),
    ("canonicalizes the complete parser failure inventory", testFailureInventory),
    ("keeps source result phases distinct", testSourceResultPhases),
    ("adapts the fixed parser corpus deterministically", testCorpusDeterminism),
    ("canonicalizes a complete surface program", testCanonicalizesProgram)
  ]

testLoadsParserSchema :: IO ()
testLoadsParserSchema = do
  source <- readCheckedInJazzSource CompilerSource "ParserTypes.jz"
  assertContains "parser schema module" "module ParserTypes" source
  assertContains "parser result" "data CanonicalParserResult" source
  assertContains "source result" "data CanonicalSourceResult" source

testJazzSchemaRendering :: IO ()
testJazzSchemaRendering = do
  result <-
    runModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      lookupSource
  path <- normalizedPath "fixtures/parser/basic.jz"
  let expected =
        renderCanonicalParserResult
          (canonicalizeParserResult path (Right (SELit (SLInt 42))))
  assertEqual "Jazz schema compile errors" [] (runCompileErrors result)
  assertEqual "Jazz schema runtime errors" [] (runRuntimeErrors result)
  assertEqual "Jazz schema output" (Just expected) (runOutput result)
  where
    lookupSource sourcePath =
      case sourcePath of
        "src/App/Main.jz" -> pure (Just jazzSchemaFixture)
        _ -> readCheckedInJazzProjectModuleSource sourcePath

jazzSchemaFixture :: Text.Text
jazzSchemaFixture =
  """
  module App::Main {
    import LexerTypes (CanonicalSourcePath).
    import ParserTypes (CanonicalParserSuccess, LiteralExpression, IntegerLiteral).
    CanonicalParserSuccess (CanonicalSourcePath "fixtures/parser/basic.jz") (LiteralExpression (IntegerLiteral "42")).
  }

  """

testSurfaceInventory :: IO ()
testSurfaceInventory = do
  let rendered = renderRuntimeValue (surfaceExprRuntimeValue surfaceInventory)
      expectedConstructors =
        [ "IntegerLiteral",
          "FractionalLiteral",
          "BooleanLiteral",
          "CharacterLiteral",
          "TextLiteral",
          "WildcardPattern",
          "VariablePattern",
          "LiteralPattern",
          "ConstructorPattern",
          "ListPattern",
          "ConsListPattern",
          "TuplePattern",
          "AsPattern",
          "OrPattern",
          "IdentifierParameter",
          "PatternParameter",
          "NamedConstructorArgument",
          "OpaqueConstructorArgument",
          "LiteralExpression",
          "VariableExpression",
          "QualifiedVariableExpression",
          "LambdaExpression",
          "OperatorValueExpression",
          "ListExpression",
          "TupleExpression",
          "ApplyExpression",
          "TypeApplicationExpression",
          "IfExpression",
          "CaseExpression",
          "BinaryExpression",
          "LeftSectionExpression",
          "RightSectionExpression",
          "BlockExpression",
          "TypeSignature",
          "ConstrainedSignature",
          "UnsupportedSignature",
          "SurfaceSignatureConstraint",
          "AppliedType",
          "ListType",
          "TupleType",
          "FunctionType",
          "SurfaceClassMethodSignature",
          "SurfaceImplMethod",
          "LetStatement",
          "SignatureStatement",
          "DataStatement",
          "ClassStatement",
          "ImplStatement",
          "ModuleStatement",
          "ImportStatement",
          "ExpressionStatement",
          "NamedExportSelector",
          "TypeExportSelector",
          "AbstractTypeSelector",
          "AllConstructorsSelector",
          "SelectedConstructorsSelector"
        ]
  mapM_ (\constructorName -> assertContains constructorName constructorName rendered) expectedConstructors
  mapM_
    (\numericType -> assertContains (showText numericType) (numericConstructorName numericType) rendered)
    allNumericTypes
  mapM_
    (\signatureToken -> assertContains (showText signatureToken) (signatureTokenConstructorName signatureToken) rendered)
    allSignatureTokens

surfaceInventory :: SurfaceExpr
surfaceInventory =
  SEBlock
    [ SSLet "allExpressions" span1 (SEList allExpressions),
      SSSignature "plain" span1 (SurfaceSignatureType SurfaceTypeInt),
      SSSignature
        "constrained"
        span1
        ( SurfaceConstrainedSignature
            [SurfaceSignatureConstraint "Comparable" [SurfaceTypeVariable "a"]]
            (SurfaceTypeFunction (SurfaceTypeVariable "a") SurfaceTypeBool)
        ),
      SSSignature "unsupported" span1 (SurfaceUnsupportedSignature allSignatureTokens),
      SSData
        span1
        "Thing"
        ["a"]
        [ SurfaceDataConstructor
            "Thing"
            [SurfaceDataConstructorArgumentName "a", SurfaceDataConstructorArgumentOpaque]
        ],
      SSClass
        span1
        "Show"
        ["a"]
        [SurfaceClassMethodSignature "show" span1 (SurfaceSignatureType SurfaceTypeText)],
      SSImpl
        span1
        "Show"
        allSignatureTypes
        [SurfaceImplMethod "show" span1 (seText "shown")],
      SSModule span1 ["App", "Main"] (Just allModuleExports),
      SSImport span1 ["Core", "Text"] (Just "TextCore") (Just ["length"]),
      SSExpr span1 patternInventory
    ]

allExpressions :: [SurfaceExpr]
allExpressions =
  [ SELit (SLInt 42),
    SELit (SLFloat 0.0 (mkFractionalLiteralSource 0 10 4) Nothing),
    SELit (SLBool True),
    SELit (SLChar 'x'),
    SELit (SLText "Jazz"),
    SEVar "value",
    SEQualifiedVar "Text" "length",
    SELambda (SurfaceLambdaIdentifier "value" :| [SurfaceLambdaPattern SPWildcard]) (SEVar "value"),
    SEOperatorValue "+",
    SEList [seInt 1],
    SETuple [seInt 1, seInt 2],
    SEApply (SEVar "identity") (seInt 1),
    SETypeApplication (SEVar "identity") span1 (SurfaceTypeName "Int"),
    SEIf (SELit (SLBool True)) (seInt 1) (seInt 0),
    patternInventory,
    SEBinary "+" (seInt 1) (seInt 2),
    SESectionLeft (seInt 1) "+",
    SESectionRight "+" (seInt 1),
    SEBlock [SSExpr span1 (seInt 1)]
  ]
    <> [SELit (SLFloat 0.0 (mkFractionalLiteralSource 1 5 1) (Just numericType)) | numericType <- allNumericTypes]

patternInventory :: SurfaceExpr
patternInventory =
  SECase
    (SEVar "value")
    [ SurfaceCaseArm SPWildcard Nothing (seInt 0),
      SurfaceCaseArm (SPVariable "name") (Just (SELit (SLBool True))) (seInt 1),
      SurfaceCaseArm (SPLiteral (SLInt 2)) Nothing (seInt 2),
      SurfaceCaseArm (SPConstructor "Just" [SPVariable "item"]) Nothing (seInt 3),
      SurfaceCaseArm (SPList [SPWildcard]) Nothing (seInt 4),
      SurfaceCaseArm (SPConsList SPWildcard (SPVariable "rest")) Nothing (seInt 5),
      SurfaceCaseArm (SPTuple [SPWildcard, SPWildcard]) Nothing (seInt 6),
      SurfaceCaseArm (SPAs "whole" SPWildcard) Nothing (seInt 7),
      SurfaceCaseArm (SPOr [SPLiteral (SLInt 8), SPLiteral (SLInt 9)]) Nothing (seInt 8)
    ]

allSignatureTypes :: [SurfaceSignatureType]
allSignatureTypes =
  [ SurfaceTypeInt,
    SurfaceTypeFloat,
    SurfaceTypeBool,
    SurfaceTypeChar,
    SurfaceTypeText,
    SurfaceTypeVariable "a",
    SurfaceTypeName "Point",
    SurfaceTypeApplication "Map" [SurfaceTypeText, SurfaceTypeInt],
    SurfaceTypeList SurfaceTypeInt,
    SurfaceTypeTuple [SurfaceTypeInt, SurfaceTypeText],
    SurfaceTypeFunction SurfaceTypeInt SurfaceTypeText
  ]
    <> map SurfaceTypeNumeric allNumericTypes

allNumericTypes :: [SurfaceNumericType]
allNumericTypes =
  [ SurfaceNumericInt8,
    SurfaceNumericInt16,
    SurfaceNumericInt32,
    SurfaceNumericInt64,
    SurfaceNumericUInt8,
    SurfaceNumericUInt16,
    SurfaceNumericUInt32,
    SurfaceNumericUInt64,
    SurfaceNumericFloat16,
    SurfaceNumericFloat32,
    SurfaceNumericFloat64
  ]

allSignatureTokens :: [SurfaceSignatureToken]
allSignatureTokens =
  [ SurfaceSignatureNameToken "a",
    SurfaceSignatureIntToken 9223372036854775808,
    SurfaceSignatureArrowToken,
    SurfaceSignatureAtToken,
    SurfaceSignatureColonToken,
    SurfaceSignatureLParenToken,
    SurfaceSignatureRParenToken,
    SurfaceSignatureLBraceToken,
    SurfaceSignatureRBraceToken,
    SurfaceSignatureLBracketToken,
    SurfaceSignatureRBracketToken,
    SurfaceSignatureCommaToken,
    SurfaceSignatureOperatorToken "+",
    SurfaceSignatureOtherToken "."
  ]

allModuleExports :: [ModuleExportSelector]
allModuleExports =
  [ ModuleExportSelector Nothing "value",
    ModuleExportSelector (Just ValueNamespace) "value",
    ModuleExportSelector (Just ConstructorNamespace) "Point",
    ModuleExportSelector (Just TypeNamespace) "Point",
    ModuleExportSelector (Just CapabilityNamespace) "Comparable",
    ModuleTypeExportSelector "Opaque" span1 AbstractType,
    ModuleTypeExportSelector "Everything" span1 (AllTypeConstructors span1),
    ModuleTypeExportSelector
      "Selected"
      span1
      ( SelectedTypeConstructors
          (LocatedModuleExportName "First" span1 :| [LocatedModuleExportName "Second" span2])
      )
  ]

testNumericFidelity :: IO ()
testNumericFidelity = do
  assertRenderedSourceContains
    "arbitrary-precision integer"
    "9223372036854775808."
    "IntegerLiteral(\"9223372036854775808\")"
  assertRenderedSourceContains
    "fractional leading and trailing zeroes"
    "value = 1.0010."
    "FractionalLiteral(\"1\", \"0010\", Nothing)"
  mapM_
    ( \(suffix, expectedType) ->
        assertRenderedSourceContains
          ("fractional suffix " <> suffix)
          ("value = 1.50" <> suffix <> ".")
          ("FractionalLiteral(\"1\", \"50\", Just(" <> expectedType <> "))")
    )
    [("f16", "Float16Type"), ("f32", "Float32Type"), ("f64", "Float64Type")]
  let overflowSource = "value = " <> Text.replicate 400 "9" <> ".0."
  case detailedSourceResult overflowSource of
    Right (Left failure) ->
      assertContains
        "Float64 overflow reason"
        "InvalidFractionalLiteral"
        (renderRuntimeValue (parserFailureRuntimeValue failure))
    result -> failTest ("expected parser overflow failure, got " <> showText result)

testFailureInventory :: IO ()
testFailureInventory =
  mapM_
    ( \(expectedConstructor, reason) -> do
        let rendered =
              renderRuntimeValue
                (parserFailureRuntimeValue (ParserFailure E0001 (Just span1) reason))
        assertContains expectedConstructor expectedConstructor rendered
    )
    parserFailureInventory

parserFailureInventory :: [(Text.Text, ParserFailureReason)]
parserFailureInventory =
  [ ("EndOfInput", ExpectedSyntax "expression" ParserEndOfInput),
    ("EndOfInputAfter", ExpectedSyntax "expression" (ParserEndOfInputAfter "operator")),
    ("EndOfInputIn", ExpectedSyntax "expression" (ParserEndOfInputIn "block")),
    ("FoundToken", ExpectedSyntax "expression" (ParserFoundToken TDot ".")),
    ("AtToken", UnexpectedSyntax (ParserAtToken TDot ".") "expression"),
    ("BeforeToken", UnexpectedSyntaxIn (ParserBeforeToken TDot "." (Just "module")) "module"),
    ("BeforeBoundary", UnexpectedSyntaxAfter (ParserBeforeBoundary "end of block") "binding"),
    ("ImplicitBoundary", ExpectedSyntax "expression" ParserImplicitBoundary),
    ("ExplicitTypeApplicationArgument", UnsupportedSyntax ExplicitTypeApplicationArgument),
    ("FractionalLiteralPattern", UnsupportedSyntax FractionalLiteralPattern),
    ("ClassMethodBody", UnsupportedSyntax (ClassMethodBody "show")),
    ("DeclarationHeaderArguments", UnsupportedSyntax (DeclarationHeaderArguments ModuleDeclaration)),
    ("AbstractionSyntax", UnsupportedSyntax (AbstractionSyntax "trait")),
    ("InvalidFractionalLiteral", InvalidFractionalLiteral "1.0"),
    ("NonAssociativeOperatorChain", NonAssociativeOperatorChain "=="),
    ("ExpressionOperatorUse", UndeclaredOperator "+" OperatorUseInExpression),
    ("BindingOperatorUse", UndeclaredOperator "+" OperatorUseInBinding),
    ("SignatureOperatorUse", UndeclaredOperator "+" OperatorUseInSignature),
    ("ConsLikeListPatternHeadCount", PatternFailure ConsLikeListPatternHeadCount)
  ]
    <> [(expected, DeclarationFailure failure) | (expected, failure) <- declarationFailureInventory]
    <> [ ("TokenStreamParseFailure", InternalParserFailure TokenStreamParseFailure),
         ("ExpressionOperatorUse", InternalParserFailure (ExpectedOperatorToken OperatorUseInExpression)),
         ("BindingOperatorUse", InternalParserFailure (ExpectedOperatorToken OperatorUseInBinding)),
         ("SignatureOperatorUse", InternalParserFailure (ExpectedOperatorToken OperatorUseInSignature)),
         ("ExpectedSignatureSeparator", InternalParserFailure ExpectedSignatureSeparator),
         ("ExpectedBindingEquals", InternalParserFailure ExpectedBindingEquals)
       ]

declarationFailureInventory :: [(Text.Text, ParserDeclarationFailure)]
declarationFailureInventory =
  [ ("BuiltinOperatorCannotBeRedeclared", BuiltinOperatorCannotBeRedeclared "+"),
    ("BuiltinOperatorCannotBeBound", BuiltinOperatorCannotBeBound "+"),
    ("BuiltinOperatorCannotBeSigned", BuiltinOperatorCannotBeSigned "+"),
    ("ReservedOperatorSymbol", ReservedOperatorSymbol "="),
    ("DuplicateOperatorDeclaration", DuplicateOperatorDeclaration "+"),
    ("InvalidOperatorSymbol", InvalidOperatorSymbol "abc"),
    ("OperatorTierOutOfRange", OperatorTierOutOfRange),
    ("OperatorPrecedenceOutOfRange", OperatorPrecedenceOutOfRange),
    ("BindingName", ReservedLiteralName BindingName "True"),
    ("ImportAlias", ReservedLiteralName ImportAlias "False"),
    ("ModuleDeclaration", DeclarationOutsideAllowedScope ModuleDeclaration),
    ("ImportDeclaration", DeclarationOutsideAllowedScope ImportDeclaration),
    ("DataDeclaration", DeclarationOutsideAllowedScope DataDeclaration),
    ("OperatorDeclaration", DeclarationOutsideAllowedScope OperatorDeclaration),
    ("OperatorBinding", DeclarationOutsideAllowedScope OperatorBinding),
    ("OperatorSignature", DeclarationOutsideAllowedScope OperatorSignature),
    ("ClassDeclaration", DeclarationOutsideAllowedScope ClassDeclaration),
    ("ImplDeclaration", DeclarationOutsideAllowedScope ImplDeclaration),
    ("ImportAliasCombinedWithSymbolList", ImportAliasCombinedWithSymbolList),
    ("ImplRequiresConcreteTarget", ImplRequiresConcreteTarget),
    ("DataTypeParameter", DuplicateName DataTypeParameter "a" DataDeclaration),
    ("DataConstructorName", DuplicateName DataConstructorName "Thing" DataDeclaration),
    ("ClassMethodName", DuplicateName ClassMethodName "show" ClassDeclaration),
    ("ImplMethodName", DuplicateName ImplMethodName "show" ImplDeclaration),
    ("ImportSymbolList", DuplicateListItem ImportSymbolList "'x'"),
    ("ModuleExportList", DuplicateListItem ModuleExportList "'x'"),
    ("ConstructorExportList", DuplicateListItem ConstructorExportList "'X'"),
    ("ExpectedOrdinaryImplMethodBinding", ExpectedOrdinaryImplMethodBinding "show"),
    ("ClassRequiresExplicitParameterList", ClassRequiresExplicitParameterList),
    ("ClassRequiresLowercaseParameter", ClassRequiresLowercaseParameter),
    ("DuplicateClassParameter", DuplicateClassParameter "a"),
    ("ClassSupportsExactlyOneParameter", ClassSupportsExactlyOneParameter),
    ("ClassParameterMustBeLowercase", ClassParameterMustBeLowercase),
    ("UndeclaredConstructorTypeParameter", UndeclaredConstructorTypeParameter "b" "Thing"),
    ("ConstructorArgumentDelimiterMismatch", ConstructorArgumentDelimiterMismatch ","),
    ("ConstructorExportGroupRequiresAll", ConstructorExportGroupRequiresAll),
    ("ModuleMustBeFirstTopLevelForm", ModuleMustBeFirstTopLevelForm)
  ]

testSourceResultPhases :: IO ()
testSourceResultPhases = do
  path <- normalizedPath "fixtures/parser/phases.jz"
  let cases =
        [ ("success", "CanonicalSourceSuccess", "value = 42."),
          ("lexical failure", "CanonicalSourceLexicalFailure", "`"),
          ("parser failure", "CanonicalSourceParserFailure", "if")
        ]
  mapM_
    ( \(label, expectedConstructor, source) ->
        assertContains
          label
          expectedConstructor
          (renderCanonicalSourceResult (canonicalizeSourceResult path (detailedSourceResult source)))
    )
    cases

testCorpusDeterminism :: IO ()
testCorpusDeterminism = do
  assertEqual "fixed corpus size" 333 (length parserFixtureCorpus)
  first <- mapM canonicalFixture parserFixtureCorpus
  second <- mapM canonicalFixture parserFixtureCorpus
  assertEqual "manifest-order deterministic rendering" first second
  assertEqual
    "corpus contains lexical failures"
    True
    (any (Text.isInfixOf "CanonicalSourceLexicalFailure") first)
  assertEqual
    "corpus contains parser failures"
    True
    (any (Text.isInfixOf "CanonicalSourceParserFailure") first)
  assertEqual
    "corpus contains successes"
    True
    (any (Text.isInfixOf "CanonicalSourceSuccess") first)
  where
    canonicalFixture fixture = do
      path <- normalizedPath (parserFixturePath fixture)
      pure
        ( renderCanonicalSourceResult
            (canonicalizeSourceResult path (detailedSourceResult (parserFixtureSource fixture)))
        )

testCanonicalizesProgram :: IO ()
testCanonicalizesProgram = do
  sourcePath <- normalizedPath "fixtures/parser/basic.jz"
  tokens <-
    case tokenizeDetailed "value = 42." of
      Left failure -> failTest ("expected lexical success, got " <> showText failure)
      Right values -> pure values
  assertContains
    "surface program"
    "IntegerLiteral(\"42\")"
    (renderCanonicalParserResult (canonicalizeParserResult sourcePath (parseSurfaceProgramTokensDetailed tokens)))

assertRenderedSourceContains :: Text.Text -> Text.Text -> Text.Text -> IO ()
assertRenderedSourceContains label source expected = do
  path <- normalizedPath "fixtures/parser/numeric.jz"
  case detailedSourceResult source of
    Right parserResult ->
      assertContains
        label
        expected
        (renderCanonicalParserResult (canonicalizeParserResult path parserResult))
    Left failure -> failTest (label <> ": unexpected lexical failure " <> showText failure)

detailedSourceResult :: Text.Text -> Either LexicalFailure (Either ParserFailure SurfaceExpr)
detailedSourceResult source =
  case tokenizeDetailed source of
    Left failure -> Left failure
    Right tokens -> Right (parseSurfaceProgramTokensDetailed tokens)

normalizedPath :: FilePath -> IO CanonicalSourcePath
normalizedPath sourcePath =
  case normalizeCanonicalSourcePath sourcePath of
    Left message -> failTest message
    Right path -> pure path

resolverConfig :: ModuleResolutionConfig
resolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}

span1 :: SourceSpan
span1 = SourceSpan 1 1

span2 :: SourceSpan
span2 = SourceSpan 2 3

seInt :: Integer -> SurfaceExpr
seInt = SELit . SLInt

seText :: Text.Text -> SurfaceExpr
seText = SELit . SLText

numericConstructorName :: SurfaceNumericType -> Text.Text
numericConstructorName numericType =
  Text.replace "SurfaceNumeric" "" (showText numericType) <> "Type"

signatureTokenConstructorName :: SurfaceSignatureToken -> Text.Text
signatureTokenConstructorName token =
  case token of
    SurfaceSignatureNameToken {} -> "SignatureNameToken"
    SurfaceSignatureIntToken {} -> "SignatureIntegerToken"
    SurfaceSignatureArrowToken -> "SignatureArrowToken"
    SurfaceSignatureAtToken -> "SignatureAtToken"
    SurfaceSignatureColonToken -> "SignatureColonToken"
    SurfaceSignatureLParenToken -> "SignatureLeftParenToken"
    SurfaceSignatureRParenToken -> "SignatureRightParenToken"
    SurfaceSignatureLBraceToken -> "SignatureLeftBraceToken"
    SurfaceSignatureRBraceToken -> "SignatureRightBraceToken"
    SurfaceSignatureLBracketToken -> "SignatureLeftBracketToken"
    SurfaceSignatureRBracketToken -> "SignatureRightBracketToken"
    SurfaceSignatureCommaToken -> "SignatureCommaToken"
    SurfaceSignatureOperatorToken {} -> "SignatureOperatorToken"
    SurfaceSignatureOtherToken {} -> "SignatureOtherToken"

showText :: (Show value) => value -> Text.Text
showText = Text.pack . show
