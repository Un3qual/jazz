{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List (sort)
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
    ParserFixtureExpectation (ParserAccepted, ParserRejected),
    ParserFixtureFamily (..),
    ParserFixtureManifestViolation (..),
    lookupParserFixtureFamily,
    parserFixtureCorpus,
    parserFixtureFamilyNames,
    validateParserFixtureManifest,
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
    ("validates fixture-family manifests deterministically", testFixtureFamilyValidation),
    ("locks the expression-foundation fixture family", testExpressionFoundationFamily),
    ("locks the types-declarations-modules fixture family", testTypesDeclarationsModulesFamily),
    ("locks the control-flow-patterns fixture family", testControlFlowPatternsFamily),
    ("locks the final parser fixture families", testFinalParserFamilies),
    ("assigns every fixture to exactly one family", testCompleteFixtureAssignment),
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
      SSLet
        "identity"
        span1
        (SELambda (SurfaceLambdaPattern (SPVariable "item") :| []) (SEVar "item")),
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
            [SurfaceTypeVariable "a", SurfaceTypeList SurfaceTypeText]
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
    "item = 1.0010."
    "FractionalLiteral(\"1\", \"0010\", Nothing)"
  mapM_
    ( \(suffix, expectedType) ->
        assertRenderedSourceContains
          ("fractional suffix " <> suffix)
          ("item = 1.50" <> suffix <> ".")
          ("FractionalLiteral(\"1\", \"50\", Just(" <> expectedType <> "))")
    )
    [("f16", "Float16Type"), ("f32", "Float32Type"), ("f64", "Float64Type")]
  let overflowSource = "item = " <> Text.replicate 400 "9" <> ".0."
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
        [ ("success", "CanonicalSourceSuccess", "item = 42."),
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

testFixtureFamilyValidation :: IO ()
testFixtureFamilyValidation =
  case parserFixtureCorpus of
    [] -> failTest "fixture corpus must not be empty"
    fixture : _ ->
      let fixtureName = parserFixtureName fixture
       in assertEqual
            "manifest violations preserve validation order"
            [ DuplicateParserFixtureName fixtureName,
              DuplicateParserFixtureFamilyMember ExpressionFoundation fixtureName,
              MissingParserFixtureFamilyMember ExpressionFoundation "missing-fixture"
            ]
            ( validateParserFixtureManifest
                [fixture, fixture]
                [(ExpressionFoundation, [fixtureName, fixtureName, "missing-fixture"])]
            )

testCompleteFixtureAssignment :: IO ()
testCompleteFixtureAssignment =
  case parserFixtureCorpus of
    firstFixture : secondFixture : _ -> do
      let firstName = parserFixtureName firstFixture
          secondName = parserFixtureName secondFixture
      assertEqual
        "cross-family and unassigned violations preserve order"
        [ DuplicateParserFixtureFamilyAssignment firstName,
          UnassignedParserFixture secondName
        ]
        ( validateParserFixtureManifest
            [firstFixture, secondFixture]
            [ (ExpressionFoundation, [firstName]),
              (Operators, [firstName])
            ]
        )
      let families = [ExpressionFoundation, TypesDeclarationsModules, ControlFlowPatterns, Operators, MixedOperatorControlFlow, CorpusClosure]
          assignedNames = concatMap parserFixtureFamilyNames families
          corpusNames = map parserFixtureName parserFixtureCorpus
      assertEqual "complete corpus size" 365 (length corpusNames)
      assertEqual "complete assignment count" 365 (length assignedNames)
      assertEqual "complete assignment membership" (sort corpusNames) (sort assignedNames)
    _ -> failTest "fixture corpus must contain at least two fixtures"

testFinalParserFamilies :: IO ()
testFinalParserFamilies = do
  assertFamilySize Operators 55
  assertFamilySize MixedOperatorControlFlow 26
  assertFamilySize CorpusClosure 56
  assertEqual
    "operator family boundaries"
    ("lexer-operator-runs", "parser-corpus-0307")
    (familyBoundaries Operators)
  assertEqual
    "mixed operator family boundaries"
    ("parser-corpus-0021", "parser-corpus-0305")
    (familyBoundaries MixedOperatorControlFlow)
  assertEqual
    "corpus closure boundaries"
    ("lexer-arbitrary-precision-integer", "parser-corpus-0312")
    (familyBoundaries CorpusClosure)
  where
    assertFamilySize family expectedSize =
      case lookupParserFixtureFamily family of
        Left violations -> failTest ("unexpected fixture manifest violations: " <> showText violations)
        Right fixtures -> assertEqual (showText family <> " family size") expectedSize (length fixtures)
    familyBoundaries family =
      case parserFixtureFamilyNames family of
        [] -> ("", "")
        firstName : remainingNames ->
          (firstName, foldl (\_ name -> name) firstName remainingNames)

testExpressionFoundationFamily :: IO ()
testExpressionFoundationFamily = do
  assertEqual
    "declared expression family order"
    expressionFoundationFixtureNames
    (parserFixtureFamilyNames ExpressionFoundation)
  fixtures <-
    case lookupParserFixtureFamily ExpressionFoundation of
      Left violations -> failTest ("unexpected fixture manifest violations: " <> showText violations)
      Right values -> pure values
  assertEqual "expression family size" 52 (length fixtures)
  assertEqual
    "resolved expression family order"
    expressionFoundationFixtureNames
    (map parserFixtureName fixtures)
  rendered <- mapM canonicalFixture fixtures
  assertEqual
    "expression family contains lexical failures"
    True
    (any (Text.isInfixOf "CanonicalSourceLexicalFailure") rendered)
  assertEqual
    "expression family contains parser failures"
    True
    (any (Text.isInfixOf "CanonicalSourceParserFailure") rendered)
  assertEqual
    "expression family contains successes"
    True
    (any (Text.isInfixOf "CanonicalSourceSuccess") rendered)
  where
    canonicalFixture fixture = do
      path <- normalizedPath (parserFixturePath fixture)
      pure
        ( renderCanonicalSourceResult
            (canonicalizeSourceResult path (detailedSourceResult (parserFixtureSource fixture)))
        )

testTypesDeclarationsModulesFamily :: IO ()
testTypesDeclarationsModulesFamily = do
  assertEqual
    "declared types/declarations/modules family order"
    typesDeclarationsModulesFixtureNames
    (parserFixtureFamilyNames TypesDeclarationsModules)
  fixtures <-
    case lookupParserFixtureFamily TypesDeclarationsModules of
      Left violations -> failTest ("unexpected fixture manifest violations: " <> showText violations)
      Right values -> pure values
  assertEqual "types/declarations/modules family size" 101 (length fixtures)
  assertEqual
    "resolved types/declarations/modules family order"
    typesDeclarationsModulesFixtureNames
    (map parserFixtureName fixtures)
  assertEqual
    "focused types/declarations/modules expectations"
    [ ("types-declarations-modules-unsupported-forall-signature", ParserAccepted),
      ("types-declarations-modules-foundational-impl-method", ParserAccepted),
      ("types-declarations-modules-applied-explicit-type-application", ParserAccepted)
    ]
    (map (\fixture -> (parserFixtureName fixture, parserFixtureExpectation fixture)) (drop 98 fixtures))
  rendered <- mapM canonicalFixture fixtures
  assertEqual
    "types/declarations/modules family excludes lexical failures"
    False
    (any (Text.isInfixOf "CanonicalSourceLexicalFailure") rendered)
  assertEqual
    "types/declarations/modules family contains parser failures"
    True
    (any (Text.isInfixOf "CanonicalSourceParserFailure") rendered)
  assertEqual
    "types/declarations/modules family contains successes"
    True
    (any (Text.isInfixOf "CanonicalSourceSuccess") rendered)
  where
    canonicalFixture fixture = do
      path <- normalizedPath (parserFixturePath fixture)
      pure
        ( renderCanonicalSourceResult
            (canonicalizeSourceResult path (detailedSourceResult (parserFixtureSource fixture)))
        )

testControlFlowPatternsFamily :: IO ()
testControlFlowPatternsFamily = do
  assertEqual
    "declared control-flow/patterns family order"
    controlFlowPatternsFixtureNames
    (parserFixtureFamilyNames ControlFlowPatterns)
  fixtures <-
    case lookupParserFixtureFamily ControlFlowPatterns of
      Left violations -> failTest ("unexpected fixture manifest violations: " <> showText violations)
      Right values -> pure values
  assertEqual "control-flow/patterns family size" 75 (length fixtures)
  assertEqual
    "resolved control-flow/patterns family order"
    controlFlowPatternsFixtureNames
    (map parserFixtureName fixtures)
  assertEqual
    "focused control-flow/pattern expectations"
    [ ("control-flow-patterns-guarded-or-pattern", ParserAccepted),
      ("control-flow-patterns-lambda-guard-rejected", ParserRejected),
      ("control-flow-patterns-recursive-block", ParserAccepted)
    ]
    (map (\fixture -> (parserFixtureName fixture, parserFixtureExpectation fixture)) (drop 72 fixtures))
  rendered <- mapM canonicalFixture fixtures
  assertEqual
    "control-flow/patterns family excludes lexical failures"
    False
    (any (Text.isInfixOf "CanonicalSourceLexicalFailure") rendered)
  assertEqual
    "control-flow/patterns family contains parser failures"
    True
    (any (Text.isInfixOf "CanonicalSourceParserFailure") rendered)
  assertEqual
    "control-flow/patterns family contains successes"
    True
    (any (Text.isInfixOf "CanonicalSourceSuccess") rendered)
  where
    canonicalFixture fixture = do
      path <- normalizedPath (parserFixturePath fixture)
      pure
        ( renderCanonicalSourceResult
            (canonicalizeSourceResult path (detailedSourceResult (parserFixtureSource fixture)))
        )

expressionFoundationFixtureNames :: [Text.Text]
expressionFoundationFixtureNames =
  [ "lexer-leading-zero-integer",
    "lexer-crlf-spans",
    "lexer-unicode-and-escape-values",
    "lexer-all-supported-escapes",
    "lexer-unexpected-character",
    "parser-corpus-0001",
    "parser-corpus-0024",
    "parser-corpus-0028",
    "parser-corpus-0032",
    "parser-corpus-0310",
    "parser-corpus-0036",
    "parser-corpus-0051",
    "parser-corpus-0182",
    "parser-corpus-0193",
    "parser-corpus-0194",
    "parser-corpus-0206",
    "parser-corpus-0214",
    "parser-corpus-0233",
    "parser-corpus-0234",
    "parser-corpus-0236",
    "parser-corpus-0237",
    "parser-corpus-0240",
    "parser-corpus-0241",
    "parser-corpus-0308",
    "parser-corpus-0309",
    "parser-corpus-0041",
    "expression-foundation-reserved-true-signature",
    "expression-foundation-reserved-false-signature",
    "expression-foundation-spaced-reserved-true-signature",
    "expression-foundation-spaced-reserved-false-signature",
    "expression-foundation-identifier-operator-tier",
    "expression-foundation-identifier-operator-precedence",
    "expression-foundation-nested-identifier-operator-tier",
    "expression-foundation-parenthesized-signature-statement-boundary",
    "expression-foundation-signature-syntax-statement-boundary",
    "expression-foundation-empty-program",
    "expression-foundation-empty-block",
    "expression-foundation-grouped-name",
    "expression-foundation-empty-list",
    "expression-foundation-list-literals",
    "expression-foundation-parenthesized-application",
    "expression-foundation-list-missing-close",
    "expression-foundation-list-trailing-comma",
    "expression-foundation-tuple-missing-close",
    "expression-foundation-tuple-trailing-comma",
    "expression-foundation-binding-missing-rhs",
    "expression-foundation-binding-missing-dot",
    "expression-foundation-expression-missing-dot",
    "expression-foundation-qualified-missing-member",
    "expression-foundation-qualified-whitespace",
    "expression-foundation-dot-without-expression",
    "expression-foundation-max-float64"
  ]

typesDeclarationsModulesFixtureNames :: [Text.Text]
typesDeclarationsModulesFixtureNames =
  [ "parser-corpus-0034",
    "parser-corpus-0038",
    "parser-corpus-0039",
    "parser-corpus-0047",
    "parser-corpus-0050",
    "parser-corpus-0074",
    "parser-corpus-0076",
    "parser-corpus-0077",
    "parser-corpus-0078",
    "parser-corpus-0131",
    "parser-corpus-0191",
    "parser-corpus-0192",
    "parser-corpus-0204",
    "parser-corpus-0205",
    "parser-corpus-0207",
    "parser-corpus-0208",
    "parser-corpus-0215",
    "parser-corpus-0216",
    "parser-corpus-0220",
    "parser-corpus-0221",
    "parser-corpus-0222",
    "parser-corpus-0210",
    "parser-corpus-0211",
    "parser-corpus-0212",
    "parser-corpus-0052",
    "parser-corpus-0053",
    "parser-corpus-0054",
    "parser-corpus-0055",
    "parser-corpus-0056",
    "parser-corpus-0057",
    "parser-corpus-0058",
    "parser-corpus-0059",
    "parser-corpus-0060",
    "parser-corpus-0061",
    "parser-corpus-0062",
    "parser-corpus-0064",
    "parser-corpus-0065",
    "parser-corpus-0066",
    "parser-corpus-0067",
    "parser-corpus-0068",
    "parser-corpus-0069",
    "parser-corpus-0070",
    "parser-corpus-0071",
    "parser-corpus-0072",
    "parser-corpus-0104",
    "parser-corpus-0105",
    "parser-corpus-0106",
    "parser-corpus-0107",
    "parser-corpus-0108",
    "parser-corpus-0109",
    "parser-corpus-0110",
    "parser-corpus-0111",
    "parser-corpus-0112",
    "parser-corpus-0113",
    "parser-corpus-0114",
    "parser-corpus-0115",
    "parser-corpus-0116",
    "parser-corpus-0117",
    "parser-corpus-0118",
    "parser-corpus-0119",
    "parser-corpus-0120",
    "parser-corpus-0121",
    "parser-corpus-0122",
    "parser-corpus-0123",
    "parser-corpus-0124",
    "parser-corpus-0125",
    "parser-corpus-0126",
    "parser-corpus-0127",
    "parser-corpus-0128",
    "parser-corpus-0129",
    "parser-corpus-0133",
    "parser-corpus-0134",
    "parser-corpus-0135",
    "parser-corpus-0136",
    "parser-corpus-0137",
    "parser-corpus-0138",
    "parser-corpus-0139",
    "parser-corpus-0140",
    "parser-corpus-0141",
    "parser-corpus-0142",
    "parser-corpus-0143",
    "parser-corpus-0144",
    "parser-corpus-0145",
    "parser-corpus-0146",
    "parser-corpus-0147",
    "parser-corpus-0148",
    "parser-corpus-0150",
    "parser-corpus-0151",
    "parser-corpus-0152",
    "parser-corpus-0153",
    "parser-corpus-0154",
    "parser-corpus-0155",
    "parser-corpus-0156",
    "parser-corpus-0157",
    "parser-corpus-0158",
    "parser-corpus-0159",
    "parser-corpus-0235",
    "parser-corpus-0306",
    "types-declarations-modules-unsupported-forall-signature",
    "types-declarations-modules-foundational-impl-method",
    "types-declarations-modules-applied-explicit-type-application"
  ]

controlFlowPatternsFixtureNames :: [Text.Text]
controlFlowPatternsFixtureNames =
  [ "parser-corpus-0042",
    "parser-corpus-0045",
    "parser-corpus-0046",
    "parser-corpus-0048",
    "parser-corpus-0049",
    "parser-corpus-0063",
    "parser-corpus-0086",
    "parser-corpus-0087",
    "parser-corpus-0088",
    "parser-corpus-0090",
    "parser-corpus-0091",
    "parser-corpus-0092",
    "parser-corpus-0093",
    "parser-corpus-0094",
    "parser-corpus-0095",
    "parser-corpus-0096",
    "parser-corpus-0097",
    "parser-corpus-0098",
    "parser-corpus-0100",
    "parser-corpus-0101",
    "parser-corpus-0102",
    "parser-corpus-0195",
    "parser-corpus-0197",
    "parser-corpus-0199",
    "parser-corpus-0200",
    "parser-corpus-0201",
    "parser-corpus-0245",
    "parser-corpus-0246",
    "parser-corpus-0247",
    "parser-corpus-0248",
    "parser-corpus-0249",
    "parser-corpus-0258",
    "parser-corpus-0259",
    "parser-corpus-0260",
    "parser-corpus-0261",
    "parser-corpus-0262",
    "parser-corpus-0263",
    "parser-corpus-0264",
    "parser-corpus-0265",
    "parser-corpus-0266",
    "parser-corpus-0267",
    "parser-corpus-0268",
    "parser-corpus-0269",
    "parser-corpus-0270",
    "parser-corpus-0272",
    "parser-corpus-0273",
    "parser-corpus-0274",
    "parser-corpus-0275",
    "parser-corpus-0276",
    "parser-corpus-0277",
    "parser-corpus-0279",
    "parser-corpus-0280",
    "parser-corpus-0282",
    "parser-corpus-0283",
    "parser-corpus-0284",
    "parser-corpus-0285",
    "parser-corpus-0286",
    "parser-corpus-0287",
    "parser-corpus-0288",
    "parser-corpus-0289",
    "parser-corpus-0290",
    "parser-corpus-0292",
    "parser-corpus-0293",
    "parser-corpus-0294",
    "parser-corpus-0295",
    "parser-corpus-0296",
    "parser-corpus-0297",
    "parser-corpus-0298",
    "parser-corpus-0301",
    "parser-corpus-0302",
    "parser-corpus-0303",
    "parser-corpus-0304",
    "control-flow-patterns-guarded-or-pattern",
    "control-flow-patterns-lambda-guard-rejected",
    "control-flow-patterns-recursive-block"
  ]

testCorpusDeterminism :: IO ()
testCorpusDeterminism = do
  assertEqual "fixed corpus size" 365 (length parserFixtureCorpus)
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
    case tokenizeDetailed "item = 42." of
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
