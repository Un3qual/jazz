{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as Text
import Jazz.Compiler.AST
import Jazz.Compiler.Bootstrap.CanonicalCoreComparison
  ( canonicalCoreExprRuntimeValue,
    canonicalCoreModuleResultRuntimeValue,
    canonicalCoreModuleRuntimeValue,
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import Jazz.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runModuleGraph,
    runRuntimeErrors,
  )
import Jazz.Compiler.FractionalLiteral (mkFractionalLiteralSource)
import Jazz.Compiler.ModuleExports
  ( LocatedModuleExportName (..),
    ModuleExportSelector (..),
    ModuleTypeConstructorSelector (..),
  )
import Jazz.Compiler.ModuleGraph
  ( CoreModule (..),
    DeclaredModuleExports (..),
    ResolvedImport (..),
  )
import Jazz.Compiler.ModuleResolver (ModuleResolutionConfig (..))
import Jazz.Compiler.Name
  ( GeneratedNameKind (..),
    NameNamespace (..),
    generatedName,
    mkIdentifier,
    qualifiedName,
    resolvedAmbientName,
    sourceName,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceExpr (SEBlock),
    SurfaceStatement (SSModule),
  )
import Jazz.Compiler.Parser.Lower
  ( lowerSurfaceModuleDetailed,
  )
import Jazz.Compiler.Runtime (renderRuntimeValue)
import Jazz.Compiler.WarningConfig (defaultWarningSettings)
import Jazz.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
    runTestSuite,
  )
import Jazz.TestSource (readCheckedInJazzProjectModuleSource)

main :: IO ()
main = runTestSuite "CanonicalCoreComparison" tests

tests :: [NamedTest]
tests =
  [ ("constructs hosted core values through the real module graph", testJazzSchemaRendering),
    ("canonicalizes every active core constructor", testCoreInventory),
    ("preserves arbitrary integers and exact fractional source parts", testNumericFidelity),
    ("rejects names introduced after lowering", testNameBoundary),
    ("canonicalizes module metadata and qualified spans", testModuleInventory),
    ("preserves structured module-lowering failures", testModuleFailureBoundary)
  ]

testJazzSchemaRendering :: IO ()
testJazzSchemaRendering = do
  expected <- expectRight "stage-0 core adapter" (canonicalCoreExprRuntimeValue simpleCoreExpression)
  result <-
    runModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      lookupSource
  assertEqual "Jazz schema compile errors" [] (runCompileErrors result)
  assertEqual "Jazz schema runtime errors" [] (runRuntimeErrors result)
  assertEqual "Jazz schema output" (Just (renderRuntimeValue expected)) (runOutput result)
  where
    lookupSource sourcePath =
      case sourcePath of
        "src/App/Main.jz" -> pure (Just jazzSchemaFixture)
        _ -> readCheckedInJazzProjectModuleSource sourcePath

testCoreInventory :: IO ()
testCoreInventory = do
  renderedValues <- mapM (fmap renderRuntimeValue . expectRight "core inventory" . canonicalCoreExprRuntimeValue) coreInventory
  let rendered = Text.intercalate "\n" renderedValues
  mapM_ (\constructorName -> assertContains constructorName constructorName rendered) expectedConstructors

testNumericFidelity :: IO ()
testNumericFidelity = do
  value <-
    expectRight
      "numeric core adapter"
      ( canonicalCoreExprRuntimeValue
          ( ETuple
              [ ELit (LInt 123456789012345678901234567890),
                ELit (LFloat 1.05 (mkFractionalLiteralSource 1 50 3) (Just NumericFloat32))
              ]
          )
      )
  let rendered = renderRuntimeValue value
  assertContains "arbitrary integer" "123456789012345678901234567890" rendered
  assertContains "exact fractional source" "CoreFractionalLiteral(\"1\", \"050\", Just(CoreFloat32Type))" rendered

testNameBoundary :: IO ()
testNameBoundary =
  assertTextLeftContains
    "resolved name rejection"
    "post-lowering name"
    ( canonicalCoreExprRuntimeValue
        (EVar (resolvedAmbientName ValueNamespace (mkIdentifier "value")))
    )

testModuleInventory :: IO ()
testModuleInventory = do
  value <- expectRight "core module adapter" (canonicalCoreModuleRuntimeValue moduleInventory)
  let rendered = renderRuntimeValue value
  assertContains "declared path" "[\"App\", \"Main\"]" rendered
  assertContains "qualified span" "CoreSpan(Just(CanonicalSourcePath(\"src/App/Main.jz\")), 1, 1)" rendered
  assertContains
    "selected export"
    "CoreSelectedConstructors(NonEmpty(CoreLocatedExportName(\"Some\", CoreSpan(Just(CanonicalSourcePath(\"src/App/Main.jz\")), 1, 1)), [CoreLocatedExportName(\"None\", CoreSpan(Just(CanonicalSourcePath(\"src/App/Main.jz\")), 1, 1))]))"
    rendered
  assertContains "import metadata" "CoreResolvedImport" rendered

testModuleFailureBoundary :: IO ()
testModuleFailureBoundary = do
  multipleDeclarations <-
    expectRight
      "multiple module declarations"
      ( canonicalCoreModuleResultRuntimeValue
          ( lowerSurfaceModuleDetailed
              "src/App/Main.jz"
              ["App", "Main"]
              ( SEBlock
                  [ SSModule span1 ["App", "First"] Nothing,
                    SSModule span2 ["App", "Second"] Nothing
                  ]
              )
          )
      )
  assertEqual
    "multiple module declaration value"
    "CoreModuleLoweringFailed(CoreMultipleModuleDeclarationsFailure(CanonicalSourcePath(\"src/App/Main.jz\"), [CoreModuleDeclaration(CoreSpan(Just(CanonicalSourcePath(\"src/App/Main.jz\")), 1, 1), [\"App\", \"First\"]), CoreModuleDeclaration(CoreSpan(Just(CanonicalSourcePath(\"src/App/Main.jz\")), 2, 3), [\"App\", \"Second\"])]))"
    (renderRuntimeValue multipleDeclarations)

  pathMismatch <-
    expectRight
      "module path mismatch"
    ( canonicalCoreModuleResultRuntimeValue
        ( lowerSurfaceModuleDetailed
            "src/App/Main.jz"
            ["App", "Main"]
            (SEBlock [SSModule span2 ["Wrong", "Path"] Nothing])
        )
    )
  assertEqual
    "module path mismatch value"
    "CoreModuleLoweringFailed(CoreModulePathMismatchFailure(CanonicalSourcePath(\"src/App/Main.jz\"), [\"App\", \"Main\"], CoreModuleDeclaration(CoreSpan(Just(CanonicalSourcePath(\"src/App/Main.jz\")), 2, 3), [\"Wrong\", \"Path\"])))"
    (renderRuntimeValue pathMismatch)

  successfulModule <-
    expectRight
      "successful module result"
      (canonicalCoreModuleResultRuntimeValue (Right moduleInventory))
  assertContains "successful module result" "CoreModuleLowered" (renderRuntimeValue successfulModule)

assertTextLeftContains :: Show value => Text.Text -> Text.Text -> Either Text.Text value -> IO ()
assertTextLeftContains label needle value =
  case value of
    Left err -> assertContains label needle err
    Right ok -> failTest (label <> ": expected Left, got Right " <> Text.pack (show ok))

simpleCoreExpression :: Expr
simpleCoreExpression =
  EApply
    (EVar (sourceName (mkIdentifier "f")))
    (ELit (LInt 42))

coreInventory :: [Expr]
coreInventory =
  [ ELit (LInt 1),
    ELit (LFloat 1.5 (mkFractionalLiteralSource 1 5 1) Nothing),
    ELit (LBool True),
    ELit (LChar 'x'),
    ELit (LText "Jazz"),
    EVar (sourceName (mkIdentifier "value")),
    EVar (qualifiedName (mkIdentifier "Alias") (mkIdentifier "member")),
    EVar (generatedName (LambdaPatternArgument 2)),
    EVar (generatedName (OperatorBinding "$operator:2B")),
    ELambda (sourceName (mkIdentifier "argument")) (EVar (sourceName (mkIdentifier "argument"))),
    EOperatorValue "+",
    EList [ELit (LInt 1)],
    ETuple [],
    EApply (EVar (sourceName (mkIdentifier "f"))) (ELit (LInt 1)),
    ETypeApplication (EVar (sourceName (mkIdentifier "id"))) span1 signatureInventory,
    EIf (ELit (LBool True)) (ELit (LInt 1)) (ELit (LInt 0)),
    EPatternCase (EVar (sourceName (mkIdentifier "value"))) [CaseArm patternInventory (Just (ELit (LBool True))) (ELit (LInt 1))],
    EBinary "+" (ELit (LInt 1)) (ELit (LInt 2)),
    ESectionLeft (ELit (LInt 1)) "+",
    ESectionRight "+" (ELit (LInt 2)),
    EBlock statementInventory
  ]

patternInventory :: Pattern
patternInventory =
  POr
    [ PWildcard,
      PVariable (sourceName (mkIdentifier "item")),
      PLiteral (LText "text"),
      PConstructor (sourceName (mkIdentifier "Some")) [PWildcard],
      PList [PWildcard],
      PConsList PWildcard (PVariable (sourceName (mkIdentifier "rest"))),
      PTuple [PWildcard, PWildcard],
      PAs (sourceName (mkIdentifier "whole")) PWildcard
    ]

statementInventory :: [Statement]
statementInventory =
  [ SLet (generatedName (OperatorBinding "$operator:2B")) span1 (ELit (LInt 1)),
    SSignature (sourceName (mkIdentifier "value")) span1 (SignatureType signatureInventory),
    SSignature
      (sourceName (mkIdentifier "constrained"))
      span1
      (ConstrainedSignature [SignatureConstraint (sourceName (mkIdentifier "Eq")) [TypeVariable (sourceName (mkIdentifier "a"))]] (TypeVariable (sourceName (mkIdentifier "a")))),
    SSignature (sourceName (mkIdentifier "unsupported")) span1 (UnsupportedSignature signatureTokenInventory),
    SData
      span1
      (sourceName (mkIdentifier "Box"))
      [sourceName (mkIdentifier "a")]
      [ DataConstructor
          (sourceName (mkIdentifier "Box"))
          [ TypeVariable (sourceName (mkIdentifier "a")),
            TypeList TypeText
          ]
      ],
    SClass
      span1
      (sourceName (mkIdentifier "Eq"))
      [sourceName (mkIdentifier "a")]
      [ClassMethodSignature (sourceName (mkIdentifier "equals")) span2 (SignatureType signatureInventory)],
    SImpl
      span1
      (sourceName (mkIdentifier "Eq"))
      [TypeInt]
      [ImplMethod (sourceName (mkIdentifier "equals")) span2 (ELit (LBool True))],
    SModule span1 ["App", "Main"],
    SImport span2 ["Lib", "Value"] (Just "Value") (Just ["item"]),
    SExpr span2 (ELit (LInt 1))
  ]

signatureInventory :: SignatureType
signatureInventory =
  TypeTuple
    [ TypeInt,
      TypeFloat,
      TypeNumeric NumericInt8,
      TypeNumeric NumericInt16,
      TypeNumeric NumericInt32,
      TypeNumeric NumericInt64,
      TypeNumeric NumericUInt8,
      TypeNumeric NumericUInt16,
      TypeNumeric NumericUInt32,
      TypeNumeric NumericUInt64,
      TypeNumeric NumericFloat16,
      TypeNumeric NumericFloat32,
      TypeNumeric NumericFloat64,
      TypeBool,
      TypeChar,
      TypeText,
      TypeVariable (sourceName (mkIdentifier "a")),
      TypeName (sourceName (mkIdentifier "Maybe")),
      TypeApplication (qualifiedName (mkIdentifier "Types") (mkIdentifier "Result")) [TypeText],
      TypeList TypeInt,
      TypeFunction TypeInt TypeBool
    ]

signatureTokenInventory :: [SignatureToken]
signatureTokenInventory =
  [ SignatureNameToken (sourceName (mkIdentifier "a")),
    SignatureIntToken 1,
    SignatureArrowToken,
    SignatureAtToken,
    SignatureColonToken,
    SignatureLParenToken,
    SignatureRParenToken,
    SignatureLBraceToken,
    SignatureRBraceToken,
    SignatureLBracketToken,
    SignatureRBracketToken,
    SignatureCommaToken,
    SignatureOperatorToken "+",
    SignatureOtherToken "?"
  ]

moduleInventory :: CoreModule
moduleInventory =
  CoreModule
    { coreModuleDeclaredPath = Just ["App", "Main"],
      coreModuleDeclaredExports =
        Just
          ( DeclaredModuleExports
              qualifiedSpan1
              [ ModuleExportSelector (Just ValueNamespace) "value",
                ModuleTypeExportSelector "Box" qualifiedSpan1 AbstractType,
                ModuleTypeExportSelector "Choice" qualifiedSpan1 (AllTypeConstructors qualifiedSpan1),
                ModuleTypeExportSelector
                  "Maybe"
                  qualifiedSpan1
                  (SelectedTypeConstructors (LocatedModuleExportName "Some" qualifiedSpan1 :| [LocatedModuleExportName "None" qualifiedSpan1]))
              ]
          ),
      coreModuleImports =
        [ ResolvedImport
            { resolvedImportSpan = qualifiedSpan2,
              resolvedImportPath = ["Lib", "Value"],
              resolvedImportAlias = Just "Value",
              resolvedImportSymbols = Just ["item"]
            }
        ],
      coreModuleExpr = EBlock [SExpr qualifiedSpan2 simpleCoreExpression]
    }

expectedConstructors :: [Text.Text]
expectedConstructors =
  [ "CoreIntegerLiteral",
    "CoreFractionalLiteral",
    "CoreBooleanLiteral",
    "CoreCharacterLiteral",
    "CoreTextLiteral",
    "CoreLiteralExpression",
    "CoreVariableExpression",
    "CoreSourceName",
    "CoreQualifiedName",
    "CoreGeneratedName",
    "CoreLambdaPatternArgument",
    "CoreOperatorBinding",
    "CoreSpan",
    "CoreLambdaExpression",
    "CoreOperatorValueExpression",
    "CoreListExpression",
    "CoreTupleExpression",
    "CoreApplyExpression",
    "CoreTypeApplicationExpression",
    "CoreIfExpression",
    "CorePatternCaseExpression",
    "CoreBinaryExpression",
    "CoreLeftSectionExpression",
    "CoreRightSectionExpression",
    "CoreBlockExpression",
    "CoreWildcardPattern",
    "CoreVariablePattern",
    "CoreLiteralPattern",
    "CoreConstructorPattern",
    "CoreListPattern",
    "CoreConsListPattern",
    "CoreTuplePattern",
    "CoreAsPattern",
    "CoreOrPattern",
    "CoreCaseArm",
    "CoreLetStatement",
    "CoreSignatureStatement",
    "CoreDataStatement",
    "CoreClassStatement",
    "CoreImplStatement",
    "CoreModuleStatement",
    "CoreImportStatement",
    "CoreExpressionStatement",
    "CoreDataConstructor",
    "CoreClassMethodSignature",
    "CoreImplMethod",
    "CoreTypeSignature",
    "CoreConstrainedSignature",
    "CoreUnsupportedSignature",
    "CoreSignatureConstraint",
    "CoreIntType",
    "CoreFloatType",
    "CoreNumericType",
    "CoreInt8Type",
    "CoreInt16Type",
    "CoreInt32Type",
    "CoreInt64Type",
    "CoreUInt8Type",
    "CoreUInt16Type",
    "CoreUInt32Type",
    "CoreUInt64Type",
    "CoreFloat16Type",
    "CoreFloat32Type",
    "CoreFloat64Type",
    "CoreBoolType",
    "CoreCharType",
    "CoreTextType",
    "CoreTypeVariable",
    "CoreNamedType",
    "CoreAppliedType",
    "CoreListType",
    "CoreTupleType",
    "CoreFunctionType",
    "CoreSignatureNameToken",
    "CoreSignatureIntegerToken",
    "CoreSignatureArrowToken",
    "CoreSignatureAtToken",
    "CoreSignatureColonToken",
    "CoreSignatureLeftParenToken",
    "CoreSignatureRightParenToken",
    "CoreSignatureLeftBraceToken",
    "CoreSignatureRightBraceToken",
    "CoreSignatureLeftBracketToken",
    "CoreSignatureRightBracketToken",
    "CoreSignatureCommaToken",
    "CoreSignatureOperatorToken",
    "CoreSignatureOtherToken"
  ]

jazzSchemaFixture :: Text.Text
jazzSchemaFixture =
  """
  module App::Main {
    import CoreTypes.
    CoreApplyExpression
      (CoreVariableExpression (CoreSourceName "f"))
      (CoreLiteralExpression (CoreIntegerLiteral "42")).
  }

  """

resolverConfig :: ModuleResolutionConfig
resolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}

span1 :: SourceSpan
span1 = SourceSpan 1 1

span2 :: SourceSpan
span2 = SourceSpan 2 3

qualifiedSpan1 :: SourceSpan
qualifiedSpan1 = SourceSpanIn "src/App/Main.jz" 1 1

qualifiedSpan2 :: SourceSpan
qualifiedSpan2 = SourceSpanIn "src/App/Main.jz" 2 3

expectRight :: Text.Text -> Either Text.Text value -> IO value
expectRight label result =
  case result of
    Left message -> failTest (label <> ": " <> message)
    Right value -> pure value
