{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

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
  ( runCompileErrors,
    runModuleGraph,
    runOutput,
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
    CoreResolvedImport (..),
    DeclaredModuleExports (..),
  )
import Jazz.Compiler.ModuleResolver (ModuleResolutionConfig (..))
import Jazz.Compiler.Name
  ( GeneratedNameKind (..),
    NameNamespace (..),
    generatedName,
    mkIdentifier,
    qualifiedName,
    sourceName,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceExprForm (SEBlock),
    SurfaceStatement (SSModule),
  )
import Jazz.Compiler.Parser.Lower
  ( lowerSurfaceModuleDetailed,
  )
import Jazz.Compiler.Runtime (renderRuntimeValue)
import Jazz.Compiler.TypeRepresentation
  ( pattern ConstrainedSignature,
    pattern NumericFloat16,
    pattern NumericFloat32,
    pattern NumericFloat64,
    pattern NumericInt16,
    pattern NumericInt32,
    pattern NumericInt64,
    pattern NumericInt8,
    pattern NumericUInt16,
    pattern NumericUInt32,
    pattern NumericUInt64,
    pattern NumericUInt8,
    pattern SignatureArrowToken,
    pattern SignatureAtToken,
    pattern SignatureColonToken,
    pattern SignatureCommaToken,
    pattern SignatureConstraint,
    pattern SignatureIntToken,
    pattern SignatureLBraceToken,
    pattern SignatureLBracketToken,
    pattern SignatureLParenToken,
    pattern SignatureNameToken,
    pattern SignatureOperatorToken,
    pattern SignatureOtherToken,
    pattern SignatureRBraceToken,
    pattern SignatureRBracketToken,
    pattern SignatureRParenToken,
    pattern SignatureType,
    pattern TypeApplication,
    pattern TypeBool,
    pattern TypeChar,
    pattern TypeFloat,
    pattern TypeFunction,
    pattern TypeInt,
    pattern TypeList,
    pattern TypeName,
    pattern TypeNumeric,
    pattern TypeText,
    pattern TypeTuple,
    pattern TypeVariable,
    pattern UnsupportedSignature,
  )
import Jazz.Compiler.WarningConfig (defaultWarningSettings)
import Jazz.TestCore
  ( loweredApply,
    loweredAsPattern,
    loweredBinary,
    loweredBlock,
    loweredCaseArm,
    loweredClass,
    loweredClassMethodSignature,
    loweredConsListPattern,
    loweredConstructor,
    loweredConstructorPattern,
    loweredData,
    loweredExpression,
    loweredIf,
    loweredImpl,
    loweredImplMethod,
    loweredImport,
    loweredLambda,
    loweredLet,
    loweredList,
    loweredListPattern,
    loweredLiteral,
    loweredLiteralPattern,
    loweredModule,
    loweredOperatorValue,
    loweredOrPattern,
    loweredPatternCase,
    loweredSectionLeft,
    loweredSectionRight,
    loweredSignature,
    loweredTuple,
    loweredTuplePattern,
    loweredTypeApplication,
    loweredVariable,
    loweredVariablePattern,
    loweredWildcardPattern,
  )
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
          ( loweredTuple
              [ loweredLiteral (LInt 123456789012345678901234567890),
                loweredLiteral (LFloat 1.05 (mkFractionalLiteralSource 1 50 3) (Just NumericFloat32))
              ]
          )
      )
  let rendered = renderRuntimeValue value
  assertContains "arbitrary integer" "123456789012345678901234567890" rendered
  assertContains "exact fractional source" "CoreFractionalLiteral(\"1\", \"050\", Just(CoreFloat32Type))" rendered

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
              ( SurfaceExpr
                  span1
                  ( SEBlock
                      [ SSModule span1 ["App", "First"] Nothing,
                        SSModule span2 ["App", "Second"] Nothing
                      ]
                  )
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
              (SurfaceExpr span2 (SEBlock [SSModule span2 ["Wrong", "Path"] Nothing]))
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

simpleCoreExpression :: Expr 'Lowered
simpleCoreExpression =
  loweredApply
    (loweredVariable (sourceName (mkIdentifier "f")))
    (loweredLiteral (LInt 42))

coreInventory :: [Expr 'Lowered]
coreInventory =
  [ loweredLiteral (LInt 1),
    loweredLiteral (LFloat 1.5 (mkFractionalLiteralSource 1 5 1) Nothing),
    loweredLiteral (LBool True),
    loweredLiteral (LChar 'x'),
    loweredLiteral (LText "Jazz"),
    loweredVariable (sourceName (mkIdentifier "value")),
    loweredVariable (qualifiedName (mkIdentifier "Alias") (mkIdentifier "member")),
    loweredVariable (generatedName (LambdaPatternArgument 2)),
    loweredVariable (generatedName (OperatorBinding "$operator:2B")),
    loweredLambda (sourceName (mkIdentifier "argument")) (loweredVariable (sourceName (mkIdentifier "argument"))),
    loweredOperatorValue "+",
    loweredList [loweredLiteral (LInt 1)],
    loweredTuple [],
    loweredApply (loweredVariable (sourceName (mkIdentifier "f"))) (loweredLiteral (LInt 1)),
    loweredTypeApplication (loweredVariable (sourceName (mkIdentifier "id"))) span1 signatureInventory,
    loweredIf (loweredLiteral (LBool True)) (loweredLiteral (LInt 1)) (loweredLiteral (LInt 0)),
    loweredPatternCase (loweredVariable (sourceName (mkIdentifier "value"))) [loweredCaseArm patternInventory (Just (loweredLiteral (LBool True))) (loweredLiteral (LInt 1))],
    loweredBinary "+" (loweredLiteral (LInt 1)) (loweredLiteral (LInt 2)),
    loweredSectionLeft (loweredLiteral (LInt 1)) "+",
    loweredSectionRight "+" (loweredLiteral (LInt 2)),
    loweredBlock statementInventory
  ]

patternInventory :: Pattern 'Lowered
patternInventory =
  loweredOrPattern
    [ loweredWildcardPattern,
      loweredVariablePattern (sourceName (mkIdentifier "item")),
      loweredLiteralPattern (LText "text"),
      loweredConstructorPattern (sourceName (mkIdentifier "Some")) [loweredWildcardPattern],
      loweredListPattern [loweredWildcardPattern],
      loweredConsListPattern loweredWildcardPattern (loweredVariablePattern (sourceName (mkIdentifier "rest"))),
      loweredTuplePattern [loweredWildcardPattern, loweredWildcardPattern],
      loweredAsPattern (sourceName (mkIdentifier "whole")) loweredWildcardPattern
    ]

statementInventory :: [Statement 'Lowered]
statementInventory =
  [ loweredLet (generatedName (OperatorBinding "$operator:2B")) span1 (loweredLiteral (LInt 1)),
    loweredSignature (sourceName (mkIdentifier "value")) span1 (SignatureType signatureInventory),
    loweredSignature
      (sourceName (mkIdentifier "constrained"))
      span1
      (ConstrainedSignature [SignatureConstraint (sourceName (mkIdentifier "Eq")) [TypeVariable (sourceName (mkIdentifier "a"))]] (TypeVariable (sourceName (mkIdentifier "a")))),
    loweredSignature (sourceName (mkIdentifier "unsupported")) span1 (UnsupportedSignature signatureTokenInventory),
    loweredData
      span1
      (sourceName (mkIdentifier "Box"))
      [sourceName (mkIdentifier "a")]
      [ loweredConstructor
          (sourceName (mkIdentifier "Box"))
          [ TypeVariable (sourceName (mkIdentifier "a")),
            TypeList TypeText
          ]
      ],
    loweredClass
      span1
      (sourceName (mkIdentifier "Eq"))
      [sourceName (mkIdentifier "a")]
      [loweredClassMethodSignature (sourceName (mkIdentifier "equals")) span2 (SignatureType signatureInventory)],
    loweredImpl
      span1
      (sourceName (mkIdentifier "Eq"))
      [TypeInt]
      [loweredImplMethod (sourceName (mkIdentifier "equals")) span2 (loweredLiteral (LBool True))],
    loweredModule span1 ["App", "Main"],
    loweredImport span2 ["Lib", "Value"] (Just "Value") (Just ["item"]),
    loweredExpression span2 (loweredLiteral (LInt 1))
  ]

signatureInventory :: SignatureType 'Lowered
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

signatureTokenInventory :: [SignatureToken 'Lowered]
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

moduleInventory :: CoreModule 'Lowered
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
        [ CoreResolvedImport
            { coreResolvedImportSpan = qualifiedSpan2,
              coreResolvedImportPath = ["Lib", "Value"],
              coreResolvedImportAlias = Just "Value",
              coreResolvedImportSymbols = Just ["item"]
            }
        ],
      coreModuleExpr = loweredBlock [loweredExpression qualifiedSpan2 simpleCoreExpression]
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
