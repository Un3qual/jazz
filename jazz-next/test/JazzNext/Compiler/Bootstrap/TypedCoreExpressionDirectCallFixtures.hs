{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures
  ( Fixture (..),
    fixtureNames,
    fixtures,
    expectedUnitProgram,
    scalarFixtures,
    scalarExpectedPrograms,
    rejectedScalarFixtures,
    admittedOperators,
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
import JazzNext.Compiler.BuiltinCatalog (BuiltinResolutionMode (ResolveKernelOnly))
import JazzNext.Compiler.Diagnostics (SourceSpan (..))
import JazzNext.Compiler.FractionalLiteral (mkFractionalLiteralSource)
import JazzNext.Compiler.ModuleExports (exportInventory)
import JazzNext.Compiler.ModuleGraph
import JazzNext.Compiler.Name (Name (SourceName))
import JazzNext.Compiler.TypedCore
import JazzNext.Compiler.TypeInference (InferenceInputs (..))
import JazzNext.Compiler.TypeInference.Types (emptyScopeCapabilityFacts)
import JazzNext.Compiler.WarningConfig (defaultWarningSettings)

data Fixture = Fixture
  { fixtureName :: Text,
    fixtureInputs :: InferenceInputs,
    fixtureSourcePath :: TypedSourcePath,
    fixtureModule :: ResolvedModule
  }

fixtureNames :: [Text]
fixtureNames =
  [ "unit-entry",
    "bool-entry",
    "char-entry",
    "default-int-entry",
    "default-float-entry",
    "arithmetic-operators",
    "ordering-operators",
    "equality-operators",
    "text-value",
    "list-value",
    "non-unit-tuple",
    "data-value",
    "conditional",
    "pattern-case",
    "local-block-binding",
    "source-diagnostic",
    "invalid-portable-source-path",
    "resolved-import",
    "ambient-prelude-input"
  ]

fixtures :: [Fixture]
fixtures =
  [ Fixture "unit-entry" emptyInputs validSourcePath unitModule,
    boolFixture,
    charFixture,
    defaultIntFixture,
    defaultFloatFixture,
    arithmeticOperatorsFixture,
    orderingOperatorsFixture,
    equalityOperatorsFixture,
    textFixture,
    listFixture,
    nonUnitTupleFixture,
    dataFixture,
    conditionalFixture,
    patternCaseFixture,
    localBlockBindingFixture,
    Fixture "source-diagnostic" emptyInputs validSourcePath sourceDiagnosticModule,
    Fixture "invalid-portable-source-path" emptyInputs (TypedSourcePath "/private/host/Main.jz") unitModule,
    Fixture "resolved-import" emptyInputs validSourcePath moduleWithImport,
    Fixture "ambient-prelude-input" ambientPreludeInputs validSourcePath unitModule
  ]

expectedUnitProgram :: TypedProgram
expectedUnitProgram = TypedProgram Nothing [entryModule] modulePath

scalarFixtures :: [Fixture]
scalarFixtures = take 7 (drop 1 fixtures)

scalarExpectedPrograms :: [(Text, TypedProgram)]
scalarExpectedPrograms =
  [ ("bool-entry", expectedScalarProgram boolInfo (boolExpr True)),
    ("char-entry", expectedScalarProgram charInfo (charExpr 'j')),
    ("default-int-entry", expectedScalarProgram intInfo (intExpr 7)),
    ("default-float-entry", expectedScalarProgram floatInfo (floatExpr 1 5 Nothing)),
    ( "arithmetic-operators",
      expectedScalarStatements
        [ binaryExpr intInfo "+" (intExpr 1) (intExpr 2),
          binaryExpr intInfo "-" (intExpr 3) (intExpr 1),
          binaryExpr intInfo "*" (intExpr 2) (intExpr 4),
          binaryExpr intInfo "/" (intExpr 8) (intExpr 2)
        ]
    ),
    ( "ordering-operators",
      expectedScalarStatements
        [ binaryExpr boolInfo "<" (intExpr 1) (intExpr 2),
          binaryExpr boolInfo "<=" (intExpr 2) (intExpr 2),
          binaryExpr boolInfo ">" (intExpr 3) (intExpr 2),
          binaryExpr boolInfo ">=" (intExpr 3) (intExpr 3)
        ]
    ),
    ( "equality-operators",
      expectedScalarStatements
        [ binaryExpr boolInfo "==" (intExpr 1) (intExpr 1),
          binaryExpr boolInfo "!=" (intExpr 1) (intExpr 2)
        ]
    )
  ]

rejectedScalarFixtures :: [Fixture]
rejectedScalarFixtures = take 7 (drop 8 fixtures)

admittedOperators :: [Text]
admittedOperators = ["+", "-", "*", "/", "<", "<=", ">", ">=", "==", "!="]

emptyInputs :: InferenceInputs
emptyInputs =
  InferenceInputs
    { inferenceBuiltinMode = ResolveKernelOnly,
      inferenceWarningSettings = defaultWarningSettings,
      inferenceImportedTypes = Map.empty,
      inferenceImportedDataTypes = Map.empty,
      inferenceImportedCapabilities = emptyScopeCapabilityFacts,
      inferenceImportedClassNames = Set.empty,
      inferenceCurrentModulePath = Just modulePath
    }

ambientPreludeInputs :: InferenceInputs
ambientPreludeInputs = emptyInputs {inferenceImportedClassNames = Set.singleton "PreludeClass"}

modulePath :: [Text]
modulePath = ["App", "Main"]

validSourcePath :: TypedSourcePath
validSourcePath = TypedSourcePath "src/App/Main.jz"

span1 :: SourceSpan
span1 = SourceSpan 1 1

unitExpr :: Expr
unitExpr = EBlock [SExpr span1 (ETuple [])]

boolFixture, charFixture, defaultIntFixture, defaultFloatFixture, arithmeticOperatorsFixture, orderingOperatorsFixture, equalityOperatorsFixture :: Fixture
boolFixture = scalarFixture "bool-entry" (ELit (LBool True))
charFixture = scalarFixture "char-entry" (ELit (LChar 'j'))
defaultIntFixture = scalarFixture "default-int-entry" (ELit (LInt 7))
defaultFloatFixture = scalarFixture "default-float-entry" (ELit (LFloat 1.5 (mkFractionalLiteralSource 1 5 1) Nothing))
arithmeticOperatorsFixture = scalarStatementsFixture "arithmetic-operators" [EBinary "+" (ELit (LInt 1)) (ELit (LInt 2)), EBinary "-" (ELit (LInt 3)) (ELit (LInt 1)), EBinary "*" (ELit (LInt 2)) (ELit (LInt 4)), EBinary "/" (ELit (LInt 8)) (ELit (LInt 2))]
orderingOperatorsFixture = scalarStatementsFixture "ordering-operators" [EBinary "<" (ELit (LInt 1)) (ELit (LInt 2)), EBinary "<=" (ELit (LInt 2)) (ELit (LInt 2)), EBinary ">" (ELit (LInt 3)) (ELit (LInt 2)), EBinary ">=" (ELit (LInt 3)) (ELit (LInt 3))]
equalityOperatorsFixture = scalarStatementsFixture "equality-operators" [EBinary "==" (ELit (LInt 1)) (ELit (LInt 1)), EBinary "!=" (ELit (LInt 1)) (ELit (LInt 2))]

textFixture, listFixture, nonUnitTupleFixture, dataFixture, conditionalFixture, patternCaseFixture, localBlockBindingFixture :: Fixture
textFixture = scalarFixture "text-value" (ELit (LText "managed"))
listFixture = scalarFixture "list-value" (EList [ELit (LInt 1)])
nonUnitTupleFixture = scalarFixture "non-unit-tuple" (ETuple [ELit (LInt 1), ELit (LInt 2)])
dataFixture = scalarFixture "data-value" (EBlock [SData span1 (SourceName "Box") [] [DataConstructor (SourceName "Box") []], SExpr (SourceSpan 2 1) (EVar (SourceName "Box"))])
conditionalFixture = scalarFixture "conditional" (EIf (ELit (LBool True)) (ELit (LInt 1)) (ELit (LInt 2)))
patternCaseFixture = scalarFixture "pattern-case" (EPatternCase (ELit (LBool True)) [CaseArm PWildcard Nothing (ELit (LInt 1))])
localBlockBindingFixture = scalarFixture "local-block-binding" (EBlock [SLet (SourceName "value") span1 (ELit (LInt 1)), SExpr (SourceSpan 2 1) (EVar (SourceName "value"))])

scalarFixture :: Text -> Expr -> Fixture
scalarFixture name expression = Fixture name emptyInputs validSourcePath (unitModule {resolvedModuleCore = CoreModule (Just modulePath) Nothing [] (EBlock [SExpr span1 expression])})

scalarStatementsFixture :: Text -> [Expr] -> Fixture
scalarStatementsFixture name expressions =
  Fixture name emptyInputs validSourcePath
    (unitModule {resolvedModuleCore = CoreModule (Just modulePath) Nothing [] (EBlock (zipWith (\line expression -> SExpr (SourceSpan line 1) expression) [1 ..] expressions))})

unitModule :: ResolvedModule
unitModule =
  ResolvedModule
    { resolvedModulePath = modulePath,
      resolvedSourcePath = "host-only/ignored.jz",
      resolvedModuleImports = [],
      resolvedModuleExportInventory = exportInventory [],
      resolvedModuleCore = CoreModule (Just modulePath) Nothing [] unitExpr
    }

sourceDiagnosticModule :: ResolvedModule
sourceDiagnosticModule = unitModule {resolvedModuleCore = CoreModule (Just modulePath) Nothing [] (EBlock [SExpr span1 (EVar (SourceName "missing"))])}

moduleWithImport :: ResolvedModule
moduleWithImport =
  unitModule
    { resolvedModuleImports = [ResolvedImport span1 ["Library", "Value"] Nothing Nothing]
    }

entryModule :: TypedModule
entryModule =
  TypedModule
    modulePath
    validSourcePath
    []
    []
    (TypedModuleInterface [] [] [] [])
    [TypedExpressionStatement (TypedSpan 1 1) (TypedTupleExpr unitInfo [])]
    unitInfo

unitInfo :: TypedNodeInfo
unitInfo = TypedNodeInfo (TypedTupleType []) TypedUnitRecipe [] []

boolInfo, charInfo, intInfo, floatInfo :: TypedNodeInfo
boolInfo = TypedNodeInfo TypedBoolType TypedBoolRecipe [] []
charInfo = TypedNodeInfo TypedCharType TypedCharRecipe [] []
intInfo = TypedNodeInfo TypedIntType (TypedSignedIntegerRecipe 64) [] []
floatInfo = TypedNodeInfo TypedFloatType (TypedFloatRecipe 64) [] []

boolExpr :: Bool -> TypedExpr
boolExpr value = TypedLiteralExpr boolInfo (TypedBooleanLiteral value)

charExpr :: Char -> TypedExpr
charExpr value = TypedLiteralExpr charInfo (TypedCharacterLiteral value)

intExpr :: Integer -> TypedExpr
intExpr value = TypedLiteralExpr intInfo (TypedIntegerLiteral (Text.pack (show value)))

floatExpr :: Integer -> Integer -> Maybe TypedNumericType -> TypedExpr
floatExpr whole fractional maybeNumericType = TypedLiteralExpr floatInfo (TypedFractionalLiteral (Text.pack (show whole)) (Text.pack (show fractional)) maybeNumericType)

binaryExpr :: TypedNodeInfo -> Text -> TypedExpr -> TypedExpr -> TypedExpr
binaryExpr resultInfo operator left right = TypedBinaryExpr resultInfo (TypedBuiltinOperator operator) left right

expectedScalarProgram :: TypedNodeInfo -> TypedExpr -> TypedProgram
expectedScalarProgram moduleInfo expression =
  TypedProgram Nothing [TypedModule modulePath validSourcePath [] [] (TypedModuleInterface [] [] [] []) [TypedExpressionStatement (TypedSpan 1 1) expression] moduleInfo] modulePath

expectedScalarStatements :: [TypedExpr] -> TypedProgram
expectedScalarStatements expressions =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        (zipWith (\line expression -> TypedExpressionStatement (TypedSpan line 1) expression) [1 ..] expressions)
        (typedExpressionInfo (last expressions))
    ]
    modulePath

typedExpressionInfo :: TypedExpr -> TypedNodeInfo
typedExpressionInfo expression =
  case expression of
    TypedLiteralExpr info _ -> info
    TypedBinaryExpr info _ _ _ -> info
    TypedTupleExpr info _ -> info
    _ -> error "scalar fixture expected a scalar expression"
