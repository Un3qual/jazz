{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures
  ( Fixture (..),
    fixtureNames,
    fixtures,
    expectedUnitProgram,
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import JazzNext.Compiler.AST
import JazzNext.Compiler.BuiltinCatalog (BuiltinResolutionMode (ResolveKernelOnly))
import JazzNext.Compiler.Diagnostics (SourceSpan (..))
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
    "source-diagnostic",
    "invalid-portable-source-path",
    "resolved-import",
    "ambient-prelude-input"
  ]

fixtures :: [Fixture]
fixtures =
  [ Fixture "unit-entry" emptyInputs validSourcePath unitModule,
    Fixture "source-diagnostic" emptyInputs validSourcePath sourceDiagnosticModule,
    Fixture "invalid-portable-source-path" emptyInputs (TypedSourcePath "/private/host/Main.jz") unitModule,
    Fixture "resolved-import" emptyInputs validSourcePath moduleWithImport,
    Fixture "ambient-prelude-input" ambientPreludeInputs validSourcePath unitModule
  ]

expectedUnitProgram :: TypedProgram
expectedUnitProgram = TypedProgram Nothing [entryModule] modulePath

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
