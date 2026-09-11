{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Shared helpers for parsing surface source into lowered compiler programs.
module Jazz.Compiler.SourceProgram
  ( parseAndLowerStandaloneSource,
    parseSurfaceWithErrorCode,
    standaloneSourceModule,
    isStandaloneSourceModule,
    scopeStatements,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CoreNode (..),
    CoreNodeId (..),
    CorePhase (..),
    Expr (..),
    Statement (..),
    expressionNode,
  )
import Jazz.Compiler.DiagnosticCatalog
  ( ErrorCode (..),
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    prependDiagnosticSummary,
    setDiagnosticErrorCode,
  )
import Jazz.Compiler.ModuleGraph (CoreModule (..), DeclaredModuleFacts (..))
import Jazz.Compiler.ModuleIdentity (mkModulePath, mkSourceFile, moduleIdentity, moduleIdentitySource, standaloneModulePath)
import Jazz.Compiler.Name (mkIdentifier)
import Jazz.Compiler.Parser
  ( parseSurfaceProgram,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceExpr,
  )
import Jazz.Compiler.Parser.Lower
  ( lowerSurfaceExpr,
    reindexLoweredExpr,
  )

parseAndLowerStandaloneSource :: Text -> Either Diagnostic (Expr 'Lowered)
parseAndLowerStandaloneSource source = do
  surfaceProgram <- parseSurfaceWithErrorCode source
  pure (lowerSurfaceExpr surfaceProgram)

-- | Give a source expression its own graph artifact and node identity space.
-- The synthetic wrapper is only needed by callers supplying a non-block AST.
standaloneSourceModule :: Expr 'Lowered -> CoreModule 'Lowered
standaloneSourceModule expression =
  case reindexLoweredExpr block of
    EBlock node statements ->
      CoreModule
        { coreModuleIdentity = moduleIdentity nominalPath (mkSourceFile standaloneSourcePath),
          coreModuleBodyNode = node,
          coreModuleImports = [],
          coreModuleStatements = statements,
          coreModuleFacts = DeclaredModuleFacts Nothing
        }
    _ -> error "standalone source wrapper lost its block"
  where
    block = case expression of
      EBlock {} -> expression
      _ ->
        let node :: CoreNode 'Lowered sort
            node = CoreNode (CoreNodeId 0) (coreNodeSpan (expressionNode expression)) ()
         in EBlock node [SExpr node expression]
    nominalPath =
      fromMaybe standaloneModulePath $
        listToMaybe
          [ mkModulePath (fmap mkIdentifier path)
          | SModule _ segments <- scopeStatements block,
            Just path <- [NonEmpty.nonEmpty segments]
          ]

-- The synthetic source identity marks an in-memory entry artifact; a named
-- module loaded from a file has its actual source identity instead.
isStandaloneSourceModule :: CoreModule phase -> Bool
isStandaloneSourceModule = (== mkSourceFile standaloneSourcePath) . moduleIdentitySource . coreModuleIdentity

standaloneSourcePath :: FilePath
standaloneSourcePath = "<standalone>"

scopeStatements :: Expr phase -> [Statement phase]
scopeStatements expr =
  case expr of
    EBlock _ statements -> statements
    _ -> []

parseSurfaceWithErrorCode :: Text -> Either Diagnostic SurfaceExpr
parseSurfaceWithErrorCode source =
  case parseSurfaceProgram source of
    Left parseError ->
      Left (setDiagnosticErrorCode E0001 (prependDiagnosticSummary "parse error: " parseError))
    Right surfaceProgram ->
      Right surfaceProgram
