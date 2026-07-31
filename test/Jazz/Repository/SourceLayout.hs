{-# LANGUAGE OverloadedStrings #-}

module Jazz.Repository.SourceLayout
  ( JazzSourceModule (..),
    JazzSourceRole (..),
    SourceLayoutViolation (..),
    renderSourceLayoutViolation,
    sourceModuleFromSurface,
    validateSourceLayering,
  )
where

import Data.List (find)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceStatement (..),
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
      jazzImportedModulePaths =
        [ modulePath
          | SSImport _ modulePath _ _ <- statements
        ]
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
