{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Parse-once module graph shared by semantic compilation and runtime.
module Jazz.Compiler.ModuleGraph
  ( DeclaredModuleExports (..),
    CoreModule (..),
    CoreResolvedImport (..),
    ImportExposure (..),
    ResolvedImport (..),
    ResolvedModule (..),
    ResolvedProgram (..),
  )
where

import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Jazz.Compiler.AST
  ( CoreNameAt,
    CorePhase (..),
    CoreSort (..),
    Expr,
    FactsAt,
  )
import Jazz.Compiler.Diagnostics (SourceSpan)
import Jazz.Compiler.ModuleExports
  ( ModuleExportInventory,
    ModuleExportSelector,
  )

-- | A source-qualified explicit export clause retained after lowering.
-- Absence means the module uses the default export-all policy; a present
-- empty selector list represents an explicit export-none clause.
data DeclaredModuleExports = DeclaredModuleExports
  { declaredModuleExportsSpan :: SourceSpan,
    declaredModuleExportSelectors :: [ModuleExportSelector]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data CoreModule phase = CoreModule
  { coreModuleDeclaredPath :: Maybe [Text],
    coreModuleDeclaredExports :: Maybe DeclaredModuleExports,
    coreModuleImports :: [CoreResolvedImport],
    coreModuleExpr :: Expr phase
  }
  deriving stock (Generic)

type role CoreModule nominal

deriving stock instance
  ( Eq (CoreNameAt phase),
    Eq (FactsAt phase 'ExpressionSort),
    Eq (FactsAt phase 'PatternSort),
    Eq (FactsAt phase 'StatementSort)
  ) =>
  Eq (CoreModule phase)

deriving stock instance
  ( Show (CoreNameAt phase),
    Show (FactsAt phase 'ExpressionSort),
    Show (FactsAt phase 'PatternSort),
    Show (FactsAt phase 'StatementSort)
  ) =>
  Show (CoreModule phase)

instance
  ( NFData (CoreNameAt phase),
    NFData (FactsAt phase 'ExpressionSort),
    NFData (FactsAt phase 'PatternSort),
    NFData (FactsAt phase 'StatementSort)
  ) =>
  NFData (CoreModule phase)

data CoreResolvedImport = CoreResolvedImport
  { coreResolvedImportSpan :: SourceSpan,
    coreResolvedImportPath :: [Text],
    coreResolvedImportAlias :: Maybe Text,
    coreResolvedImportSymbols :: Maybe [Text]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data ImportExposure
  = ImportAll
  | ImportOnly (NonEmpty Text)
  | ImportQualified Text
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data ResolvedImport = ResolvedImport
  { resolvedImportSpan :: SourceSpan,
    resolvedImportPath :: [Text],
    resolvedImportExposure :: ImportExposure
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data ResolvedModule = ResolvedModule
  { resolvedModulePath :: [Text],
    resolvedSourcePath :: FilePath,
    resolvedModuleImports :: [ResolvedImport],
    resolvedModuleExportInventory :: ModuleExportInventory,
    resolvedModuleCore :: CoreModule 'Resolved
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data ResolvedProgram = ResolvedProgram
  { resolvedProgramEntryPath :: [Text],
    resolvedProgramModules :: [ResolvedModule]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
