{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Phase-indexed module and whole-program carriers.
module Jazz.Compiler.ModuleGraph
  ( AnalyzedModuleFacts (..),
    analyzedModuleDiagnostics,
    analyzedProgramDiagnostics,
    analyzedProgramErrors,
    orderedProgramDiagnostics,
    isStandaloneSourceModule,
    CoreModule (..),
    CoreProgram,
    DeclaredImportExposure (..),
    DeclaredModuleExports (..),
    DeclaredModuleFacts (..),
    ImportExposure (..),
    ImportExposureAt,
    ModuleFactsAt,
    ModuleImport (..),
    PreludeArtifact (..),
    ProgramInvariantFailure (..),
    ResolvedModuleFacts (..),
    coreModuleExpr,
    coreModulePath,
    coreProgramEntry,
    coreProgramModules,
    coreProgramPrelude,
    importAlias,
    lookupCoreModule,
    mkCoreProgram,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Foldable (toList)
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Jazz.Compiler.AST
  ( CoreNameAt,
    CoreNode,
    CorePhase (..),
    CoreSort (..),
    Expr (EBlock),
    FactsAt,
    Statement,
  )
import Jazz.Compiler.Diagnostics (CompilationDiagnostics, Diagnostic, SourceSpan, compilationDiagnostics, isErrorDiagnostic)
import Jazz.Compiler.ModuleExports
  ( ModuleExportInventory,
    ModuleExportSelector,
  )
import Jazz.Compiler.ModuleIdentity
  ( ModuleIdentity,
    ModulePath,
    ModuleQualifier,
    moduleIdentityPath,
    moduleIdentitySource,
    standaloneSourceFile,
  )
import Jazz.Compiler.ModuleImportScope (ValidatedImportScope)
import Jazz.Compiler.ModuleInterface (ModuleInterface)
import Jazz.Compiler.Name (Identifier)

data DeclaredModuleExports = DeclaredModuleExports
  { declaredModuleExportsSpan :: SourceSpan,
    declaredModuleExportSelectors :: [ModuleExportSelector]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data DeclaredImportExposure
  = DeclaredImportAll (Maybe ModuleQualifier)
  | DeclaredImportOnly (Maybe ModuleQualifier) (NonEmpty Identifier)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data ImportExposure
  = ImportAllUnqualified
  | ImportOnlyUnqualified (NonEmpty Identifier)
  | ImportQualifiedOnly ModuleQualifier
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

type family ImportExposureAt (phase :: CorePhase) :: Type where
  ImportExposureAt 'Lowered = DeclaredImportExposure
  ImportExposureAt 'Resolved = ImportExposure
  ImportExposureAt 'Analyzed = ImportExposure

data ModuleImport (phase :: CorePhase) = ModuleImport
  { moduleImportNode :: CoreNode phase 'StatementSort,
    importedModule :: ModulePath,
    importExposure :: ImportExposureAt phase
  }
  deriving stock (Generic)

type role ModuleImport nominal

-- | Raw imports retain invalid alias/selection combinations for diagnostics.
importAlias :: ModuleImport 'Lowered -> Maybe ModuleQualifier
importAlias declaration = case importExposure declaration of
  DeclaredImportAll alias -> alias
  DeclaredImportOnly alias _ -> alias

data DeclaredModuleFacts = DeclaredModuleFacts
  { declaredModuleExports :: Maybe DeclaredModuleExports
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data ResolvedModuleFacts = ResolvedModuleFacts
  { resolvedModuleExports :: ModuleExportInventory,
    resolvedModuleExportSelectors :: Maybe [ModuleExportSelector],
    resolvedModuleImportScope :: ValidatedImportScope
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data AnalyzedModuleFacts = AnalyzedModuleFacts
  { analyzedModuleExports :: ModuleExportInventory,
    analyzedModuleExportSelectors :: Maybe [ModuleExportSelector],
    analyzedModuleInterface :: ModuleInterface,
    analyzedModuleImportScope :: ValidatedImportScope,
    analyzedModuleDiagnosticGroups :: CompilationDiagnostics
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

analyzedModuleDiagnostics :: AnalyzedModuleFacts -> [Diagnostic]
analyzedModuleDiagnostics = compilationDiagnostics . analyzedModuleDiagnosticGroups

type family ModuleFactsAt (phase :: CorePhase) :: Type where
  ModuleFactsAt 'Lowered = DeclaredModuleFacts
  ModuleFactsAt 'Resolved = ResolvedModuleFacts
  ModuleFactsAt 'Analyzed = AnalyzedModuleFacts

data CoreModule (phase :: CorePhase) = CoreModule
  { coreModuleIdentity :: ModuleIdentity,
    coreModuleBodyNode :: CoreNode phase 'ExpressionSort,
    coreModuleImports :: [ModuleImport phase],
    coreModuleStatements :: [Statement phase],
    coreModuleFacts :: ModuleFactsAt phase
  }
  deriving stock (Generic)

type role CoreModule nominal

coreModulePath :: CoreModule phase -> ModulePath
coreModulePath = moduleIdentityPath . coreModuleIdentity

coreModuleExpr :: CoreModule phase -> Expr phase
coreModuleExpr coreModule =
  EBlock (coreModuleBodyNode coreModule) (coreModuleStatements coreModule)

data PreludeArtifact (phase :: CorePhase) = PreludeArtifact
  { preludeIdentity :: ModuleIdentity,
    preludeModule :: Maybe (CoreModule phase)
  }
  deriving stock (Generic)

type role PreludeArtifact nominal

data CoreProgram (phase :: CorePhase) = CoreProgram
  { storedCoreProgramPrelude :: PreludeArtifact phase,
    storedCoreProgramEntry :: ModulePath,
    storedCoreProgramModules :: NonEmpty (CoreModule phase),
    storedCoreProgramModuleIndex :: Map ModulePath (CoreModule phase)
  }

type role CoreProgram nominal

coreProgramPrelude :: CoreProgram phase -> PreludeArtifact phase
coreProgramPrelude = storedCoreProgramPrelude

coreProgramEntry :: CoreProgram phase -> ModulePath
coreProgramEntry = storedCoreProgramEntry

coreProgramModules :: CoreProgram phase -> NonEmpty (CoreModule phase)
coreProgramModules = storedCoreProgramModules

data ProgramInvariantFailure
  = MissingEntryModule ModulePath
  | DuplicateModulePath ModulePath
  | DependencyAfterDependent ModulePath ModulePath
  | UnknownImportedModule ModulePath ModulePath
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

mkCoreProgram ::
  PreludeArtifact phase ->
  ModulePath ->
  NonEmpty (CoreModule phase) ->
  Either (NonEmpty ProgramInvariantFailure) (CoreProgram phase)
mkCoreProgram prelude entry modules =
  case NonEmpty.nonEmpty (toList failures) of
    Just invariantFailures -> Left invariantFailures
    Nothing ->
      Right
        CoreProgram
          { storedCoreProgramPrelude = prelude,
            storedCoreProgramEntry = entry,
            storedCoreProgramModules = modules,
            storedCoreProgramModuleIndex = moduleIndex
          }
  where
    moduleList = NonEmpty.toList modules
    modulePaths = map coreModulePath moduleList
    pathSet = Set.fromList modulePaths
    moduleIndex = Map.fromList [(coreModulePath coreModule, coreModule) | coreModule <- moduleList]
    failures =
      missingEntryFailures
        <> duplicatePathFailures
        <> snd (foldl validateModule (Set.empty, Seq.empty) moduleList)
    missingEntryFailures =
      if entry `Set.member` pathSet then Seq.empty else Seq.singleton (MissingEntryModule entry)
    duplicatePathFailures = fmap DuplicateModulePath (duplicatePaths modulePaths)

    validateModule (seen, failuresByModule) coreModule =
      ( Set.insert dependentPath seen,
        failuresByModule <> foldMap (validateImport dependentPath seen) (coreModuleImports coreModule)
      )
      where
        dependentPath = coreModulePath coreModule

    validateImport dependentPath seen importDecl
      | dependencyPath `Set.notMember` pathSet =
          Seq.singleton (UnknownImportedModule dependentPath dependencyPath)
      | dependencyPath `Set.notMember` seen =
          Seq.singleton (DependencyAfterDependent dependentPath dependencyPath)
      | otherwise = Seq.empty
      where
        dependencyPath = importedModule importDecl

duplicatePaths :: (Ord value) => [value] -> Seq value
duplicatePaths = third . foldl collect (Set.empty, Set.empty, Seq.empty)
  where
    third (_, _, values) = values
    collect (seen, reported, duplicates) value
      | value `Set.member` seen,
        value `Set.notMember` reported =
          (seen, Set.insert value reported, duplicates Seq.|> value)
      | otherwise = (Set.insert value seen, reported, duplicates)

lookupCoreModule :: ModulePath -> CoreProgram phase -> Maybe (CoreModule phase)
lookupCoreModule modulePath = Map.lookup modulePath . storedCoreProgramModuleIndex

type CoreConstraints (c :: Type -> Constraint) phase =
  ( c (CoreNameAt phase),
    c (FactsAt phase 'ExpressionSort),
    c (FactsAt phase 'PatternSort),
    c (FactsAt phase 'StatementSort),
    c (ImportExposureAt phase),
    c (ModuleFactsAt phase)
  )

deriving stock instance (CoreConstraints Eq phase) => Eq (ModuleImport phase)

deriving stock instance (CoreConstraints Show phase) => Show (ModuleImport phase)

instance (CoreConstraints NFData phase) => NFData (ModuleImport phase)

deriving stock instance (CoreConstraints Eq phase) => Eq (CoreModule phase)

deriving stock instance (CoreConstraints Show phase) => Show (CoreModule phase)

instance (CoreConstraints NFData phase) => NFData (CoreModule phase)

deriving stock instance (CoreConstraints Eq phase) => Eq (PreludeArtifact phase)

deriving stock instance (CoreConstraints Show phase) => Show (PreludeArtifact phase)

instance (CoreConstraints NFData phase) => NFData (PreludeArtifact phase)

instance (CoreConstraints Eq phase) => Eq (CoreProgram phase) where
  left == right =
    coreProgramPrelude left == coreProgramPrelude right
      && coreProgramEntry left == coreProgramEntry right
      && coreProgramModules left == coreProgramModules right

instance (CoreConstraints Show phase) => Show (CoreProgram phase) where
  showsPrec precedence program =
    showParen (precedence > 10) $
      showString "CoreProgram "
        . shows (coreProgramPrelude program)
        . showChar ' '
        . shows (coreProgramEntry program)
        . showChar ' '
        . shows (coreProgramModules program)

instance (CoreConstraints NFData phase) => NFData (CoreProgram phase) where
  rnf (CoreProgram prelude entry modules moduleIndex) =
    rnf prelude `seq` rnf entry `seq` rnf modules `seq` rnf moduleIndex

-- The source identity marks an in-memory entry artifact; a named module loaded
-- from a file retains that file's identity instead.
isStandaloneSourceModule :: CoreModule phase -> Bool
isStandaloneSourceModule = (== standaloneSourceFile) . moduleIdentitySource . coreModuleIdentity

analyzedProgramDiagnostics :: CoreProgram 'Analyzed -> [Diagnostic]
analyzedProgramDiagnostics program =
  orderedProgramDiagnostics program (preludeDiagnostics : map moduleDiagnostics (toList (coreProgramModules program)))
  where
    preludeDiagnostics = maybe mempty moduleDiagnostics (preludeModule (coreProgramPrelude program))
    moduleDiagnostics = analyzedModuleDiagnosticGroups . coreModuleFacts

orderedProgramDiagnostics :: CoreProgram phase -> [CompilationDiagnostics] -> [Diagnostic]
orderedProgramDiagnostics program
  | any isStandaloneSourceModule (coreProgramModules program) = compilationDiagnostics . mconcat
  | otherwise = concatMap compilationDiagnostics

analyzedProgramErrors :: CoreProgram 'Analyzed -> [Diagnostic]
analyzedProgramErrors = filter isErrorDiagnostic . analyzedProgramDiagnostics
