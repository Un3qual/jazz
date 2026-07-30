{-# LANGUAGE OverloadedStrings #-}

-- | Parse-once module graph shared by semantic compilation and runtime.
module JazzNext.Compiler.ModuleGraph
  ( DeclaredModuleExports (..),
    CoreModule (..),
    ResolvedImport (..),
    ResolvedModule (..),
    ResolvedProgram (..),
    unresolvedResolvedModuleNames
  ) where

import Data.Text (Text)
import JazzNext.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Pattern (..),
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureToken (..),
    SignatureType (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics (SourceSpan)
import JazzNext.Compiler.ModuleExports
  ( ModuleExportInventory,
    ModuleExportSelector
  )
import JazzNext.Compiler.Name (Name (..))

-- | A source-qualified explicit export clause retained after lowering.
-- Absence means the module uses the default export-all policy; a present
-- empty selector list represents an explicit export-none clause.
data DeclaredModuleExports = DeclaredModuleExports
  { declaredModuleExportsSpan :: SourceSpan,
    declaredModuleExportSelectors :: [ModuleExportSelector]
  }
  deriving (Eq, Show)

data CoreModule = CoreModule
  { coreModuleDeclaredPath :: Maybe [Text],
    coreModuleDeclaredExports :: Maybe DeclaredModuleExports,
    coreModuleImports :: [ResolvedImport],
    coreModuleExpr :: Expr
  }
  deriving (Eq, Show)

data ResolvedImport = ResolvedImport
  { resolvedImportSpan :: SourceSpan,
    resolvedImportPath :: [Text],
    resolvedImportAlias :: Maybe Text,
    resolvedImportSymbols :: Maybe [Text]
  }
  deriving (Eq, Show)

data ResolvedModule = ResolvedModule
  { resolvedModulePath :: [Text],
    resolvedSourcePath :: FilePath,
    resolvedModuleImports :: [ResolvedImport],
    resolvedModuleExportInventory :: ModuleExportInventory,
    resolvedModuleCore :: CoreModule
  }
  deriving (Eq, Show)

data ResolvedProgram = ResolvedProgram
  { resolvedProgramEntryPath :: [Text],
    resolvedProgramModules :: [ResolvedModule]
  }
  deriving (Eq, Show)

-- | Test/audit helper enforcing that resolver output contains no surface-name
-- constructors. Generated names are already compiler-owned and remain valid.
unresolvedResolvedModuleNames :: ResolvedModule -> [Name]
unresolvedResolvedModuleNames = filter unresolved . exprNames . coreModuleExpr . resolvedModuleCore
  where
    unresolved name =
      case name of
        SourceName {} -> True
        QualifiedName {} -> True
        _ -> False

exprNames :: Expr -> [Name]
exprNames expr =
  case expr of
    ELit _ -> []
    EVar name -> [name]
    ELambda parameter body -> parameter : exprNames body
    EOperatorValue _ -> []
    EList items -> concatMap exprNames items
    ETuple items -> concatMap exprNames items
    EApply function argument -> exprNames function <> exprNames argument
    ETypeApplication function _ _ -> exprNames function
    EIf condition trueBranch falseBranch ->
      exprNames condition <> exprNames trueBranch <> exprNames falseBranch
    EPatternCase scrutinee arms -> exprNames scrutinee <> concatMap caseArmNames arms
    EBinary _ left right -> exprNames left <> exprNames right
    ESectionLeft left _ -> exprNames left
    ESectionRight _ right -> exprNames right
    EBlock statements -> concatMap statementNames statements

caseArmNames :: CaseArm -> [Name]
caseArmNames (CaseArm patternValue guard body) =
  patternNames patternValue <> maybe [] exprNames guard <> exprNames body

patternNames :: Pattern -> [Name]
patternNames patternValue =
  case patternValue of
    PWildcard -> []
    PVariable name -> [name]
    PLiteral _ -> []
    PConstructor name patterns -> name : concatMap patternNames patterns
    PList patterns -> concatMap patternNames patterns
    PConsList headPattern tailPattern -> patternNames headPattern <> patternNames tailPattern
    PTuple patterns -> concatMap patternNames patterns
    PAs name pattern' -> name : patternNames pattern'
    POr patterns -> concatMap patternNames patterns

statementNames :: Statement -> [Name]
statementNames statement =
  case statement of
    SLet name _ value -> name : exprNames value
    SSignature name _ payload -> name : signaturePayloadNames payload
    SData _ name parameters constructors ->
      name : parameters <> concatMap dataConstructorNames constructors
    SClass _ name parameters methods ->
      name : parameters <> concatMap classMethodNames methods
    SImpl _ name arguments methods ->
      name : concatMap signatureTypeNames arguments <> concatMap implMethodNames methods
    SModule {} -> []
    SImport {} -> []
    SExpr _ value -> exprNames value

dataConstructorNames :: DataConstructor -> [Name]
dataConstructorNames (DataConstructor name fieldTypes) =
  name : concatMap signatureTypeNames fieldTypes

classMethodNames :: ClassMethodSignature -> [Name]
classMethodNames (ClassMethodSignature name _ payload) =
  name : signaturePayloadNames payload

implMethodNames :: ImplMethod -> [Name]
implMethodNames (ImplMethod name _ body) = name : exprNames body

signaturePayloadNames :: SignaturePayload -> [Name]
signaturePayloadNames payload =
  case payload of
    SignatureType signatureType -> signatureTypeNames signatureType
    ConstrainedSignature constraints signatureType ->
      concatMap signatureConstraintNames constraints <> signatureTypeNames signatureType
    UnsupportedSignature tokens ->
      [name | SignatureNameToken name <- tokens]

signatureConstraintNames :: SignatureConstraint -> [Name]
signatureConstraintNames (SignatureConstraint name arguments) =
  name : concatMap signatureTypeNames arguments

signatureTypeNames :: SignatureType -> [Name]
signatureTypeNames signatureType =
  case signatureType of
    TypeVariable {} -> []
    TypeName name -> [name]
    TypeApplication name arguments -> name : concatMap signatureTypeNames arguments
    TypeList innerType -> signatureTypeNames innerType
    TypeTuple elementTypes -> concatMap signatureTypeNames elementTypes
    TypeFunction argumentType resultType ->
      signatureTypeNames argumentType <> signatureTypeNames resultType
    _ -> []
