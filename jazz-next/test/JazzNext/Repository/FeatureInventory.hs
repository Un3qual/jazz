{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Repository.FeatureInventory
  ( SurfaceFeature (..),
    inventorySurface,
    requiredAuthoredFeatures,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.ModuleExports
  ( ModuleExportSelector (..),
    ModuleTypeConstructorSelector (..),
  )
import JazzNext.Compiler.Name
  ( Identifier,
    IdentifierLike (identifierPurity),
    NameNamespace (..),
    isOperatorBindingIdentifierText,
    identifierText,
  )
import JazzNext.Compiler.Parser.AST
import JazzNext.Compiler.Purity (Purity (..))

data SurfaceFeature
  = LiteralFeature
  | NumericWidthFeature
  | OrdinaryBindingFeature
  | CompactLambdaFeature
  | PatternLambdaFeature
  | PatternLambdaClausesFeature
  | MultiParameterLambdaFeature
  | LambdaOrPatternFeature
  | MultiArmCaseFeature
  | PartialApplicationFeature
  | ListFeature
  | TupleFeature
  | UnitFeature
  | GenericAdtFeature
  | StructuredConstructorFieldFeature
  | WildcardPatternFeature
  | VariablePatternFeature
  | LiteralPatternFeature
  | ConstructorPatternFeature
  | ListPatternFeature
  | ConsPatternFeature
  | TuplePatternFeature
  | AsPatternFeature
  | OrPatternFeature
  | GuardedCaseFeature
  | ConditionalFeature
  | ApplicationFeature
  | DollarApplicationFeature
  | OperatorValueFeature
  | LeftSectionFeature
  | RightSectionFeature
  | DeclaredOperatorFeature
  | SignatureFeature
  | ConstrainedSignatureFeature
  | ExplicitTypeApplicationFeature
  | ModuleFeature
  | AliasFeature
  | ImportFeature
  | ExplicitImportFeature
  | ValueExportFeature
  | TypeExportFeature
  | ConstructorExportFeature
  | ClassExportFeature
  | ClassFeature
  | ImplFeature
  | QualifiedMethodFeature
  | PureFunctionFeature
  | EffectfulFunctionFeature
  deriving (Bounded, Enum, Eq, Ord, Show)

requiredAuthoredFeatures :: Set SurfaceFeature
requiredAuthoredFeatures = Set.fromList [minBound .. maxBound]

inventorySurface :: Text -> SurfaceExpr -> Set SurfaceFeature
inventorySurface source surface =
  inventoryExpr surface
    <> erasedSourceFeatures source
    <> Set.fromList
      [ PartialApplicationFeature
      | containsPartialApplication Map.empty surface
      ]

erasedSourceFeatures :: Text -> Set SurfaceFeature
erasedSourceFeatures source =
  Set.fromList
    ( [DollarApplicationFeature | " $ " `Text.isInfixOf` source]
        <> [DeclaredOperatorFeature | "\n  operator " `Text.isInfixOf` source]
        <> [NumericWidthFeature | any (`Text.isInfixOf` source) numericSuffixes]
    )
  where
    numericSuffixes =
      [ "i8",
        "i16",
        "i32",
        "i64",
        "u8",
        "u16",
        "u32",
        "u64",
        "f16",
        "f32",
        "f64"
      ]

inventoryExpr :: SurfaceExpr -> Set SurfaceFeature
inventoryExpr expression =
  case expression of
    SELit literal -> Set.singleton LiteralFeature <> inventoryLiteral literal
    SEVar _ -> Set.empty
    SEQualifiedVar _ _ -> Set.singleton QualifiedMethodFeature
    SELambda parameters body ->
      let parameterList = NonEmpty.toList parameters
       in Set.fromList
            ( [CompactLambdaFeature]
                <> [PatternLambdaFeature | any lambdaParameterIsPattern parameterList]
                <> [MultiParameterLambdaFeature | length parameterList > 1]
                <> [LambdaOrPatternFeature | any lambdaParameterIsOrPattern parameterList]
            )
            <> Set.unions
              (inventoryExpr body : map inventoryLambdaParameter parameterList)
    SEPatternLambda clauses ->
      let clauseList = NonEmpty.toList clauses
          SurfacePatternLambdaClause _ firstPatterns _ = NonEmpty.head clauses
          patternList = concatMap clausePatterns clauseList
       in Set.fromList
            ( [ CompactLambdaFeature,
                PatternLambdaFeature,
                PatternLambdaClausesFeature
              ]
                <> [MultiParameterLambdaFeature | NonEmpty.length firstPatterns > 1]
                <> [LambdaOrPatternFeature | any patternIsOrPattern patternList]
            )
            <> Set.unions (map inventoryPatternLambdaClause clauseList)
    SEOperatorValue _ -> Set.singleton OperatorValueFeature
    SEList items ->
      Set.insert ListFeature (Set.unions (map inventoryExpr items))
    SETuple items ->
      Set.fromList
        ([TupleFeature] <> [UnitFeature | null items])
        <> Set.unions (map inventoryExpr items)
    SEApply function argument ->
      Set.singleton ApplicationFeature
        <> inventoryExpr function
        <> inventoryExpr argument
    SETypeApplication function _ signatureType ->
      Set.insert ExplicitTypeApplicationFeature
        (inventoryExpr function <> inventorySignatureType signatureType)
    SEIf condition thenBranch elseBranch ->
      Set.insert ConditionalFeature
        (Set.unions (map inventoryExpr [condition, thenBranch, elseBranch]))
    SECase scrutinee arms ->
      Set.fromList
        ( [MultiArmCaseFeature | length arms > 1]
            <> [GuardedCaseFeature | any armHasGuard arms]
        )
        <> inventoryExpr scrutinee
        <> Set.unions (map inventoryCaseArm arms)
    SEBinary _ left right ->
      inventoryExpr left <> inventoryExpr right
    SESectionLeft left _ ->
      Set.insert LeftSectionFeature (inventoryExpr left)
    SESectionRight _ right ->
      Set.insert RightSectionFeature (inventoryExpr right)
    SEBlock statements ->
      Set.unions (map inventoryStatement statements)
  where
    armHasGuard (SurfaceCaseArm _ guardExpr _) =
      case guardExpr of
        Nothing -> False
        Just _ -> True

    lambdaParameterIsPattern parameter =
      case parameter of
        SurfaceLambdaIdentifier _ -> False
        SurfaceLambdaPattern _ -> True

    lambdaParameterIsOrPattern parameter =
      case parameter of
        SurfaceLambdaPattern (SPOr _) -> True
        _ -> False

    clausePatterns (SurfacePatternLambdaClause _ patterns _) =
      NonEmpty.toList patterns

    patternIsOrPattern patternValue =
      case patternValue of
        SPOr _ -> True
        _ -> False

containsPartialApplication :: Map Text Int -> SurfaceExpr -> Bool
containsPartialApplication arities expression =
  case expression of
    SELit _ -> False
    SEVar _ -> False
    SEQualifiedVar _ _ -> False
    SELambda parameters body ->
      containsPartialApplication
        (removeBoundNames arities (Set.unions (map lambdaParameterBindings (NonEmpty.toList parameters))))
        body
    SEPatternLambda clauses ->
      any (patternLambdaClauseContainsPartialApplication arities) clauses
    SEOperatorValue _ -> False
    SEList items -> any (containsPartialApplication arities) items
    SETuple items -> any (containsPartialApplication arities) items
    SEApply {} ->
      let (function, arguments) = applicationSpine expression
          appliedArity = length arguments
          isPartial =
            case callableArity arities function of
              Just expectedArity ->
                appliedArity > 0 && appliedArity < expectedArity
              Nothing -> False
       in isPartial
            || containsPartialApplication arities function
            || any (containsPartialApplication arities) arguments
    SETypeApplication function _ _ ->
      containsPartialApplication arities function
    SEIf condition thenBranch elseBranch ->
      any
        (containsPartialApplication arities)
        [condition, thenBranch, elseBranch]
    SECase scrutinee arms ->
      containsPartialApplication arities scrutinee
        || any (caseArmContainsPartialApplication arities) arms
    SEBinary _ left right ->
      containsPartialApplication arities left
        || containsPartialApplication arities right
    SESectionLeft left _ ->
      containsPartialApplication arities left
    SESectionRight _ right ->
      containsPartialApplication arities right
    SEBlock statements ->
      let blockArities = foldl declareStatementArity arities statements
       in any (statementContainsPartialApplication blockArities) statements

applicationSpine :: SurfaceExpr -> (SurfaceExpr, [SurfaceExpr])
applicationSpine = go []
  where
    go arguments candidate =
      case candidate of
        SEApply function argument ->
          go (argument : arguments) function
        _ -> (candidate, arguments)

callableArity :: Map Text Int -> SurfaceExpr -> Maybe Int
callableArity arities expression =
  case expression of
    SEVar name -> Map.lookup (identifierText name) arities
    SELambda parameters _ -> Just (NonEmpty.length parameters)
    SEPatternLambda clauses ->
      let SurfacePatternLambdaClause _ patterns _ = NonEmpty.head clauses
       in Just (NonEmpty.length patterns)
    SEOperatorValue _ -> Just 2
    SETypeApplication function _ _ -> callableArity arities function
    _ -> Nothing

declareStatementArity :: Map Text Int -> SurfaceStatement -> Map Text Int
declareStatementArity arities statement =
  case statement of
    SSLet name _ body ->
      updateArity (identifierText name) (callableArity arities body) arities
    SSData _ _ _ constructors ->
      foldl declareConstructorArity arities constructors
    _ -> arities

declareConstructorArity :: Map Text Int -> SurfaceDataConstructor -> Map Text Int
declareConstructorArity arities (SurfaceDataConstructor name fields) =
  updateArity (identifierText name) (Just (length fields)) arities

updateArity :: Text -> Maybe Int -> Map Text Int -> Map Text Int
updateArity name maybeArity arities =
  case maybeArity of
    Just arity -> Map.insert name arity arities
    Nothing -> Map.delete name arities

statementContainsPartialApplication :: Map Text Int -> SurfaceStatement -> Bool
statementContainsPartialApplication arities statement =
  case statement of
    SSLet _ _ body -> containsPartialApplication arities body
    SSImpl _ _ _ methods ->
      any (implMethodContainsPartialApplication arities) methods
    SSExpr _ body -> containsPartialApplication arities body
    _ -> False

implMethodContainsPartialApplication :: Map Text Int -> SurfaceImplMethod -> Bool
implMethodContainsPartialApplication arities (SurfaceImplMethod _ _ body) =
  containsPartialApplication arities body

caseArmContainsPartialApplication :: Map Text Int -> SurfaceCaseArm -> Bool
caseArmContainsPartialApplication arities (SurfaceCaseArm patternValue guardExpr body) =
  let armArities = removeBoundNames arities (patternBindings patternValue)
   in maybe False (containsPartialApplication armArities) guardExpr
        || containsPartialApplication armArities body

patternLambdaClauseContainsPartialApplication ::
  Map Text Int ->
  SurfacePatternLambdaClause ->
  Bool
patternLambdaClauseContainsPartialApplication arities (SurfacePatternLambdaClause _ patterns body) =
  containsPartialApplication
    (removeBoundNames arities (Set.unions (map patternBindings (NonEmpty.toList patterns))))
    body

lambdaParameterBindings :: SurfaceLambdaParameter -> Set Text
lambdaParameterBindings parameter =
  case parameter of
    SurfaceLambdaIdentifier name -> Set.singleton (identifierText name)
    SurfaceLambdaPattern patternValue -> patternBindings patternValue

patternBindings :: SurfacePattern -> Set Text
patternBindings patternValue =
  case patternValue of
    SPVariable name -> Set.singleton (identifierText name)
    SPConstructor _ arguments -> Set.unions (map patternBindings arguments)
    SPList items -> Set.unions (map patternBindings items)
    SPConsList headPattern tailPattern ->
      patternBindings headPattern <> patternBindings tailPattern
    SPTuple items -> Set.unions (map patternBindings items)
    SPAs name nested ->
      Set.insert (identifierText name) (patternBindings nested)
    SPOr alternatives -> Set.unions (map patternBindings alternatives)
    _ -> Set.empty

removeBoundNames :: Map Text Int -> Set Text -> Map Text Int
removeBoundNames = Set.foldr Map.delete

inventoryStatement :: SurfaceStatement -> Set SurfaceFeature
inventoryStatement statement =
  case statement of
    SSLet name _ expression ->
      Set.insert OrdinaryBindingFeature
        (inventoryIdentifier name <> inventoryExpr expression <> operatorBindingFeature name)
    SSSignature name _ payload ->
      Set.insert SignatureFeature
        (inventoryIdentifier name <> inventorySignaturePayload payload)
    SSData _ _ typeParameters constructors ->
      Set.fromList
        ( [GenericAdtFeature | not (null typeParameters)]
            <> [ StructuredConstructorFieldFeature
               | any constructorHasFields constructors
               ]
        )
        <> Set.unions (map inventoryDataConstructor constructors)
    SSClass _ _ _ methods ->
      Set.insert ClassFeature
        (Set.unions (map inventoryClassMethod methods))
    SSImpl _ _ arguments methods ->
      Set.insert ImplFeature
        ( Set.unions
            ( map inventorySignatureType arguments
                <> map inventoryImplMethod methods
            )
        )
    SSModule _ _ exports ->
      Set.insert ModuleFeature
        (maybe Set.empty (Set.unions . map inventoryExport) exports)
    SSImport _ _ alias importedNames ->
      Set.fromList
        ( [ImportFeature]
            <> [AliasFeature | hasValue alias]
            <> [ExplicitImportFeature | hasValue importedNames]
        )
    SSExpr _ expression -> inventoryExpr expression
  where
    constructorHasFields (SurfaceDataConstructor _ fields) = not (null fields)
    hasValue maybeValue =
      case maybeValue of
        Nothing -> False
        Just _ -> True

inventoryLambdaParameter :: SurfaceLambdaParameter -> Set SurfaceFeature
inventoryLambdaParameter parameter =
  case parameter of
    SurfaceLambdaIdentifier _ -> Set.singleton VariablePatternFeature
    SurfaceLambdaPattern patternValue -> inventoryPattern patternValue

inventoryCaseArm :: SurfaceCaseArm -> Set SurfaceFeature
inventoryCaseArm (SurfaceCaseArm patternValue guardExpr body) =
  inventoryPattern patternValue
    <> maybe Set.empty inventoryExpr guardExpr
    <> inventoryExpr body

inventoryPatternLambdaClause :: SurfacePatternLambdaClause -> Set SurfaceFeature
inventoryPatternLambdaClause (SurfacePatternLambdaClause _ patterns body) =
  Set.unions (inventoryExpr body : map inventoryPattern (NonEmpty.toList patterns))

inventoryPattern :: SurfacePattern -> Set SurfaceFeature
inventoryPattern patternValue =
  case patternValue of
    SPWildcard -> Set.singleton WildcardPatternFeature
    SPVariable _ -> Set.singleton VariablePatternFeature
    SPLiteral literal ->
      Set.fromList [LiteralFeature, LiteralPatternFeature]
        <> inventoryLiteral literal
    SPConstructor _ arguments ->
      Set.insert ConstructorPatternFeature
        (Set.unions (map inventoryPattern arguments))
    SPList items ->
      Set.insert ListPatternFeature
        (Set.unions (map inventoryPattern items))
    SPConsList headPattern tailPattern ->
      Set.insert ConsPatternFeature
        (inventoryPattern headPattern <> inventoryPattern tailPattern)
    SPTuple items ->
      Set.fromList
        ([TuplePatternFeature] <> [UnitFeature | null items])
        <> Set.unions (map inventoryPattern items)
    SPAs _ nested ->
      Set.insert AsPatternFeature (inventoryPattern nested)
    SPOr alternatives ->
      Set.insert OrPatternFeature
        (Set.unions (map inventoryPattern alternatives))

inventoryDataConstructor :: SurfaceDataConstructor -> Set SurfaceFeature
inventoryDataConstructor (SurfaceDataConstructor _ fields) =
  Set.unions (map inventorySignatureType fields)

inventoryClassMethod :: SurfaceClassMethodSignature -> Set SurfaceFeature
inventoryClassMethod (SurfaceClassMethodSignature name _ payload) =
  inventoryIdentifier name <> inventorySignaturePayload payload

inventoryImplMethod :: SurfaceImplMethod -> Set SurfaceFeature
inventoryImplMethod (SurfaceImplMethod name _ body) =
  inventoryIdentifier name <> inventoryExpr body

inventoryIdentifier :: Identifier -> Set SurfaceFeature
inventoryIdentifier name =
  Set.singleton
    ( case identifierPurity name of
        Pure -> PureFunctionFeature
        Impure -> EffectfulFunctionFeature
    )

operatorBindingFeature :: Identifier -> Set SurfaceFeature
operatorBindingFeature name
  | isOperatorBindingIdentifierText (identifierText name) =
      Set.singleton DeclaredOperatorFeature
  | otherwise = Set.empty

inventoryLiteral :: SurfaceLiteral -> Set SurfaceFeature
inventoryLiteral literal =
  case literal of
    SLFloat _ _ (Just _) -> Set.singleton NumericWidthFeature
    _ -> Set.empty

inventorySignaturePayload :: SurfaceSignaturePayload -> Set SurfaceFeature
inventorySignaturePayload payload =
  case payload of
    SurfaceSignatureType signatureType ->
      inventorySignatureType signatureType
    SurfaceConstrainedSignature constraints signatureType ->
      Set.insert ConstrainedSignatureFeature
        ( Set.unions
            ( inventorySignatureType signatureType
                : map inventorySignatureConstraint constraints
            )
        )
    SurfaceUnsupportedSignature _ -> Set.empty

inventorySignatureConstraint :: SurfaceSignatureConstraint -> Set SurfaceFeature
inventorySignatureConstraint (SurfaceSignatureConstraint _ arguments) =
  Set.unions (map inventorySignatureType arguments)

inventorySignatureType :: SurfaceSignatureType -> Set SurfaceFeature
inventorySignatureType signatureType =
  case signatureType of
    SurfaceTypeNumeric _ -> Set.singleton NumericWidthFeature
    SurfaceTypeApplication _ arguments ->
      Set.unions (map inventorySignatureType arguments)
    SurfaceTypeList itemType ->
      Set.insert ListFeature (inventorySignatureType itemType)
    SurfaceTypeTuple itemTypes ->
      Set.fromList ([TupleFeature] <> [UnitFeature | null itemTypes])
        <> Set.unions (map inventorySignatureType itemTypes)
    SurfaceTypeFunction argument result ->
      inventorySignatureType argument <> inventorySignatureType result
    _ -> Set.empty

inventoryExport :: ModuleExportSelector -> Set SurfaceFeature
inventoryExport selector =
  case selector of
    ModuleExportSelector namespace _ ->
      case namespace of
        Just ValueNamespace -> Set.singleton ValueExportFeature
        Just ConstructorNamespace -> Set.singleton ConstructorExportFeature
        Just TypeNamespace -> Set.singleton TypeExportFeature
        Just CapabilityNamespace -> Set.singleton ClassExportFeature
        Nothing -> Set.singleton ValueExportFeature
    ModuleTypeExportSelector _ _ constructorSelector ->
      Set.insert TypeExportFeature
        ( case constructorSelector of
            AbstractType -> Set.empty
            AllTypeConstructors _ -> Set.singleton ConstructorExportFeature
            SelectedTypeConstructors _ -> Set.singleton ConstructorExportFeature
        )
