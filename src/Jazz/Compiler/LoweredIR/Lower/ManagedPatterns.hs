-- | Pure admission and exhaustiveness analysis for managed pattern cases.
module Jazz.Compiler.LoweredIR.Lower.ManagedPatterns
  ( ManagedPattern (..),
    ManagedPatternArm (..),
    ManagedPatternConstructor (..),
    analyzeManagedPatternCase,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Jazz.Compiler.LoweredIR
import Jazz.Compiler.LoweredIR.Lower.ManagedLayouts
  ( managedPatternConstructorFor,
    managedPatternConstructorsFor,
    productLayoutFields,
    representationForRecipe,
  )
import Jazz.Compiler.LoweredIR.Lower.Types
import Jazz.Compiler.TypedCore

data ManagedPattern
  = ManagedWildcard TypedNodeInfo
  | ManagedVariable TypedNodeInfo TypedBinderId
  | ManagedLiteral TypedNodeInfo TypedLiteral
  | ManagedConstructor ManagedPatternConstructor [ManagedPattern]
  | ManagedTuple TypedNodeInfo LoweredLayoutId [ManagedPattern]
  | ManagedAs TypedNodeInfo TypedBinderId ManagedPattern
  | ManagedOr TypedNodeInfo (NonEmpty ManagedPattern)
  deriving (Eq, Show)

data ManagedPatternArm = ManagedPatternArm
  { managedPatternArmPattern :: ManagedPattern,
    managedPatternArmGuard :: Maybe TypedExpr,
    managedPatternArmBody :: TypedExpr
  }
  deriving (Eq, Show)

analyzeManagedPatternCase ::
  ManagedLayoutCatalog ->
  [Text] ->
  [Int] ->
  [Int] ->
  TypedExpr ->
  [TypedCaseArm] ->
  Either LoweredIRLoweringFailure (NonEmpty ManagedPatternArm)
analyzeManagedPatternCase catalog modulePath statementPath expressionPath scrutinee arms = do
  normalizedArms <- traverse normalizeArm (zip [0 ..] arms)
  case NonEmpty.nonEmpty normalizedArms of
    Nothing -> Left incompleteFailure
    Just armPlan
      | not (supportedScrutinee scrutineeInfo) -> Left unsupportedExpressionFailure
      | hasNonFinalUnguardedCatchAll normalizedArms -> Left incompleteFailure
      | patternMatrixIsTotal catalog scrutineeInfo normalizedArms -> Right armPlan
      | otherwise -> Left incompleteFailure
  where
    scrutineeInfo = typedExpressionInfo scrutinee
    normalizeArm (armIndex, TypedCaseArm patternValue guard body) = do
      normalized <- normalizePattern catalog modulePath statementPath (expressionPath <> [armIndex]) True scrutineeInfo patternValue
      pure
        ManagedPatternArm
          { managedPatternArmPattern = normalized,
            managedPatternArmGuard = guard,
            managedPatternArmBody = body
          }
    supportedScrutinee info =
      case (typedNodeType info, typedNodeRecipe info) of
        (TypedTupleType (_ : _), TypedManagedProductRecipe (_ : _)) -> concreteRepresentation info
        (TypedDataType name arguments, TypedManagedVariantRecipe recipeName recipeArguments) ->
          name == recipeName && arguments == recipeArguments && concreteRepresentation info
        (_, recipe) -> scalarRecipe recipe && concreteRepresentation info
    concreteRepresentation = maybe False (const True) . representationForRecipe catalog . typedNodeRecipe
    unsupportedExpressionFailure = expressionFailure LoweredIRUnsupportedPattern
    incompleteFailure = expressionFailure LoweredIRIncompletePatternCase
    expressionFailure kind =
      LoweredIRLoweringFailure
        (TypedExpressionPath modulePath statementPath expressionPath)
        kind
        LoweredIRNoFailureDetail

normalizePattern ::
  ManagedLayoutCatalog ->
  [Text] ->
  [Int] ->
  [Int] ->
  Bool ->
  TypedNodeInfo ->
  TypedPattern ->
  Either LoweredIRLoweringFailure ManagedPattern
normalizePattern catalog modulePath statementPath patternPath allowAlternative expectedInfo patternValue
  | not (matchingInfo expectedInfo (patternInfo patternValue)) = unsupported
  | otherwise =
      case patternValue of
        TypedWildcardPattern info -> Right (ManagedWildcard info)
        TypedVariablePattern info binder _ -> Right (ManagedVariable info binder)
        TypedLiteralPattern info literal
          | scalarRecipe (typedNodeRecipe info), not (textLiteral literal) -> Right (ManagedLiteral info literal)
          | otherwise -> unsupported
        TypedConstructorPattern _ name children -> do
          constructor <- maybe unsupported Right (managedPatternConstructorFor catalog name expectedInfo)
          normalizedChildren <- normalizeChildren (managedPatternConstructorFields constructor) children
          Right (ManagedConstructor constructor normalizedChildren)
        TypedTuplePattern info children ->
          case tupleFields info of
            Just (layoutId, fieldInfos) -> ManagedTuple info layoutId <$> normalizeChildren fieldInfos children
            Nothing -> unsupported
        TypedAsPattern info binder _ nested
          | managedStructuredInfo expectedInfo ->
              ManagedAs info binder <$> normalizePattern catalog modulePath statementPath (patternPath <> [0]) False expectedInfo nested
        TypedAsPattern {} -> unsupported
        TypedOrPattern info alternatives
          | allowAlternative,
            managedStructuredInfo expectedInfo,
            Just nonEmptyAlternatives <- NonEmpty.nonEmpty alternatives -> do
              normalizedAlternatives <-
                traverse
                  (uncurry (normalizeAlternative expectedInfo))
                  (NonEmpty.zip (0 :| [1 ..]) nonEmptyAlternatives)
              Right (ManagedOr info (canonicalizeAlternatives normalizedAlternatives))
        TypedOrPattern {} -> unsupported
        TypedListPattern {} -> unsupported
        TypedConsListPattern {} -> unsupported
  where
    unsupported =
      Left
        ( LoweredIRLoweringFailure
            (TypedPatternPath modulePath statementPath patternPath)
            LoweredIRUnsupportedPattern
            LoweredIRNoFailureDetail
        )
    normalizeChildren expectedChildren actualChildren
      | length expectedChildren == length actualChildren =
          sequence
            [ normalizePattern catalog modulePath statementPath (patternPath <> [index]) False childInfo childPattern
            | (index, childInfo, childPattern) <- zip3 [0 ..] expectedChildren actualChildren
            ]
      | otherwise = unsupported
    normalizeAlternative info index alternative =
      normalizePattern catalog modulePath statementPath (patternPath <> [index]) False info alternative
    tupleFields info =
      case (typedNodeType info, typedNodeRecipe info, representationForRecipe catalog (typedNodeRecipe info)) of
        (TypedTupleType types@(_ : _), TypedManagedProductRecipe recipes, Just (LoweredManagedReferenceRepresentation layoutId))
          | length types == length recipes,
            let fieldInfos = zipWith (\typeValue recipe -> TypedNodeInfo typeValue recipe [] []) types recipes,
            productLayoutFields catalog layoutId == traverse (representationForRecipe catalog . typedNodeRecipe) fieldInfos ->
              Just (layoutId, fieldInfos)
        _ -> Nothing

patternInfo :: TypedPattern -> TypedNodeInfo
patternInfo patternValue =
  case patternValue of
    TypedWildcardPattern info -> info
    TypedVariablePattern info _ _ -> info
    TypedLiteralPattern info _ -> info
    TypedConstructorPattern info _ _ -> info
    TypedListPattern info _ -> info
    TypedConsListPattern info _ _ -> info
    TypedTuplePattern info _ -> info
    TypedAsPattern info _ _ _ -> info
    TypedOrPattern info _ -> info

matchingInfo :: TypedNodeInfo -> TypedNodeInfo -> Bool
matchingInfo expected actual =
  typedNodeType expected == typedNodeType actual
    && typedNodeRecipe expected == typedNodeRecipe actual

managedStructuredInfo :: TypedNodeInfo -> Bool
managedStructuredInfo info =
  case (typedNodeType info, typedNodeRecipe info) of
    (TypedTupleType (_ : _), TypedManagedProductRecipe (_ : _)) -> True
    (TypedDataType name arguments, TypedManagedVariantRecipe recipeName recipeArguments) ->
      name == recipeName && arguments == recipeArguments
    _ -> False

scalarRecipe :: TypedRepresentationRecipe -> Bool
scalarRecipe recipe =
  case recipe of
    TypedUnitRecipe -> True
    TypedBoolRecipe -> True
    TypedSignedIntegerRecipe _ -> True
    TypedUnsignedIntegerRecipe _ -> True
    TypedFloatRecipe _ -> True
    TypedCharRecipe -> True
    _ -> False

textLiteral :: TypedLiteral -> Bool
textLiteral literal =
  case literal of
    TypedTextLiteral _ -> True
    _ -> False

hasNonFinalUnguardedCatchAll :: [ManagedPatternArm] -> Bool
hasNonFinalUnguardedCatchAll arms =
  any unguardedCatchAll (dropLast arms)
  where
    unguardedCatchAll arm =
      managedPatternArmGuard arm == Nothing
        && irrefutablePattern (managedPatternArmPattern arm)
    dropLast [] = []
    dropLast values = init values

patternMatrixIsTotal :: ManagedLayoutCatalog -> TypedNodeInfo -> [ManagedPatternArm] -> Bool
patternMatrixIsTotal catalog scrutineeInfo arms =
  not (matrixHasWitness catalog [scrutineeInfo] rows)
  where
    rows =
      [ [alternative]
      | arm <- arms,
        managedPatternArmGuard arm == Nothing,
        alternative <- topLevelAlternatives (managedPatternArmPattern arm)
      ]

topLevelAlternatives :: ManagedPattern -> [ManagedPattern]
topLevelAlternatives patternValue =
  case patternValue of
    ManagedOr _ alternatives -> NonEmpty.toList alternatives
    _ -> [patternValue]

data MatrixConstructor
  = MatrixProduct LoweredLayoutId [TypedNodeInfo]
  | MatrixVariant ManagedPatternConstructor

matrixHasWitness :: ManagedLayoutCatalog -> [TypedNodeInfo] -> [[ManagedPattern]] -> Bool
matrixHasWitness _ [] rows = null rows
matrixHasWitness catalog (info : laterInfos) rows =
  case matrixConstructors catalog info of
    Just constructors ->
      any
        (\constructor -> matrixHasWitness catalog (matrixConstructorFields constructor <> laterInfos) (concatMap (specializeRows constructor) rows))
        constructors
    Nothing ->
      matrixHasWitness catalog laterInfos (mapMaybe defaultRow rows)

matrixConstructors :: ManagedLayoutCatalog -> TypedNodeInfo -> Maybe [MatrixConstructor]
matrixConstructors catalog info =
  case (typedNodeType info, typedNodeRecipe info, representationForRecipe catalog (typedNodeRecipe info)) of
    (TypedTupleType types@(_ : _), TypedManagedProductRecipe recipes, Just (LoweredManagedReferenceRepresentation layoutId))
      | length types == length recipes ->
          Just [MatrixProduct layoutId (zipWith (\typeValue recipe -> TypedNodeInfo typeValue recipe [] []) types recipes)]
    (TypedDataType {}, TypedManagedVariantRecipe {}, _) ->
      map MatrixVariant <$> managedPatternConstructorsFor catalog info
    _ -> Nothing

matrixConstructorFields :: MatrixConstructor -> [TypedNodeInfo]
matrixConstructorFields constructor =
  case constructor of
    MatrixProduct _ fields -> fields
    MatrixVariant variant -> managedPatternConstructorFields variant

specializeRows :: MatrixConstructor -> [ManagedPattern] -> [[ManagedPattern]]
specializeRows _ [] = []
specializeRows constructor (patternValue : laterPatterns) =
  case patternValue of
    ManagedWildcard _ -> wildcardFields
    ManagedVariable _ _ -> wildcardFields
    ManagedAs _ _ nested -> specializeRows constructor (nested : laterPatterns)
    ManagedTuple _ layoutId children ->
      case constructor of
        MatrixProduct expectedLayout _
          | layoutId == expectedLayout -> [children <> laterPatterns]
        _ -> []
    ManagedConstructor actual children ->
      case constructor of
        MatrixVariant expected
          | sameVariantConstructor expected actual -> [children <> laterPatterns]
        _ -> []
    ManagedOr _ alternatives ->
      concatMap (\alternative -> specializeRows constructor (alternative : laterPatterns)) (NonEmpty.toList alternatives)
    ManagedLiteral {} -> []
  where
    wildcardFields = [map ManagedWildcard (matrixConstructorFields constructor) <> laterPatterns]

defaultRow :: [ManagedPattern] -> Maybe [ManagedPattern]
defaultRow [] = Nothing
defaultRow (patternValue : laterPatterns)
  | irrefutablePattern patternValue = Just laterPatterns
  | otherwise = Nothing

irrefutablePattern :: ManagedPattern -> Bool
irrefutablePattern patternValue =
  case patternValue of
    ManagedWildcard _ -> True
    ManagedVariable _ _ -> True
    ManagedAs _ _ nested -> irrefutablePattern nested
    ManagedOr _ alternatives -> any irrefutablePattern alternatives
    _ -> False

type BinderContract = (TypedCoreName, TypedType, TypedRepresentationRecipe)

canonicalizeAlternatives :: NonEmpty ManagedPattern -> NonEmpty ManagedPattern
canonicalizeAlternatives (firstAlternative :| laterAlternatives) =
  firstAlternative :| map (canonicalizePattern canonicalBinders) laterAlternatives
  where
    canonicalBinders = patternBinderContracts firstAlternative

canonicalizePattern :: [(BinderContract, TypedBinderId)] -> ManagedPattern -> ManagedPattern
canonicalizePattern canonicalBinders patternValue =
  case patternValue of
    ManagedVariable info binder -> ManagedVariable info (canonicalBinder info binder)
    ManagedConstructor constructor children -> ManagedConstructor constructor (map (canonicalizePattern canonicalBinders) children)
    ManagedTuple info layoutId children -> ManagedTuple info layoutId (map (canonicalizePattern canonicalBinders) children)
    ManagedAs info binder nested ->
      ManagedAs info (canonicalBinder info binder) (canonicalizePattern canonicalBinders nested)
    ManagedOr info alternatives -> ManagedOr info (fmap (canonicalizePattern canonicalBinders) alternatives)
    _ -> patternValue
  where
    canonicalBinder info binder =
      maybe binder id (lookup (binderContract info binder) canonicalBinders)

patternBinderContracts :: ManagedPattern -> [(BinderContract, TypedBinderId)]
patternBinderContracts patternValue =
  case patternValue of
    ManagedVariable info binder -> [(binderContract info binder, binder)]
    ManagedConstructor _ children -> concatMap patternBinderContracts children
    ManagedTuple _ _ children -> concatMap patternBinderContracts children
    ManagedAs info binder nested -> (binderContract info binder, binder) : patternBinderContracts nested
    ManagedOr _ alternatives -> patternBinderContracts (NonEmpty.head alternatives)
    _ -> []

binderContract :: TypedNodeInfo -> TypedBinderId -> BinderContract
binderContract info (TypedBinderId (_, _, name)) =
  (name, typedNodeType info, typedNodeRecipe info)

sameVariantConstructor :: ManagedPatternConstructor -> ManagedPatternConstructor -> Bool
sameVariantConstructor left right =
  managedPatternConstructorName left == managedPatternConstructorName right
    && managedPatternConstructorLayout left == managedPatternConstructorLayout right
