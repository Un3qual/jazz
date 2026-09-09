{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CorePhase (Lowered),
    Expr (..),
    Statement (..),
  )
import Jazz.Compiler.Diagnostics.Render (renderDiagnostic)
import Jazz.Compiler.Name
  ( UnresolvedName,
    mkIdentifier,
    operatorBindingName,
    sourceName,
  )
import Jazz.Compiler.Parser (parseSurfaceProgram)
import Jazz.Compiler.Parser.Lower (lowerSurfaceExpr)
import Jazz.Compiler.RecursiveBindings
  ( PreparedRecursiveScope,
    buildRecursiveScopeFacts,
    collectBindingNames,
    collectLambdaCaptureHints,
    freeVarsExprWithBound,
    freeVarsScopeWithBound,
    inferRecursiveGroupsOrdered,
    inferSelfRecursiveBindings,
    lambdaCaptureHintsChild,
    lookupLambdaCapturedNames,
    lookupLambdaCapturedNamesOrdered,
    prepareRecursiveScope,
    preparedRecursiveScopeBindingNames,
    preparedRecursiveScopeGroups,
    preparedRecursiveScopeOuterBindingNames,
    preparedRecursiveScopeStatements,
    recursiveScopeBindingNames,
    recursiveScopeGroups,
  )
import Jazz.TestHarness (NamedTest, assertEqual, runTestSuite)

main :: IO ()
main = runTestSuite "RecursiveBindings" tests

tests :: [NamedTest]
tests =
  [ ("collect binding names keeps let declaration indices", testCollectBindingNames),
    ("recursive scope facts own binding names and ordered groups together", testRecursiveScopeFacts),
    ("prepared recursive scopes own exact statements and derived maps", testPreparedRecursiveScope),
    ("lambda capture plans address nested lambdas without AST keys", testLambdaCapturePlans),
    ("lambda capture plans preserve first occurrence order", testLambdaCaptureOrder),
    ("free vars treat lambda parameters as bound", testFreeVarsLambdaParameterBound),
    ("ordinary binding initializers keep their own name free", testFreeVarsScopeKeepsOrdinaryInitializerNameFree),
    ("ordinary binding initializers resolve an outer same-name binding", testFreeVarsScopeResolvesOuterInitializerName),
    ("ordinary rebinding initializers resolve the nearest prior local binding", testFreeVarsScopeResolvesPriorLocalInitializerName),
    ("recursive groups keep singleton self-recursive bindings", testRecursiveGroupsKeepSingletonSelfRecursion),
    ("recursive groups keep top-level self-recursive lambdas", testRecursiveGroupsKeepTopLevelSelfRecursiveLambda),
    ("recursive groups keep top-level mutually recursive lambdas", testRecursiveGroupsKeepTopLevelMutualLambdas),
    ("recursive groups ignore same-name non-alias references", testRecursiveGroupsIgnoreSameNameNonAliasReference),
    ("recursive groups ignore mixed alias and eager self wrapper branches", testRecursiveGroupsIgnoreMixedAliasAndEagerSelfWrapper),
    ("recursive groups ignore eager block statements before alias terminal", testRecursiveGroupsIgnoreEagerBlockStatementsBeforeAliasTerminal),
    ("recursive groups ignore eager self use before an unrelated callable result", testRecursiveGroupsIgnoreEagerSelfBeforeCallableResult),
    ("recursive groups keep pattern binders from resolving through prior callables", testRecursiveGroupsRespectPatternBinderFunctionShadowing),
    ("recursive groups resolve aliases in their definition-site pattern scope", testRecursiveGroupsRespectAliasDefinitionPatternScope),
    ("recursive groups keep a callable pattern case with a guarded self-reference", testRecursiveGroupsKeepCallablePatternGuardSelfReference),
    ("recursive groups follow a block alias to the nearest prior callable rebinding", testRecursiveGroupsFollowPriorBlockCallableRebinding),
    ("recursive groups follow forward aliases within a nested recursive group", testRecursiveGroupsFollowNestedRecursiveForwardAlias),
    ("recursive groups reject ordinary nested forward callable aliases", testRecursiveGroupsRejectNestedNonRecursiveForwardAlias),
    ("recursive groups use the latest callable block rebinding", testRecursiveGroupsUseLatestBlockCallableRebinding),
    ("recursive groups let a scalar block rebinding hide a prior callable", testRecursiveGroupsPreferLatestScalarBlockRebinding),
    ("recursive groups ignore eager self operator use in a conditional", testRecursiveGroupsIgnoreEagerOperatorConditional),
    ("recursive groups retain alias-only operator self cycles", testRecursiveGroupsKeepAliasOnlyOperatorSelfCycle),
    ("recursive groups suppress singleton self edge when outer binding exists", testRecursiveGroupsPreferOuterBindingForSingletonName),
    ("nested local self recursion stays local to the block", testFreeVarsScopeKeepsNestedSelfRecursionLocal),
    ("nested block SCC free vars stay local to the block", testFreeVarsScopeKeepsNestedRecursivePeersLocal),
    ("nested ordinary initializers retain enclosing mutual recursion", testRecursiveGroupsKeepNestedInitializerMutualRecursion),
    ("nested aliases resolve a nearest prior outer declaration", testRecursiveGroupsKeepNestedPriorOuterAliasMutualRecursion),
    ("nested conditional aliases resolve a nearest prior outer declaration", testRecursiveGroupsKeepNestedPriorOuterConditionalAliasMutualRecursion),
    ("nested aliases without an outer declaration remain local self cycles", testRecursiveGroupsKeepNoOuterNestedAliasLocal),
    ("nested aliases do not resolve to the current enclosing declaration", testRecursiveGroupsKeepCurrentNestedAliasLocal),
    ("nested self-recursive lambdas stay out of enclosing SCCs", testRecursiveGroupsKeepNestedSelfRecursiveLambdaLocal),
    ("nested conditional self-recursive lambdas stay out of enclosing SCCs", testRecursiveGroupsKeepNestedConditionalSelfRecursiveLambdaLocal),
    ("recursive groups do not leak nested block SCC peers to outer scope", testRecursiveGroupsDoNotLeakNestedBlockPeers),
    ("recursive groups preserve declaration order through alias bridge", testRecursiveGroupsPreserveDeclarationOrder),
    ("recursive groups prefer nearest earlier rebinding over later declaration", testRecursiveGroupsPreferNearestEarlierRebinding),
    ("self-recursive binding detection is parameterized by caller predicate", testInferSelfRecursiveBindingsIsParameterized),
    ("self-recursive binding detection respects outer names", testInferSelfRecursiveBindingsRespectsOuterNames)
  ]

testCollectBindingNames :: IO ()
testCollectBindingNames =
  assertEqual "binding names" (Map.fromList [(0, "x"), (2, "y")]) (collectBindingNames (indexedAt [0, 1, 2] "x = 1. x :: Int. y = x."))

testRecursiveScopeFacts :: IO ()
testRecursiveScopeFacts = do
  assertEqual "scope fact binding names" (Map.fromList [(0, "left"), (2, "right")]) (recursiveScopeBindingNames facts)
  assertEqual "scope fact recursive groups" (Map.fromList [(0, [0, 2]), (2, [0, 2])]) (recursiveScopeGroups facts)
  where
    facts = buildRecursiveScopeFacts Set.empty (indexedAt [0, 1, 2] "left = \\(item) -> right. 0. right = \\(item) -> left.")

testPreparedRecursiveScope :: IO ()
testPreparedRecursiveScope = do
  assertEqual "prepared statements" statements (preparedRecursiveScopeStatements preparedScope)
  assertEqual "prepared binding names" (Map.fromList [(0, "left"), (2, "right")]) (preparedRecursiveScopeBindingNames preparedScope)
  assertEqual "prepared recursive groups" (Map.fromList [(0, [0, 2]), (2, [0, 2])]) (preparedRecursiveScopeGroups preparedScope)
  assertEqual "prepared outer binding names" (Set.singleton (ident "outside")) (preparedRecursiveScopeOuterBindingNames preparedScope)
  where
    preparedScope :: PreparedRecursiveScope 'Lowered
    preparedScope = prepareRecursiveScope (Set.singleton (ident "outside")) statements
    statements = programStatements "left = \\(item) -> right. 0. right = \\(item) -> left."

testLambdaCapturePlans :: IO ()
testLambdaCapturePlans = do
  assertEqual "outer lambda captures" (Just (Set.singleton (ident "outside"))) (fst <$> lookupLambdaCapturedNames outerLambdaHints)
  assertEqual "ordered outer lambda captures" (Just [ident "outside"]) (fst <$> lookupLambdaCapturedNamesOrdered outerLambdaHints)
  assertEqual "nested lambda captures" (Just (Set.fromList [ident "outside", ident "outer"])) (fst <$> lookupLambdaCapturedNames nestedLambdaHints)
  assertEqual "ordered nested lambda captures" (Just [ident "outer", ident "outside"]) (fst <$> lookupLambdaCapturedNamesOrdered nestedLambdaHints)
  where
    rootHints = collectLambdaCaptureHints expression
    outerLambdaHints = lambdaCaptureHintsChild 1 rootHints
    nestedBodyHints = maybe rootHints snd (lookupLambdaCapturedNames outerLambdaHints)
    nestedLambdaHints = lambdaCaptureHintsChild 0 nestedBodyHints
    expression = fixtureExpression "consume (\\(outer) -> (\\(inner) -> (outer, inner, outside)) outer)."

testLambdaCaptureOrder :: IO ()
testLambdaCaptureOrder = do
  assertEqual "ordered captures deduplicate by first occurrence and exclude the parameter" (Just [ident "right", ident "left"]) (fst <$> lookupLambdaCapturedNamesOrdered rootHints)
  assertEqual "ordered captures exclude a prior block-local binding" (Just [ident "outside", ident "tail"]) (fst <$> lookupLambdaCapturedNamesOrdered blockHints)
  where
    rootHints = collectLambdaCaptureHints (fixtureExpression "probe = \\(item) -> (right, left, right, item).")
    blockHints = collectLambdaCaptureHints (fixtureExpression "probe = \\(item) -> { local = outside. (local, outside, tail, item). }.")

testFreeVarsLambdaParameterBound :: IO ()
testFreeVarsLambdaParameterBound =
  assertEqual "lambda free vars" (Set.singleton "y") (freeVarsExprWithBound Set.empty (fixtureExpression "probe = \\(x) -> x y."))

testFreeVarsScopeKeepsOrdinaryInitializerNameFree :: IO ()
testFreeVarsScopeKeepsOrdinaryInitializerNameFree =
  assertEqual "scope free vars" (Set.fromList ["f", "g"]) (freeVarsScopeWithBound Set.empty (programStatements "f = f g."))

testFreeVarsScopeResolvesOuterInitializerName :: IO ()
testFreeVarsScopeResolvesOuterInitializerName =
  assertEqual "outer same-name binding is not free" (Set.singleton "g") (freeVarsScopeWithBound (Set.singleton "f") (programStatements "f = f g."))

testFreeVarsScopeResolvesPriorLocalInitializerName :: IO ()
testFreeVarsScopeResolvesPriorLocalInitializerName =
  assertEqual "nearest prior local binding is not free" (Set.singleton "g") (freeVarsScopeWithBound Set.empty (programStatements "f = 0. f = f g."))

testRecursiveGroupsKeepSingletonSelfRecursion :: IO ()
testRecursiveGroupsKeepSingletonSelfRecursion = assertGroups "singleton self-recursive group" (Map.fromList [(0, [0])]) [0] "f = f."

testRecursiveGroupsKeepTopLevelSelfRecursiveLambda :: IO ()
testRecursiveGroupsKeepTopLevelSelfRecursiveLambda = assertGroups "top-level lambda self recursion" (Map.fromList [(1, [1])]) [1] "loop = \\(item) -> loop item."

testRecursiveGroupsKeepTopLevelMutualLambdas :: IO ()
testRecursiveGroupsKeepTopLevelMutualLambdas = assertGroups "top-level lambda mutual recursion" (Map.fromList [(1, [1, 3]), (3, [1, 3])]) [1, 3] "left = \\(item) -> right item. right = \\(item) -> left item."

testRecursiveGroupsIgnoreSameNameNonAliasReference :: IO ()
testRecursiveGroupsIgnoreSameNameNonAliasReference = assertGroups "same-name non-alias reference does not create self edge" Map.empty [0] "f = (\\(x) -> x) f."

testRecursiveGroupsIgnoreMixedAliasAndEagerSelfWrapper :: IO ()
testRecursiveGroupsIgnoreMixedAliasAndEagerSelfWrapper = assertGroups "mixed alias and eager self wrapper does not create self edge" Map.empty [0] "f = if True then f + 1 else f."

testRecursiveGroupsIgnoreEagerBlockStatementsBeforeAliasTerminal :: IO ()
testRecursiveGroupsIgnoreEagerBlockStatementsBeforeAliasTerminal = assertGroups "eager block statement before alias terminal does not create self edge" Map.empty [0] "f = { f + 1. f. }."

testRecursiveGroupsIgnoreEagerSelfBeforeCallableResult :: IO ()
testRecursiveGroupsIgnoreEagerSelfBeforeCallableResult = assertGroups "eager self use is not owned by an unrelated callable result" Map.empty [0] "f = { f True. \\(x) -> x. }."

testRecursiveGroupsRespectPatternBinderFunctionShadowing :: IO ()
testRecursiveGroupsRespectPatternBinderFunctionShadowing =
  assertGroups "pattern-bound scalar does not manufacture a recursive function owner" Map.empty [0] "f = { apparent = \\(x) -> x. captured = \\(x) -> f. case True { | apparent -> apparent }. }."

testRecursiveGroupsRespectAliasDefinitionPatternScope :: IO ()
testRecursiveGroupsRespectAliasDefinitionPatternScope =
  assertGroups "pattern-bound use site does not hide an alias initializer's prior callable" (Map.fromList [(0, [0])]) [0] "f = { target = \\(x) -> f. alias = target. case True { | target -> alias }. }."

testRecursiveGroupsKeepCallablePatternGuardSelfReference :: IO ()
testRecursiveGroupsKeepCallablePatternGuardSelfReference =
  assertGroups "callable pattern-case guard owns its self-reference" (Map.fromList [(0, [0])]) [0] "f = case 1 { | 1 if f 0 == 0 -> \\(x) -> x | _ -> \\(x) -> x }."

testRecursiveGroupsFollowPriorBlockCallableRebinding :: IO ()
testRecursiveGroupsFollowPriorBlockCallableRebinding =
  assertGroups "same-name block alias follows the nearest prior callable declaration" (Map.fromList [(0, [0])]) [0] "f = { inner = \\(x) -> f x. inner = inner. inner. }."

testRecursiveGroupsFollowNestedRecursiveForwardAlias :: IO ()
testRecursiveGroupsFollowNestedRecursiveForwardAlias =
  assertGroups "nested recursive peer makes its forward alias callable-producing" (Map.fromList [(0, [0])]) [0] "f = { a = b. b = if False then a else \\(x) -> f x. a. }."

testRecursiveGroupsRejectNestedNonRecursiveForwardAlias :: IO ()
testRecursiveGroupsRejectNestedNonRecursiveForwardAlias =
  assertGroups "ordinary nested forward declaration does not make its alias callable-producing" Map.empty [0] "f = { a = b. b = \\(x) -> f x. a. }."

testRecursiveGroupsUseLatestBlockCallableRebinding :: IO ()
testRecursiveGroupsUseLatestBlockCallableRebinding =
  assertGroups "terminal block name uses the latest callable declaration" (Map.fromList [(0, [0])]) [0] "f = { inner = True. inner = \\(x) -> f x. inner. }."

testRecursiveGroupsPreferLatestScalarBlockRebinding :: IO ()
testRecursiveGroupsPreferLatestScalarBlockRebinding =
  assertGroups "terminal block name does not reach through the latest scalar declaration" Map.empty [0] "f = { inner = \\(x) -> f x. inner = True. inner. }."

testRecursiveGroupsIgnoreEagerOperatorConditional :: IO ()
testRecursiveGroupsIgnoreEagerOperatorConditional =
  assertEqual "eager operator condition prevents alias-only self ownership" Map.empty (inferRecursiveGroupsOrdered Set.empty [(0, operatorLet "operator %% tier 2. (%%) = if True %% False then (%%) else (%%).")])

testRecursiveGroupsKeepAliasOnlyOperatorSelfCycle :: IO ()
testRecursiveGroupsKeepAliasOnlyOperatorSelfCycle =
  assertEqual "operator value alias retains self ownership" (Map.fromList [(0, [0])]) (inferRecursiveGroupsOrdered Set.empty [(0, operatorLet "operator %% tier 2. (%%) = (%%).")])

testRecursiveGroupsPreferOuterBindingForSingletonName :: IO ()
testRecursiveGroupsPreferOuterBindingForSingletonName =
  assertEqual "outer singleton binding suppresses self edge" Map.empty (inferRecursiveGroupsOrdered (Set.singleton "f") (indexedAt [0] "f = f."))

testFreeVarsScopeKeepsNestedSelfRecursionLocal :: IO ()
testFreeVarsScopeKeepsNestedSelfRecursionLocal = assertEqual "nested self-recursive name stays local to block" Set.empty (freeVarsScopeWithBound Set.empty (programStatements "loop = loop. loop."))

testFreeVarsScopeKeepsNestedRecursivePeersLocal :: IO ()
testFreeVarsScopeKeepsNestedRecursivePeersLocal = assertEqual "nested recursive peer names stay local to block" Set.empty (freeVarsScopeWithBound Set.empty (programStatements "y = z. z = y. y."))

testRecursiveGroupsKeepNestedInitializerMutualRecursion :: IO ()
testRecursiveGroupsKeepNestedInitializerMutualRecursion =
  assertGroups "nested ordinary initializers preserve enclosing owner edges" (Map.fromList [(1, [1, 3]), (3, [1, 3])]) [1, 3] "left = \\(item) -> { right = right item. item. }. right = \\(item) -> { left = left item. item. }."

testRecursiveGroupsKeepNestedPriorOuterAliasMutualRecursion :: IO ()
testRecursiveGroupsKeepNestedPriorOuterAliasMutualRecursion = assertGroups "nested alias preserves prior outer mutual edge" mutualGroup [1, 3] (nestedPriorOuterSource "left")

testRecursiveGroupsKeepNestedPriorOuterConditionalAliasMutualRecursion :: IO ()
testRecursiveGroupsKeepNestedPriorOuterConditionalAliasMutualRecursion = assertGroups "nested conditional alias preserves prior outer mutual edge" mutualGroup [1, 3] (nestedPriorOuterSource "if True then left else left")

testRecursiveGroupsKeepNoOuterNestedAliasLocal :: IO ()
testRecursiveGroupsKeepNoOuterNestedAliasLocal = assertGroups "nested alias keeps a local self cycle instead of resolving to a future outer peer" Map.empty [1, 3] "owner = { local = local. 0. }. local = owner."

testRecursiveGroupsKeepCurrentNestedAliasLocal :: IO ()
testRecursiveGroupsKeepCurrentNestedAliasLocal = assertGroups "nested alias does not manufacture an enclosing self edge" Map.empty [1] "owner = { owner = owner. 0. }."

testRecursiveGroupsKeepNestedSelfRecursiveLambdaLocal :: IO ()
testRecursiveGroupsKeepNestedSelfRecursiveLambdaLocal = assertGroups "nested self-recursive lambda does not form an enclosing mutual SCC" Map.empty [1, 3] (nestedSelfRecursiveLambdaSource "\\(nested) -> loop nested")

testRecursiveGroupsKeepNestedConditionalSelfRecursiveLambdaLocal :: IO ()
testRecursiveGroupsKeepNestedConditionalSelfRecursiveLambdaLocal = assertGroups "nested conditional self-recursive lambda does not form an enclosing mutual SCC" Map.empty [1, 3] (nestedSelfRecursiveLambdaSource "if True then \\(nested) -> loop nested else \\(nested) -> nested")

testRecursiveGroupsDoNotLeakNestedBlockPeers :: IO ()
testRecursiveGroupsDoNotLeakNestedBlockPeers = assertGroups "nested block recursive peer names do not form outer SCC" Map.empty [0, 1] "x = { y = z. z = y. y. }. z = x."

testRecursiveGroupsPreserveDeclarationOrder :: IO ()
testRecursiveGroupsPreserveDeclarationOrder = assertGroups "ordered recursive group" (Map.fromList [(0, [0, 1, 2]), (1, [0, 1, 2]), (2, [0, 1, 2])]) [0, 1, 2] "f = h. h = g. g = f."

testRecursiveGroupsPreferNearestEarlierRebinding :: IO ()
testRecursiveGroupsPreferNearestEarlierRebinding = assertGroups "nearest earlier rebinding wins" Map.empty [0, 1, 2] "x = 0. f = x. x = f."

testInferSelfRecursiveBindingsIsParameterized :: IO ()
testInferSelfRecursiveBindingsIsParameterized = do
  assertEqual "wrapped lambda policy marks self recursion" (Set.singleton 0) (inferSelfRecursiveBindings Set.empty hasWrappedLambdaBranch indexedStatements)
  assertEqual "bare lambda policy does not mark wrapped self recursion" Set.empty (inferSelfRecursiveBindings Set.empty isBareLambda indexedStatements)
  where
    indexedStatements = indexedAt [0] "f = if True then \\(x) -> f x else 0."
    hasWrappedLambdaBranch (EIf _ _ (ELambda {}) _) = True
    hasWrappedLambdaBranch _ = False
    isBareLambda ELambda {} = True
    isBareLambda _ = False

testInferSelfRecursiveBindingsRespectsOuterNames :: IO ()
testInferSelfRecursiveBindingsRespectsOuterNames =
  assertEqual "an outer builtin-like name suppresses a self-recursive function cell" Set.empty (inferSelfRecursiveBindings (Set.singleton (ident "map")) isLambda (indexedAt [0] "map = \\(items) -> map items."))
  where
    isLambda ELambda {} = True
    isLambda _ = False

mutualGroup :: Map.Map Int [Int]
mutualGroup = Map.fromList [(1, [1, 3]), (3, [1, 3])]

assertGroups :: Text -> Map.Map Int [Int] -> [Int] -> Text -> IO ()
assertGroups label expected indices source = assertEqual label expected (inferRecursiveGroupsOrdered Set.empty (letStatementsAt indices source))

nestedPriorOuterSource :: Text -> Text
nestedPriorOuterSource nestedAlias = "left = \\(item) -> right item. right = \\(item) -> { left = " <> nestedAlias <> ". item. }."

nestedSelfRecursiveLambdaSource :: Text -> Text
nestedSelfRecursiveLambdaSource localLoop = "owner = \\(item) -> { loop = " <> localLoop <> ". item. }. loop = \\(item) -> owner item."

operatorLet :: Text -> Statement 'Lowered
operatorLet source =
  case [statement | statement@(SLet _ name _) <- programStatements source, name == operatorBindingName "%%"] of
    [statement] -> statement
    statements -> error ("expected one lowered operator binding, got " <> show statements)

letStatementsAt :: [Int] -> Text -> [(Int, Statement 'Lowered)]
letStatementsAt indices source = pairIndices indices [statement | statement@SLet {} <- programStatements source]

indexedAt :: [Int] -> Text -> [(Int, Statement 'Lowered)]
indexedAt indices = pairIndices indices . programStatements

pairIndices :: [Int] -> [Statement 'Lowered] -> [(Int, Statement 'Lowered)]
pairIndices indices statements
  | length indices == length statements = zip indices statements
  | otherwise = error ("fixture index count mismatch: " <> show (indices, statements))

fixtureExpression :: Text -> Expr 'Lowered
fixtureExpression source =
  case programStatements source of
    [SExpr _ expression] -> expression
    [SLet _ _ expression] -> expression
    statements -> error ("expected one expression fixture, got " <> show statements)

programStatements :: Text -> [Statement 'Lowered]
programStatements source =
  case loweredProgram source of
    EBlock _ statements -> statements
    expression -> error ("expected lowered program block, got " <> show expression)

loweredProgram :: Text -> Expr 'Lowered
loweredProgram source =
  case parseSurfaceProgram source of
    Left diagnostic -> error (Text.unpack (renderDiagnostic diagnostic))
    Right surface -> lowerSurfaceExpr surface

ident :: Text -> UnresolvedName
ident = sourceName . mkIdentifier
