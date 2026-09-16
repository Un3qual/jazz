{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    CoreNode (..),
    CoreNodeId (..),
    CorePhase (Lowered),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Pattern (..),
    Statement (..),
  )
import Jazz.Compiler.CoreIdentity (ResolvedNodeFacts (..), ResolvedReference (..))
import Jazz.Compiler.DiagnosticCatalog (ErrorCode (E4005, E4006, E4010), errorCode)
import Jazz.Compiler.Diagnostics (SourceSpan (..), diagnosticCode, diagnosticPrimarySpan, diagnosticSummary)
import Jazz.Compiler.ModuleAnalysis
  ( analyzeResolvedExpression,
  )
import Jazz.Compiler.ModuleExports (exportInventory)
import Jazz.Compiler.ModuleIdentity (mkModulePath, mkSourceFile, moduleIdentity)
import Jazz.Compiler.ModuleResolver (resolveStandaloneExprNames)
import Jazz.Compiler.Name (identifierText)
import Jazz.Compiler.Parser (parseSurfaceProgram)
import Jazz.Compiler.Parser.AST (SurfaceExpr (..), SurfaceExprForm (..), SurfaceStatement (..))
import Jazz.Compiler.Parser.Lower (lowerSurfaceExpr, lowerSurfaceModule)
import Jazz.Compiler.WarningConfig (defaultWarningSettings)
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertRight,
    failTest,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "CoreNormalization" tests

tests :: [NamedTest]
tests =
  [ ("analyzed if remains the canonical boolean conditional", testIfRemainsCanonicalIf),
    ("dollar resolves through its ordinary function", testDollarLowersToApplication),
    ("operator values resolve to callable references", testOperatorValuesResolveToReferences),
    ("declared sections and binary operators resolve to applications", testDeclaredOperatorsResolveToApplications),
    ("lowering assigns deterministic pre-order node identities", testDeterministicNodeIdentities),
    ("lowering preserves a complete span on every canonical node", testCompleteCanonicalSpans),
    ("module lowering preserves declaration and import diagnostics", testModuleLoweringDiagnostics)
  ]

testIfRemainsCanonicalIf :: IO ()
testIfRemainsCanonicalIf =
  assertRight "parse if" (parseSurfaceProgram "if True then 1 else 2.") $ \surface -> do
    let lowered = lowerSurfaceExpr surface
        resolved = resolveStandaloneExprNames (exportInventory []) lowered
    (_, analyzed) <- analyzeResolvedExpression defaultWarningSettings resolved
    assertRight "analyze if" analyzed $ \expression ->
      case expression of
        Just (EBlock _ [SExpr _ EIf {}]) -> pure ()
        _ -> failTest ("expected analyzed conditional block, got " <> Text.pack (show expression))

testDollarLowersToApplication :: IO ()
testDollarLowersToApplication =
  assertRight "parse dollar" (parseSurfaceProgram "f $ x.") $ \surface ->
    case resolveStandaloneExprNames (exportInventory []) (lowerSurfaceExpr surface) of
      EBlock _ [SExpr _ (EApply _ (EApply _ (EVar _ applyName) (EVar _ functionName)) (EVar _ argumentName))] ->
        assertEqual "ordinary application names" ["apply", "f", "x"] (map identifierText [applyName, functionName, argumentName])
      resolved -> failTest ("dollar did not resolve to apply: " <> Text.pack (show resolved))

testOperatorValuesResolveToReferences :: IO ()
testOperatorValuesResolveToReferences =
  assertRight "parse operator value" (parseSurfaceProgram "(+).") $ \surface ->
    case lowerSurfaceExpr surface of
      lowered@(EBlock _ [SExpr _ (EOperatorValue sourceNode "+")]) ->
        let resolved = resolveStandaloneExprNames (exportInventory []) lowered
         in case resolved of
              EBlock _ [SExpr _ (EVar node name)] -> do
                assertEqual "callable name" "add" (identifierText name)
                assertEqual "ordinary function target" (Just (UnresolvedReference name)) (resolvedNodeReference (coreNodeFacts node))
                assertEqual "operator identity" (coreNodeId sourceNode) (coreNodeId node)
                assertEqual "authored operator span" (coreNodeSpan sourceNode) (coreNodeSpan node)
              _ -> assertEqual "resolved operator shape" "callable reference" (show resolved)
      _ -> assertEqual "lowered operator stays canonical" "operator value" (show (lowerSurfaceExpr surface))

testDeclaredOperatorsResolveToApplications :: IO ()
testDeclaredOperatorsResolveToApplications =
  assertRight "parse declared operator forms" (parseSurfaceProgram "operator %% tier 2. (%%) = \\(left, right) -> left - right. ((3 %%) 2, (%% 3) 2, 3 %% 2).") $ \surface -> do
    let lowered = lowerSurfaceExpr surface
        resolved = resolveStandaloneExprNames (exportInventory []) lowered
    let identities = map fst (canonicalNodeFacts resolved)
    assertEqual "generated identities are distinct" (length identities) (length (nub identities))
    assertEqual "authored identities survive normalization" True (all (`elem` identities) (canonicalNodeIds lowered))
    assertEqual "generated spans retain the operator location" True (all (completeSpan . snd) (canonicalNodeFacts resolved))
    case resolved of
      EBlock _ [SLet {}, SExpr _ (ETuple _ [EApply _ (EApply _ ELambda {} _) _, EApply _ (EApply _ ELambda {} _) _, EApply _ (EApply _ EVar {} _) _])] -> pure ()
      _ -> assertEqual "normalized declared operators" "capturing applications and lambdas" (show resolved)

testDeterministicNodeIdentities :: IO ()
testDeterministicNodeIdentities =
  assertRight "parse nested source" (parseSurfaceProgram nestedSource) $ \surface -> do
    let firstIds = canonicalNodeIds (lowerSurfaceExpr surface)
        secondIds = canonicalNodeIds (lowerSurfaceExpr surface)
    assertEqual "repeat lowering" firstIds secondIds
    assertEqual "strict source pre-order" (map CoreNodeId [0 .. length firstIds - 1]) firstIds
    assertEqual "no duplicate IDs" (length firstIds) (length (nub firstIds))

testCompleteCanonicalSpans :: IO ()
testCompleteCanonicalSpans =
  assertRight "parse nested source" (parseSurfaceProgram nestedSource) $ \surface ->
    assertEqual
      "all canonical spans are source locations"
      True
      (all completeSpan (canonicalNodeSpans (lowerSurfaceExpr surface)))

nestedSource :: Text
nestedSource = "f = \\(item) -> case item { | True -> if item then 1 else f item }. x = f True."

completeSpan :: SourceSpan -> Bool
completeSpan spanValue =
  case spanValue of
    SourceSpan line column -> line > 0 && column > 0
    SourceSpanIn sourcePath line column -> not (null sourcePath) && line > 0 && column > 0
    SourceRange line column endLine endColumn -> validRange line column endLine endColumn
    SourceRangeIn sourcePath line column endLine endColumn ->
      not (null sourcePath) && validRange line column endLine endColumn
  where
    validRange line column endLine endColumn =
      all (> 0) [line, column, endLine, endColumn]
        && (line, column) < (endLine, endColumn)

canonicalNodeIds :: Expr 'Lowered -> [CoreNodeId]
canonicalNodeIds = map fst . canonicalNodeFacts

canonicalNodeSpans :: Expr 'Lowered -> [SourceSpan]
canonicalNodeSpans = map snd . canonicalNodeFacts

canonicalNodeFacts :: Expr phase -> [(CoreNodeId, SourceSpan)]
canonicalNodeFacts expression =
  case expression of
    ELit node _ -> nodeFact node
    EVar node _ -> nodeFact node
    ELambda node _ body -> nodeFact node <> canonicalNodeFacts body
    EOperatorValue node _ -> nodeFact node
    EList node items -> nodeFact node <> concatMap canonicalNodeFacts items
    ETuple node items -> nodeFact node <> concatMap canonicalNodeFacts items
    EApply node function argument -> nodeFact node <> canonicalNodeFacts function <> canonicalNodeFacts argument
    ETypeApplication node function _ _ -> nodeFact node <> canonicalNodeFacts function
    EIf node condition trueBranch falseBranch ->
      nodeFact node <> canonicalNodeFacts condition <> canonicalNodeFacts trueBranch <> canonicalNodeFacts falseBranch
    EPatternCase node scrutinee arms -> nodeFact node <> canonicalNodeFacts scrutinee <> concatMap caseArmNodeFacts arms
    EBinary node _ left right -> nodeFact node <> canonicalNodeFacts left <> canonicalNodeFacts right
    ESectionLeft node left _ -> nodeFact node <> canonicalNodeFacts left
    ESectionRight node _ right -> nodeFact node <> canonicalNodeFacts right
    EBlock node statements -> nodeFact node <> concatMap statementNodeFacts statements

caseArmNodeFacts :: CaseArm phase -> [(CoreNodeId, SourceSpan)]
caseArmNodeFacts (CaseArm node patternValue guard body) =
  nodeFact node <> patternNodeFacts patternValue <> maybe [] canonicalNodeFacts guard <> canonicalNodeFacts body

patternNodeFacts :: Pattern phase -> [(CoreNodeId, SourceSpan)]
patternNodeFacts patternValue =
  case patternValue of
    PWildcard node -> nodeFact node
    PVariable node _ -> nodeFact node
    PLiteral node _ -> nodeFact node
    PConstructor node _ patterns -> nodeFact node <> concatMap patternNodeFacts patterns
    PList node patterns -> nodeFact node <> concatMap patternNodeFacts patterns
    PConsList node headPattern tailPattern -> nodeFact node <> patternNodeFacts headPattern <> patternNodeFacts tailPattern
    PTuple node patterns -> nodeFact node <> concatMap patternNodeFacts patterns
    PAs node _ nestedPattern -> nodeFact node <> patternNodeFacts nestedPattern
    POr node alternatives -> nodeFact node <> concatMap patternNodeFacts alternatives

statementNodeFacts :: Statement phase -> [(CoreNodeId, SourceSpan)]
statementNodeFacts statement =
  case statement of
    SLet node _ value -> nodeFact node <> canonicalNodeFacts value
    SSignature node _ _ -> nodeFact node
    SData node _ _ constructors -> nodeFact node <> concatMap dataConstructorNodeFacts constructors
    SClass node _ _ methods _ _ -> nodeFact node <> concatMap classMethodNodeFacts methods
    SImpl node _ _ methods _ -> nodeFact node <> concatMap implMethodNodeFacts methods
    SModule node _ -> nodeFact node
    SImport node _ _ _ -> nodeFact node
    SExpr node value -> nodeFact node <> canonicalNodeFacts value

dataConstructorNodeFacts :: DataConstructor phase -> [(CoreNodeId, SourceSpan)]
dataConstructorNodeFacts (DataConstructor node _ _) = nodeFact node

classMethodNodeFacts :: ClassMethodSignature phase -> [(CoreNodeId, SourceSpan)]
classMethodNodeFacts (ClassMethodSignature node _ _) = nodeFact node

implMethodNodeFacts :: ImplMethod phase -> [(CoreNodeId, SourceSpan)]
implMethodNodeFacts (ImplMethod node _ body) = nodeFact node <> canonicalNodeFacts body

nodeFact :: CoreNode phase sort -> [(CoreNodeId, SourceSpan)]
nodeFact node = [(coreNodeId node, coreNodeSpan node)]

-- These invalid surface trees cannot be constructed by the parser. Keep the
-- lowering boundary covered directly after retiring the comparison harness.
testModuleLoweringDiagnostics :: IO ()
testModuleLoweringDiagnostics =
  forM_ cases $ \(statements, code, message, location) ->
    case lowerSurfaceModule identity (SurfaceExpr spanValue (SEBlock statements)) of
      Left diagnostic -> do
        assertEqual "module diagnostic code" (errorCode code) (diagnosticCode diagnostic)
        assertEqual "module diagnostic" message (diagnosticSummary diagnostic)
        assertEqual "module diagnostic location" location (diagnosticPrimarySpan diagnostic)
      Right _ -> failTest "invalid module lowered successfully"
  where
    identity = moduleIdentity (mkModulePath ("App" :| ["Main"])) (mkSourceFile "src/App/Main.jz")
    spanValue = SourceSpan 2 3
    cases =
      [ ( [SSModule spanValue ["App", "First"] Nothing, SSModule spanValue ["App", "Second"] Nothing],
          E4005,
          "multiple module declarations in 'src/App/Main.jz': App::First, App::Second",
          Nothing
        ),
        ( [SSModule spanValue ["Wrong"] Nothing],
          E4006,
          "module declaration mismatch at 'src/App/Main.jz': expected 'App::Main', found 'Wrong'",
          Nothing
        ),
        ( [SSImport spanValue ["Library"] Nothing (Just [])],
          E4010,
          "invalid empty module import in 'src/App/Main.jz' for 'Library'",
          Just spanValue
        ),
        ( [SSImport spanValue [] Nothing Nothing],
          E4010,
          "invalid empty module import in 'src/App/Main.jz' for ''",
          Just spanValue
        )
      ]
