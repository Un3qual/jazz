{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List (nub)
import Data.Text (Text)
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
import Jazz.Compiler.Diagnostics (SourceSpan (..))
import Jazz.Compiler.ModuleExports (exportInventory)
import Jazz.Compiler.ModuleResolver (resolveStandaloneExprNames)
import Jazz.Compiler.Parser (parseSurfaceProgram)
import Jazz.Compiler.Parser.Lower (lowerSurfaceExpr)
import Jazz.Compiler.TypeInference
  ( inferExpressionDefault,
  )
import Jazz.Compiler.TypeInference.Result (InferenceResult (inferredExpr))
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertRight,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "CoreNormalization" tests

tests :: [NamedTest]
tests =
  [ ("if remains the canonical boolean conditional", testIfRemainsCanonicalIf),
    ("dollar lowers directly to application", testDollarLowersToApplication),
    ("lowering assigns deterministic pre-order node identities", testDeterministicNodeIdentities),
    ("lowering preserves a complete span on every canonical node", testCompleteCanonicalSpans)
  ]

testIfRemainsCanonicalIf :: IO ()
testIfRemainsCanonicalIf =
  assertRight "parse if" (parseSurfaceProgram "if True then 1 else 2.") $ \surface -> do
    let lowered = lowerSurfaceExpr surface
    assertRight "resolve if" (resolveStandaloneExprNames (exportInventory []) lowered) $ \resolved -> do
      inference <- inferExpressionDefault resolved
      assertEqual "resolved equals inferred" resolved (inferredExpr inference)

testDollarLowersToApplication :: IO ()
testDollarLowersToApplication =
  assertRight "parse dollar" (parseSurfaceProgram "f $ x.") $ \surface ->
    case lowerSurfaceExpr surface of
      EBlock _ [SExpr statementNode (EApply _ (EVar _ "f") (EVar _ "x"))]
        | coreNodeSpan statementNode == SourceRange 1 1 1 2 -> pure ()
      lowered -> assertEqual "canonical dollar shape" "application block" (show lowered)

testDeterministicNodeIdentities :: IO ()
testDeterministicNodeIdentities =
  assertRight "parse nested source" (parseSurfaceProgram nestedSource) $ \surface -> do
    let firstIds = canonicalNodeIds (lowerSurfaceExpr surface)
        secondIds = canonicalNodeIds (lowerSurfaceExpr surface)
    assertEqual "repeat lowering" firstIds secondIds
    assertEqual "strict source pre-order" [CoreNodeId 0 .. CoreNodeId (length firstIds - 1)] firstIds
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
    SClass node _ _ methods -> nodeFact node <> concatMap classMethodNodeFacts methods
    SImpl node _ _ methods -> nodeFact node <> concatMap implMethodNodeFacts methods
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
