{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.CanonicalLexerComparison
  ( CanonicalSourcePath,
    normalizeCanonicalSourcePath,
  )
import JazzNext.Compiler.Bootstrap.CanonicalParserComparison
  ( canonicalizeSourceResult,
    renderCanonicalSourceResult,
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runModuleGraph,
    runRuntimeErrors,
  )
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
  )
import JazzNext.Compiler.Parser
  ( parseSurfaceProgramTokensDetailed,
  )
import JazzNext.Compiler.Parser.Lexer
  ( tokenizeDetailed,
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    failTest,
    runTestSuite,
  )
import JazzNext.TestSource
  ( readCheckedInJazzProjectModuleSource,
  )

main :: IO ()
main = runTestSuite "JazzParserOperatorsFullParity" tests

tests :: [NamedTest]
tests =
  [ ("owns centralized operator metadata", testOperatorMetadata),
    ("threads immutable operator context", testOperatorContext),
    ("parses frozen precedence", assertStage0Parity "frozen precedence" "x = 1 + 2 * 3."),
    ("parses frozen left associativity", assertStage0Parity "frozen left associativity" "x = 10 - 3 - 1."),
    ("parses frozen right associativity", assertStage0Parity "frozen right associativity" "x = f $ g $ z."),
    ("parses operator values", assertStage0Parity "operator value" "x = (+)."),
    ("parses left sections", assertStage0Parity "left section" "x = (10 +)."),
    ("parses right sections", assertStage0Parity "right section" "x = (+ 10)."),
    ("keeps application tighter than operators", assertStage0Parity "application precedence" "x = f x + g y * z."),
    ("rejects missing right operands", assertStage0Parity "missing operator operand" "x = 1 +."),
    ("rejects undeclared operators", assertStage0Parity "undeclared operator" "x = 1 %% 2."),
    ("parses fixed-tier declarations", assertStage0Parity "fixed-tier declaration" "operator %% tier 2. x = 1 %% 2 * 3."),
    ("parses numeric precedence declarations", assertStage0Parity "numeric precedence" "operator %% precedence 25. x = 1 + 2 %% 3 * 4."),
    ("parses explicit left associativity", assertStage0Parity "declared left associativity" "operator %% tier 2 left. x = 10 %% 3 %% 1."),
    ("parses explicit right associativity", assertStage0Parity "declared right associativity" "operator <| precedence 10 right. x = a <| b <| c."),
    ("rejects non-associative chains", assertStage0Parity "non-associative chain" "operator ?> precedence 10 nonassoc. x = 1 ?> 2 ?> 3."),
    ("rejects invalid declaration ranges", assertStage0Parity "declaration ranges" "operator %% precedence 100."),
    ("rejects invalid associativity words", assertStage0Parity "associativity word" "operator %% tier 2 sideways."),
    ("rejects built-in redeclarations", assertStage0Parity "built-in redeclaration" "operator + tier 2."),
    ("rejects reserved symbols", assertStage0Parity "reserved operator" "operator -> tier 5."),
    ("rejects invalid symbols", assertStage0Parity "invalid operator symbol" "operator abc tier 2."),
    ("rejects duplicate declarations", assertStage0Parity "duplicate operator" "operator %% tier 2. operator %% tier 3."),
    ("keeps declarations source ordered", assertStage0Parity "forward operator use" "x = 1 %% 2. operator %% tier 2."),
    ("parses declared operator values and sections", assertStage0Parity "declared values and sections" "operator %% tier 2. op = (%%). left = (10 %%). right = (%% 10)."),
    ("parses declared operator signatures and bindings", assertStage0Parity "operator signature binding" "operator %% tier 2. (%%) :: Int -> Int -> Int. (%%) = \\(left, right) -> left + right. x = 1 %% 2."),
    ("rejects undeclared operator signatures", assertStage0Parity "undeclared signature" "(%%) :: Int -> Int -> Int."),
    ("rejects undeclared operator bindings", assertStage0Parity "undeclared binding" "(%%) = \\(left, right) -> left + right."),
    ("rejects nested operator declarations", assertStage0Parity "nested declaration" "x = { operator %% tier 2. 0. }."),
    ("rejects nested operator signatures", assertStage0Parity "nested signature" "operator %% tier 2. x = { (%%) :: Int -> Int -> Int. 0. }."),
    ("rejects nested operator bindings", assertStage0Parity "nested binding" "operator %% tier 2. x = { (%%) = \\(left, right) -> left + right. 0. }."),
    ("isolates module operator context", assertStage0Parity "module operator context" "module Demo { operator %% tier 2. (%%) = \\(left, right) -> left + right. x = 1 %% 2. }"),
    ("parses operators in lambdas", assertStage0Parity "lambda operator body" "f = \\(left, right) -> left + right."),
    ("parses operators in conditionals", assertStage0Parity "conditional operator" "x = if value > 0 then value + 1 else value - 1."),
    ("parses operators in case scrutinees", assertStage0Parity "case operator scrutinee" "x = case left + right { | 0 -> False | _ -> True }."),
    ("parses operators in case guards", assertStage0Parity "case operator guard" "x = case value { | item if item > 0 -> item | _ -> 0 }."),
    ("parses operators in case bodies", assertStage0Parity "case operator body" "x = case value { | item -> item + 1 | _ -> 0 }."),
    ("preserves case-arm pipe ownership", assertStage0Parity "case arm pipe" "x = case value { | item if left | right | True -> item }."),
    ("parses operators in nested blocks", assertStage0Parity "nested block operator" "x = { y = 1 + 2 * 3. y. }."),
    ("parses sections inside composite expressions", assertStage0Parity "composite sections" "xs = [(+ 1), (10 -), (*)].")
  ]

testOperatorMetadata :: IO ()
testOperatorMetadata =
  assertOperatorOutput
    "operator metadata"
    """
    { builtin = case operatorLookup [] "*" {
        | Just info -> (operatorInfoSymbol info, operatorInfoPrecedence info, operatorInfoAssociativity info)
        | Nothing -> ("", 0, LeftAssociative)
      }.
      tier = case operatorForTier "%%" 5 {
        | Just info -> (operatorInfoPrecedence info, operatorInfoAssociativity info)
        | Nothing -> (0, LeftAssociative)
      }.
      custom = case operatorForPrecedence "<|" 25 {
        | Just info -> case operatorWithAssociativity info RightAssociative {
          | changed -> (operatorInfoPrecedence changed, operatorInfoAssociativity changed)
        }
        | Nothing -> (0, LeftAssociative)
      }.
      ( builtin
      , tier
      , custom
      , operatorIsBuiltin "+"
      , operatorIsReserved "->"
      , operatorIsValidUserSymbol "%%"
      , operatorIsValidUserSymbol "abc"
      ).
    }
    """
    "((\"*\", 5, LeftAssociative), (1, RightAssociative), (25, RightAssociative), True, True, True, False)"

testOperatorContext :: IO ()
testOperatorContext =
  assertOperatorOutput
    "operator context"
    """
    case operatorForTier "%%" 2 {
      | Just info -> {
        top = parserContextRegisterOperator parserContextInitial info.
        nested = parserContextNestedBlock top.
        moduleBody = parserContextModuleBody top.
        ( case parserContextLookupOperator top "%%" {
            | Just found -> operatorInfoPrecedence found
            | Nothing -> 0
          }
        , case parserContextLookupOperator nested "%%" {
            | Just found -> operatorInfoPrecedence found
            | Nothing -> 0
          }
        , case parserContextLookupOperator moduleBody "%%" {
            | Just found -> operatorInfoPrecedence found
            | Nothing -> 0
          }
        ).
      }
      | Nothing -> (0, 0, 0)
    }
    """
    "(4, 4, 0)"

assertStage0Parity :: Text.Text -> Text.Text -> IO ()
assertStage0Parity label source = do
  path <- canonicalPath
  let expected =
        renderCanonicalSourceResult
          ( canonicalizeSourceResult
              path
              ( case tokenizeDetailed source of
                  Left failure -> Left failure
                  Right tokens -> Right (parseSurfaceProgramTokensDetailed tokens)
              )
          )
      expression = "parseSource componentPath " <> Text.pack (show source)
  result <-
    runModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      (lookupSource expression)
  assertEqual (label <> " compile errors") [] (runCompileErrors result)
  assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)
  assertEqual (label <> " output") (Just expected) (runOutput result)

canonicalPath :: IO CanonicalSourcePath
canonicalPath =
  case normalizeCanonicalSourcePath "fixtures/parser/operators-full-parity.jz" of
    Left message -> failTest message
    Right path -> pure path

assertOperatorOutput :: Text.Text -> Text.Text -> Text.Text -> IO ()
assertOperatorOutput label expression expected = do
  result <-
    runModuleGraph
      defaultWarningSettings
      resolverConfig
      ["App", "Main"]
      (lookupOperatorSource expression)
  assertEqual (label <> " compile errors") [] (runCompileErrors result)
  assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)
  assertEqual (label <> " output") (Just expected) (runOutput result)

lookupOperatorSource :: Text.Text -> FilePath -> IO (Maybe Text.Text)
lookupOperatorSource expression sourcePath =
  case sourcePath of
    "src/App/Main.jz" ->
      pure
        ( Just
            ( Text.replace
                "__EXPRESSION__"
                expression
                """
                module App::Main {
                  import Maybe.
                  import ParserContext.
                  import ParserOperator.
                  __EXPRESSION__.
                }

                """
            )
        )
    _ -> readCheckedInJazzProjectModuleSource sourcePath

lookupSource :: Text.Text -> FilePath -> IO (Maybe Text.Text)
lookupSource expression sourcePath =
  case sourcePath of
    "src/App/Main.jz" ->
      pure
        ( Just
            ( Text.replace
                "__EXPRESSION__"
                expression
                """
                module App::Main {
                  import LexerTypes (CanonicalSourcePath).
                  import Parser (parseSource).
                  componentPath = CanonicalSourcePath "fixtures/parser/operators-full-parity.jz".
                  __EXPRESSION__.
                }

                """
            )
        )
    _ -> readCheckedInJazzProjectModuleSource sourcePath

resolverConfig :: ModuleResolutionConfig
resolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
