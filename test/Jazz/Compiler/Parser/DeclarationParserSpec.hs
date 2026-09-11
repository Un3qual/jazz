{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Jazz.Compiler.DiagnosticCatalog
  ( diagnosticCodeText,
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
    diagnosticCode,
    diagnosticPrimarySpan,
    diagnosticSummary,
  )
import Jazz.Compiler.Name
  ( mkIdentifier,
  )
import Jazz.Compiler.Parser
  ( parseSurfaceProgram,
    parseSurfaceProgramTokensDetailed,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceDataConstructor (..),
    SurfaceStatement (..),
  )
import Jazz.Compiler.Parser.Declaration
  ( parseDataStatementParser,
  )
import qualified Jazz.Compiler.Parser.Declaration as Declaration
import Jazz.Compiler.Parser.Failure
  ( ParserDeclarationFailure (..),
    ParserDeclarationKind (..),
    ParserDuplicateNameRole (..),
    ParserEncountered (ParserEndOfInput),
    ParserFailure (..),
    ParserFailureReason (..),
    parserFailure,
  )
import Jazz.Compiler.Parser.Lexer (Token (..))
import Jazz.Compiler.Parser.TestSupport
  ( lexSource,
  )
import Jazz.Compiler.Parser.TokenParser (failParserFailure, peekToken, runTokenParserPrefix, runTokenStreamParserPrefixDetailed)
import Jazz.Compiler.Parser.TokenStream
  ( tokenStreamFromList,
  )
import Jazz.Compiler.TypeRepresentation (SignatureType (..))
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertLeftDiagnosticContains,
    failTest,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "DeclarationParser" tests

tests :: [NamedTest]
tests =
  [ ("rejects Haskell-style function equations", testRejectsFunctionEquations),
    ("preserves failure spans after an owned declaration", testFailureSpanAfterOwnedDeclaration),
    ("parses data constructors with named and grouped payloads", testParsesDataConstructors),
    ("rejects crossed parenthesis then bracket constructor payload", testRejectsCrossedParenBracketPayload),
    ("rejects crossed bracket then parenthesis constructor payload", testRejectsCrossedBracketParenPayload),
    ("accepts correctly nested constructor field types", testAcceptsNestedConstructorFieldTypes),
    ("reports nested imports as structured scope failures", testDetailedNestedImport),
    ("reports nested data declarations at the declaration span", testRejectsNestedData),
    ("preserves duplicate data parameter spans", testDetailedDuplicateDataTypeParameter),
    ("preserves undeclared constructor parameter spans", testDetailedUndeclaredConstructorTypeParameter),
    ("preserves direct capability callback diagnostics", testCapabilityCallbackDiagnostic),
    ("rejects imports in nested expression blocks at the import span", testRejectsNestedImport),
    ("accepts imports directly in module bodies", testAcceptsModuleBodyImport)
  ]

testRejectsFunctionEquations :: IO ()
testRejectsFunctionEquations =
  case parseSurfaceProgram
    """
    length [] = 0.
    length [_ | rest] = 1 + length rest.
    """ of
    Left _ -> pure ()
    Right _ -> failTest "expected Haskell-style function equations to be rejected"

testFailureSpanAfterOwnedDeclaration :: IO ()
testFailureSpanAfterOwnedDeclaration =
  case parseSurfaceProgram "import Lib::Math.\nentry = )." of
    Left diagnostic -> do
      assertEqual "post-import failure span" (Just (SourceRange 2 9 2 10)) (diagnosticPrimarySpan diagnostic)
      assertEqual "post-import failure summary" "unexpected token ')'; expected expression" (diagnosticSummary diagnostic)
    Right _ -> failTest "expected the malformed statement after the import to fail"

testParsesDataConstructors :: IO ()
testParsesDataConstructors = do
  tokens <- lexSource "data Maybe a = None | Some a | Pair (a, a) [a]."
  assertEqual
    "data declaration"
    ( Right
        ( SSData
            (SourceSpan 1 1)
            (mkIdentifier "Maybe")
            [mkIdentifier "a"]
            [ SurfaceDataConstructor (mkIdentifier "None") [],
              SurfaceDataConstructor
                (mkIdentifier "Some")
                [TypeVariable (mkIdentifier "a")],
              SurfaceDataConstructor
                (mkIdentifier "Pair")
                [ TypeTuple
                    [ TypeVariable (mkIdentifier "a"),
                      TypeVariable (mkIdentifier "a")
                    ],
                  TypeList (TypeVariable (mkIdentifier "a"))
                ]
            ],
          []
        )
    )
    (parseDataDeclarationForTest tokens)

testRejectsCrossedParenBracketPayload :: IO ()
testRejectsCrossedParenBracketPayload = do
  tokens <- lexSource "data Box = Box ([)]."
  assertLeftDiagnosticContains
    "crossed parenthesis then bracket payload"
    "expected '(', found ')'"
    (parseDataDeclarationForTest tokens)

testRejectsCrossedBracketParenPayload :: IO ()
testRejectsCrossedBracketParenPayload = do
  tokens <- lexSource "data Box = Box [(])."
  assertLeftDiagnosticContains
    "crossed bracket then parenthesis payload"
    "expected '(', found ']'"
    (parseDataDeclarationForTest tokens)

testAcceptsNestedConstructorFieldTypes :: IO ()
testAcceptsNestedConstructorFieldTypes = do
  tokens <- lexSource "data Box = Box ([()]) [(())]."
  case parseDataDeclarationForTest tokens of
    Right _ -> pure ()
    Left diagnostic -> failTest ("expected nested constructor field types to parse, got " <> diagnosticSummary diagnostic)

testRejectsNestedImport :: IO ()
testRejectsNestedImport =
  -- Explicit escapes are intentional: this case asserts exact whitespace or source spans.
  case parseSurfaceProgram "main = {\n  import Lib::Value.\n  result.\n}." of
    Left diagnostic -> do
      assertEqual "nested import code" "E0001" (diagnosticCodeText (diagnosticCode diagnostic))
      assertEqual "nested import span" (Just (SourceRange 2 3 2 9)) (diagnosticPrimarySpan diagnostic)
    Right _ -> failTest "expected nested import to fail"

testDetailedNestedImport :: IO ()
testDetailedNestedImport = do
  tokens <-
    lexSource
      """
      main = {
        import Lib::Value.
        result.
      }.
      """
  case parseSurfaceProgramTokensDetailed tokens of
    Left failure -> do
      assertEqual "nested import detailed span" (Just (SourceSpan 2 3)) (parserFailureSpan failure)
      assertEqual
        "nested import detailed reason"
        (DeclarationFailure (DeclarationOutsideAllowedScope ImportDeclaration))
        (parserFailureReason failure)
    Right _ -> failTest "expected detailed nested import failure"

testRejectsNestedData :: IO ()
testRejectsNestedData =
  case parseSurfaceProgram
    """
    main = {
      data Status = Ready.
      Ready.
    }.
    """ of
    Left diagnostic -> do
      assertEqual
        "nested data diagnostic summary"
        "data declaration must remain at file scope or directly in a module body"
        (diagnosticSummary diagnostic)
      assertEqual "nested data diagnostic span" (Just (SourceRange 2 3 2 7)) (diagnosticPrimarySpan diagnostic)
    Right _ -> failTest "expected nested data declaration to fail"

testDetailedDuplicateDataTypeParameter :: IO ()
testDetailedDuplicateDataTypeParameter = do
  tokens <- lexSource "data Pair a a = Pair a a."
  case parseSurfaceProgramTokensDetailed tokens of
    Left failure -> do
      assertEqual "duplicate data parameter span" (Just (SourceSpan 1 13)) (parserFailureSpan failure)
      assertEqual
        "duplicate data parameter reason"
        (DeclarationFailure (DuplicateName DataTypeParameter "a" DataDeclaration))
        (parserFailureReason failure)
    Right _ -> failTest "expected duplicate data parameter failure"

testDetailedUndeclaredConstructorTypeParameter :: IO ()
testDetailedUndeclaredConstructorTypeParameter = do
  tokens <- lexSource "data Maybe a = Just b."
  case parseSurfaceProgramTokensDetailed tokens of
    Left failure -> do
      assertEqual "undeclared constructor parameter span" (Just (SourceSpan 1 21)) (parserFailureSpan failure)
      assertEqual
        "undeclared constructor parameter reason"
        (DeclarationFailure (UndeclaredConstructorTypeParameter "b" "Maybe"))
        (parserFailureReason failure)
    Right _ -> failTest "expected undeclared constructor parameter failure"

testCapabilityCallbackDiagnostic :: IO ()
testCapabilityCallbackDiagnostic = do
  tokens <- lexSource "impl Show(Int) { show = item. }."
  let tokenStream = tokenStreamFromList tokens
      expectedFailure = parserFailure (ExpectedSyntax "callback expression" ParserEndOfInput)
      unexpectedCursorFailure = parserFailure (ExpectedSyntax "expression callback at 'item'" ParserEndOfInput)
      parseImplExpression = do
        next <- peekToken
        failParserFailure $ case tokenLexeme <$> next of
          Just "item" -> expectedFailure
          _ -> unexpectedCursorFailure
      parseCapability parser =
        runTokenStreamParserPrefixDetailed "capability declaration" (Declaration.parseCapabilityDeclarationParser parser) tokenStream
  assertEqual "capability callback failure" (Left expectedFailure) (parseCapability (failParserFailure expectedFailure))
  assertEqual "capability callback cursor consumption" (Left expectedFailure) (parseCapability parseImplExpression)

testAcceptsModuleBodyImport :: IO ()
testAcceptsModuleBodyImport =
  case parseSurfaceProgram
    """
    module App::Main {
      import Lib::Value.
      result.
    }
    """ of
    Right _ -> pure ()
    Left diagnostic -> failTest ("expected module-body import to parse, got " <> diagnosticSummary diagnostic)

parseDataDeclarationForTest :: [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseDataDeclarationForTest =
  runTokenParserPrefix "owned data declaration" parseDataStatementParser
