{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Parser.Foundation.InvalidSyntaxTests
  ( invalidSyntaxTests
  ) where

import Control.Monad (forM_)
import qualified Data.Text as Text
import JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  )
import JazzNext.Compiler.Parser.Foundation.Shared
import JazzNext.TestHarness
  ( NamedTest,
    assertLeftDiagnosticContains,
    failTest
  )

invalidSyntaxTests :: [NamedTest]
invalidSyntaxTests =
  [ ("rejects non-finite fractional literals", testRejectsNonFiniteFractionalLiteral)
    , ("rejects source-exact Float64 fractional literal overflow", testRejectsSourceExactFloat64FractionalLiteralOverflow)
    , ("rejects fractional literal case patterns", testRejectsFractionalLiteralCasePatterns)
    , ("rejects fractional literal lambda patterns", testRejectsFractionalLiteralLambdaPatterns)
    , ("rejects unsupported explicit type application argument", testRejectsUnsupportedExplicitTypeApplicationArgument)
    , ("rejects empty named explicit type application arguments", testRejectsEmptyNamedExplicitTypeApplicationArguments)
    , ("rejects missing statement terminator", testRejectsMissingDotTerminator)
    , ("rejects unterminated block expression", testRejectsUnterminatedBlockExpression)
    , ("rejects signature missing terminator before next statement", testRejectsMissingSignatureDot)
    , ("rejects signature missing terminator before class declaration", testRejectsMissingSignatureDotBeforeClass)
    , ("rejects negative literal syntax for now", testRejectsNegativeLiteralSyntax)
    , ("rejects class capability declarations without parameters", testRejectsClassCapabilityDeclarationWithoutParameters)
    , ("rejects class capability declarations with multiple parameters", testRejectsClassCapabilityDeclarationWithMultipleParameters)
    , ("rejects class method body syntax", testRejectsClassMethodBodySyntax)
    , ("rejects duplicate class method signatures", testRejectsDuplicateClassMethodSignatures)
    , ("rejects non-signature class body items", testRejectsNonSignatureClassBodyItem)
    , ("rejects variable-target impl method bindings", testRejectsVariableTargetImplMethodBindings)
    , ("rejects variable-target impl declarations with empty bodies", testRejectsVariableTargetEmptyImplDeclarations)
    , ("rejects duplicate impl method bindings", testRejectsDuplicateImplMethodBindings)
    , ("rejects non-binding impl body items", testRejectsNonBindingImplBodyItem)
    , ("rejects duplicate class parameters", testRejectsDuplicateClassParameters)
    , ("rejects concrete class parameters", testRejectsConcreteClassParameters)
    , ("rejects malformed class capability headers", testRejectsMalformedClassCapabilityHeader)
    , ("rejects trait abstraction declarations as non-canonical syntax", testRejectsTraitAbstractionSyntax)
    , ("rejects lowercase trait abstraction declarations", testRejectsLowercaseTraitAbstractionSyntax)
    , ("rejects trait abstraction declarations inside module bodies", testRejectsTraitAbstractionSyntaxInModuleBody)
    , ("rejects value in every ordinary identifier position", testRejectsReservedValueIdentifiers)
  ]

testRejectsReservedValueIdentifiers :: IO ()
testRejectsReservedValueIdentifiers =
  forM_ invalidSources $ \(label, source) ->
    case parseSurfaceProgram source of
      Left _ -> pure ()
      Right parsed ->
        failTest
          ( label
              <> ": expected reserved value rejection, got "
              <> Text.pack (show parsed)
          )
  where
    invalidSources =
      [ ("binding name", "value = 1."),
        ("signature name", "value :: Int."),
        ("lambda parameter", "identity = \\value -> value."),
        ("pattern binder", "identity = case 1 { | value -> value }."),
        ("data type parameter", "data Box value = Box value."),
        ("module path", "module value { answer = 1. }."),
        ("module alias", "import Example as value.")
      ]

testRejectsNonFiniteFractionalLiteral :: IO ()
testRejectsNonFiniteFractionalLiteral =
  assertLeftDiagnosticContains
    "non-finite fractional literal"
    "invalid fractional literal"
    (parseSurfaceProgram (Text.pack ("x = " <> replicate 400 '9' <> ".0.")))

testRejectsSourceExactFloat64FractionalLiteralOverflow :: IO ()
testRejectsSourceExactFloat64FractionalLiteralOverflow =
  assertLeftDiagnosticContains
    "source-exact Float64 fractional literal overflow"
    "invalid fractional literal"
    (parseSurfaceProgram (Text.pack ("x = " <> show (float64MaxFiniteInteger + 1) <> ".0.")))

testRejectsFractionalLiteralCasePatterns :: IO ()
testRejectsFractionalLiteralCasePatterns =
  assertLeftDiagnosticContains
    "fractional literal case pattern"
    "fractional literal patterns"
    (parseSurfaceProgram "x = case 1 { | 1.5 -> True | _ -> False }.")

testRejectsFractionalLiteralLambdaPatterns :: IO ()
testRejectsFractionalLiteralLambdaPatterns =
  assertLeftDiagnosticContains
    "fractional literal lambda pattern"
    "fractional literal patterns"
    (parseSurfaceProgram "f = \\(1.5) -> True.")

testRejectsUnsupportedExplicitTypeApplicationArgument :: IO ()
testRejectsUnsupportedExplicitTypeApplicationArgument =
  assertLeftDiagnosticContains
    "unsupported explicit type application argument"
    "unsupported explicit type application argument after '@'"
    (parseSurfaceProgram """
    result = id @ 1.
    result.
    """)

testRejectsEmptyNamedExplicitTypeApplicationArguments :: IO ()
testRejectsEmptyNamedExplicitTypeApplicationArguments =
  assertLeftDiagnosticContains
    "empty named explicit type application arguments"
    "unsupported explicit type application argument after '@'"
    (parseSurfaceProgram """
    result = id @Maybe().
    result.
    """)

testRejectsMissingDotTerminator :: IO ()
testRejectsMissingDotTerminator =
  assertLeftDiagnosticContains
    "missing dot error"
    "expected '.'"
    (parseSurfaceProgram "x = 1 y = 2.")

testRejectsUnterminatedBlockExpression :: IO ()
testRejectsUnterminatedBlockExpression =
  assertLeftDiagnosticContains
    "unterminated block expression"
    "expected '}'"
    (parseSurfaceProgram "x = { y = 1. y.")

testRejectsMissingSignatureDot :: IO ()
testRejectsMissingSignatureDot =
  assertLeftDiagnosticContains
    "missing signature dot error"
    "expected '.'"
    (parseSurfaceProgram """
    x :: Int
    x = 1.
    """)

testRejectsMissingSignatureDotBeforeClass :: IO ()
testRejectsMissingSignatureDotBeforeClass =
  assertLeftDiagnosticContains
    "missing signature dot before class"
    "expected '.' before 'class'"
    (parseSurfaceProgram """
    x :: Int
    class Eq { }.
    """)

testRejectsNegativeLiteralSyntax :: IO ()
testRejectsNegativeLiteralSyntax =
  assertLeftDiagnosticContains
    "negative literal unsupported"
    "expected expression"
    (parseSurfaceProgram "x = -1.")

testRejectsClassCapabilityDeclarationWithoutParameters :: IO ()
testRejectsClassCapabilityDeclarationWithoutParameters =
  assertLeftDiagnosticContains
    "class capability declaration without parameters"
    "explicit parameter list"
    (parseSurfaceProgram "class Eq { }.")

testRejectsClassCapabilityDeclarationWithMultipleParameters :: IO ()
testRejectsClassCapabilityDeclarationWithMultipleParameters =
  assertLeftDiagnosticContains
    "class capability declaration with multiple parameters"
    "exactly one parameter"
    (parseSurfaceProgram "class Eq(a, b) { }.")

testRejectsClassMethodBodySyntax :: IO ()
testRejectsClassMethodBodySyntax =
  assertLeftDiagnosticContains
    "class method body syntax"
    "method body/default syntax"
    (parseSurfaceProgram "class Eq(a) { equals = \\item -> item. }.")

testRejectsDuplicateClassMethodSignatures :: IO ()
testRejectsDuplicateClassMethodSignatures =
  assertLeftDiagnosticContains
    "duplicate class method signature"
    "duplicate method signature 'equals'"
    (parseSurfaceProgram "class Eq(a) { equals :: Int. equals :: Bool. }.")

testRejectsNonSignatureClassBodyItem :: IO ()
testRejectsNonSignatureClassBodyItem =
  assertLeftDiagnosticContains
    "non-signature class body item"
    "signature-only method declaration"
    (parseSurfaceProgram "class Eq(a) { 1. }.")

testRejectsVariableTargetImplMethodBindings :: IO ()
testRejectsVariableTargetImplMethodBindings =
  assertLeftDiagnosticContains
    "variable-target impl method binding"
    "concrete impl target"
    (parseSurfaceProgram "impl Eq(a) { equals = 1. }.")

testRejectsVariableTargetEmptyImplDeclarations :: IO ()
testRejectsVariableTargetEmptyImplDeclarations =
  assertLeftDiagnosticContains
    "variable-target empty impl declaration"
    "concrete impl target"
    (parseSurfaceProgram "impl Eq(a) { }.")

testRejectsDuplicateImplMethodBindings :: IO ()
testRejectsDuplicateImplMethodBindings =
  assertLeftDiagnosticContains
    "duplicate impl method binding"
    "duplicate method binding 'equals'"
    (parseSurfaceProgram "impl Eq(Int) { equals = 1. equals = 2. }.")

testRejectsNonBindingImplBodyItem :: IO ()
testRejectsNonBindingImplBodyItem =
  assertLeftDiagnosticContains
    "non-binding impl body item"
    "ordinary method binding"
    (parseSurfaceProgram "impl Eq(Int) { equals :: Int. }.")

testRejectsDuplicateClassParameters :: IO ()
testRejectsDuplicateClassParameters =
  assertLeftDiagnosticContains
    "duplicate class parameter"
    "duplicate class parameter 'a'"
    (parseSurfaceProgram "class Eq(a, a) { }.")

testRejectsConcreteClassParameters :: IO ()
testRejectsConcreteClassParameters =
  assertLeftDiagnosticContains
    "concrete class parameter"
    "class parameters must be lowercase type variables"
    (parseSurfaceProgram "class Eq(Int) { }.")

testRejectsMalformedClassCapabilityHeader :: IO ()
testRejectsMalformedClassCapabilityHeader =
  assertLeftDiagnosticContains
    "malformed class capability header"
    "unexpected token 'Bar' in class declaration header"
    (parseSurfaceProgram "class Foo Bar Baz(Int, String) { }.")

testRejectsTraitAbstractionSyntax :: IO ()
testRejectsTraitAbstractionSyntax =
  assertLeftDiagnosticContains
    "trait abstraction syntax non-canonical"
    "unsupported abstraction syntax 'trait'"
    (parseSurfaceProgram "trait Eq { }.")

testRejectsLowercaseTraitAbstractionSyntax :: IO ()
testRejectsLowercaseTraitAbstractionSyntax =
  assertLeftDiagnosticContains
    "lowercase trait abstraction syntax non-canonical"
    "unsupported abstraction syntax 'trait'"
    (parseSurfaceProgram "trait eq { }.")

testRejectsTraitAbstractionSyntaxInModuleBody :: IO ()
testRejectsTraitAbstractionSyntaxInModuleBody =
  assertLeftDiagnosticContains
    "trait abstraction syntax in module body"
    "unsupported abstraction syntax 'trait'"
    (parseSurfaceProgram """
    module App::Core {
    trait Eq { }.
    }
    """)
