{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Parser.Foundation.InvalidSyntaxTests
  ( invalidSyntaxTests,
  )
where

import Control.Monad (forM_)
import qualified Data.Text as Text
import Jazz.Compiler.Parser
  ( parseSurfaceProgram,
  )
import Jazz.Compiler.Parser.Foundation.Shared
import Jazz.TestHarness
  ( NamedTest,
    assertLeftDiagnosticContains,
    failTest,
  )

invalidSyntaxTests :: [NamedTest]
invalidSyntaxTests =
  [ ("rejects non-finite fractional literals", testRejectsNonFiniteFractionalLiteral),
    ("rejects source-exact Float64 fractional literal overflow", testRejectsSourceExactFloat64FractionalLiteralOverflow),
    ("rejects fractional literal case patterns", testRejectsFractionalLiteralCasePatterns),
    ("rejects fractional literal lambda patterns", testRejectsFractionalLiteralLambdaPatterns),
    ("rejects unsupported explicit type application argument", testRejectsUnsupportedExplicitTypeApplicationArgument),
    ("rejects empty named explicit type application arguments", testRejectsEmptyNamedExplicitTypeApplicationArguments),
    ("rejects missing statement terminator", testRejectsMissingDotTerminator),
    ("rejects unterminated block expression", testRejectsUnterminatedBlockExpression),
    ("rejects signature missing terminator before next statement", testRejectsMissingSignatureDot),
    ("rejects signature missing terminator before class declaration", testRejectsMissingSignatureDotBeforeClass),
    ("rejects negative literal syntax for now", testRejectsNegativeLiteralSyntax),
    ("rejects class capability declarations without parameters", testRejectsClassCapabilityDeclarationWithoutParameters),
    ("rejects class capability declarations with multiple parameters", testRejectsClassCapabilityDeclarationWithMultipleParameters),
    ("rejects duplicate class defaults", testRejectsDuplicateClassDefaults),
    ("rejects duplicate class method signatures", testRejectsDuplicateClassMethodSignatures),
    ("rejects non-signature class body items", testRejectsNonSignatureClassBodyItem),
    ("rejects variable-target impl method bindings", testRejectsVariableTargetImplMethodBindings),
    ("rejects variable-target impl declarations with empty bodies", testRejectsVariableTargetEmptyImplDeclarations),
    ("rejects duplicate impl method bindings", testRejectsDuplicateImplMethodBindings),
    ("rejects non-binding impl body items", testRejectsNonBindingImplBodyItem),
    ("rejects duplicate class parameters", testRejectsDuplicateClassParameters),
    ("rejects concrete class parameters", testRejectsConcreteClassParameters),
    ("rejects malformed class capability headers", testRejectsMalformedClassCapabilityHeader),
    ("rejects qualified class declarations", testRejectsQualifiedClassDeclaration),
    ("rejects malformed alias-qualified class methods", testRejectsMalformedAliasQualifiedClassMethod),
    ("rejects overlong alias-qualified class methods", testRejectsOverlongAliasQualifiedClassMethod),
    ("rejects missing or non-identifier aliases in constraints", testRejectsInvalidConstraintAliases),
    ("rejects statement boundaries inside unfinished signatures", testRejectsUnfinishedSignatures),
    ("rejects trait abstraction declarations as non-canonical syntax", testRejectsTraitAbstractionSyntax),
    ("rejects lowercase trait abstraction declarations", testRejectsLowercaseTraitAbstractionSyntax),
    ("rejects trait abstraction declarations inside module bodies", testRejectsTraitAbstractionSyntaxInModuleBody),
    ("rejects value in every ordinary identifier position", testRejectsReservedValueIdentifiers)
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
    ( parseSurfaceProgram
        """
        result = id @ 1.
        result.
        """
    )

testRejectsEmptyNamedExplicitTypeApplicationArguments :: IO ()
testRejectsEmptyNamedExplicitTypeApplicationArguments =
  assertLeftDiagnosticContains
    "empty named explicit type application arguments"
    "unsupported explicit type application argument after '@'"
    ( parseSurfaceProgram
        """
        result = id @Maybe().
        result.
        """
    )

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
    ( parseSurfaceProgram
        """
        x :: Int
        x = 1.
        """
    )

testRejectsMissingSignatureDotBeforeClass :: IO ()
testRejectsMissingSignatureDotBeforeClass =
  assertLeftDiagnosticContains
    "missing signature dot before class"
    "expected '.' before 'class'"
    ( parseSurfaceProgram
        """
        x :: Int
        class Equatable { }.
        """
    )

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
    (parseSurfaceProgram "class Equatable { }.")

testRejectsClassCapabilityDeclarationWithMultipleParameters :: IO ()
testRejectsClassCapabilityDeclarationWithMultipleParameters =
  assertLeftDiagnosticContains
    "class capability declaration with multiple parameters"
    "exactly one parameter"
    (parseSurfaceProgram "class Equatable(a, b) { }.")

testRejectsDuplicateClassDefaults :: IO ()
testRejectsDuplicateClassDefaults =
  assertLeftDiagnosticContains
    "duplicate class defaults"
    "duplicate method signature 'equals'"
    (parseSurfaceProgram "class Equatable(a) { equals = \\(item) -> item. equals = \\(item) -> item. }.")

testRejectsDuplicateClassMethodSignatures :: IO ()
testRejectsDuplicateClassMethodSignatures =
  assertLeftDiagnosticContains
    "duplicate class method signature"
    "duplicate method signature 'equals'"
    (parseSurfaceProgram "class Equatable(a) { equals :: Int. equals :: Bool. }.")

testRejectsNonSignatureClassBodyItem :: IO ()
testRejectsNonSignatureClassBodyItem =
  assertLeftDiagnosticContains
    "non-signature class body item"
    "method signature, default binding"
    (parseSurfaceProgram "class Equatable(a) { 1. }.")

testRejectsVariableTargetImplMethodBindings :: IO ()
testRejectsVariableTargetImplMethodBindings =
  assertLeftDiagnosticContains
    "variable-target impl method binding"
    "constructor-headed impl target"
    (parseSurfaceProgram "impl Equatable(a) { equals = 1. }.")

testRejectsVariableTargetEmptyImplDeclarations :: IO ()
testRejectsVariableTargetEmptyImplDeclarations =
  assertLeftDiagnosticContains
    "variable-target empty impl declaration"
    "constructor-headed impl target"
    (parseSurfaceProgram "impl Equatable(a) { }.")

testRejectsDuplicateImplMethodBindings :: IO ()
testRejectsDuplicateImplMethodBindings =
  assertLeftDiagnosticContains
    "duplicate impl method binding"
    "duplicate method binding 'equals'"
    (parseSurfaceProgram "impl Equatable(Int) { equals = 1. equals = 2. }.")

testRejectsNonBindingImplBodyItem :: IO ()
testRejectsNonBindingImplBodyItem =
  assertLeftDiagnosticContains
    "non-binding impl body item"
    "ordinary method binding"
    (parseSurfaceProgram "impl Equatable(Int) { equals :: Int. }.")

testRejectsDuplicateClassParameters :: IO ()
testRejectsDuplicateClassParameters =
  assertLeftDiagnosticContains
    "duplicate class parameter"
    "duplicate class parameter 'a'"
    (parseSurfaceProgram "class Equatable(a, a) { }.")

testRejectsConcreteClassParameters :: IO ()
testRejectsConcreteClassParameters =
  assertLeftDiagnosticContains
    "concrete class parameter"
    "class parameters must be lowercase type variables"
    (parseSurfaceProgram "class Equatable(Int) { }.")

testRejectsMalformedClassCapabilityHeader :: IO ()
testRejectsMalformedClassCapabilityHeader =
  assertLeftDiagnosticContains
    "malformed class capability header"
    "unexpected token 'Bar' in class declaration header"
    (parseSurfaceProgram "class Foo Bar Baz(Int, String) { }.")

testRejectsQualifiedClassDeclaration :: IO ()
testRejectsQualifiedClassDeclaration =
  assertLeftDiagnosticContains
    "qualified class declaration"
    "expected unqualified class name"
    (parseSurfaceProgram "class Facts::Equatable(a) { }.")

testRejectsMalformedAliasQualifiedClassMethod :: IO ()
testRejectsMalformedAliasQualifiedClassMethod =
  assertLeftDiagnosticContains
    "malformed alias-qualified class method"
    "expected method name after '::'"
    (parseSurfaceProgram "Facts::Equatable::.")

testRejectsOverlongAliasQualifiedClassMethod :: IO ()
testRejectsOverlongAliasQualifiedClassMethod =
  assertLeftDiagnosticContains
    "overlong alias-qualified class method"
    "unexpected token '::' in qualified class method name"
    (parseSurfaceProgram "Facts::Equatable::equals::extra.")

testRejectsInvalidConstraintAliases :: IO ()
testRejectsInvalidConstraintAliases =
  forM_
    [ "f :: @{1::Equatable(a)}: a -> a.",
      "f :: @{((1::Equatable(a)))}: a -> a.",
      "f :: @{::Equatable(a)}: a -> a.",
      "f :: @{(::Equatable(a))}: a -> a.",
      "f :: @{Equatable(a), ::Comparable(a)}: a -> a."
    ]
    $ \source ->
      assertLeftDiagnosticContains ("constraint alias: " <> source) "expected alias before '::'" (parseSurfaceProgram source)

testRejectsUnfinishedSignatures :: IO ()
testRejectsUnfinishedSignatures =
  forM_
    [ ("f :: (Int\nnext = 1.", "expected '.' before 'next'"),
      ("f :: @{Equatable(a)\nnext = 1.", "expected '.' before 'next'"),
      ("f :: (Int\nclass Equatable(a) { }.", "expected '.' before 'class'"),
      ("f :: (Int.", "expected closing delimiter"),
      ("f :: [Int.", "expected closing delimiter"),
      ("f :: @{Equatable(a).", "expected closing delimiter"),
      ("f :: Int\nnext :: Bool.", "expected '.' before 'next'")
    ]
    $ \(source, expected) ->
      assertLeftDiagnosticContains ("unfinished signature: " <> source) expected (parseSurfaceProgram source)

testRejectsTraitAbstractionSyntax :: IO ()
testRejectsTraitAbstractionSyntax =
  assertLeftDiagnosticContains
    "trait abstraction syntax non-canonical"
    "unsupported abstraction syntax 'trait'"
    (parseSurfaceProgram "trait Equatable { }.")

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
    ( parseSurfaceProgram
        """
        module App::Core {
        trait Equatable { }.
        }
        """
    )
