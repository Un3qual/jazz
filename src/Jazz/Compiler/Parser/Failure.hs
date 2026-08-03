{-# LANGUAGE OverloadedStrings #-}

-- | Structured parser failures kept separate from their diagnostic rendering.
module Jazz.Compiler.Parser.Failure
  ( ParserDeclarationFailure (..),
    ParserDeclarationKind (..),
    ParserDuplicateNameRole (..),
    ParserEncountered (..),
    ParserFailure (..),
    ParserFailureReason (..),
    ParserInternalInvariant (..),
    ParserListKind (..),
    ParserNameRole (..),
    ParserOperatorUse (..),
    ParserPatternFailure (..),
    ParserUnsupportedFeature (..),
    parserFailure,
    parserFailureAt,
    parserFailureDiagnostic,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.DiagnosticCatalog
  ( ErrorCode (E0001),
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (CompilationOrigin),
    SourceSpan,
    mkErrorDiagnostic,
    setDiagnosticPrimaryLabel,
  )
import Jazz.Compiler.Parser.Lexer
  ( TokenKind,
  )

data ParserEncountered
  = ParserEndOfInput
  | ParserEndOfInputAfter Text
  | ParserEndOfInputIn Text
  | ParserFoundToken TokenKind Text
  | ParserAtToken TokenKind Text
  | ParserBeforeToken TokenKind Text (Maybe Text)
  | ParserBeforeBoundary Text
  | ParserImplicitBoundary
  deriving (Eq, Ord, Show)

data ParserFailureReason
  = ExpectedSyntax Text ParserEncountered
  | UnexpectedSyntax ParserEncountered Text
  | UnexpectedSyntaxIn ParserEncountered Text
  | UnexpectedSyntaxAfter ParserEncountered Text
  | UnsupportedSyntax ParserUnsupportedFeature
  | InvalidFractionalLiteral Text
  | NonAssociativeOperatorChain Text
  | UndeclaredOperator Text ParserOperatorUse
  | DeclarationFailure ParserDeclarationFailure
  | PatternFailure ParserPatternFailure
  | InternalParserFailure ParserInternalInvariant
  deriving (Eq, Ord, Show)

data ParserOperatorUse
  = OperatorUseInExpression
  | OperatorUseInBinding
  | OperatorUseInSignature
  deriving (Eq, Ord, Show)

data ParserDeclarationKind
  = ModuleDeclaration
  | ImportDeclaration
  | DataDeclaration
  | OperatorDeclaration
  | OperatorBinding
  | OperatorSignature
  | ClassDeclaration
  | ImplDeclaration
  deriving (Eq, Ord, Show)

data ParserNameRole
  = BindingName
  | ImportAlias
  deriving (Eq, Ord, Show)

data ParserDuplicateNameRole
  = DataTypeParameter
  | DataConstructorName
  | ClassMethodName
  | ImplMethodName
  deriving (Eq, Ord, Show)

data ParserListKind
  = ImportSymbolList
  | ModuleExportList
  | ConstructorExportList
  deriving (Eq, Ord, Show)

data ParserUnsupportedFeature
  = ExplicitTypeApplicationArgument
  | FractionalLiteralPattern
  | ClassMethodBody Text
  | DeclarationHeaderArguments ParserDeclarationKind
  | AbstractionSyntax Text
  deriving (Eq, Ord, Show)

data ParserPatternFailure
  = ConsLikeListPatternHeadCount
  | PatternLambdaClauseArityMismatch Int Int
  deriving (Eq, Ord, Show)

data ParserDeclarationFailure
  = BuiltinOperatorCannotBeRedeclared Text
  | BuiltinOperatorCannotBeBound Text
  | BuiltinOperatorCannotBeSigned Text
  | ReservedOperatorSymbol Text
  | DuplicateOperatorDeclaration Text
  | InvalidOperatorSymbol Text
  | OperatorTierOutOfRange
  | OperatorPrecedenceOutOfRange
  | ReservedLiteralName ParserNameRole Text
  | DeclarationOutsideAllowedScope ParserDeclarationKind
  | ImportAliasCombinedWithSymbolList
  | ImplRequiresConcreteTarget
  | DuplicateName ParserDuplicateNameRole Text ParserDeclarationKind
  | DuplicateListItem ParserListKind Text
  | ExpectedOrdinaryImplMethodBinding Text
  | ClassRequiresExplicitParameterList
  | ClassRequiresLowercaseParameter
  | DuplicateClassParameter Text
  | ClassSupportsExactlyOneParameter
  | ClassParameterMustBeLowercase
  | UndeclaredConstructorTypeParameter Text Text
  | ConstructorArgumentDelimiterMismatch Text
  | ConstructorExportGroupRequiresAll
  | ModuleMustBeFirstTopLevelForm
  deriving (Eq, Ord, Show)

data ParserInternalInvariant
  = TokenStreamParseFailure
  | ExpectedOperatorToken ParserOperatorUse
  | ExpectedSignatureSeparator
  | ExpectedBindingEquals
  deriving (Eq, Ord, Show)

data ParserFailure = ParserFailure
  { parserFailureCode :: ErrorCode,
    parserFailureSpan :: Maybe SourceSpan,
    parserFailureReason :: ParserFailureReason
  }
  deriving (Eq, Ord, Show)

parserFailure :: ParserFailureReason -> ParserFailure
parserFailure reason =
  ParserFailure
    { parserFailureCode = E0001,
      parserFailureSpan = Nothing,
      parserFailureReason = reason
    }

parserFailureAt :: SourceSpan -> ParserFailureReason -> ParserFailure
parserFailureAt spanValue reason =
  (parserFailure reason) {parserFailureSpan = Just spanValue}

parserFailureDiagnostic :: ParserFailure -> Diagnostic
parserFailureDiagnostic failure =
  case parserFailureSpan failure of
    Nothing -> diagnostic
    Just spanValue -> setDiagnosticPrimaryLabel spanValue "here" diagnostic
  where
    diagnostic =
      mkErrorDiagnostic
        (parserFailureCode failure)
        CompilationOrigin
        (renderParserFailureReason (parserFailureReason failure))

renderParserFailureReason :: ParserFailureReason -> Text
renderParserFailureReason reason =
  case reason of
    ExpectedSyntax expected encountered ->
      case encountered of
        ParserEndOfInput -> "expected " <> expected <> " before end of input"
        ParserEndOfInputAfter syntax ->
          "expected " <> expected <> " before end of input after " <> syntax
        ParserEndOfInputIn syntax ->
          "expected " <> expected <> " before end of input in " <> syntax
        ParserFoundToken _ lexeme ->
          "expected " <> expected <> ", found '" <> lexeme <> "'"
        ParserAtToken _ _ -> "expected " <> expected
        ParserBeforeToken _ lexeme maybeContext ->
          "expected "
            <> expected
            <> " before '"
            <> lexeme
            <> "'"
            <> maybe "" (" in " <>) maybeContext
        ParserBeforeBoundary boundary ->
          "expected " <> expected <> " before " <> boundary
        ParserImplicitBoundary -> "expected " <> expected
    UnexpectedSyntax encountered expected ->
      case encountered of
        ParserFoundToken _ lexeme ->
          "unexpected token '" <> lexeme <> "'; expected " <> expected
        _ -> "unexpected syntax; expected " <> expected
    UnexpectedSyntaxIn encountered syntax ->
      case encountered of
        ParserFoundToken _ lexeme ->
          "unexpected token '" <> lexeme <> "' in " <> syntax
        _ -> "unexpected syntax in " <> syntax
    UnexpectedSyntaxAfter encountered syntax ->
      case encountered of
        ParserFoundToken _ lexeme ->
          "unexpected token '" <> lexeme <> "' after " <> syntax
        _ -> "unexpected syntax after " <> syntax
    UnsupportedSyntax feature -> renderUnsupportedFeature feature
    InvalidFractionalLiteral literalText ->
      "invalid fractional literal '" <> literalText <> "'"
    NonAssociativeOperatorChain symbol ->
      "non-associative operator '"
        <> symbol
        <> "' cannot be chained without parentheses"
    UndeclaredOperator symbol operatorUse ->
      "operator '"
        <> symbol
        <> "' must be declared before "
        <> case operatorUse of
          OperatorUseInExpression -> "use"
          OperatorUseInBinding -> "binding"
          OperatorUseInSignature -> "signature"
    DeclarationFailure failure -> renderDeclarationFailure failure
    PatternFailure failure ->
      case failure of
        ConsLikeListPatternHeadCount ->
          "cons-like list patterns require exactly one head pattern before '|'"
        PatternLambdaClauseArityMismatch expected actual ->
          "pattern-lambda clauses must all have "
            <> Text.pack (show expected)
            <> " parameter(s), found "
            <> Text.pack (show actual)
    InternalParserFailure invariant -> renderInternalInvariant invariant

renderUnsupportedFeature :: ParserUnsupportedFeature -> Text
renderUnsupportedFeature feature =
  case feature of
    ExplicitTypeApplicationArgument ->
      "unsupported explicit type application argument after '@'"
    FractionalLiteralPattern -> "fractional literal patterns are not supported"
    ClassMethodBody methodName ->
      "unsupported class method body/default syntax for '"
        <> methodName
        <> "': only signature-only method declarations are implemented in jazz"
    DeclarationHeaderArguments declarationKind ->
      "unsupported " <> renderDeclarationKind declarationKind <> " header arguments"
    AbstractionSyntax "trait" ->
      "unsupported abstraction syntax 'trait': trait declarations are non-canonical; use class/impl once abstraction semantics land in jazz"
    AbstractionSyntax name ->
      "unsupported abstraction syntax '"
        <> name
        <> "': executable class/impl abstraction semantics are deferred in jazz"

renderDeclarationFailure :: ParserDeclarationFailure -> Text
renderDeclarationFailure failure =
  case failure of
    BuiltinOperatorCannotBeRedeclared symbol ->
      "cannot redeclare built-in operator '" <> symbol <> "'"
    BuiltinOperatorCannotBeBound symbol ->
      "cannot bind built-in operator '" <> symbol <> "'"
    BuiltinOperatorCannotBeSigned symbol ->
      "cannot sign built-in operator '" <> symbol <> "'"
    ReservedOperatorSymbol symbol -> "reserved operator symbol '" <> symbol <> "'"
    DuplicateOperatorDeclaration symbol ->
      "duplicate operator declaration '" <> symbol <> "'"
    InvalidOperatorSymbol symbol -> "invalid operator symbol '" <> symbol <> "'"
    OperatorTierOutOfRange -> "operator tier must be between 1 and 5"
    OperatorPrecedenceOutOfRange -> "operator precedence must be between 1 and 99"
    ReservedLiteralName role name ->
      "reserved literal '"
        <> name
        <> "' cannot be used as "
        <> renderNameRoleWithArticle role
    DeclarationOutsideAllowedScope declarationKind ->
      case declarationKind of
        ModuleDeclaration -> "module declaration must remain top-level"
        ImportDeclaration ->
          "import declaration must remain at file scope or directly in a module body"
        DataDeclaration ->
          "data declaration must remain at file scope or directly in a module body"
        OperatorDeclaration ->
          "operator declarations are only allowed at file scope or directly in module bodies"
        OperatorBinding ->
          "operator bindings are only allowed at file scope or directly in module bodies"
        OperatorSignature ->
          "operator signatures are only allowed at file scope or directly in module bodies"
        ClassDeclaration -> "class declaration must remain top-level"
        ImplDeclaration -> "impl declaration must remain top-level"
    ImportAliasCombinedWithSymbolList -> "cannot combine import alias and symbol list"
    ImplRequiresConcreteTarget -> "impl declarations require a concrete impl target"
    DuplicateName role name declarationKind ->
      "duplicate "
        <> renderDuplicateNameRole role
        <> " '"
        <> name
        <> "' in "
        <> renderDeclarationKind declarationKind
    DuplicateListItem listKind renderedItem ->
      "duplicate " <> renderListItemKind listKind <> " " <> renderedItem
    ExpectedOrdinaryImplMethodBinding name ->
      "expected ordinary method binding for '" <> name <> "' in impl declaration body"
    ClassRequiresExplicitParameterList ->
      "class declarations require an explicit parameter list"
    ClassRequiresLowercaseParameter ->
      "class declarations require at least one explicit lowercase parameter"
    DuplicateClassParameter name -> "duplicate class parameter '" <> name <> "'"
    ClassSupportsExactlyOneParameter ->
      "class declarations currently support exactly one parameter"
    ClassParameterMustBeLowercase ->
      "class parameters must be lowercase type variables"
    UndeclaredConstructorTypeParameter parameterName typeName ->
      "constructor payload type parameter '"
        <> parameterName
        <> "' is not declared in data type '"
        <> typeName
        <> "'"
    ConstructorArgumentDelimiterMismatch lexeme ->
      "unexpected '" <> lexeme <> "' in constructor argument"
    ConstructorExportGroupRequiresAll ->
      "expected exactly '..' followed by ')' in constructor export group"
    ModuleMustBeFirstTopLevelForm ->
      "module declaration must be the first top-level form"

renderInternalInvariant :: ParserInternalInvariant -> Text
renderInternalInvariant invariant =
  case invariant of
    TokenStreamParseFailure -> "unexpected token stream parse error"
    ExpectedOperatorToken OperatorUseInExpression ->
      "internal parser error: expected operator token"
    ExpectedOperatorToken OperatorUseInBinding ->
      "internal parser error: expected operator token in operator binding"
    ExpectedOperatorToken OperatorUseInSignature ->
      "internal parser error: expected operator token in operator signature"
    ExpectedSignatureSeparator ->
      "internal parser error: expected '::' after signature name"
    ExpectedBindingEquals ->
      "internal parser error: expected '=' after binding name"

renderDeclarationKind :: ParserDeclarationKind -> Text
renderDeclarationKind declarationKind =
  case declarationKind of
    ModuleDeclaration -> "module declaration"
    ImportDeclaration -> "import declaration"
    DataDeclaration -> "data declaration"
    OperatorDeclaration -> "operator declaration"
    OperatorBinding -> "operator binding"
    OperatorSignature -> "operator signature"
    ClassDeclaration -> "class declaration"
    ImplDeclaration -> "impl declaration"

renderDuplicateNameRole :: ParserDuplicateNameRole -> Text
renderDuplicateNameRole role =
  case role of
    DataTypeParameter -> "type parameter"
    DataConstructorName -> "constructor declaration"
    ClassMethodName -> "method signature"
    ImplMethodName -> "method binding"

renderNameRoleWithArticle :: ParserNameRole -> Text
renderNameRoleWithArticle role =
  case role of
    BindingName -> "a binding name"
    ImportAlias -> "an import alias"

renderListItemKind :: ParserListKind -> Text
renderListItemKind listKind =
  case listKind of
    ImportSymbolList -> "import symbol"
    ModuleExportList -> "module export"
    ConstructorExportList -> "constructor export"
