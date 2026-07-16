{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Bootstrap.CanonicalParserComparison
  ( CanonicalParserResult,
    CanonicalSourceResult,
    canonicalParserResultRuntimeValue,
    canonicalSourceResultRuntimeValue,
    canonicalizeParserResult,
    canonicalizeSourceResult,
    parserFailureRuntimeValue,
    renderCanonicalParserResult,
    renderCanonicalSourceResult,
    surfaceExprRuntimeValue,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.CanonicalLexerComparison
  ( CanonicalSourcePath,
    canonicalLexErrorRuntimeValue,
    canonicalTokenKindRuntimeValue,
    canonicalizeFailure,
    canonicalizeTokenKind,
  )
import JazzNext.Compiler.Bootstrap.CanonicalValue
  ( canonicalConstructor,
    canonicalNullaryConstructor,
    canonicalSourcePathRuntimeValue,
    canonicalSpanRuntimeValue,
    canonicalizeSpan,
  )
import JazzNext.Compiler.DiagnosticCatalog
  ( diagnosticCodeText,
    errorCode,
  )
import JazzNext.Compiler.FractionalLiteral
  ( fractionalLiteralSourceParts,
  )
import JazzNext.Compiler.ModuleExports
  ( LocatedModuleExportName (..),
    ModuleExportSelector (..),
    ModuleTypeConstructorSelector (..),
  )
import JazzNext.Compiler.Name
  ( IdentifierLike (identifierText),
    NameNamespace (..),
  )
import JazzNext.Compiler.Parser.AST
import JazzNext.Compiler.Parser.Failure
import JazzNext.Compiler.Parser.Lexer
  ( LexicalFailure,
  )
import JazzNext.Compiler.Runtime
  ( RuntimeValue (..),
    renderRuntimeValue,
  )

newtype CanonicalParserResult = CanonicalParserResult RuntimeValue
  deriving (Eq, Show)

newtype CanonicalSourceResult = CanonicalSourceResult RuntimeValue
  deriving (Eq, Show)

canonicalizeParserResult :: CanonicalSourcePath -> Either ParserFailure SurfaceExpr -> CanonicalParserResult
canonicalizeParserResult sourcePath result =
  CanonicalParserResult
    ( case result of
        Right expression ->
          canonicalConstructor
            "CanonicalParserSuccess"
            [canonicalSourcePathRuntimeValue sourcePath, surfaceExprRuntimeValue expression]
        Left failure ->
          canonicalConstructor
            "CanonicalParserFailure"
            [canonicalSourcePathRuntimeValue sourcePath, parserFailureRuntimeValue failure]
    )

canonicalizeSourceResult ::
  CanonicalSourcePath ->
  Either LexicalFailure (Either ParserFailure SurfaceExpr) ->
  CanonicalSourceResult
canonicalizeSourceResult sourcePath result =
  CanonicalSourceResult
    ( case result of
        Right (Right expression) ->
          canonicalConstructor
            "CanonicalSourceSuccess"
            [canonicalSourcePathRuntimeValue sourcePath, surfaceExprRuntimeValue expression]
        Left lexicalFailure ->
          canonicalConstructor
            "CanonicalSourceLexicalFailure"
            [ canonicalSourcePathRuntimeValue sourcePath,
              canonicalLexErrorRuntimeValue (canonicalizeFailure lexicalFailure)
            ]
        Right (Left parserFailureValue) ->
          canonicalConstructor
            "CanonicalSourceParserFailure"
            [canonicalSourcePathRuntimeValue sourcePath, parserFailureRuntimeValue parserFailureValue]
    )

canonicalParserResultRuntimeValue :: CanonicalParserResult -> RuntimeValue
canonicalParserResultRuntimeValue (CanonicalParserResult value) = value

canonicalSourceResultRuntimeValue :: CanonicalSourceResult -> RuntimeValue
canonicalSourceResultRuntimeValue (CanonicalSourceResult value) = value

renderCanonicalParserResult :: CanonicalParserResult -> Text
renderCanonicalParserResult = renderRuntimeValue . canonicalParserResultRuntimeValue

renderCanonicalSourceResult :: CanonicalSourceResult -> Text
renderCanonicalSourceResult = renderRuntimeValue . canonicalSourceResultRuntimeValue

surfaceLiteralRuntimeValue :: SurfaceLiteral -> RuntimeValue
surfaceLiteralRuntimeValue literalValue =
  case literalValue of
    SLInt value -> canonicalConstructor "IntegerLiteral" [decimalIntegerValue value]
    SLFloat _ source maybeWidth ->
      let (wholePart, fractionalPart, scale) = fractionalLiteralSourceParts source
       in canonicalConstructor
            "FractionalLiteral"
            [ decimalIntegerValue wholePart,
              VText (fractionalDigits fractionalPart scale),
              maybeRuntimeValue surfaceNumericTypeRuntimeValue maybeWidth
            ]
    SLBool value -> canonicalConstructor "BooleanLiteral" [VBool value]
    SLChar value -> canonicalConstructor "CharacterLiteral" [VChar value]
    SLText value -> canonicalConstructor "TextLiteral" [VText value]

fractionalDigits :: Integer -> Integer -> Text
fractionalDigits fractionalPart scale =
  Text.justifyRight digitCount '0' (Text.pack (show (abs fractionalPart)))
  where
    digitCount = max 0 (length (show scale) - 1)

surfacePatternRuntimeValue :: SurfacePattern -> RuntimeValue
surfacePatternRuntimeValue patternValue =
  case patternValue of
    SPWildcard -> canonicalNullaryConstructor "WildcardPattern"
    SPVariable name -> canonicalConstructor "VariablePattern" [identifierRuntimeValue name]
    SPLiteral literalValue -> canonicalConstructor "LiteralPattern" [surfaceLiteralRuntimeValue literalValue]
    SPConstructor name arguments ->
      canonicalConstructor
        "ConstructorPattern"
        [identifierRuntimeValue name, listRuntimeValue surfacePatternRuntimeValue arguments]
    SPList elements -> canonicalConstructor "ListPattern" [listRuntimeValue surfacePatternRuntimeValue elements]
    SPConsList headPattern tailPattern ->
      canonicalConstructor
        "ConsListPattern"
        [surfacePatternRuntimeValue headPattern, surfacePatternRuntimeValue tailPattern]
    SPTuple elements -> canonicalConstructor "TuplePattern" [listRuntimeValue surfacePatternRuntimeValue elements]
    SPAs name nestedPattern ->
      canonicalConstructor "AsPattern" [identifierRuntimeValue name, surfacePatternRuntimeValue nestedPattern]
    SPOr alternatives -> canonicalConstructor "OrPattern" [listRuntimeValue surfacePatternRuntimeValue alternatives]

surfaceCaseArmRuntimeValue :: SurfaceCaseArm -> RuntimeValue
surfaceCaseArmRuntimeValue (SurfaceCaseArm patternValue guardExpression bodyExpression) =
  canonicalConstructor
    "SurfaceCaseArm"
    [ surfacePatternRuntimeValue patternValue,
      maybeRuntimeValue surfaceExprRuntimeValue guardExpression,
      surfaceExprRuntimeValue bodyExpression
    ]

surfaceLambdaParameterRuntimeValue :: SurfaceLambdaParameter -> RuntimeValue
surfaceLambdaParameterRuntimeValue parameter =
  case parameter of
    SurfaceLambdaIdentifier name -> canonicalConstructor "IdentifierParameter" [identifierRuntimeValue name]
    SurfaceLambdaPattern patternValue -> canonicalConstructor "PatternParameter" [surfacePatternRuntimeValue patternValue]

surfaceDataConstructorArgumentRuntimeValue :: SurfaceDataConstructorArgument -> RuntimeValue
surfaceDataConstructorArgumentRuntimeValue argument =
  case argument of
    SurfaceDataConstructorArgumentName name ->
      canonicalConstructor "NamedConstructorArgument" [identifierRuntimeValue name]
    SurfaceDataConstructorArgumentOpaque -> canonicalNullaryConstructor "OpaqueConstructorArgument"

surfaceDataConstructorRuntimeValue :: SurfaceDataConstructor -> RuntimeValue
surfaceDataConstructorRuntimeValue (SurfaceDataConstructor name arguments) =
  canonicalConstructor
    "SurfaceDataConstructor"
    [identifierRuntimeValue name, listRuntimeValue surfaceDataConstructorArgumentRuntimeValue arguments]

surfaceExprRuntimeValue :: SurfaceExpr -> RuntimeValue
surfaceExprRuntimeValue expression =
  case expression of
    SELit literalValue -> canonicalConstructor "LiteralExpression" [surfaceLiteralRuntimeValue literalValue]
    SEVar name -> canonicalConstructor "VariableExpression" [identifierRuntimeValue name]
    SEQualifiedVar qualifier member ->
      canonicalConstructor
        "QualifiedVariableExpression"
        [identifierRuntimeValue qualifier, identifierRuntimeValue member]
    SELambda parameters body ->
      canonicalConstructor
        "LambdaExpression"
        [nonEmptyRuntimeValue surfaceLambdaParameterRuntimeValue parameters, surfaceExprRuntimeValue body]
    SEOperatorValue symbol -> canonicalConstructor "OperatorValueExpression" [VText symbol]
    SEList elements -> canonicalConstructor "ListExpression" [listRuntimeValue surfaceExprRuntimeValue elements]
    SETuple elements -> canonicalConstructor "TupleExpression" [listRuntimeValue surfaceExprRuntimeValue elements]
    SEApply function argument ->
      canonicalConstructor "ApplyExpression" [surfaceExprRuntimeValue function, surfaceExprRuntimeValue argument]
    SETypeApplication function spanValue signatureType ->
      canonicalConstructor
        "TypeApplicationExpression"
        [ surfaceExprRuntimeValue function,
          canonicalSpanRuntimeValue (canonicalizeSpan spanValue),
          surfaceSignatureTypeRuntimeValue signatureType
        ]
    SEIf condition thenBranch elseBranch ->
      canonicalConstructor
        "IfExpression"
        [ surfaceExprRuntimeValue condition,
          surfaceExprRuntimeValue thenBranch,
          surfaceExprRuntimeValue elseBranch
        ]
    SECase scrutinee arms ->
      canonicalConstructor
        "CaseExpression"
        [surfaceExprRuntimeValue scrutinee, listRuntimeValue surfaceCaseArmRuntimeValue arms]
    SEBinary symbol left right ->
      canonicalConstructor
        "BinaryExpression"
        [VText symbol, surfaceExprRuntimeValue left, surfaceExprRuntimeValue right]
    SESectionLeft left symbol ->
      canonicalConstructor "LeftSectionExpression" [surfaceExprRuntimeValue left, VText symbol]
    SESectionRight symbol right ->
      canonicalConstructor "RightSectionExpression" [VText symbol, surfaceExprRuntimeValue right]
    SEBlock statements -> canonicalConstructor "BlockExpression" [listRuntimeValue surfaceStatementRuntimeValue statements]

surfaceNumericTypeRuntimeValue :: SurfaceNumericType -> RuntimeValue
surfaceNumericTypeRuntimeValue numericType =
  canonicalNullaryConstructor
    ( case numericType of
        SurfaceNumericInt8 -> "Int8Type"
        SurfaceNumericInt16 -> "Int16Type"
        SurfaceNumericInt32 -> "Int32Type"
        SurfaceNumericInt64 -> "Int64Type"
        SurfaceNumericUInt8 -> "UInt8Type"
        SurfaceNumericUInt16 -> "UInt16Type"
        SurfaceNumericUInt32 -> "UInt32Type"
        SurfaceNumericUInt64 -> "UInt64Type"
        SurfaceNumericFloat16 -> "Float16Type"
        SurfaceNumericFloat32 -> "Float32Type"
        SurfaceNumericFloat64 -> "Float64Type"
    )

surfaceSignaturePayloadRuntimeValue :: SurfaceSignaturePayload -> RuntimeValue
surfaceSignaturePayloadRuntimeValue payload =
  case payload of
    SurfaceSignatureType signatureType ->
      canonicalConstructor "TypeSignature" [surfaceSignatureTypeRuntimeValue signatureType]
    SurfaceConstrainedSignature constraints signatureType ->
      canonicalConstructor
        "ConstrainedSignature"
        [ listRuntimeValue surfaceSignatureConstraintRuntimeValue constraints,
          surfaceSignatureTypeRuntimeValue signatureType
        ]
    SurfaceUnsupportedSignature tokens ->
      canonicalConstructor "UnsupportedSignature" [listRuntimeValue surfaceSignatureTokenRuntimeValue tokens]

surfaceSignatureConstraintRuntimeValue :: SurfaceSignatureConstraint -> RuntimeValue
surfaceSignatureConstraintRuntimeValue (SurfaceSignatureConstraint name arguments) =
  canonicalConstructor
    "SurfaceSignatureConstraint"
    [identifierRuntimeValue name, listRuntimeValue surfaceSignatureTypeRuntimeValue arguments]

surfaceSignatureTypeRuntimeValue :: SurfaceSignatureType -> RuntimeValue
surfaceSignatureTypeRuntimeValue signatureType =
  case signatureType of
    SurfaceTypeInt -> canonicalNullaryConstructor "IntType"
    SurfaceTypeFloat -> canonicalNullaryConstructor "FloatType"
    SurfaceTypeNumeric numericType -> canonicalConstructor "NumericType" [surfaceNumericTypeRuntimeValue numericType]
    SurfaceTypeBool -> canonicalNullaryConstructor "BoolType"
    SurfaceTypeChar -> canonicalNullaryConstructor "CharType"
    SurfaceTypeText -> canonicalNullaryConstructor "TextType"
    SurfaceTypeVariable name -> canonicalConstructor "TypeVariable" [identifierRuntimeValue name]
    SurfaceTypeName name -> canonicalConstructor "NamedType" [identifierRuntimeValue name]
    SurfaceTypeApplication name arguments ->
      canonicalConstructor
        "AppliedType"
        [identifierRuntimeValue name, listRuntimeValue surfaceSignatureTypeRuntimeValue arguments]
    SurfaceTypeList elementType -> canonicalConstructor "ListType" [surfaceSignatureTypeRuntimeValue elementType]
    SurfaceTypeTuple elementTypes ->
      canonicalConstructor "TupleType" [listRuntimeValue surfaceSignatureTypeRuntimeValue elementTypes]
    SurfaceTypeFunction argumentType resultType ->
      canonicalConstructor
        "FunctionType"
        [surfaceSignatureTypeRuntimeValue argumentType, surfaceSignatureTypeRuntimeValue resultType]

surfaceSignatureTokenRuntimeValue :: SurfaceSignatureToken -> RuntimeValue
surfaceSignatureTokenRuntimeValue token =
  case token of
    SurfaceSignatureNameToken name -> canonicalConstructor "SignatureNameToken" [VText name]
    SurfaceSignatureIntToken value -> canonicalConstructor "SignatureIntegerToken" [decimalIntegerValue value]
    SurfaceSignatureArrowToken -> canonicalNullaryConstructor "SignatureArrowToken"
    SurfaceSignatureAtToken -> canonicalNullaryConstructor "SignatureAtToken"
    SurfaceSignatureColonToken -> canonicalNullaryConstructor "SignatureColonToken"
    SurfaceSignatureLParenToken -> canonicalNullaryConstructor "SignatureLeftParenToken"
    SurfaceSignatureRParenToken -> canonicalNullaryConstructor "SignatureRightParenToken"
    SurfaceSignatureLBraceToken -> canonicalNullaryConstructor "SignatureLeftBraceToken"
    SurfaceSignatureRBraceToken -> canonicalNullaryConstructor "SignatureRightBraceToken"
    SurfaceSignatureLBracketToken -> canonicalNullaryConstructor "SignatureLeftBracketToken"
    SurfaceSignatureRBracketToken -> canonicalNullaryConstructor "SignatureRightBracketToken"
    SurfaceSignatureCommaToken -> canonicalNullaryConstructor "SignatureCommaToken"
    SurfaceSignatureOperatorToken symbol -> canonicalConstructor "SignatureOperatorToken" [VText symbol]
    SurfaceSignatureOtherToken value -> canonicalConstructor "SignatureOtherToken" [VText value]

surfaceClassMethodSignatureRuntimeValue :: SurfaceClassMethodSignature -> RuntimeValue
surfaceClassMethodSignatureRuntimeValue (SurfaceClassMethodSignature name spanValue payload) =
  canonicalConstructor
    "SurfaceClassMethodSignature"
    [ identifierRuntimeValue name,
      canonicalSpanRuntimeValue (canonicalizeSpan spanValue),
      surfaceSignaturePayloadRuntimeValue payload
    ]

surfaceImplMethodRuntimeValue :: SurfaceImplMethod -> RuntimeValue
surfaceImplMethodRuntimeValue (SurfaceImplMethod name spanValue body) =
  canonicalConstructor
    "SurfaceImplMethod"
    [ identifierRuntimeValue name,
      canonicalSpanRuntimeValue (canonicalizeSpan spanValue),
      surfaceExprRuntimeValue body
    ]

surfaceStatementRuntimeValue :: SurfaceStatement -> RuntimeValue
surfaceStatementRuntimeValue statement =
  case statement of
    SSLet name spanValue expression ->
      canonicalConstructor
        "LetStatement"
        [ identifierRuntimeValue name,
          canonicalSpanRuntimeValue (canonicalizeSpan spanValue),
          surfaceExprRuntimeValue expression
        ]
    SSSignature name spanValue payload ->
      canonicalConstructor
        "SignatureStatement"
        [ identifierRuntimeValue name,
          canonicalSpanRuntimeValue (canonicalizeSpan spanValue),
          surfaceSignaturePayloadRuntimeValue payload
        ]
    SSData spanValue name parameters constructors ->
      canonicalConstructor
        "DataStatement"
        [ canonicalSpanRuntimeValue (canonicalizeSpan spanValue),
          identifierRuntimeValue name,
          listRuntimeValue identifierRuntimeValue parameters,
          listRuntimeValue surfaceDataConstructorRuntimeValue constructors
        ]
    SSClass spanValue name parameters methods ->
      canonicalConstructor
        "ClassStatement"
        [ canonicalSpanRuntimeValue (canonicalizeSpan spanValue),
          identifierRuntimeValue name,
          listRuntimeValue identifierRuntimeValue parameters,
          listRuntimeValue surfaceClassMethodSignatureRuntimeValue methods
        ]
    SSImpl spanValue name targets methods ->
      canonicalConstructor
        "ImplStatement"
        [ canonicalSpanRuntimeValue (canonicalizeSpan spanValue),
          identifierRuntimeValue name,
          listRuntimeValue surfaceSignatureTypeRuntimeValue targets,
          listRuntimeValue surfaceImplMethodRuntimeValue methods
        ]
    SSModule spanValue modulePath maybeExports ->
      canonicalConstructor
        "ModuleStatement"
        [ canonicalSpanRuntimeValue (canonicalizeSpan spanValue),
          listRuntimeValue VText modulePath,
          maybeRuntimeValue (listRuntimeValue moduleExportSelectorRuntimeValue) maybeExports
        ]
    SSImport spanValue modulePath maybeAlias maybeNames ->
      canonicalConstructor
        "ImportStatement"
        [ canonicalSpanRuntimeValue (canonicalizeSpan spanValue),
          listRuntimeValue VText modulePath,
          maybeRuntimeValue VText maybeAlias,
          maybeRuntimeValue (listRuntimeValue VText) maybeNames
        ]
    SSExpr spanValue expression ->
      canonicalConstructor
        "ExpressionStatement"
        [canonicalSpanRuntimeValue (canonicalizeSpan spanValue), surfaceExprRuntimeValue expression]

moduleExportSelectorRuntimeValue :: ModuleExportSelector -> RuntimeValue
moduleExportSelectorRuntimeValue selector =
  case selector of
    ModuleExportSelector maybeNamespace name ->
      canonicalConstructor
        "NamedExportSelector"
        [maybeRuntimeValue nameNamespaceRuntimeValue maybeNamespace, VText name]
    ModuleTypeExportSelector name spanValue constructorSelector ->
      canonicalConstructor
        "TypeExportSelector"
        [ VText name,
          canonicalSpanRuntimeValue (canonicalizeSpan spanValue),
          moduleTypeConstructorSelectorRuntimeValue constructorSelector
        ]

nameNamespaceRuntimeValue :: NameNamespace -> RuntimeValue
nameNamespaceRuntimeValue namespace =
  canonicalNullaryConstructor
    ( case namespace of
        ValueNamespace -> "ValueName"
        ConstructorNamespace -> "ConstructorName"
        TypeNamespace -> "TypeName"
        CapabilityNamespace -> "CapabilityName"
    )

moduleTypeConstructorSelectorRuntimeValue :: ModuleTypeConstructorSelector -> RuntimeValue
moduleTypeConstructorSelectorRuntimeValue selector =
  case selector of
    AbstractType -> canonicalNullaryConstructor "AbstractTypeSelector"
    AllTypeConstructors spanValue ->
      canonicalConstructor "AllConstructorsSelector" [canonicalSpanRuntimeValue (canonicalizeSpan spanValue)]
    SelectedTypeConstructors names ->
      canonicalConstructor
        "SelectedConstructorsSelector"
        [nonEmptyRuntimeValue locatedModuleExportNameRuntimeValue names]

locatedModuleExportNameRuntimeValue :: LocatedModuleExportName -> RuntimeValue
locatedModuleExportNameRuntimeValue locatedName =
  canonicalConstructor
    "SurfaceLocatedExportName"
    [ VText (locatedModuleExportName locatedName),
      canonicalSpanRuntimeValue (canonicalizeSpan (locatedModuleExportSpan locatedName))
    ]

parserFailureRuntimeValue :: ParserFailure -> RuntimeValue
parserFailureRuntimeValue failure =
  canonicalConstructor
    "ParserFailure"
    [ VText (diagnosticCodeText (errorCode (parserFailureCode failure))),
      maybeRuntimeValue (canonicalSpanRuntimeValue . canonicalizeSpan) (parserFailureSpan failure),
      parserFailureReasonRuntimeValue (parserFailureReason failure)
    ]

parserFailureReasonRuntimeValue :: ParserFailureReason -> RuntimeValue
parserFailureReasonRuntimeValue reason =
  case reason of
    ExpectedSyntax expected encountered ->
      canonicalConstructor "ExpectedSyntax" [VText expected, parserEncounteredRuntimeValue encountered]
    UnexpectedSyntax encountered expected ->
      canonicalConstructor "UnexpectedSyntax" [parserEncounteredRuntimeValue encountered, VText expected]
    UnexpectedSyntaxIn encountered syntax ->
      canonicalConstructor "UnexpectedSyntaxIn" [parserEncounteredRuntimeValue encountered, VText syntax]
    UnexpectedSyntaxAfter encountered syntax ->
      canonicalConstructor "UnexpectedSyntaxAfter" [parserEncounteredRuntimeValue encountered, VText syntax]
    UnsupportedSyntax feature -> canonicalConstructor "UnsupportedSyntax" [parserUnsupportedFeatureRuntimeValue feature]
    InvalidFractionalLiteral source -> canonicalConstructor "InvalidFractionalLiteral" [VText source]
    NonAssociativeOperatorChain symbol -> canonicalConstructor "NonAssociativeOperatorChain" [VText symbol]
    UndeclaredOperator symbol operatorUse ->
      canonicalConstructor "UndeclaredOperator" [VText symbol, parserOperatorUseRuntimeValue operatorUse]
    DeclarationFailure failure -> canonicalConstructor "DeclarationFailure" [parserDeclarationFailureRuntimeValue failure]
    PatternFailure failure -> canonicalConstructor "PatternFailure" [parserPatternFailureRuntimeValue failure]
    InternalParserFailure invariant ->
      canonicalConstructor "InternalParserFailure" [parserInternalInvariantRuntimeValue invariant]

parserEncounteredRuntimeValue :: ParserEncountered -> RuntimeValue
parserEncounteredRuntimeValue encountered =
  case encountered of
    ParserEndOfInput -> canonicalNullaryConstructor "EndOfInput"
    ParserEndOfInputAfter syntax -> canonicalConstructor "EndOfInputAfter" [VText syntax]
    ParserEndOfInputIn syntax -> canonicalConstructor "EndOfInputIn" [VText syntax]
    ParserFoundToken kind lexeme ->
      canonicalConstructor
        "FoundToken"
        [canonicalTokenKindRuntimeValue (canonicalizeTokenKind kind), VText lexeme]
    ParserAtToken kind lexeme ->
      canonicalConstructor
        "AtToken"
        [canonicalTokenKindRuntimeValue (canonicalizeTokenKind kind), VText lexeme]
    ParserBeforeToken kind lexeme maybeContext ->
      canonicalConstructor
        "BeforeToken"
        [ canonicalTokenKindRuntimeValue (canonicalizeTokenKind kind),
          VText lexeme,
          maybeRuntimeValue VText maybeContext
        ]
    ParserBeforeBoundary boundary -> canonicalConstructor "BeforeBoundary" [VText boundary]
    ParserImplicitBoundary -> canonicalNullaryConstructor "ImplicitBoundary"

parserOperatorUseRuntimeValue :: ParserOperatorUse -> RuntimeValue
parserOperatorUseRuntimeValue operatorUse =
  canonicalNullaryConstructor
    ( case operatorUse of
        OperatorUseInExpression -> "ExpressionOperatorUse"
        OperatorUseInBinding -> "BindingOperatorUse"
        OperatorUseInSignature -> "SignatureOperatorUse"
    )

parserDeclarationKindRuntimeValue :: ParserDeclarationKind -> RuntimeValue
parserDeclarationKindRuntimeValue declarationKind =
  canonicalNullaryConstructor
    ( case declarationKind of
        ModuleDeclaration -> "ModuleDeclaration"
        ImportDeclaration -> "ImportDeclaration"
        DataDeclaration -> "DataDeclaration"
        OperatorDeclaration -> "OperatorDeclaration"
        OperatorBinding -> "OperatorBinding"
        OperatorSignature -> "OperatorSignature"
        ClassDeclaration -> "ClassDeclaration"
        ImplDeclaration -> "ImplDeclaration"
    )

parserNameRoleRuntimeValue :: ParserNameRole -> RuntimeValue
parserNameRoleRuntimeValue nameRole =
  canonicalNullaryConstructor
    ( case nameRole of
        BindingName -> "BindingName"
        ImportAlias -> "ImportAlias"
    )

parserDuplicateNameRoleRuntimeValue :: ParserDuplicateNameRole -> RuntimeValue
parserDuplicateNameRoleRuntimeValue duplicateNameRole =
  canonicalNullaryConstructor
    ( case duplicateNameRole of
        DataTypeParameter -> "DataTypeParameter"
        DataConstructorName -> "DataConstructorName"
        ClassMethodName -> "ClassMethodName"
        ImplMethodName -> "ImplMethodName"
    )

parserListKindRuntimeValue :: ParserListKind -> RuntimeValue
parserListKindRuntimeValue listKind =
  canonicalNullaryConstructor
    ( case listKind of
        ImportSymbolList -> "ImportSymbolList"
        ModuleExportList -> "ModuleExportList"
        ConstructorExportList -> "ConstructorExportList"
    )

parserUnsupportedFeatureRuntimeValue :: ParserUnsupportedFeature -> RuntimeValue
parserUnsupportedFeatureRuntimeValue feature =
  case feature of
    ExplicitTypeApplicationArgument -> canonicalNullaryConstructor "ExplicitTypeApplicationArgument"
    FractionalLiteralPattern -> canonicalNullaryConstructor "FractionalLiteralPattern"
    ClassMethodBody name -> canonicalConstructor "ClassMethodBody" [VText name]
    DeclarationHeaderArguments declarationKind ->
      canonicalConstructor "DeclarationHeaderArguments" [parserDeclarationKindRuntimeValue declarationKind]
    AbstractionSyntax syntax -> canonicalConstructor "AbstractionSyntax" [VText syntax]

parserPatternFailureRuntimeValue :: ParserPatternFailure -> RuntimeValue
parserPatternFailureRuntimeValue ConsLikeListPatternHeadCount =
  canonicalNullaryConstructor "ConsLikeListPatternHeadCount"

parserDeclarationFailureRuntimeValue :: ParserDeclarationFailure -> RuntimeValue
parserDeclarationFailureRuntimeValue failure =
  case failure of
    BuiltinOperatorCannotBeRedeclared symbol -> unaryText "BuiltinOperatorCannotBeRedeclared" symbol
    BuiltinOperatorCannotBeBound symbol -> unaryText "BuiltinOperatorCannotBeBound" symbol
    BuiltinOperatorCannotBeSigned symbol -> unaryText "BuiltinOperatorCannotBeSigned" symbol
    ReservedOperatorSymbol symbol -> unaryText "ReservedOperatorSymbol" symbol
    DuplicateOperatorDeclaration symbol -> unaryText "DuplicateOperatorDeclaration" symbol
    InvalidOperatorSymbol symbol -> unaryText "InvalidOperatorSymbol" symbol
    OperatorTierOutOfRange -> canonicalNullaryConstructor "OperatorTierOutOfRange"
    OperatorPrecedenceOutOfRange -> canonicalNullaryConstructor "OperatorPrecedenceOutOfRange"
    ReservedLiteralName role name ->
      canonicalConstructor "ReservedLiteralName" [parserNameRoleRuntimeValue role, VText name]
    DeclarationOutsideAllowedScope declarationKind ->
      canonicalConstructor "DeclarationOutsideAllowedScope" [parserDeclarationKindRuntimeValue declarationKind]
    ImportAliasCombinedWithSymbolList -> canonicalNullaryConstructor "ImportAliasCombinedWithSymbolList"
    ImplRequiresConcreteTarget -> canonicalNullaryConstructor "ImplRequiresConcreteTarget"
    DuplicateName role name declarationKind ->
      canonicalConstructor
        "DuplicateName"
        [ parserDuplicateNameRoleRuntimeValue role,
          VText name,
          parserDeclarationKindRuntimeValue declarationKind
        ]
    DuplicateListItem listKind item ->
      canonicalConstructor "DuplicateListItem" [parserListKindRuntimeValue listKind, VText item]
    ExpectedOrdinaryImplMethodBinding name -> unaryText "ExpectedOrdinaryImplMethodBinding" name
    ClassRequiresExplicitParameterList -> canonicalNullaryConstructor "ClassRequiresExplicitParameterList"
    ClassRequiresLowercaseParameter -> canonicalNullaryConstructor "ClassRequiresLowercaseParameter"
    DuplicateClassParameter name -> unaryText "DuplicateClassParameter" name
    ClassSupportsExactlyOneParameter -> canonicalNullaryConstructor "ClassSupportsExactlyOneParameter"
    ClassParameterMustBeLowercase -> canonicalNullaryConstructor "ClassParameterMustBeLowercase"
    UndeclaredConstructorTypeParameter parameterName typeName ->
      canonicalConstructor "UndeclaredConstructorTypeParameter" [VText parameterName, VText typeName]
    ConstructorArgumentDelimiterMismatch lexeme -> unaryText "ConstructorArgumentDelimiterMismatch" lexeme
    ConstructorExportGroupRequiresAll -> canonicalNullaryConstructor "ConstructorExportGroupRequiresAll"
    ModuleMustBeFirstTopLevelForm -> canonicalNullaryConstructor "ModuleMustBeFirstTopLevelForm"

parserInternalInvariantRuntimeValue :: ParserInternalInvariant -> RuntimeValue
parserInternalInvariantRuntimeValue invariant =
  case invariant of
    TokenStreamParseFailure -> canonicalNullaryConstructor "TokenStreamParseFailure"
    ExpectedOperatorToken operatorUse ->
      canonicalConstructor "ExpectedOperatorToken" [parserOperatorUseRuntimeValue operatorUse]
    ExpectedSignatureSeparator -> canonicalNullaryConstructor "ExpectedSignatureSeparator"
    ExpectedBindingEquals -> canonicalNullaryConstructor "ExpectedBindingEquals"

unaryText :: Text -> Text -> RuntimeValue
unaryText constructorName value = canonicalConstructor constructorName [VText value]

identifierRuntimeValue :: (IdentifierLike identifier) => identifier -> RuntimeValue
identifierRuntimeValue = VText . identifierText

decimalIntegerValue :: Integer -> RuntimeValue
decimalIntegerValue = VText . Text.pack . show

listRuntimeValue :: (value -> RuntimeValue) -> [value] -> RuntimeValue
listRuntimeValue toRuntimeValue values = VList (map toRuntimeValue values) Nothing

nonEmptyRuntimeValue :: (value -> RuntimeValue) -> NonEmpty value -> RuntimeValue
nonEmptyRuntimeValue toRuntimeValue values =
  canonicalConstructor
    "NonEmpty"
    [toRuntimeValue (NonEmpty.head values), listRuntimeValue toRuntimeValue (NonEmpty.tail values)]

maybeRuntimeValue :: (value -> RuntimeValue) -> Maybe value -> RuntimeValue
maybeRuntimeValue toRuntimeValue maybeValue =
  case maybeValue of
    Nothing -> canonicalNullaryConstructor "Nothing"
    Just value -> canonicalConstructor "Just" [toRuntimeValue value]
