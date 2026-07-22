{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Bootstrap.CanonicalCoreComparison
  ( canonicalCoreExprRuntimeValue,
    canonicalCoreSourceResultRuntimeValue,
    canonicalCoreModuleResultRuntimeValue,
    canonicalCoreModuleRuntimeValue,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
import JazzNext.Compiler.Bootstrap.CanonicalLexerComparison
  ( canonicalLexErrorRuntimeValue,
    canonicalizeFailure,
  )
import JazzNext.Compiler.Bootstrap.CanonicalParserComparison
  ( parserFailureRuntimeValue,
  )
import JazzNext.Compiler.Bootstrap.CanonicalValue
  ( CanonicalSourcePath,
    canonicalConstructor,
    canonicalNullaryConstructor,
    canonicalSourcePathRuntimeValue,
    normalizeCanonicalSourcePath,
    runtimeIntValue,
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    qualifySourceSpan,
  )
import JazzNext.Compiler.FractionalLiteral (fractionalLiteralSourceParts)
import JazzNext.Compiler.ModuleExports
  ( LocatedModuleExportName (..),
    ModuleExportSelector (..),
    ModuleTypeConstructorSelector (..),
  )
import JazzNext.Compiler.ModuleGraph
  ( CoreModule (..),
    DeclaredModuleExports (..),
    ResolvedImport (..),
  )
import JazzNext.Compiler.Name
  ( GeneratedNameKind (..),
    IdentifierLike (identifierText),
    Name (..),
    NameNamespace (..),
  )
import JazzNext.Compiler.Parser.Lower
  ( ModuleDeclaration (..),
    ModuleLoweringFailure (..),
  )
import JazzNext.Compiler.Parser.Failure (ParserFailure)
import JazzNext.Compiler.Parser.Lexer (LexicalFailure)
import JazzNext.Compiler.Runtime (RuntimeValue (..))

canonicalCoreExprRuntimeValue :: Expr -> Either Text RuntimeValue
canonicalCoreExprRuntimeValue expression =
  case expression of
    ELit literalValue -> constructor1 "CoreLiteralExpression" <$> coreLiteralRuntimeValue literalValue
    EVar name -> constructor1 "CoreVariableExpression" <$> coreNameRuntimeValue name
    ELambda parameter body ->
      constructor2 "CoreLambdaExpression"
        <$> coreNameRuntimeValue parameter
        <*> canonicalCoreExprRuntimeValue body
    EOperatorValue symbol -> pure (constructor1 "CoreOperatorValueExpression" (VText symbol))
    EList elements -> constructor1 "CoreListExpression" <$> listRuntimeValue canonicalCoreExprRuntimeValue elements
    ETuple elements -> constructor1 "CoreTupleExpression" <$> listRuntimeValue canonicalCoreExprRuntimeValue elements
    EApply function argument ->
      constructor2 "CoreApplyExpression"
        <$> canonicalCoreExprRuntimeValue function
        <*> canonicalCoreExprRuntimeValue argument
    ETypeApplication function spanValue signatureType ->
      constructor3 "CoreTypeApplicationExpression"
        <$> canonicalCoreExprRuntimeValue function
        <*> coreSpanRuntimeValue spanValue
        <*> coreSignatureTypeRuntimeValue signatureType
    EIf condition trueBranch falseBranch ->
      constructor3 "CoreIfExpression"
        <$> canonicalCoreExprRuntimeValue condition
        <*> canonicalCoreExprRuntimeValue trueBranch
        <*> canonicalCoreExprRuntimeValue falseBranch
    EPatternCase scrutinee arms ->
      constructor2 "CorePatternCaseExpression"
        <$> canonicalCoreExprRuntimeValue scrutinee
        <*> listRuntimeValue coreCaseArmRuntimeValue arms
    EBinary symbol left right ->
      constructor3 "CoreBinaryExpression" (VText symbol)
        <$> canonicalCoreExprRuntimeValue left
        <*> canonicalCoreExprRuntimeValue right
    ESectionLeft left symbol ->
      constructor2 "CoreLeftSectionExpression"
        <$> canonicalCoreExprRuntimeValue left
        <*> pure (VText symbol)
    ESectionRight symbol right ->
      constructor2 "CoreRightSectionExpression" (VText symbol)
        <$> canonicalCoreExprRuntimeValue right
    EBlock statements -> constructor1 "CoreBlockExpression" <$> listRuntimeValue coreStatementRuntimeValue statements

canonicalCoreModuleRuntimeValue :: CoreModule -> Either Text RuntimeValue
canonicalCoreModuleRuntimeValue coreModule =
  canonicalConstructor "CoreModule"
    <$> sequence
      [ pure (maybeRuntimeValuePure (listRuntimeValuePure VText) (coreModuleDeclaredPath coreModule)),
        maybeRuntimeValue coreDeclaredModuleExportsRuntimeValue (coreModuleDeclaredExports coreModule),
        listRuntimeValue coreResolvedImportRuntimeValue (coreModuleImports coreModule),
        canonicalCoreExprRuntimeValue (coreModuleExpr coreModule)
      ]

canonicalCoreModuleResultRuntimeValue :: Either ModuleLoweringFailure CoreModule -> Either Text RuntimeValue
canonicalCoreModuleResultRuntimeValue result =
  case result of
    Right coreModule -> constructor1 "CoreModuleLowered" <$> canonicalCoreModuleRuntimeValue coreModule
    Left failure -> constructor1 "CoreModuleLoweringFailed" <$> coreModuleLoweringFailureRuntimeValue failure

canonicalCoreSourceResultRuntimeValue ::
  CanonicalSourcePath ->
  Either LexicalFailure (Either ParserFailure (Either ModuleLoweringFailure CoreModule)) ->
  Either Text RuntimeValue
canonicalCoreSourceResultRuntimeValue sourcePath result =
  case result of
    Left lexicalFailure ->
      pure
        ( canonicalConstructor
            "CanonicalCoreSourceLexicalFailure"
            [ canonicalSourcePathRuntimeValue sourcePath,
              canonicalLexErrorRuntimeValue (canonicalizeFailure lexicalFailure)
            ]
        )
    Right (Left parserFailure) ->
      pure
        ( canonicalConstructor
            "CanonicalCoreSourceParserFailure"
            [canonicalSourcePathRuntimeValue sourcePath, parserFailureRuntimeValue parserFailure]
        )
    Right (Right moduleResult) ->
      constructor1 "CanonicalCoreSourceModuleResult"
        <$> canonicalCoreModuleResultRuntimeValue moduleResult

coreModuleLoweringFailureRuntimeValue :: ModuleLoweringFailure -> Either Text RuntimeValue
coreModuleLoweringFailureRuntimeValue failure =
  case failure of
    MultipleModuleDeclarations sourcePath declarations ->
      constructor2 "CoreMultipleModuleDeclarationsFailure"
        <$> coreSourcePathRuntimeValue sourcePath
        <*> listRuntimeValue (coreModuleDeclarationRuntimeValue sourcePath) declarations
    ModulePathMismatch sourcePath expectedPath declaration ->
      constructor3 "CoreModulePathMismatchFailure"
        <$> coreSourcePathRuntimeValue sourcePath
        <*> pure (listRuntimeValuePure VText expectedPath)
        <*> coreModuleDeclarationRuntimeValue sourcePath declaration

coreModuleDeclarationRuntimeValue :: FilePath -> ModuleDeclaration -> Either Text RuntimeValue
coreModuleDeclarationRuntimeValue sourcePath declaration =
  constructor2 "CoreModuleDeclaration"
    <$> coreSpanRuntimeValue (qualifySourceSpan sourcePath (moduleDeclarationSpan declaration))
    <*> pure (listRuntimeValuePure VText (moduleDeclarationPath declaration))

coreSourcePathRuntimeValue :: FilePath -> Either Text RuntimeValue
coreSourcePathRuntimeValue sourcePath =
  canonicalSourcePathRuntimeValue <$> normalizeCanonicalSourcePath sourcePath

coreLiteralRuntimeValue :: Literal -> Either Text RuntimeValue
coreLiteralRuntimeValue literalValue =
  case literalValue of
    LInt value -> pure (constructor1 "CoreIntegerLiteral" (decimalIntegerValue value))
    LFloat _ source maybeWidth ->
      let (wholePart, fractionalPart, scale) = fractionalLiteralSourceParts source
       in pure
            ( canonicalConstructor
                "CoreFractionalLiteral"
                [ decimalIntegerValue wholePart,
                  VText (fractionalDigits fractionalPart scale),
                  maybeRuntimeValuePure coreNumericTypeRuntimeValue maybeWidth
                ]
            )
    LBool value -> pure (constructor1 "CoreBooleanLiteral" (VBool value))
    LChar value -> pure (constructor1 "CoreCharacterLiteral" (VChar value))
    LText value -> pure (constructor1 "CoreTextLiteral" (VText value))

corePatternRuntimeValue :: Pattern -> Either Text RuntimeValue
corePatternRuntimeValue patternValue =
  case patternValue of
    PWildcard -> pure (canonicalNullaryConstructor "CoreWildcardPattern")
    PVariable name -> constructor1 "CoreVariablePattern" <$> coreNameRuntimeValue name
    PLiteral literalValue -> constructor1 "CoreLiteralPattern" <$> coreLiteralRuntimeValue literalValue
    PConstructor name patterns ->
      constructor2 "CoreConstructorPattern"
        <$> coreNameRuntimeValue name
        <*> listRuntimeValue corePatternRuntimeValue patterns
    PList patterns -> constructor1 "CoreListPattern" <$> listRuntimeValue corePatternRuntimeValue patterns
    PConsList headPattern tailPattern ->
      constructor2 "CoreConsListPattern"
        <$> corePatternRuntimeValue headPattern
        <*> corePatternRuntimeValue tailPattern
    PTuple patterns -> constructor1 "CoreTuplePattern" <$> listRuntimeValue corePatternRuntimeValue patterns
    PAs name pattern ->
      constructor2 "CoreAsPattern"
        <$> coreNameRuntimeValue name
        <*> corePatternRuntimeValue pattern
    POr patterns -> constructor1 "CoreOrPattern" <$> listRuntimeValue corePatternRuntimeValue patterns

coreCaseArmRuntimeValue :: CaseArm -> Either Text RuntimeValue
coreCaseArmRuntimeValue (CaseArm patternValue maybeGuard body) =
  constructor3 "CoreCaseArm"
    <$> corePatternRuntimeValue patternValue
    <*> maybeRuntimeValue canonicalCoreExprRuntimeValue maybeGuard
    <*> canonicalCoreExprRuntimeValue body

coreStatementRuntimeValue :: Statement -> Either Text RuntimeValue
coreStatementRuntimeValue statement =
  case statement of
    SLet name spanValue expression ->
      constructor3 "CoreLetStatement"
        <$> coreNameRuntimeValue name
        <*> coreSpanRuntimeValue spanValue
        <*> canonicalCoreExprRuntimeValue expression
    SSignature name spanValue payload ->
      constructor3 "CoreSignatureStatement"
        <$> coreNameRuntimeValue name
        <*> coreSpanRuntimeValue spanValue
        <*> coreSignaturePayloadRuntimeValue payload
    SData spanValue name parameters constructors ->
      constructor4 "CoreDataStatement"
        <$> coreSpanRuntimeValue spanValue
        <*> coreNameRuntimeValue name
        <*> listRuntimeValue coreNameRuntimeValue parameters
        <*> listRuntimeValue coreDataConstructorRuntimeValue constructors
    SClass spanValue name parameters methods ->
      constructor4 "CoreClassStatement"
        <$> coreSpanRuntimeValue spanValue
        <*> coreNameRuntimeValue name
        <*> listRuntimeValue coreNameRuntimeValue parameters
        <*> listRuntimeValue coreClassMethodRuntimeValue methods
    SImpl spanValue name arguments methods ->
      constructor4 "CoreImplStatement"
        <$> coreSpanRuntimeValue spanValue
        <*> coreNameRuntimeValue name
        <*> listRuntimeValue coreSignatureTypeRuntimeValue arguments
        <*> listRuntimeValue coreImplMethodRuntimeValue methods
    SModule spanValue path ->
      constructor2 "CoreModuleStatement"
        <$> coreSpanRuntimeValue spanValue
        <*> pure (listRuntimeValuePure VText path)
    SImport spanValue path maybeAlias maybeSymbols ->
      canonicalConstructor "CoreImportStatement"
        <$> sequence
          [ coreSpanRuntimeValue spanValue,
            pure (listRuntimeValuePure VText path),
            pure (maybeRuntimeValuePure VText maybeAlias),
            pure (maybeRuntimeValuePure (listRuntimeValuePure VText) maybeSymbols)
          ]
    SExpr spanValue expression ->
      constructor2 "CoreExpressionStatement"
        <$> coreSpanRuntimeValue spanValue
        <*> canonicalCoreExprRuntimeValue expression

coreDataConstructorRuntimeValue :: DataConstructor -> Either Text RuntimeValue
coreDataConstructorRuntimeValue (DataConstructor name arguments) =
  constructor2 "CoreDataConstructor"
    <$> coreNameRuntimeValue name
    <*> listRuntimeValue coreDataConstructorArgumentRuntimeValue arguments

coreDataConstructorArgumentRuntimeValue :: DataConstructorArgument -> Either Text RuntimeValue
coreDataConstructorArgumentRuntimeValue argument =
  case argument of
    DataConstructorArgumentName name -> constructor1 "CoreNamedConstructorArgument" <$> coreNameRuntimeValue name
    DataConstructorArgumentOpaque -> pure (canonicalNullaryConstructor "CoreOpaqueConstructorArgument")

coreClassMethodRuntimeValue :: ClassMethodSignature -> Either Text RuntimeValue
coreClassMethodRuntimeValue (ClassMethodSignature name spanValue payload) =
  constructor3 "CoreClassMethodSignature"
    <$> coreNameRuntimeValue name
    <*> coreSpanRuntimeValue spanValue
    <*> coreSignaturePayloadRuntimeValue payload

coreImplMethodRuntimeValue :: ImplMethod -> Either Text RuntimeValue
coreImplMethodRuntimeValue (ImplMethod name spanValue body) =
  constructor3 "CoreImplMethod"
    <$> coreNameRuntimeValue name
    <*> coreSpanRuntimeValue spanValue
    <*> canonicalCoreExprRuntimeValue body

coreSignaturePayloadRuntimeValue :: SignaturePayload -> Either Text RuntimeValue
coreSignaturePayloadRuntimeValue payload =
  case payload of
    SignatureType signatureType -> constructor1 "CoreTypeSignature" <$> coreSignatureTypeRuntimeValue signatureType
    ConstrainedSignature constraints signatureType ->
      constructor2 "CoreConstrainedSignature"
        <$> listRuntimeValue coreSignatureConstraintRuntimeValue constraints
        <*> coreSignatureTypeRuntimeValue signatureType
    UnsupportedSignature tokens ->
      constructor1 "CoreUnsupportedSignature" <$> listRuntimeValue coreSignatureTokenRuntimeValue tokens

coreSignatureConstraintRuntimeValue :: SignatureConstraint -> Either Text RuntimeValue
coreSignatureConstraintRuntimeValue (SignatureConstraint name arguments) =
  constructor2 "CoreSignatureConstraint"
    <$> coreNameRuntimeValue name
    <*> listRuntimeValue coreSignatureTypeRuntimeValue arguments

coreSignatureTypeRuntimeValue :: SignatureType -> Either Text RuntimeValue
coreSignatureTypeRuntimeValue signatureType =
  case signatureType of
    TypeInt -> pure (canonicalNullaryConstructor "CoreIntType")
    TypeFloat -> pure (canonicalNullaryConstructor "CoreFloatType")
    TypeNumeric numericType -> pure (constructor1 "CoreNumericType" (coreNumericTypeRuntimeValue numericType))
    TypeBool -> pure (canonicalNullaryConstructor "CoreBoolType")
    TypeChar -> pure (canonicalNullaryConstructor "CoreCharType")
    TypeText -> pure (canonicalNullaryConstructor "CoreTextType")
    TypeVariable name -> constructor1 "CoreTypeVariable" <$> coreNameRuntimeValue name
    TypeName name -> constructor1 "CoreNamedType" <$> coreNameRuntimeValue name
    TypeApplication name arguments ->
      constructor2 "CoreAppliedType"
        <$> coreNameRuntimeValue name
        <*> listRuntimeValue coreSignatureTypeRuntimeValue arguments
    TypeList elementType -> constructor1 "CoreListType" <$> coreSignatureTypeRuntimeValue elementType
    TypeTuple elementTypes -> constructor1 "CoreTupleType" <$> listRuntimeValue coreSignatureTypeRuntimeValue elementTypes
    TypeFunction argumentType resultType ->
      constructor2 "CoreFunctionType"
        <$> coreSignatureTypeRuntimeValue argumentType
        <*> coreSignatureTypeRuntimeValue resultType

coreSignatureTokenRuntimeValue :: SignatureToken -> Either Text RuntimeValue
coreSignatureTokenRuntimeValue token =
  case token of
    SignatureNameToken name -> constructor1 "CoreSignatureNameToken" <$> coreNameRuntimeValue name
    SignatureIntToken value -> pure (constructor1 "CoreSignatureIntegerToken" (decimalIntegerValue value))
    SignatureArrowToken -> pure (canonicalNullaryConstructor "CoreSignatureArrowToken")
    SignatureAtToken -> pure (canonicalNullaryConstructor "CoreSignatureAtToken")
    SignatureColonToken -> pure (canonicalNullaryConstructor "CoreSignatureColonToken")
    SignatureLParenToken -> pure (canonicalNullaryConstructor "CoreSignatureLeftParenToken")
    SignatureRParenToken -> pure (canonicalNullaryConstructor "CoreSignatureRightParenToken")
    SignatureLBraceToken -> pure (canonicalNullaryConstructor "CoreSignatureLeftBraceToken")
    SignatureRBraceToken -> pure (canonicalNullaryConstructor "CoreSignatureRightBraceToken")
    SignatureLBracketToken -> pure (canonicalNullaryConstructor "CoreSignatureLeftBracketToken")
    SignatureRBracketToken -> pure (canonicalNullaryConstructor "CoreSignatureRightBracketToken")
    SignatureCommaToken -> pure (canonicalNullaryConstructor "CoreSignatureCommaToken")
    SignatureOperatorToken symbol -> pure (constructor1 "CoreSignatureOperatorToken" (VText symbol))
    SignatureOtherToken value -> pure (constructor1 "CoreSignatureOtherToken" (VText value))

coreNumericTypeRuntimeValue :: NumericType -> RuntimeValue
coreNumericTypeRuntimeValue numericType =
  canonicalNullaryConstructor
    ( case numericType of
        NumericInt8 -> "CoreInt8Type"
        NumericInt16 -> "CoreInt16Type"
        NumericInt32 -> "CoreInt32Type"
        NumericInt64 -> "CoreInt64Type"
        NumericUInt8 -> "CoreUInt8Type"
        NumericUInt16 -> "CoreUInt16Type"
        NumericUInt32 -> "CoreUInt32Type"
        NumericUInt64 -> "CoreUInt64Type"
        NumericFloat16 -> "CoreFloat16Type"
        NumericFloat32 -> "CoreFloat32Type"
        NumericFloat64 -> "CoreFloat64Type"
    )

coreNameRuntimeValue :: Name -> Either Text RuntimeValue
coreNameRuntimeValue name =
  case name of
    SourceName identifier -> pure (constructor1 "CoreSourceName" (VText (identifierText identifier)))
    QualifiedName qualifier member ->
      pure
        ( canonicalConstructor
            "CoreQualifiedName"
            [VText (identifierText qualifier), VText (identifierText member)]
        )
    GeneratedName generated -> constructor1 "CoreGeneratedName" <$> coreGeneratedNameKindRuntimeValue generated
    ResolvedName {} -> Left "post-lowering name cannot enter canonical lowering comparison"
    BuiltinName {} -> Left "post-lowering name cannot enter canonical lowering comparison"

coreGeneratedNameKindRuntimeValue :: GeneratedNameKind -> Either Text RuntimeValue
coreGeneratedNameKindRuntimeValue generated =
  case generated of
    LambdaPatternArgument parameterIndex ->
      pure (constructor1 "CoreLambdaPatternArgument" (runtimeIntValue parameterIndex))
    OperatorBinding storageName -> pure (constructor1 "CoreOperatorBinding" (VText storageName))
    OperatorSectionFunction -> Left "post-lowering generated section name cannot enter canonical lowering comparison"
    OperatorSectionLeft -> Left "post-lowering generated section name cannot enter canonical lowering comparison"
    OperatorSectionRight -> Left "post-lowering generated section name cannot enter canonical lowering comparison"

coreSpanRuntimeValue :: SourceSpan -> Either Text RuntimeValue
coreSpanRuntimeValue spanValue =
  case spanValue of
    SourceSpan line column ->
      pure
        ( canonicalConstructor
              "CoreSpan"
            [ canonicalNullaryConstructor "Nothing",
              runtimeIntValue line,
              runtimeIntValue column
            ]
        )
    SourceSpanIn sourcePath line column -> do
      canonicalPath <- normalizeCanonicalSourcePath sourcePath
      pure
        ( canonicalConstructor
            "CoreSpan"
            [ constructor1 "Just" (canonicalSourcePathRuntimeValue canonicalPath),
              runtimeIntValue line,
              runtimeIntValue column
            ]
        )

coreDeclaredModuleExportsRuntimeValue :: DeclaredModuleExports -> Either Text RuntimeValue
coreDeclaredModuleExportsRuntimeValue declaredExports =
  constructor2 "CoreDeclaredModuleExports"
    <$> coreSpanRuntimeValue (declaredModuleExportsSpan declaredExports)
    <*> listRuntimeValue coreModuleExportSelectorRuntimeValue (declaredModuleExportSelectors declaredExports)

coreResolvedImportRuntimeValue :: ResolvedImport -> Either Text RuntimeValue
coreResolvedImportRuntimeValue resolvedImport =
  canonicalConstructor "CoreResolvedImport"
    <$> sequence
      [ coreSpanRuntimeValue (resolvedImportSpan resolvedImport),
        pure (listRuntimeValuePure VText (resolvedImportPath resolvedImport)),
        pure (maybeRuntimeValuePure VText (resolvedImportAlias resolvedImport)),
        pure (maybeRuntimeValuePure (listRuntimeValuePure VText) (resolvedImportSymbols resolvedImport))
      ]

coreModuleExportSelectorRuntimeValue :: ModuleExportSelector -> Either Text RuntimeValue
coreModuleExportSelectorRuntimeValue selector =
  case selector of
    ModuleExportSelector maybeNamespace name ->
      pure
        ( canonicalConstructor
            "CoreNamedExportSelector"
            [maybeRuntimeValuePure coreNameNamespaceRuntimeValue maybeNamespace, VText name]
        )
    ModuleTypeExportSelector name spanValue constructorSelector ->
      constructor3 "CoreTypeExportSelector" (VText name)
        <$> coreSpanRuntimeValue spanValue
        <*> coreTypeConstructorSelectorRuntimeValue constructorSelector

coreNameNamespaceRuntimeValue :: NameNamespace -> RuntimeValue
coreNameNamespaceRuntimeValue namespace =
  canonicalNullaryConstructor
    ( case namespace of
        ValueNamespace -> "CoreValueName"
        ConstructorNamespace -> "CoreConstructorName"
        TypeNamespace -> "CoreTypeName"
        CapabilityNamespace -> "CoreCapabilityName"
    )

coreTypeConstructorSelectorRuntimeValue :: ModuleTypeConstructorSelector -> Either Text RuntimeValue
coreTypeConstructorSelectorRuntimeValue selector =
  case selector of
    AbstractType -> pure (canonicalNullaryConstructor "CoreAbstractType")
    AllTypeConstructors spanValue -> constructor1 "CoreAllConstructors" <$> coreSpanRuntimeValue spanValue
    SelectedTypeConstructors names ->
      constructor1 "CoreSelectedConstructors"
        <$> nonEmptyRuntimeValue coreLocatedExportNameRuntimeValue names

coreLocatedExportNameRuntimeValue :: LocatedModuleExportName -> Either Text RuntimeValue
coreLocatedExportNameRuntimeValue locatedName =
  constructor2 "CoreLocatedExportName" (VText (locatedModuleExportName locatedName))
    <$> coreSpanRuntimeValue (locatedModuleExportSpan locatedName)

constructor1 :: Text -> RuntimeValue -> RuntimeValue
constructor1 name argument = canonicalConstructor name [argument]

constructor2 :: Text -> RuntimeValue -> RuntimeValue -> RuntimeValue
constructor2 name first second = canonicalConstructor name [first, second]

constructor3 :: Text -> RuntimeValue -> RuntimeValue -> RuntimeValue -> RuntimeValue
constructor3 name first second third = canonicalConstructor name [first, second, third]

constructor4 :: Text -> RuntimeValue -> RuntimeValue -> RuntimeValue -> RuntimeValue -> RuntimeValue
constructor4 name first second third fourth = canonicalConstructor name [first, second, third, fourth]

listRuntimeValue :: (value -> Either Text RuntimeValue) -> [value] -> Either Text RuntimeValue
listRuntimeValue converter values = (`VList` Nothing) <$> mapM converter values

listRuntimeValuePure :: (value -> RuntimeValue) -> [value] -> RuntimeValue
listRuntimeValuePure converter values = VList (map converter values) Nothing

nonEmptyRuntimeValue :: (value -> Either Text RuntimeValue) -> NonEmpty.NonEmpty value -> Either Text RuntimeValue
nonEmptyRuntimeValue converter values =
  constructor2 "NonEmpty"
    <$> converter (NonEmpty.head values)
    <*> listRuntimeValue converter (NonEmpty.tail values)

maybeRuntimeValue :: (value -> Either Text RuntimeValue) -> Maybe value -> Either Text RuntimeValue
maybeRuntimeValue converter maybeValue =
  case maybeValue of
    Nothing -> pure (canonicalNullaryConstructor "Nothing")
    Just value -> constructor1 "Just" <$> converter value

maybeRuntimeValuePure :: (value -> RuntimeValue) -> Maybe value -> RuntimeValue
maybeRuntimeValuePure converter maybeValue =
  case maybeValue of
    Nothing -> canonicalNullaryConstructor "Nothing"
    Just value -> constructor1 "Just" (converter value)

decimalIntegerValue :: Integer -> RuntimeValue
decimalIntegerValue = VText . Text.pack . show

fractionalDigits :: Integer -> Integer -> Text
fractionalDigits fractionalPart scale =
  Text.justifyRight digitCount '0' (Text.pack (show (abs fractionalPart)))
  where
    -- FractionalLiteralSource constructs scale as 10^fractionalDigitCount,
    -- making the decimal exponent the exact source-width of these digits.
    digitCount = max 0 (length (show scale) - 1)
