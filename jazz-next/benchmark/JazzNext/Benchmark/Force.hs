module JazzNext.Benchmark.Force
  ( forceCompiledProgram,
    forceCompiledProgramResult,
    forceExpr,
    forceInferenceResult,
    forceProgramCaseResult,
    forceRuntimeProgramResult,
    forceSurfaceExpr,
    forceTokens,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import JazzNext.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    DataConstructor (..),
    DataConstructorArgument (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    Pattern (..),
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureToken,
    SignatureType (..),
    Statement (..),
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic (..),
    SourceSpan,
    WarningRecord (..),
  )
import JazzNext.Compiler.ModuleExports
  ( ModuleExportSelector (..),
  )
import JazzNext.Compiler.ModuleInterface
  ( CompiledModule (..),
    CompiledPrelude (..),
    CompiledProgram (..),
    ModuleInterface (..),
  )
import JazzNext.Compiler.ModuleRuntime
  ( RuntimeModule (..),
    RuntimeProgram (..),
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceCaseArm (..),
    SurfaceClassMethodSignature (..),
    SurfaceDataConstructor (..),
    SurfaceDataConstructorArgument (..),
    SurfaceExpr (..),
    SurfaceImplMethod (..),
    SurfaceLambdaParameter (..),
    SurfaceLiteral (..),
    SurfacePattern (..),
    SurfaceSignatureConstraint (..),
    SurfaceSignaturePayload (..),
    SurfaceSignatureToken (..),
    SurfaceSignatureType (..),
    SurfaceStatement (..),
  )
import JazzNext.Compiler.Parser.Lexer
  ( Token (..),
    TokenKind (..),
  )
import JazzNext.Compiler.Runtime.Types
  ( RuntimeClosure (..),
    RuntimeMethodCandidate (..),
    RuntimeValue (..),
    foldRuntimeExplicitResultHints,
    runtimeExplicitResultHintsView,
  )
import JazzNext.Compiler.TypeInference (InferenceResult (..))
import JazzNext.ProgramCorpus.Runner (ProgramCaseResult (..))

forceExpr :: Expr -> ()
forceExpr expression =
  case expression of
    ELit literal -> forceLiteral literal
    EVar name -> name `seq` ()
    ELambda name body -> name `seq` forceExpr body
    EOperatorValue symbol -> symbol `seq` ()
    EList values -> forceListWith forceExpr values
    ETuple values -> forceListWith forceExpr values
    EApply callable argument -> forceExpr callable `seq` forceExpr argument
    ETypeApplication value sourceSpan signatureType -> forceExpr value `seq` forceSourceSpan sourceSpan `seq` forceSignatureType signatureType
    EIf condition whenTrue whenFalse -> forceExpr condition `seq` forceExpr whenTrue `seq` forceExpr whenFalse
    EPatternCase value arms -> forceExpr value `seq` forceListWith forceCaseArm arms
    EBinary operator left right -> operator `seq` forceExpr left `seq` forceExpr right
    ESectionLeft value operator -> forceExpr value `seq` operator `seq` ()
    ESectionRight operator value -> operator `seq` forceExpr value
    EBlock statements -> forceListWith forceStatement statements

forceTokens :: [Token] -> ()
forceTokens = forceListWith forceToken

forceToken :: Token -> ()
forceToken token =
  forceTokenKind (tokenKind token) `seq`
    tokenLexeme token `seq`
      forceSourceSpan (tokenSpan token)

forceTokenKind :: TokenKind -> ()
forceTokenKind tokenKindValue =
  case tokenKindValue of
    TIdentifier value -> value `seq` ()
    TInt value -> value `seq` ()
    TChar value -> value `seq` ()
    TText value -> value `seq` ()
    TOperator value -> value `seq` ()
    _ -> tokenKindValue `seq` ()

forceSurfaceExpr :: SurfaceExpr -> ()
forceSurfaceExpr expression =
  case expression of
    SELit literal -> forceSurfaceLiteral literal
    SEVar name -> name `seq` ()
    SEQualifiedVar qualifier member -> qualifier `seq` member `seq` ()
    SELambda parameters body ->
      forceListWith forceSurfaceLambdaParameter (NonEmpty.toList parameters) `seq`
        forceSurfaceExpr body
    SEOperatorValue operator -> operator `seq` ()
    SEList values -> forceListWith forceSurfaceExpr values
    SETuple values -> forceListWith forceSurfaceExpr values
    SEApply callable argument -> forceSurfaceExpr callable `seq` forceSurfaceExpr argument
    SETypeApplication value sourceSpan signatureType ->
      forceSurfaceExpr value `seq`
        forceSourceSpan sourceSpan `seq`
          forceSurfaceSignatureType signatureType
    SEIf condition whenTrue whenFalse ->
      forceSurfaceExpr condition `seq`
        forceSurfaceExpr whenTrue `seq`
          forceSurfaceExpr whenFalse
    SECase value arms -> forceSurfaceExpr value `seq` forceListWith forceSurfaceCaseArm arms
    SEBinary operator left right -> operator `seq` forceSurfaceExpr left `seq` forceSurfaceExpr right
    SESectionLeft value operator -> forceSurfaceExpr value `seq` operator `seq` ()
    SESectionRight operator value -> operator `seq` forceSurfaceExpr value
    SEBlock statements -> forceListWith forceSurfaceStatement statements

forceSurfaceLiteral :: SurfaceLiteral -> ()
forceSurfaceLiteral literal =
  case literal of
    SLInt value -> value `seq` ()
    SLFloat value source numericType -> value `seq` source `seq` numericType `seq` ()
    SLBool value -> value `seq` ()
    SLChar value -> value `seq` ()
    SLText value -> value `seq` ()

forceSurfaceLambdaParameter :: SurfaceLambdaParameter -> ()
forceSurfaceLambdaParameter parameter =
  case parameter of
    SurfaceLambdaIdentifier name -> name `seq` ()
    SurfaceLambdaPattern patternValue -> forceSurfacePattern patternValue

forceSurfacePattern :: SurfacePattern -> ()
forceSurfacePattern patternValue =
  case patternValue of
    SPWildcard -> ()
    SPVariable name -> name `seq` ()
    SPLiteral literal -> forceSurfaceLiteral literal
    SPConstructor name patterns -> name `seq` forceListWith forceSurfacePattern patterns
    SPList patterns -> forceListWith forceSurfacePattern patterns
    SPConsList headPattern tailPattern -> forceSurfacePattern headPattern `seq` forceSurfacePattern tailPattern
    SPTuple patterns -> forceListWith forceSurfacePattern patterns
    SPAs name patternInner -> name `seq` forceSurfacePattern patternInner
    SPOr patterns -> forceListWith forceSurfacePattern patterns

forceSurfaceCaseArm :: SurfaceCaseArm -> ()
forceSurfaceCaseArm (SurfaceCaseArm patternValue guard body) =
  forceSurfacePattern patternValue `seq`
    forceMaybeWith forceSurfaceExpr guard `seq`
      forceSurfaceExpr body

forceSurfaceStatement :: SurfaceStatement -> ()
forceSurfaceStatement statement =
  case statement of
    SSLet name sourceSpan value -> name `seq` forceSourceSpan sourceSpan `seq` forceSurfaceExpr value
    SSSignature name sourceSpan payload ->
      name `seq` forceSourceSpan sourceSpan `seq` forceSurfaceSignaturePayload payload
    SSData sourceSpan name parameters constructors ->
      forceSourceSpan sourceSpan `seq`
        name `seq`
          forceListWhnf parameters `seq`
            forceListWith forceSurfaceDataConstructor constructors
    SSClass sourceSpan name parameters methods ->
      forceSourceSpan sourceSpan `seq`
        name `seq`
          forceListWhnf parameters `seq`
            forceListWith forceSurfaceClassMethod methods
    SSImpl sourceSpan name targets methods ->
      forceSourceSpan sourceSpan `seq`
        name `seq`
          forceListWith forceSurfaceSignatureType targets `seq`
            forceListWith forceSurfaceImplMethod methods
    SSModule sourceSpan path exports ->
      forceSourceSpan sourceSpan `seq`
        forceListWhnf path `seq`
          forceMaybeWith (forceListWith forceModuleExportSelector) exports
    SSImport sourceSpan path alias symbols ->
      forceSourceSpan sourceSpan `seq`
        forceListWhnf path `seq`
          alias `seq`
            forceMaybeWith forceListWhnf symbols
    SSExpr sourceSpan value -> forceSourceSpan sourceSpan `seq` forceSurfaceExpr value

forceSurfaceDataConstructor :: SurfaceDataConstructor -> ()
forceSurfaceDataConstructor (SurfaceDataConstructor name arguments) =
  name `seq` forceListWith forceSurfaceDataConstructorArgument arguments

forceSurfaceDataConstructorArgument :: SurfaceDataConstructorArgument -> ()
forceSurfaceDataConstructorArgument argument =
  case argument of
    SurfaceDataConstructorArgumentName name -> name `seq` ()
    SurfaceDataConstructorArgumentOpaque -> ()

forceSurfaceClassMethod :: SurfaceClassMethodSignature -> ()
forceSurfaceClassMethod (SurfaceClassMethodSignature name sourceSpan payload) =
  name `seq` forceSourceSpan sourceSpan `seq` forceSurfaceSignaturePayload payload

forceSurfaceImplMethod :: SurfaceImplMethod -> ()
forceSurfaceImplMethod (SurfaceImplMethod name sourceSpan body) =
  name `seq` forceSourceSpan sourceSpan `seq` forceSurfaceExpr body

forceSurfaceSignaturePayload :: SurfaceSignaturePayload -> ()
forceSurfaceSignaturePayload payload =
  case payload of
    SurfaceSignatureType signatureType -> forceSurfaceSignatureType signatureType
    SurfaceConstrainedSignature constraints signatureType ->
      forceListWith forceSurfaceSignatureConstraint constraints `seq`
        forceSurfaceSignatureType signatureType
    SurfaceUnsupportedSignature tokens -> forceListWith forceSurfaceSignatureToken tokens

forceSurfaceSignatureConstraint :: SurfaceSignatureConstraint -> ()
forceSurfaceSignatureConstraint (SurfaceSignatureConstraint name arguments) =
  name `seq` forceListWith forceSurfaceSignatureType arguments

forceSurfaceSignatureType :: SurfaceSignatureType -> ()
forceSurfaceSignatureType signatureType =
  case signatureType of
    SurfaceTypeNumeric numericType -> numericType `seq` ()
    SurfaceTypeVariable name -> name `seq` ()
    SurfaceTypeName name -> name `seq` ()
    SurfaceTypeApplication name arguments -> name `seq` forceListWith forceSurfaceSignatureType arguments
    SurfaceTypeList element -> forceSurfaceSignatureType element
    SurfaceTypeTuple elements -> forceListWith forceSurfaceSignatureType elements
    SurfaceTypeFunction argument result ->
      forceSurfaceSignatureType argument `seq` forceSurfaceSignatureType result
    _ -> signatureType `seq` ()

forceSurfaceSignatureToken :: SurfaceSignatureToken -> ()
forceSurfaceSignatureToken token =
  case token of
    SurfaceSignatureNameToken value -> value `seq` ()
    SurfaceSignatureIntToken value -> value `seq` ()
    SurfaceSignatureOperatorToken value -> value `seq` ()
    SurfaceSignatureOtherToken value -> value `seq` ()
    _ -> token `seq` ()

forceModuleExportSelector :: ModuleExportSelector -> ()
forceModuleExportSelector selector =
  moduleExportSelectorNamespace selector `seq`
    moduleExportSelectorName selector `seq`
      ()

forceInferenceResult :: InferenceResult -> ()
forceInferenceResult result =
  forceExpr (inferredExpr result) `seq`
    forceListWith forceWarning (inferredWarnings result) `seq`
      forceListWith forceDiagnostic (inferredErrors result) `seq`
        forceMapWhnf (inferredRuntimeTypeHints result) `seq`
          forceModuleInterface (inferredModuleInterface result)

forceCompiledProgramResult :: Either Diagnostic CompiledProgram -> ()
forceCompiledProgramResult result =
  case result of
    Left diagnostic -> forceDiagnostic diagnostic
    Right compiledProgram -> forceCompiledProgram compiledProgram

forceCompiledProgram :: CompiledProgram -> ()
forceCompiledProgram compiledProgram =
  forceCompiledPrelude (compiledProgramPrelude compiledProgram) `seq`
    forceListWhnf (compiledProgramEntryPath compiledProgram) `seq`
      forceListWith forceCompiledModule (compiledProgramModules compiledProgram) `seq`
        forceListWith forceWarning (compiledProgramWarnings compiledProgram) `seq`
          forceListWith forceDiagnostic (compiledProgramErrors compiledProgram)

forceRuntimeProgramResult :: Either Diagnostic RuntimeProgram -> ()
forceRuntimeProgramResult result =
  case result of
    Left diagnostic -> forceDiagnostic diagnostic
    Right runtimeProgram ->
      forceListWith forceRuntimeModule (runtimeProgramModules runtimeProgram) `seq`
        forceMaybeWith forceRuntimeValue (runtimeProgramOutput runtimeProgram)

forceProgramCaseResult :: ProgramCaseResult -> ()
forceProgramCaseResult result =
  programCaseResultTermination result `seq`
    programCaseResultStdout result `seq`
      forceListWith forceDiagnostic (programCaseResultDiagnostics result) `seq`
        forceListWith forceWarning (programCaseResultWarnings result) `seq`
          programCaseResultObservation result `seq`
            ()

forceLiteral :: Literal -> ()
forceLiteral literal =
  case literal of
    LInt value -> value `seq` ()
    LFloat value source numericType -> value `seq` source `seq` numericType `seq` ()
    LBool value -> value `seq` ()
    LChar value -> value `seq` ()
    LText value -> value `seq` ()

forceCaseArm :: CaseArm -> ()
forceCaseArm (CaseArm pattern' guard body) =
  forcePattern pattern' `seq`
    forceMaybeWith forceExpr guard `seq`
      forceExpr body

forcePattern :: Pattern -> ()
forcePattern pattern' =
  case pattern' of
    PWildcard -> ()
    PVariable name -> name `seq` ()
    PLiteral literal -> forceLiteral literal
    PConstructor name arguments -> name `seq` forceListWith forcePattern arguments
    PList values -> forceListWith forcePattern values
    PConsList headPattern tailPattern -> forcePattern headPattern `seq` forcePattern tailPattern
    PTuple values -> forceListWith forcePattern values
    PAs name inner -> name `seq` forcePattern inner
    POr alternatives -> forceListWith forcePattern alternatives

forceStatement :: Statement -> ()
forceStatement statement =
  case statement of
    SLet name sourceSpan value -> name `seq` forceSourceSpan sourceSpan `seq` forceExpr value
    SSignature name sourceSpan payload -> name `seq` forceSourceSpan sourceSpan `seq` forceSignaturePayload payload
    SData sourceSpan name parameters constructors ->
      forceSourceSpan sourceSpan `seq`
        name `seq`
          forceListWhnf parameters `seq`
            forceListWith forceDataConstructor constructors
    SClass sourceSpan name parameters methods ->
      forceSourceSpan sourceSpan `seq`
        name `seq`
          forceListWhnf parameters `seq`
            forceListWith forceClassMethod methods
    SImpl sourceSpan name targets methods ->
      forceSourceSpan sourceSpan `seq`
        name `seq`
          forceListWith forceSignatureType targets `seq`
            forceListWith forceImplMethod methods
    SModule sourceSpan path -> forceSourceSpan sourceSpan `seq` forceListWhnf path
    SImport sourceSpan path alias symbols ->
      forceSourceSpan sourceSpan `seq`
        forceListWhnf path `seq`
          alias `seq`
            forceMaybeWith forceListWhnf symbols
    SExpr sourceSpan value -> forceSourceSpan sourceSpan `seq` forceExpr value

forceDataConstructor :: DataConstructor -> ()
forceDataConstructor (DataConstructor name arguments) = name `seq` forceListWith forceDataConstructorArgument arguments

forceDataConstructorArgument :: DataConstructorArgument -> ()
forceDataConstructorArgument argument =
  case argument of
    DataConstructorArgumentName name -> name `seq` ()
    DataConstructorArgumentOpaque -> ()

forceClassMethod :: ClassMethodSignature -> ()
forceClassMethod (ClassMethodSignature name sourceSpan payload) =
  name `seq` forceSourceSpan sourceSpan `seq` forceSignaturePayload payload

forceImplMethod :: ImplMethod -> ()
forceImplMethod (ImplMethod name sourceSpan body) = name `seq` forceSourceSpan sourceSpan `seq` forceExpr body

forceSignaturePayload :: SignaturePayload -> ()
forceSignaturePayload payload =
  case payload of
    SignatureType signatureType -> forceSignatureType signatureType
    ConstrainedSignature constraints signatureType ->
      forceListWith forceSignatureConstraint constraints `seq` forceSignatureType signatureType
    UnsupportedSignature tokens -> forceListWith forceSignatureToken tokens

forceSignatureConstraint :: SignatureConstraint -> ()
forceSignatureConstraint (SignatureConstraint name arguments) = name `seq` forceListWith forceSignatureType arguments

forceSignatureType :: SignatureType -> ()
forceSignatureType signatureType =
  case signatureType of
    TypeInt -> ()
    TypeFloat -> ()
    TypeNumeric numericType -> numericType `seq` ()
    TypeBool -> ()
    TypeChar -> ()
    TypeText -> ()
    TypeVariable name -> name `seq` ()
    TypeName name -> name `seq` ()
    TypeApplication name arguments -> name `seq` forceListWith forceSignatureType arguments
    TypeList element -> forceSignatureType element
    TypeTuple elements -> forceListWith forceSignatureType elements
    TypeFunction argument result -> forceSignatureType argument `seq` forceSignatureType result

forceSignatureToken :: SignatureToken -> ()
forceSignatureToken token = token `seq` ()

forceSourceSpan :: SourceSpan -> ()
forceSourceSpan sourceSpan = sourceSpan `seq` ()

forceDiagnostic :: Diagnostic -> ()
forceDiagnostic diagnostic =
  diagnosticCode diagnostic `seq`
    diagnosticSummary diagnostic `seq`
      diagnosticPrimarySpan diagnostic `seq`
        diagnosticRelatedSpan diagnostic `seq`
          diagnosticSubject diagnostic `seq`
            forceListWhnf (diagnosticNotes diagnostic)

forceWarning :: WarningRecord -> ()
forceWarning warning =
  warningCategory warning `seq`
    warningCodeText warning `seq`
      warningVariableName warning `seq`
        warningPrimarySpan warning `seq`
          warningPreviousSpan warning `seq`
            warningMessage warning `seq`
              ()

forceModuleInterface :: ModuleInterface -> ()
forceModuleInterface interface =
  forceMapWhnf (interfaceValueTypes interface) `seq`
    forceMapWhnf (interfaceDataTypes interface) `seq`
      forceMapWhnf (interfaceClassFacts interface) `seq`
        forceSetWhnf (interfaceGeneratedEqualityClassFacts interface) `seq`
          forceSetWhnf (interfaceConcreteImplFacts interface) `seq`
            forceMapWhnf (interfaceClassMethods interface) `seq`
              forceMapWhnf (interfaceConcreteImplMethods interface) `seq`
                forceMapWhnf (interfaceRuntimeHints interface)

forceCompiledPrelude :: CompiledPrelude -> ()
forceCompiledPrelude compiledPrelude =
  compiledPreludeBuiltinMode compiledPrelude `seq`
    forceModuleInterface (compiledPreludeInterface compiledPrelude) `seq`
      forceListWith forceWarning (compiledPreludeWarnings compiledPrelude) `seq`
        forceListWith forceDiagnostic (compiledPreludeErrors compiledPrelude) `seq`
          forceMaybeWith forceExpr (compiledPreludeExpr compiledPrelude) `seq`
            forceMapWhnf (compiledPreludeRuntimeHints compiledPrelude)

forceCompiledModule :: CompiledModule -> ()
forceCompiledModule compiledModule =
  compiledResolvedModule compiledModule `seq`
    forceModuleInterface (compiledModuleInterface compiledModule) `seq`
      forceListWith forceWarning (compiledModuleWarnings compiledModule) `seq`
        forceListWith forceDiagnostic (compiledModuleErrors compiledModule) `seq`
          forceExpr (compiledModuleExpr compiledModule)

forceRuntimeModule :: RuntimeModule -> ()
forceRuntimeModule runtimeModule =
  forceListWhnf (runtimeModulePath runtimeModule) `seq`
    forceMapWith forceRuntimeCell (runtimeModuleExports runtimeModule)

forceRuntimeCell :: Either Diagnostic RuntimeValue -> ()
forceRuntimeCell runtimeCell =
  case runtimeCell of
    Left diagnostic -> forceDiagnostic diagnostic
    Right runtimeValue -> forceRuntimeValue runtimeValue

forceRuntimeValue :: RuntimeValue -> ()
forceRuntimeValue runtimeValue =
  case runtimeExplicitResultHintsView runtimeValue of
    Just (hints, innerValue) ->
      foldRuntimeExplicitResultHints
        (\() signatureType -> forceSignatureType signatureType)
        ()
        hints
        `seq` forceRuntimeValue innerValue
    Nothing -> forceRuntimeValueWithoutExplicitResultHints runtimeValue

forceRuntimeValueWithoutExplicitResultHints :: RuntimeValue -> ()
forceRuntimeValueWithoutExplicitResultHints runtimeValue =
  case runtimeValue of
    VInt value metadata -> value `seq` metadata `seq` ()
    VFloat value metadata -> value `seq` metadata `seq` ()
    VBool value -> value `seq` ()
    VChar value -> value `seq` ()
    VText value -> value `seq` ()
    VList values signatureType -> forceListWith forceRuntimeValue values `seq` signatureType `seq` ()
    VTuple values -> forceListWith forceRuntimeValue values
    VClosure closure ->
      Map.size (runtimeClosureEnvironment closure) `seq`
        runtimeClosureEnvironmentMayReachHostCells closure `seq`
          runtimeClosureParameter closure `seq`
            forceExpr (runtimeClosureBody closure) `seq`
              runtimeClosureTypeHint closure `seq`
                runtimeClosureModulePath closure `seq`
                  runtimeClosureCallableIdentity closure `seq`
                    ()
    VBuiltin builtin arguments -> builtin `seq` forceListWith forceRuntimeValue arguments
    VOperator operator arguments -> operator `seq` forceListWith forceRuntimeValue arguments
    VSectionLeft operator value -> operator `seq` forceRuntimeValue value
    VSectionRight operator value -> operator `seq` forceRuntimeValue value
    VConstructor typeName parameters name arguments values ->
      typeName `seq`
        forceListWhnf parameters `seq`
          name `seq`
            forceListWith forceDataConstructorArgument arguments `seq`
              forceListWith forceRuntimeValue values
    VQualifiedMethod className methodName payload candidates arguments ->
      className `seq`
        methodName `seq`
          forceSignaturePayload payload `seq`
            forceListWith forceRuntimeMethodCandidate candidates `seq`
              forceListWith forceRuntimeValue arguments
    VTyped signatureType value -> forceSignatureType signatureType `seq` forceRuntimeValue value
    VExplicitTypeApplication signatureType value -> forceSignatureType signatureType `seq` forceRuntimeValue value
    VDeferredHostBinding key modulePath body _ runtimeHints numericType signatureType ->
      key `seq`
        modulePath `seq`
          forceExpr body `seq`
            forceMapWhnf runtimeHints `seq`
              numericType `seq`
                signatureType `seq`
                  ()
    _ -> error "unexpected explicit-result-hint runtime value"

forceRuntimeMethodCandidate :: RuntimeMethodCandidate -> ()
forceRuntimeMethodCandidate (RuntimeMethodCandidate evidence runtimeCell) =
  evidence `seq` forceRuntimeCell runtimeCell

forceListWhnf :: [value] -> ()
forceListWhnf = forceListWith (`seq` ())

forceListWith :: (value -> ()) -> [value] -> ()
forceListWith forceValue values =
  case values of
    [] -> ()
    value : remaining -> forceValue value `seq` forceListWith forceValue remaining

forceMaybeWith :: (value -> ()) -> Maybe value -> ()
forceMaybeWith = maybe ()

forceMapWhnf :: Map.Map key value -> ()
forceMapWhnf = forceMapWith (`seq` ())

forceMapWith :: (value -> ()) -> Map.Map key value -> ()
forceMapWith forceValue = Map.foldrWithKey (\key value rest -> key `seq` forceValue value `seq` rest) ()

forceSetWhnf :: Set.Set value -> ()
forceSetWhnf = Set.foldr seq ()
