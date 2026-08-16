-- | Named strictness boundaries for compiler phases and benchmarks.
--
-- Pure compiler products use their declaration-local 'NFData' contracts.
-- Runtime output remains deliberately selective: only the rendered result is
-- forced, so closures and unused runtime exports do not need an 'NFData'
-- instance.
module Jazz.Compiler.Force
  ( forceCompiledProgram,
    forceCompiledModule,
    forceCompiledModules,
    forceCompiledProgramResult,
    forceDiagnostic,
    forceExpr,
    forceInferenceResult,
    forceListWith,
    forceLoweredProgram,
    forceResolvedModule,
    forceRuntimeProgramOutputResult,
    forceSurfaceExpr,
    forceTypedProgram,
    forceTokens,
  )
where

import Control.DeepSeq (rnf)
import qualified Data.Text as Text
import Jazz.Compiler.AST (Expr)
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.Diagnostics.Strictness (forceDiagnostic)
import Jazz.Compiler.LoweredIR (LoweredProgram)
import Jazz.Compiler.ModuleGraph (ResolvedModule)
import Jazz.Compiler.ModuleInterface (CompiledModule, CompiledProgram)
import Jazz.Compiler.ModuleRuntime (RuntimeProgram (runtimeProgramOutput))
import Jazz.Compiler.Parser.AST (SurfaceExpr)
import Jazz.Compiler.Parser.Lexer (Token)
import Jazz.Compiler.Runtime.Semantics (renderRuntimeValue)
import Jazz.Compiler.Runtime.Types (RuntimeValue)
import Jazz.Compiler.TypeInference.Result (InferenceResult)
import qualified Jazz.Compiler.TypedCore as Typed

forceExpr :: Expr -> ()
forceExpr = rnf

forceTokens :: [Token] -> ()
forceTokens = rnf

forceSurfaceExpr :: SurfaceExpr -> ()
forceSurfaceExpr = rnf

forceTypedProgram :: Typed.TypedProgram -> ()
forceTypedProgram = rnf

forceInferenceResult :: InferenceResult -> ()
forceInferenceResult = rnf

forceCompiledProgramResult :: Either Diagnostic CompiledProgram -> ()
forceCompiledProgramResult = rnf

forceCompiledProgram :: CompiledProgram -> ()
forceCompiledProgram = rnf

forceRuntimeProgramOutputResult :: Either Diagnostic RuntimeProgram -> ()
forceRuntimeProgramOutputResult result =
  case result of
    Left diagnostic -> forceDiagnostic diagnostic
    Right runtimeProgram ->
      maybe () forceRenderedRuntimeValue (runtimeProgramOutput runtimeProgram)

forceRenderedRuntimeValue :: RuntimeValue -> ()
forceRenderedRuntimeValue runtimeValue =
  Text.length (renderRuntimeValue runtimeValue) `seq` ()

forceLoweredProgram :: LoweredProgram -> ()
forceLoweredProgram = rnf

forceCompiledModule :: CompiledModule -> ()
forceCompiledModule = rnf

forceResolvedModule :: ResolvedModule -> ()
forceResolvedModule = rnf

forceCompiledModules :: [CompiledModule] -> ()
forceCompiledModules = rnf

forceListWith :: (value -> ()) -> [value] -> ()
forceListWith forceValue values =
  case values of
    [] -> ()
    value : remaining -> forceValue value `seq` forceListWith forceValue remaining
