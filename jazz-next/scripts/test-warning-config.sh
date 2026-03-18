#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"

RUNGHC_INCLUDES=(
  -i./jazz-next/src
  -i./jazz-next/test
)

TEST_FILES=(
  jazz-next/test/JazzNext/Compiler/Config/WarningConfigSpec.hs
  jazz-next/test/JazzNext/Compiler/Semantics/RebindingWarningSpec.hs
  jazz-next/test/JazzNext/Compiler/Diagnostics/StructuredErrorDiagnosticsSpec.hs
  jazz-next/test/JazzNext/CLI/CLISpec.hs
  jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  jazz-next/test/JazzNext/Compiler/Semantics/RecursiveBindingsSpec.hs
  jazz-next/test/JazzNext/Compiler/Semantics/PuritySemanticsSpec.hs
  jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
  jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs
  jazz-next/test/JazzNext/Compiler/Parser/IfExpressionParserSpec.hs
  jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs
  jazz-next/test/JazzNext/Compiler/Semantics/IfExpressionTypeSpec.hs
  jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs
  jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs
  jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs
  jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs
  jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
  jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
  jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
  jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
  jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs
  jazz-next/test/JazzNext/Compiler/Parser/OperatorSectionSpec.hs
  jazz-next/test/JazzNext/Compiler/Parser/OperatorInvalidSyntaxSpec.hs
)

for test_file in "${TEST_FILES[@]}"; do
  runghc "${RUNGHC_INCLUDES[@]}" "$test_file"
done
