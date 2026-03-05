#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"

RUNGHC_INCLUDES=(
  -i./jazz-next/src
  -i./jazz-next/test
)

TEST_FILES=(
  jazz-next/test/WarningConfigSpec.hs
  jazz-next/test/RebindingWarningSpec.hs
  jazz-next/test/CLISpec.hs
  jazz-next/test/BindingSignatureCoherenceSpec.hs
  jazz-next/test/PuritySemanticsSpec.hs
  jazz-next/test/ParserFoundationSpec.hs
  jazz-next/test/IfExpressionParserSpec.hs
  jazz-next/test/IfExpressionTypeSpec.hs
  jazz-next/test/ModuleImportParserSpec.hs
  jazz-next/test/ModuleResolutionSpec.hs
  jazz-next/test/LoaderSpec.hs
  jazz-next/test/PrimitiveSemanticsSpec.hs
  jazz-next/test/RuntimeSemanticsSpec.hs
  jazz-next/test/PreludeLoadingSpec.hs
  jazz-next/test/BuiltinCatalogSpec.hs
  jazz-next/test/OperatorFixitySpec.hs
  jazz-next/test/OperatorSectionSpec.hs
  jazz-next/test/OperatorInvalidSyntaxSpec.hs
)

for test_file in "${TEST_FILES[@]}"; do
  runghc "${RUNGHC_INCLUDES[@]}" "$test_file"
done
