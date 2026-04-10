#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"
RUNGHC="${ROOT}/jazz-next/scripts/runghc.sh"

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

tmpdir="$(mktemp -d)"
cleanup() {
  rm -rf "$tmpdir"
}
trap cleanup EXIT

bash_bin="$(command -v bash)"
empty_path_dir="${tmpdir}/empty-path"
mkdir -p "$empty_path_dir"
runghc_stderr="${tmpdir}/runghc-stderr.txt"
if env -u HOME PATH="$empty_path_dir" "$bash_bin" "$RUNGHC" >/dev/null 2>"$runghc_stderr"; then
  echo "FAIL: runghc wrapper should fail cleanly when HOME is unset and runghc is unavailable" >&2
  exit 1
fi

if grep -q "HOME: unbound variable" "$runghc_stderr"; then
  echo "FAIL: runghc wrapper should not crash on unset HOME" >&2
  exit 1
fi

if ! grep -q "runghc not found on PATH" "$runghc_stderr"; then
  echo "FAIL: runghc wrapper should report missing runghc when HOME is unset" >&2
  exit 1
fi

echo "PASS: runghc wrapper handles missing HOME without unbound-variable crash"

for test_file in "${TEST_FILES[@]}"; do
  "$RUNGHC" "${RUNGHC_INCLUDES[@]}" "$test_file"
done
