#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"

runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/WarningConfigSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/RebindingWarningSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/CLISpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/BindingSignatureCoherenceSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/ParserFoundationSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/IfExpressionParserSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/IfExpressionTypeSpec.hs
