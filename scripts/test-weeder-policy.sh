#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
probe="$(mktemp -d "${TMPDIR:-/tmp}/jazz-weeder-policy.XXXXXX")"
trap 'rm -r -- "$probe"' EXIT
cd "$probe"
mkdir -p src/Jazz/Compiler

# Use a real baseline path and type so this checks named retention as well as
# rejection of new types in an already-exempted module.
cat > src/Jazz/Compiler/Diagnostics.hs <<'HASKELL'
module Jazz.Compiler.Diagnostics where
data DiagnosticOrigin = CompilationOrigin deriving (Eq, Ord, Show)
HASKELL

ghc -v0 -fforce-recomp -fno-code -fwrite-ide-info -hiedir hie src/Jazz/Compiler/Diagnostics.hs
for config in weeder-production.toml weeder.toml; do
  weeder --config="$ROOT/$config" --hie-directory=hie --no-default-fields
done

cat >> src/Jazz/Compiler/Diagnostics.hs <<'HASKELL'
data UnusedPayload = UnusedPayload DiagnosticOrigin deriving (Eq, Ord, Show)
data DiagnosticOriginExtra = DiagnosticOriginExtra deriving (Eq, Ord, Show)
HASKELL

ghc -v0 -fforce-recomp -fno-code -fwrite-ide-info -hiedir hie src/Jazz/Compiler/Diagnostics.hs
for config in weeder-production.toml weeder.toml; do
  if output="$(weeder --config="$ROOT/$config" --hie-directory=hie --no-default-fields 2>&1)"; then
    printf 'FAIL: %s accepted new unused types\n' "$config" >&2
    exit 1
  fi
  for name in UnusedPayload DiagnosticOriginExtra; do
    case "$output" in
      *": $name"*) ;;
      *)
        printf 'FAIL: %s did not report %s:\n%s\n' "$config" "$name" "$output" >&2
        exit 1
        ;;
    esac
  done
done
printf 'PASS: both Weeder policies retain named types and reject new unused types\n'
