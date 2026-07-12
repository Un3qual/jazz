#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"
RUNGHC="${ROOT}/jazz-next/scripts/runghc.sh"
CABAL_MANIFEST="${ROOT}/jazz-next/jazz-next.cabal"

tmpdir="$(mktemp -d)"
cleanup() {
  rm -rf "$tmpdir"
}
trap cleanup EXIT

bash jazz-next/scripts/test-check-stdlib-format.sh
bash jazz-next/scripts/check-stdlib-format.sh

RUNGHC_INCLUDES=(
  -i./jazz-next/src
  -i./jazz-next/test
)

discover_test_suite_main_files() {
  manifest_path="$1"
  output_path="$2"
  awk '
    function finish_test_suite() {
      if (!in_test_suite) {
        return
      }
      suite_count++
      if (main_count != 1) {
        printf "malformed test-suite %s: expected exactly one main-is field, found %d\n", suite_name, main_count > "/dev/stderr"
        malformed = 1
      }
    }

    /^[^[:space:]#]/ {
      finish_test_suite()
      in_test_suite = ($1 == "test-suite")
      suite_name = in_test_suite ? $2 : ""
      main_count = 0
      next
    }

    in_test_suite && /^[[:space:]]*main-is[[:space:]]*:/ {
      main_path = $0
      sub(/^[[:space:]]*main-is[[:space:]]*:[[:space:]]*/, "", main_path)
      sub(/[[:space:]]*$/, "", main_path)
      main_count++
      print main_path
    }

    END {
      finish_test_suite()
      if (suite_count == 0) {
        print "test-suite inventory is empty" > "/dev/stderr"
        malformed = 1
      }
      if (malformed) {
        exit 2
      }
    }
  ' "$manifest_path" >"$output_path"
}

inventory_file="${tmpdir}/test-suite-main-is.txt"
if ! discover_test_suite_main_files "$CABAL_MANIFEST" "$inventory_file"; then
  echo "FAIL: malformed test-suite inventory in ${CABAL_MANIFEST}" >&2
  exit 1
fi

empty_manifest="${tmpdir}/empty.cabal"
empty_inventory="${tmpdir}/empty-inventory.txt"
empty_inventory_stderr="${tmpdir}/empty-inventory-stderr.txt"
printf 'executable jazz-next\n  main-is: Main.hs\n' >"$empty_manifest"
if discover_test_suite_main_files "$empty_manifest" "$empty_inventory" 2>"$empty_inventory_stderr"; then
  echo "FAIL: empty test-suite inventory should be rejected" >&2
  exit 1
fi
if ! grep -q 'test-suite inventory is empty' "$empty_inventory_stderr"; then
  echo "FAIL: empty test-suite inventory should produce a clear diagnostic" >&2
  exit 1
fi

malformed_manifest="${tmpdir}/malformed.cabal"
malformed_inventory="${tmpdir}/malformed-inventory.txt"
malformed_inventory_stderr="${tmpdir}/malformed-inventory-stderr.txt"
printf 'test-suite missing-main\n  type: exitcode-stdio-1.0\n' >"$malformed_manifest"
if discover_test_suite_main_files "$malformed_manifest" "$malformed_inventory" 2>"$malformed_inventory_stderr"; then
  echo "FAIL: malformed test-suite inventory should be rejected" >&2
  exit 1
fi
if ! grep -q 'malformed test-suite missing-main' "$malformed_inventory_stderr"; then
  echo "FAIL: malformed test-suite inventory should produce a clear diagnostic" >&2
  exit 1
fi

echo "PASS: Cabal inventory discovery rejects empty and malformed test-suite stanzas"

TEST_FILES=()
while IFS= read -r test_main; do
  TEST_FILES+=("jazz-next/test/${test_main}")
done <"$inventory_file"

if [[ "${#TEST_FILES[@]}" -eq 0 ]]; then
  echo "FAIL: no test-suite main-is files discovered from ${CABAL_MANIFEST}" >&2
  exit 1
fi

for test_file in "${TEST_FILES[@]}"; do
  if [[ ! -f "$test_file" ]]; then
    echo "FAIL: discovered test-suite file does not exist: ${test_file}" >&2
    exit 1
  fi
done

echo "discovered ${#TEST_FILES[@]} Cabal test suites"

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

fake_cabal_bin="${tmpdir}/fake-cabal-bin"
mkdir -p "$fake_cabal_bin"
fake_runghc_counter="${tmpdir}/fake-runghc-counter"

cat >"${fake_cabal_bin}/cabal" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail

if [[ -n "${FAKE_CABAL_PROBE_STATUS-}" ]]; then
  exit "$FAKE_CABAL_PROBE_STATUS"
fi

case " $* " in
  *" command -v runghc "*)
    case "${FAKE_CABAL_PROBE_MODE-default}" in
      default) printf '%s\n' '__jazz_next_runghc_probe__:available' ;;
      noisy) printf '%s\n' 'Resolving dependencies...' '__jazz_next_runghc_probe__:available' ;;
      ambiguous) printf '%s\n' '__jazz_next_runghc_probe__:available' '__jazz_next_runghc_probe__:missing' ;;
      missing) printf '%s\n' 'Resolving dependencies...' ;;
      *) exit 3 ;;
    esac
    exit 0
    ;;
esac

for argument in "$@"; do
  if [[ "$argument" == "runghc" ]]; then
    exec "${FAKE_CABAL_BIN}/runghc"
  fi
done

exit 2
EOF

cat >"${fake_cabal_bin}/runghc" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
printf 'run\n' >>"${FAKE_RUNGHC_COUNTER}"
exit 127
EOF
chmod +x "${fake_cabal_bin}/cabal" "${fake_cabal_bin}/runghc"

set +e
env -u JAZZ_NEXT_RUNGHC_IN_CABAL \
  -u JAZZ_NEXT_RUNGHC_NO_CABAL \
  PATH="${fake_cabal_bin}:${PATH}" \
  FAKE_CABAL_BIN="$fake_cabal_bin" \
  FAKE_RUNGHC_COUNTER="$fake_runghc_counter" \
  "$RUNGHC" "${tmpdir}/legitimate-127.hs"
fake_runghc_status=$?
set -e

if [[ "$fake_runghc_status" -ne 127 ]]; then
  echo "FAIL: runghc wrapper returned ${fake_runghc_status}, expected launched-child status 127" >&2
  exit 1
fi

fake_runghc_count="$(awk 'END { print NR + 0 }' "$fake_runghc_counter")"
if [[ "$fake_runghc_count" -ne 1 ]]; then
  echo "FAIL: runghc wrapper launched the child ${fake_runghc_count} times after status 127" >&2
  exit 1
fi

echo "PASS: runghc wrapper preserves a launched child's status 127 without retrying"

: >"$fake_runghc_counter"
set +e
env -u JAZZ_NEXT_RUNGHC_IN_CABAL \
  -u JAZZ_NEXT_RUNGHC_NO_CABAL \
  PATH="${fake_cabal_bin}:${PATH}" \
  FAKE_CABAL_BIN="$fake_cabal_bin" \
  FAKE_CABAL_PROBE_MODE=noisy \
  FAKE_RUNGHC_COUNTER="$fake_runghc_counter" \
  "$RUNGHC" "${tmpdir}/noisy-probe.hs"
noisy_probe_status=$?
set -e

if [[ "$noisy_probe_status" -ne 127 ]]; then
  echo "FAIL: runghc wrapper returned ${noisy_probe_status}, expected launched-child status 127 after noisy probe" >&2
  exit 1
fi

noisy_probe_launch_count="$(awk 'END { print NR + 0 }' "$fake_runghc_counter")"
if [[ "$noisy_probe_launch_count" -ne 1 ]]; then
  echo "FAIL: runghc wrapper launched the child ${noisy_probe_launch_count} times after noisy probe" >&2
  exit 1
fi

echo "PASS: runghc wrapper accepts harmless Cabal output around one probe marker"

for invalid_probe_mode in ambiguous missing; do
  : >"$fake_runghc_counter"
  invalid_probe_stderr="${tmpdir}/${invalid_probe_mode}-probe-stderr.txt"
  set +e
  env -u JAZZ_NEXT_RUNGHC_IN_CABAL \
    -u JAZZ_NEXT_RUNGHC_NO_CABAL \
    PATH="${fake_cabal_bin}:${PATH}" \
    FAKE_CABAL_BIN="$fake_cabal_bin" \
    FAKE_CABAL_PROBE_MODE="$invalid_probe_mode" \
    FAKE_RUNGHC_COUNTER="$fake_runghc_counter" \
    "$RUNGHC" "${tmpdir}/${invalid_probe_mode}-probe.hs" \
    >/dev/null 2>"$invalid_probe_stderr"
  invalid_probe_status=$?
  set -e

  if [[ "$invalid_probe_status" -ne 1 ]]; then
    echo "FAIL: ${invalid_probe_mode} Cabal probe returned ${invalid_probe_status}, expected status 1" >&2
    exit 1
  fi
  if ! grep -q 'unexpected Cabal runghc probe output' "$invalid_probe_stderr"; then
    echo "FAIL: ${invalid_probe_mode} Cabal probe should produce a clear diagnostic" >&2
    exit 1
  fi
  invalid_probe_launch_count="$(awk 'END { print NR + 0 }' "$fake_runghc_counter")"
  if [[ "$invalid_probe_launch_count" -ne 0 ]]; then
    echo "FAIL: runghc wrapper launched a child after ${invalid_probe_mode} Cabal probe" >&2
    exit 1
  fi
done

echo "PASS: runghc wrapper rejects missing and ambiguous Cabal probe markers"

: >"$fake_runghc_counter"
set +e
env -u JAZZ_NEXT_RUNGHC_IN_CABAL \
  -u JAZZ_NEXT_RUNGHC_NO_CABAL \
  PATH="${fake_cabal_bin}:${PATH}" \
  FAKE_CABAL_BIN="$fake_cabal_bin" \
  FAKE_CABAL_PROBE_STATUS=127 \
  FAKE_RUNGHC_COUNTER="$fake_runghc_counter" \
  "$RUNGHC" "${tmpdir}/cabal-probe-failed.hs"
fake_cabal_probe_status=$?
set -e

if [[ "$fake_cabal_probe_status" -ne 127 ]]; then
  echo "FAIL: runghc wrapper returned ${fake_cabal_probe_status}, expected Cabal probe status 127" >&2
  exit 1
fi

fake_probe_fallback_count="$(awk 'END { print NR + 0 }' "$fake_runghc_counter")"
if [[ "$fake_probe_fallback_count" -ne 0 ]]; then
  echo "FAIL: runghc wrapper launched fallback after Cabal probe itself returned 127" >&2
  exit 1
fi

echo "PASS: runghc wrapper propagates Cabal probe status 127 without fallback"

if rg -n '^data (ExpressionType|InferState|TypeScheme)\b' jazz-next/src/JazzNext/Compiler/TypeInference.hs; then
  echo "TypeInference facade still owns internal model types" >&2
  exit 1
fi

if rg -n 'ModuleReplay|moduleGraphValidationExpr|moduleGraphRuntimeExpr|__module::|ModuleReplayBridge' jazz-next/src jazz-next/test jazz-next/jazz-next.cabal; then
  echo "Removed module-replay architecture resurfaced" >&2
  exit 1
fi

if rg -n 'JazzNext\.Compiler\.Identifier' jazz-next/src jazz-next/test jazz-next/jazz-next.cabal; then
  echo "legacy Identifier module is still referenced" >&2
  exit 1
fi

if rg -n 'parsedModule(Exports|ValueNames|DataTypeNames|ConstructorNames|ClassNames)|resolved(ExportsState|ValueExportsState|DataTypeExportsState|ConstructorExportsState|ClassExportsState)' jazz-next/src/JazzNext/Compiler/ModuleResolver.hs; then
  echo "ModuleResolver still carries parallel namespace export inventories" >&2
  exit 1
fi

if rg -n '^library$' jazz-next/jazz-next.cabal; then
  echo "public compiler library remains exposed" >&2
  exit 1
fi

if rg -n 'length original - length remaining' jazz-next/src/JazzNext/Compiler/Parser/Declaration.hs; then
  echo "declaration parser still rescans token suffixes to advance owned prefixes" >&2
  exit 1
fi

for test_file in "${TEST_FILES[@]}"; do
  "$RUNGHC" "${RUNGHC_INCLUDES[@]}" "$test_file"
done

if rg -n 'SurfaceConstrainedSignatureType|ConstraintSignatureType' \
  jazz-next/src jazz-next/test jazz-next/jazz-next.cabal; then
  echo "parallel constrained signature type representation remains" >&2
  exit 1
fi

echo "signature type unification checks passed"
