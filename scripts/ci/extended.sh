#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
cd "$ROOT"

JAZZ_ARTIFACT_ROOT="${JAZZ_ARTIFACT_ROOT:-artifacts/extended}"
: "${JAZZ_BENCHMARK_LABEL:?JAZZ_BENCHMARK_LABEL is required}"
if [[ -e "$JAZZ_ARTIFACT_ROOT" && ! -d "$JAZZ_ARTIFACT_ROOT" ]]; then
  printf 'FAIL: artifact root exists and is not a directory: %s\n' "$JAZZ_ARTIFACT_ROOT" >&2
  exit 1
fi
if [[ -d "$JAZZ_ARTIFACT_ROOT" && -n "$(find "$JAZZ_ARTIFACT_ROOT" -mindepth 1 -print -quit)" ]]; then
  printf 'FAIL: artifact root must be empty before extended verification: %s\n' "$JAZZ_ARTIFACT_ROOT" >&2
  exit 1
fi
mkdir -p "$JAZZ_ARTIFACT_ROOT/corpus" "$JAZZ_ARTIFACT_ROOT/benchmarks"

corpus_log_root="$(mktemp -d "${TMPDIR:-/tmp}/jazz-corpus-logs.XXXXXX")"
trap 'rm -r -- "$corpus_log_root"' EXIT
mkdir -p "$corpus_log_root/first" "$corpus_log_root/second"

full_scale_components=(
  jazz-parser-scale-full-expression-spec
  jazz-parser-scale-full-declarations-spec
  jazz-parser-scale-full-control-flow-spec
  jazz-parser-scale-full-operator-spec
)

cabal test all "${full_scale_components[@]}" \
  -ffull-parser-scale \
  --test-show-details=always \
  --test-log="$corpus_log_root/first/\$test-suite.log"

cabal test program-corpus-spec \
  --test-show-details=always \
  --test-log="$corpus_log_root/second/\$test-suite.log"

python3 - \
  "$corpus_log_root/first/program-corpus-spec.log" \
  "$corpus_log_root/second/program-corpus-spec.log" \
  "$JAZZ_ARTIFACT_ROOT/corpus/pass-one.txt" \
  "$JAZZ_ARTIFACT_ROOT/corpus/pass-two.txt" <<'PY'
import sys
from pathlib import Path

first_log, second_log, first_destination, second_destination = map(Path, sys.argv[1:])


def normalize(log_path: Path) -> str:
    if not log_path.is_file():
        raise SystemExit(f"program corpus log is missing: {log_path}")
    stable_lines = [
        line.rstrip()
        for line in log_path.read_text(encoding="utf-8").replace("\r\n", "\n").splitlines()
        if line.startswith("PASS: ") or line == "All ProgramCorpus tests passed."
    ]
    if not stable_lines or stable_lines[-1] != "All ProgramCorpus tests passed.":
        raise SystemExit(f"program corpus log has no complete successful result: {log_path}")
    return "\n".join(stable_lines) + "\n"


first_destination.write_text(normalize(first_log), encoding="utf-8")
second_destination.write_text(normalize(second_log), encoding="utf-8")
PY

cmp \
  "$JAZZ_ARTIFACT_ROOT/corpus/pass-one.txt" \
  "$JAZZ_ARTIFACT_ROOT/corpus/pass-two.txt"

JAZZ_ARTIFACT_ROOT="$JAZZ_ARTIFACT_ROOT/determinism" \
  bash scripts/ci/determinism.sh

cabal --project-file=cabal.project.profile-stages \
  build all --builddir=dist-newstyle-profile-stages
cabal --project-file=cabal.project.profile-hotspots \
  build all --builddir=dist-newstyle-profile-hotspots

cabal bench jazz-bench \
  --benchmark-option="--environment-label=$JAZZ_BENCHMARK_LABEL" \
  --benchmark-option="--result-root=$JAZZ_ARTIFACT_ROOT/benchmarks"

python3 - "$JAZZ_ARTIFACT_ROOT/benchmarks" "$JAZZ_BENCHMARK_LABEL" <<'PY'
import csv
import json
import sys
from pathlib import Path

benchmark_root = Path(sys.argv[1]).resolve()
expected_label = sys.argv[2]
label_root = benchmark_root / expected_label

root_entries = sorted(benchmark_root.iterdir())
if root_entries != [label_root] or not label_root.is_dir():
    raise SystemExit("benchmark output must contain exactly the requested label directory")

run_directories = [path for path in label_root.iterdir() if path.is_dir()]
if len(list(label_root.iterdir())) != 1 or len(run_directories) != 1:
    raise SystemExit("benchmark label directory must contain exactly one run directory")

run_directory = run_directories[0]
environment_path = run_directory / "environment.json"
results_path = run_directory / "results.csv"
if not environment_path.is_file() or environment_path.stat().st_size == 0:
    raise SystemExit("generated benchmark environment.json is missing or empty")
if not results_path.is_file() or results_path.stat().st_size == 0:
    raise SystemExit("generated benchmark results.csv is missing or empty")

metadata = json.loads(environment_path.read_text(encoding="utf-8"))
if not isinstance(metadata, dict):
    raise SystemExit("generated benchmark environment metadata must be a JSON object")
if metadata.get("environment_label") != expected_label:
    raise SystemExit("generated benchmark environment label does not match the invocation")
if metadata.get("schema_version") != 2:
    raise SystemExit("generated benchmark environment metadata schema must be version 2")
if metadata.get("run_id") != run_directory.name:
    raise SystemExit("generated benchmark run id does not match its directory")

with results_path.open(newline="", encoding="utf-8") as results_file:
    rows = list(csv.reader(results_file))
if len(rows) < 2 or not rows[0] or any(not heading for heading in rows[0]):
    raise SystemExit("generated benchmark results.csv has no data rows or a malformed header")
PY

cabal test benchmark-metadata-spec --test-show-details=direct

manifest_path="$JAZZ_ARTIFACT_ROOT/manifest.json"
python3 - "$JAZZ_ARTIFACT_ROOT" "$manifest_path" <<'PY'
import hashlib
import json
import sys
from pathlib import Path

artifact_root = Path(sys.argv[1]).resolve()
manifest_path = Path(sys.argv[2]).resolve()
artifacts = []
for path in sorted(artifact_root.rglob("*")):
    if not path.is_file() or path.resolve() == manifest_path:
        continue
    artifacts.append(
        {
            "path": path.relative_to(artifact_root).as_posix(),
            "sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
        }
    )

if not artifacts:
    raise SystemExit("extended verification produced no artifacts")

manifest_path.write_text(
    json.dumps({"schema_version": 1, "artifacts": artifacts}, indent=2) + "\n",
    encoding="utf-8",
)
PY

printf 'Extended checks passed; artifact manifest: %s\n' "$manifest_path"
