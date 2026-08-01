#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
cd "$ROOT"

if [[ -n "${NIX_CONFIG:-}" ]]; then
  NIX_CONFIG+=$'\n'
fi
NIX_CONFIG+='extra-experimental-features = nix-command flakes'
export NIX_CONFIG

: "${JAZZ_RELEASE_VERSION:?JAZZ_RELEASE_VERSION is required}"
if [[ ! "$JAZZ_RELEASE_VERSION" =~ ^0\.[0-9]+\.[0-9]+-alpha\.[0-9]+$ ]]; then
  printf 'FAIL: JAZZ_RELEASE_VERSION must match 0.<minor>.<patch>-alpha.<n>\n' >&2
  exit 1
fi

JAZZ_ARTIFACT_ROOT="${JAZZ_ARTIFACT_ROOT:-artifacts/release-candidate/$JAZZ_RELEASE_VERSION/extended}"
JAZZ_BENCHMARK_LABEL="${JAZZ_BENCHMARK_LABEL:-release-candidate}"
JAZZ_RELEASE_OUTPUT_ROOT="${JAZZ_RELEASE_OUTPUT_ROOT:-artifacts/release/$JAZZ_RELEASE_VERSION}"
JAZZ_RELEASE_SDIST_ROOT="${JAZZ_RELEASE_SDIST_ROOT:-dist-newstyle/sdist}"
JAZZ_NIX_RESULT="${JAZZ_NIX_RESULT:-result}"
export JAZZ_ARTIFACT_ROOT JAZZ_BENCHMARK_LABEL JAZZ_RELEASE_OUTPUT_ROOT

python3 - "$JAZZ_ARTIFACT_ROOT" "$JAZZ_RELEASE_OUTPUT_ROOT" <<'PY'
import os
import sys

evidence_root, release_root = (os.path.realpath(path) for path in sys.argv[1:])
common = os.path.commonpath((evidence_root, release_root))
if evidence_root == release_root or common in (evidence_root, release_root):
    raise SystemExit("release evidence root must be fresh and outside the final release directory")
PY

require_path() {
  local path="$1"
  if [[ ! -e "$path" ]]; then
    printf 'FAIL: release prerequisite output is missing: %s\n' "$path" >&2
    exit 1
  fi
}

validate_artifact_manifest() {
  local manifest_path="$1"
  local expected_label="$2"
  python3 - "$manifest_path" "$expected_label" <<'PY'
import csv
import hashlib
import json
import sys
from pathlib import Path, PurePosixPath

manifest_path = Path(sys.argv[1]).resolve()
if not manifest_path.is_file():
    raise SystemExit(f"artifact manifest is missing: {manifest_path}")

manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
if manifest.get("schema_version") != 1:
    raise SystemExit("artifact manifest has an unsupported schema version")

entries = manifest.get("artifacts")
if not isinstance(entries, list) or not entries:
    raise SystemExit("artifact manifest must contain at least one artifact")

artifact_root = manifest_path.parent
seen_paths = set()
manifest_paths = {}
for entry in entries:
    if not isinstance(entry, dict) or set(entry) != {"path", "sha256"}:
        raise SystemExit("artifact manifest entries require only path and sha256")
    relative = PurePosixPath(entry["path"])
    if relative.is_absolute() or ".." in relative.parts or relative.as_posix() in seen_paths:
        raise SystemExit(f"artifact manifest contains an unsafe or duplicate path: {relative}")
    seen_paths.add(relative.as_posix())
    artifact_path = artifact_root.joinpath(*relative.parts)
    if not artifact_path.is_file():
        raise SystemExit(f"manifest artifact is missing: {relative}")
    digest = hashlib.sha256(artifact_path.read_bytes()).hexdigest()
    if digest != entry["sha256"]:
        raise SystemExit(f"manifest artifact digest does not match: {relative}")
    manifest_paths[relative.as_posix()] = artifact_path

required_artifacts = {
    "corpus/pass-one.txt",
    "corpus/pass-two.txt",
    "determinism/stats-one.stdout",
    "determinism/stats-one.stderr",
    "determinism/stats-two.stdout",
    "determinism/stats-two.stderr",
    "determinism/profile-one.stdout",
    "determinism/profile-one.stderr",
    "determinism/profile-one.speedscope.json",
    "determinism/profile-two.stdout",
    "determinism/profile-two.stderr",
    "determinism/profile-two.speedscope.json",
}
missing_artifacts = sorted(required_artifacts - set(manifest_paths))
if missing_artifacts:
    raise SystemExit(
        "artifact manifest is missing required determinism or corpus outputs: "
        + ", ".join(missing_artifacts)
    )

first_corpus = manifest_paths["corpus/pass-one.txt"].read_bytes()
second_corpus = manifest_paths["corpus/pass-two.txt"].read_bytes()
if not first_corpus or first_corpus != second_corpus:
    raise SystemExit("normalized program corpus outputs must be nonempty and identical")

for profile_name in (
    "determinism/profile-one.speedscope.json",
    "determinism/profile-two.speedscope.json",
):
    profile = json.loads(manifest_paths[profile_name].read_text(encoding="utf-8"))
    if not isinstance(profile, dict):
        raise SystemExit(f"determinism profile must be a JSON object: {profile_name}")

results_paths = [
    path
    for path in manifest_paths
    if path.startswith("benchmarks/") and path.endswith("/results.csv")
]
environment_paths = [
    path
    for path in manifest_paths
    if path.startswith("benchmarks/") and path.endswith("/environment.json")
]
if len(results_paths) != 1 or len(environment_paths) != 1:
    raise SystemExit("artifact manifest must contain exactly one benchmark CSV/environment pair")

results_relative = PurePosixPath(results_paths[0])
environment_relative = PurePosixPath(environment_paths[0])
if results_relative.parent != environment_relative.parent:
    raise SystemExit("benchmark CSV and environment metadata must belong to the same run")
if len(results_relative.parts) != 4 or results_relative.parts[1] != sys.argv[2]:
    raise SystemExit("benchmark artifact path does not contain the requested environment label")

results_path = manifest_paths[results_paths[0]]
environment_path = manifest_paths[environment_paths[0]]
with results_path.open(newline="", encoding="utf-8") as results_file:
    rows = list(csv.reader(results_file))
if len(rows) < 2 or not rows[0]:
    raise SystemExit("release benchmark results.csv must contain a header and data")

metadata = json.loads(environment_path.read_text(encoding="utf-8"))
if not isinstance(metadata, dict):
    raise SystemExit("release benchmark environment metadata must be a JSON object")
if metadata.get("schema_version") != 2 or metadata.get("environment_label") != sys.argv[2]:
    raise SystemExit("release benchmark environment metadata has the wrong schema or label")
if metadata.get("run_id") != results_relative.parts[2]:
    raise SystemExit("release benchmark run id does not match its artifact directory")
PY
}

bash scripts/ci/main-functional.sh
bash scripts/ci/extended.sh

bash scripts/check-docs.sh
npm --prefix website ci
npm --prefix website run clear
npm --prefix website run build
bash scripts/check-website.sh

cabal sdist all --output-directory="$JAZZ_RELEASE_SDIST_ROOT"
nix build .#jazz --out-link "$JAZZ_NIX_RESULT"

require_path website/build/index.html
require_path "$JAZZ_RELEASE_SDIST_ROOT"
require_path "$JAZZ_NIX_RESULT"
validate_artifact_manifest "$JAZZ_ARTIFACT_ROOT/manifest.json" "$JAZZ_BENCHMARK_LABEL"

shopt -s nullglob
sdist_archives=("$JAZZ_RELEASE_SDIST_ROOT"/*.tar.gz)
if (( ${#sdist_archives[@]} != 1 )); then
  printf 'FAIL: cabal sdist did not produce exactly one source archive\n' >&2
  exit 1
fi

printf 'Release candidate %s passed all prerequisite checks.\n' "$JAZZ_RELEASE_VERSION"
