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

if [[ -n "$(git status --porcelain=v1 --untracked-files=all)" ]]; then
  printf 'FAIL: build-alpha requires a clean tracked and untracked worktree\n' >&2
  exit 1
fi

release_parent="$ROOT/artifacts/release"
release_directory="$release_parent/$JAZZ_RELEASE_VERSION"
lock_parent="$release_parent/.locks"
release_lock="$lock_parent/$JAZZ_RELEASE_VERSION"
mkdir -p "$lock_parent"
if ! mkdir "$release_lock" 2>/dev/null; then
  printf 'FAIL: release build is already in progress for %s\n' "$JAZZ_RELEASE_VERSION" >&2
  exit 1
fi

work_root=""
cleanup() {
  if [[ -n "$work_root" ]]; then
    rm -r -- "$work_root"
  fi
  if ! rmdir "$release_lock" 2>/dev/null; then
    :
  fi
  if ! rmdir "$lock_parent" 2>/dev/null; then
    :
  fi
}
trap cleanup EXIT

if [[ -e "$release_directory" ]]; then
  printf 'FAIL: release artifact directory already exists: %s\n' "$release_directory" >&2
  exit 1
fi

work_root="$(mktemp -d "${TMPDIR:-/tmp}/jazz-alpha.XXXXXX")"
evidence_root="$work_root/extended"
sdist_root="$work_root/sdist"
nix_result="$work_root/nix-result"
staged_release="$work_root/$JAZZ_RELEASE_VERSION"
mkdir -p "$sdist_root" "$staged_release"

JAZZ_ARTIFACT_ROOT="$evidence_root" \
JAZZ_BENCHMARK_LABEL="release-$JAZZ_RELEASE_VERSION" \
JAZZ_RELEASE_OUTPUT_ROOT="$release_directory" \
JAZZ_RELEASE_SDIST_ROOT="$sdist_root" \
JAZZ_NIX_RESULT="$nix_result" \
  bash scripts/ci/release-candidate.sh

shopt -s nullglob
source_archives=("$sdist_root"/*.tar.gz)
if (( ${#source_archives[@]} != 1 )); then
  printf 'FAIL: release candidate must produce exactly one Cabal source archive\n' >&2
  exit 1
fi
if [[ ! -d "$nix_result" || ! -x "$nix_result/bin/jazz" ]]; then
  printf 'FAIL: release candidate did not produce an executable Nix result\n' >&2
  exit 1
fi
if [[ ! -f website/build/index.html ]]; then
  printf 'FAIL: release candidate did not produce the static documentation index\n' >&2
  exit 1
fi
if [[ ! -f "$evidence_root/manifest.json" ]]; then
  printf 'FAIL: release candidate did not produce extended evidence\n' >&2
  exit 1
fi

system="$(nix eval --raw --impure --expr builtins.currentSystem)"
if [[ ! "$system" =~ ^[A-Za-z0-9][A-Za-z0-9._+]*-[A-Za-z0-9][A-Za-z0-9._+-]*$ ]]; then
  printf 'FAIL: Nix returned an unsafe current system name: %s\n' "$system" >&2
  exit 1
fi

source_name="jazz-$JAZZ_RELEASE_VERSION-source.tar.gz"
nix_name="jazz-$JAZZ_RELEASE_VERSION-nix-$system.tar.gz"
docs_name="jazz-$JAZZ_RELEASE_VERSION-docs.tar.gz"
evidence_name="jazz-$JAZZ_RELEASE_VERSION-benchmark-evidence.tar.gz"
cp "${source_archives[0]}" "$staged_release/$source_name"

nix_closure_stage="$work_root/nix-closure-stage/nix-closure"
mkdir -p "$nix_closure_stage"
root_store_path="$(readlink "$nix_result")"
if [[ ! "$root_store_path" =~ ^/nix/store/[a-z0-9]{32}-[A-Za-z0-9+._?=-]+$ ]]; then
  printf 'FAIL: Nix result does not resolve to a valid store path: %s\n' "$root_store_path" >&2
  exit 1
fi
LC_ALL=C nix-store --query --requisites "$nix_result" \
  | LC_ALL=C sort -u > "$nix_closure_stage/store-paths"
if ! grep -Fqx "$root_store_path" "$nix_closure_stage/store-paths"; then
  printf 'FAIL: Nix runtime closure does not include its root store path\n' >&2
  exit 1
fi
closure_paths=()
while IFS= read -r store_path; do
  closure_paths+=("$store_path")
done < "$nix_closure_stage/store-paths"
if (( ${#closure_paths[@]} == 0 )); then
  printf 'FAIL: Nix runtime closure contains no store paths\n' >&2
  exit 1
fi
nix-store --export "${closure_paths[@]}" > "$nix_closure_stage/closure.nar"
printf '%s\n' "$root_store_path" > "$nix_closure_stage/root-store-path"
printf '%s\n' "$system" > "$nix_closure_stage/system"

source_date_epoch="$(git log -1 --format=%ct HEAD)"
create_tree_archive() {
  local source_root="$1"
  local destination="$2"
  local archive_prefix="$3"
  python3 - "$source_root" "$destination" "$archive_prefix" "$source_date_epoch" <<'PY'
import gzip
import os
import stat
import sys
import tarfile
from pathlib import Path, PurePosixPath

source = Path(sys.argv[1]).resolve(strict=True)
destination = Path(sys.argv[2])
prefix = PurePosixPath(sys.argv[3]) if sys.argv[3] else None
timestamp = int(sys.argv[4])

paths = [source, *sorted(source.rglob("*"), key=lambda path: path.relative_to(source).as_posix())]
with destination.open("wb") as raw_stream:
    with gzip.GzipFile(filename="", mode="wb", fileobj=raw_stream, mtime=timestamp) as gzip_stream:
        with tarfile.open(fileobj=gzip_stream, mode="w", format=tarfile.PAX_FORMAT) as archive:
            for path in paths:
                relative = path.relative_to(source)
                if relative == Path("."):
                    if prefix is None:
                        continue
                    archive_name = prefix.as_posix()
                else:
                    relative_name = PurePosixPath(relative.as_posix())
                    archive_name = (prefix / relative_name).as_posix() if prefix else relative_name.as_posix()
                if path.is_symlink():
                    raise SystemExit(f"refusing to archive symbolic link: {path}")
                metadata = path.stat()
                member = tarfile.TarInfo(archive_name)
                member.uid = 0
                member.gid = 0
                member.uname = ""
                member.gname = ""
                member.mtime = timestamp
                member.pax_headers = {}
                if path.is_dir():
                    member.type = tarfile.DIRTYPE
                    member.mode = 0o755
                    archive.addfile(member)
                elif path.is_file():
                    member.mode = 0o755 if metadata.st_mode & stat.S_IXUSR else 0o644
                    member.size = metadata.st_size
                    with path.open("rb") as contents:
                        archive.addfile(member, contents)
                else:
                    raise SystemExit(f"refusing to archive special file: {path}")
PY
}

create_tree_archive "$work_root/nix-closure-stage" "$staged_release/$nix_name" ""
create_tree_archive website/build "$staged_release/$docs_name" ""
create_tree_archive "$evidence_root" "$staged_release/$evidence_name" ""

python3 - "$staged_release" <<'PY'
import hashlib
import sys
from pathlib import Path

release_directory = Path(sys.argv[1])
lines = []
for path in sorted(release_directory.glob("*.tar.gz"), key=lambda candidate: candidate.name):
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    lines.append(f"{digest.hexdigest()}  {path.name}")
if len(lines) != 4:
    raise SystemExit("release assembly did not produce exactly four archives")
(release_directory / "SHA256SUMS").write_text("\n".join(lines) + "\n", encoding="utf-8")
PY

python3 scripts/release/verify-artifacts.py "$staged_release"
mkdir -p "$release_parent"
mv "$staged_release" "$release_directory"
python3 scripts/release/verify-artifacts.py "$release_directory"
printf 'Jazz alpha artifacts are ready at %s\n' "$release_directory"
