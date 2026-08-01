#!/usr/bin/env python3
"""Verify the exact, self-contained artifact set for one Jazz alpha."""

from __future__ import annotations

import csv
import hashlib
import json
import re
import shutil
import subprocess
import sys
import tarfile
import tempfile
from pathlib import Path, PurePosixPath


VERSION_PATTERN = re.compile(r"^0\.[0-9]+\.[0-9]+-alpha\.[0-9]+$")
SYSTEM_PATTERN = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._+]*-[A-Za-z0-9][A-Za-z0-9._+-]*$")
SHA256_PATTERN = re.compile(r"^([0-9a-f]{64})  ([A-Za-z0-9][A-Za-z0-9._+-]*\.tar\.gz)$")
HEX_SHA256_PATTERN = re.compile(r"^[0-9a-f]{64}$")
STORE_PATH_PATTERN = re.compile(
    r"^/nix/store/[a-z0-9]{32}-[A-Za-z0-9+._?=-]+$"
)

EVIDENCE_FILES = {
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


class VerificationError(ValueError):
    """A release artifact violates the checked contract."""


class CheckedArchive:
    def __init__(self, path: Path) -> None:
        self.path = path
        if not path.is_file() or path.stat().st_size == 0:
            raise VerificationError(f"archive is missing or empty: {path.name}")
        try:
            self.archive = tarfile.open(path, "r:gz")
        except (OSError, tarfile.TarError) as error:
            raise VerificationError(f"archive is not a readable gzip tar: {path.name}") from error
        self.members: dict[str, tarfile.TarInfo] = {}
        try:
            for member in self.archive.getmembers():
                normalized = self._safe_name(member.name)
                if normalized in self.members:
                    raise VerificationError(
                        f"archive contains duplicate member: {path.name}: {normalized}"
                    )
                if not (member.isdir() or member.isreg()):
                    raise VerificationError(
                        f"unsafe archive member type: {path.name}: {normalized}"
                    )
                self.members[normalized] = member
        except Exception:
            self.archive.close()
            raise
        if not any(member.isreg() for member in self.members.values()):
            self.archive.close()
            raise VerificationError(f"archive contains no files: {path.name}")

    @staticmethod
    def _safe_name(name: str) -> str:
        path = PurePosixPath(name)
        if (
            not name
            or name.startswith("/")
            or "\\" in name
            or path.is_absolute()
            or any(part in ("", ".", "..") for part in path.parts)
        ):
            raise VerificationError(f"unsafe archive member path: {name!r}")
        return path.as_posix()

    def close(self) -> None:
        self.archive.close()

    def regular_files(self) -> set[str]:
        return {name for name, member in self.members.items() if member.isreg()}

    def read(self, name: str, *, maximum_size: int = 16 * 1024 * 1024) -> bytes:
        member = self.members.get(name)
        if member is None or not member.isreg():
            raise VerificationError(f"archive member is missing: {self.path.name}: {name}")
        if member.size > maximum_size:
            raise VerificationError(f"archive member is too large to validate: {self.path.name}: {name}")
        extracted = self.archive.extractfile(member)
        if extracted is None:
            raise VerificationError(f"archive member cannot be read: {self.path.name}: {name}")
        return extracted.read()

    def copy_to(self, name: str, destination: Path) -> None:
        member = self.members.get(name)
        if member is None or not member.isreg():
            raise VerificationError(f"archive member is missing: {self.path.name}: {name}")
        extracted = self.archive.extractfile(member)
        if extracted is None:
            raise VerificationError(f"archive member cannot be read: {self.path.name}: {name}")
        with destination.open("wb") as stream:
            shutil.copyfileobj(extracted, stream)


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def verify_checksums(release_directory: Path, archives: set[str]) -> None:
    checksum_path = release_directory / "SHA256SUMS"
    try:
        lines = checksum_path.read_text(encoding="utf-8").splitlines()
    except (OSError, UnicodeError) as error:
        raise VerificationError("SHA256SUMS is missing or not UTF-8") from error
    entries: dict[str, str] = {}
    order: list[str] = []
    for line in lines:
        match = SHA256_PATTERN.fullmatch(line)
        if match is None:
            raise VerificationError(f"malformed SHA256SUMS entry: {line!r}")
        digest, filename = match.groups()
        if filename in entries:
            raise VerificationError(f"duplicate SHA256SUMS entry: {filename}")
        entries[filename] = digest
        order.append(filename)
    if set(entries) != archives:
        raise VerificationError("SHA256SUMS does not name the exact archive set")
    if order != sorted(order):
        raise VerificationError("SHA256SUMS entries are not sorted by filename")
    for filename in order:
        if sha256_file(release_directory / filename) != entries[filename]:
            raise VerificationError(f"SHA-256 mismatch: {filename}")


def verify_source(archive: CheckedArchive) -> None:
    files = archive.regular_files()
    roots = {PurePosixPath(name).parts[0] for name in files}
    if len(roots) != 1:
        raise VerificationError("source archive must contain one package root")
    package_root = next(iter(roots))
    if re.fullmatch(r"jazz-[0-9]+(?:\.[0-9]+)+", package_root) is None:
        raise VerificationError("source archive package root is not a numeric Jazz sdist")
    if f"{package_root}/jazz.cabal" not in files:
        raise VerificationError("source archive is missing jazz.cabal")

    forbidden_roots = {
        ".codex",
        ".git",
        "artifacts",
        "benchmark-results",
        "profile-results",
    }
    for name in sorted(files):
        relative_parts = PurePosixPath(name).parts[1:]
        if not relative_parts:
            continue
        first = relative_parts[0]
        forbidden = (
            first in forbidden_roots
            or first.startswith("dist-newstyle")
            or "node_modules" in relative_parts
            or relative_parts[:2] in (("website", "build"), ("website", ".docusaurus"))
        )
        if forbidden:
            raise VerificationError(f"source archive contains forbidden path: {name}")


def decode_line_file(archive: CheckedArchive, name: str) -> list[str]:
    try:
        contents = archive.read(name).decode("utf-8")
    except UnicodeError as error:
        raise VerificationError(f"Nix closure metadata is not UTF-8: {name}") from error
    lines = contents.splitlines()
    if not lines or any(not line for line in lines):
        raise VerificationError(f"Nix closure metadata is empty or malformed: {name}")
    return lines


def verify_nix(archive: CheckedArchive, system: str) -> None:
    files = archive.regular_files()
    required = {
        "nix-closure/closure.nar",
        "nix-closure/root-store-path",
        "nix-closure/store-paths",
        "nix-closure/system",
    }
    if files != required:
        raise VerificationError("Nix archive does not contain the exact runtime closure set")
    closure = archive.members["nix-closure/closure.nar"]
    if closure.size == 0:
        raise VerificationError("Nix runtime closure export is empty")
    root_lines = decode_line_file(archive, "nix-closure/root-store-path")
    store_paths = decode_line_file(archive, "nix-closure/store-paths")
    system_lines = decode_line_file(archive, "nix-closure/system")
    if len(root_lines) != 1 or STORE_PATH_PATTERN.fullmatch(root_lines[0]) is None:
        raise VerificationError("Nix closure root store path is invalid")
    if store_paths != sorted(set(store_paths)) or any(
        STORE_PATH_PATTERN.fullmatch(path) is None for path in store_paths
    ):
        raise VerificationError("Nix closure store paths must be valid, unique, and sorted")
    if root_lines[0] not in store_paths:
        raise VerificationError("Nix closure root store path is absent from the exported closure")
    if system_lines != [system]:
        raise VerificationError("Nix closure system does not match the artifact filename")

    temporary_parent = "/private/tmp" if Path("/private/tmp").is_dir() else None
    with tempfile.TemporaryDirectory(
        prefix="jazz-nix-verify-", dir=temporary_parent
    ) as temporary_directory:
        temporary_root = Path(temporary_directory)
        export_path = temporary_root / "closure.nar"
        local_store = temporary_root / "store"
        archive.copy_to("nix-closure/closure.nar", export_path)
        store_arguments = ["nix-store", "--store", f"local?root={local_store}"]
        try:
            with export_path.open("rb") as export_stream:
                imported = subprocess.run(
                    [*store_arguments, "--import"],
                    stdin=export_stream,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    check=False,
                )
        except FileNotFoundError as error:
            raise VerificationError("nix-store is required to validate the Nix closure export") from error
        if imported.returncode != 0:
            raise VerificationError("Nix runtime closure export cannot be imported")
        try:
            imported_paths = imported.stdout.decode("utf-8").splitlines()
        except UnicodeError as error:
            raise VerificationError("Nix import returned non-UTF-8 store paths") from error
        if (
            len(imported_paths) != len(set(imported_paths))
            or any(STORE_PATH_PATTERN.fullmatch(path) is None for path in imported_paths)
            or sorted(imported_paths) != store_paths
        ):
            raise VerificationError("Nix closure store paths do not match the imported export")

        queried = subprocess.run(
            [*store_arguments, "--query", "--requisites", root_lines[0]],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False,
        )
        if queried.returncode != 0:
            raise VerificationError("Nix closure root cannot be queried after import")
        try:
            root_requisites = queried.stdout.decode("utf-8").splitlines()
        except UnicodeError as error:
            raise VerificationError("Nix closure query returned non-UTF-8 store paths") from error
        if sorted(set(root_requisites)) != store_paths:
            raise VerificationError("Nix closure root requisites do not match the imported export")


def verify_docs(archive: CheckedArchive) -> None:
    index = archive.members.get("index.html")
    if index is None or not index.isreg() or index.size == 0:
        raise VerificationError("docs archive is missing index.html")


def decode_json(archive: CheckedArchive, name: str) -> object:
    try:
        return json.loads(archive.read(name))
    except (UnicodeError, json.JSONDecodeError) as error:
        raise VerificationError(f"archive JSON is invalid: {name}") from error


def verify_evidence(archive: CheckedArchive, version: str) -> None:
    files = archive.regular_files()
    if "manifest.json" not in files:
        raise VerificationError("benchmark evidence is missing manifest.json")
    manifest = decode_json(archive, "manifest.json")
    if not isinstance(manifest, dict) or set(manifest) != {"schema_version", "artifacts"}:
        raise VerificationError("artifact manifest has an invalid structure")
    if manifest["schema_version"] != 1 or not isinstance(manifest["artifacts"], list):
        raise VerificationError("artifact manifest has an unsupported schema")

    entries: dict[str, str] = {}
    for entry in manifest["artifacts"]:
        if not isinstance(entry, dict) or set(entry) != {"path", "sha256"}:
            raise VerificationError("artifact manifest entry has an invalid structure")
        path = entry["path"]
        digest = entry["sha256"]
        if not isinstance(path, str) or path in entries:
            raise VerificationError(f"duplicate artifact manifest path: {path}")
        if CheckedArchive._safe_name(path) != path:
            raise VerificationError(f"unsafe artifact manifest path: {path}")
        if not isinstance(digest, str) or HEX_SHA256_PATTERN.fullmatch(digest) is None:
            raise VerificationError(f"artifact manifest has an invalid SHA-256: {path}")
        entries[path] = digest

    evidence_files = files - {"manifest.json"}
    if set(entries) != evidence_files:
        raise VerificationError("artifact manifest does not name every evidence file exactly once")
    for path, expected_digest in entries.items():
        actual_digest = hashlib.sha256(archive.read(path)).hexdigest()
        if actual_digest != expected_digest:
            raise VerificationError(f"artifact manifest SHA-256 mismatch: {path}")

    missing = sorted(EVIDENCE_FILES - evidence_files)
    if missing:
        raise VerificationError("benchmark evidence is missing required files: " + ", ".join(missing))
    corpus_one = archive.read("corpus/pass-one.txt")
    corpus_two = archive.read("corpus/pass-two.txt")
    if not corpus_one or corpus_one != corpus_two:
        raise VerificationError("program corpus evidence must be nonempty and identical")

    determinism_pairs = (
        (
            "runtime statistics stdout",
            "determinism/stats-one.stdout",
            "determinism/stats-two.stdout",
            True,
        ),
        (
            "runtime statistics stderr",
            "determinism/stats-one.stderr",
            "determinism/stats-two.stderr",
            True,
        ),
        (
            "runtime profile stdout",
            "determinism/profile-one.stdout",
            "determinism/profile-two.stdout",
            True,
        ),
        (
            "runtime profile stderr",
            "determinism/profile-one.stderr",
            "determinism/profile-two.stderr",
            False,
        ),
        (
            "Speedscope profiles",
            "determinism/profile-one.speedscope.json",
            "determinism/profile-two.speedscope.json",
            True,
        ),
    )
    for label, first_path, second_path, require_nonempty in determinism_pairs:
        first_contents = archive.read(first_path)
        second_contents = archive.read(second_path)
        if require_nonempty and (not first_contents or not second_contents):
            raise VerificationError(f"determinism evidence must be nonempty: {label}")
        if first_contents != second_contents:
            raise VerificationError(f"{label} differs between repeated runs")

    for profile in (
        "determinism/profile-one.speedscope.json",
        "determinism/profile-two.speedscope.json",
    ):
        if not isinstance(decode_json(archive, profile), dict):
            raise VerificationError(f"Speedscope evidence must be a JSON object: {profile}")

    results_paths = sorted(name for name in evidence_files if name.endswith("/results.csv"))
    environment_paths = sorted(name for name in evidence_files if name.endswith("/environment.json"))
    if len(results_paths) != 1 or len(environment_paths) != 1:
        raise VerificationError("benchmark evidence must contain exactly one results/metadata pair")
    results_path = PurePosixPath(results_paths[0])
    environment_path = PurePosixPath(environment_paths[0])
    if results_path.parent != environment_path.parent or len(results_path.parts) != 4:
        raise VerificationError("benchmark results and metadata paths do not form one owned run")
    expected_label = f"release-{version}"
    if results_path.parts[:2] != ("benchmarks", expected_label):
        raise VerificationError("benchmark evidence label does not match the release version")

    try:
        rows = list(csv.reader(archive.read(results_paths[0]).decode("utf-8").splitlines()))
    except UnicodeError as error:
        raise VerificationError("benchmark results.csv is not UTF-8") from error
    if len(rows) < 2 or not rows[0] or any(not heading for heading in rows[0]):
        raise VerificationError("benchmark results.csv has no timing data")
    metadata = decode_json(archive, environment_paths[0])
    if not isinstance(metadata, dict) or metadata.get("schema_version") != 2:
        raise VerificationError("benchmark environment metadata has an unsupported schema")
    if metadata.get("environment_label") != expected_label:
        raise VerificationError("benchmark environment label does not match its path")
    if metadata.get("run_id") != results_path.parts[2]:
        raise VerificationError("benchmark environment run id does not match its path")
    if not isinstance(metadata.get("time_mode"), str) or not metadata["time_mode"]:
        raise VerificationError("benchmark environment metadata is missing its time mode")
    if not isinstance(metadata.get("run_timestamp"), str) or not metadata["run_timestamp"]:
        raise VerificationError("benchmark environment metadata is missing its run timestamp")


def verify_release_directory(release_directory: Path) -> tuple[str, str]:
    if not release_directory.is_dir():
        raise VerificationError(f"release directory does not exist: {release_directory}")
    version = release_directory.name
    if VERSION_PATTERN.fullmatch(version) is None:
        raise VerificationError("release directory name must match 0.<minor>.<patch>-alpha.<n>")

    source_name = f"jazz-{version}-source.tar.gz"
    docs_name = f"jazz-{version}-docs.tar.gz"
    evidence_name = f"jazz-{version}-benchmark-evidence.tar.gz"
    nix_prefix = f"jazz-{version}-nix-"
    entries = {path.name for path in release_directory.iterdir()}
    nix_names = sorted(
        name for name in entries if name.startswith(nix_prefix) and name.endswith(".tar.gz")
    )
    if len(nix_names) != 1:
        raise VerificationError("artifact set does not match the required files")
    nix_name = nix_names[0]
    system = nix_name[len(nix_prefix) : -len(".tar.gz")]
    if SYSTEM_PATTERN.fullmatch(system) is None:
        raise VerificationError("Nix artifact has an invalid system name")
    archives = {source_name, nix_name, docs_name, evidence_name}
    if entries != archives | {"SHA256SUMS"}:
        raise VerificationError("artifact set does not match the required files")

    verify_checksums(release_directory, archives)
    checked: dict[str, CheckedArchive] = {}
    try:
        for name in sorted(archives):
            checked[name] = CheckedArchive(release_directory / name)
        verify_source(checked[source_name])
        verify_nix(checked[nix_name], system)
        verify_docs(checked[docs_name])
        verify_evidence(checked[evidence_name], version)
    finally:
        for archive in checked.values():
            archive.close()
    return version, system


def main(arguments: list[str]) -> int:
    if len(arguments) != 1:
        print("usage: verify-artifacts.py <release-directory>", file=sys.stderr)
        return 2
    try:
        version, system = verify_release_directory(Path(arguments[0]).resolve())
    except (OSError, VerificationError) as error:
        print(f"FAIL: {error}", file=sys.stderr)
        return 1
    print(f"Verified Jazz alpha artifacts for {version} ({system}).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
