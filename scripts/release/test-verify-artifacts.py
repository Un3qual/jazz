#!/usr/bin/env python3
"""Behavior tests for Jazz alpha release artifact verification."""

from __future__ import annotations

import hashlib
import io
import json
import os
import shutil
import subprocess
import sys
import tarfile
import tempfile
import unittest
from pathlib import Path


VERIFY = Path(__file__).with_name("verify-artifacts.py")
BUILD = Path(__file__).with_name("build-alpha.sh")
VERSION = "0.1.0-alpha.1"
SYSTEM = "aarch64-darwin"
NIX_DEPENDENCY = "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-runtime-dependency"
NIX_ROOT = "/nix/store/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-jazz-0.1.0.0"


class ArtifactVerifierTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        temporary_parent = "/private/tmp" if Path("/private/tmp").is_dir() else None
        with tempfile.TemporaryDirectory(dir=temporary_parent) as temporary_directory:
            fixture_root = Path(temporary_directory)
            store_root = fixture_root / "store"
            payload = fixture_root / "jazz-verifier-fixture"
            payload.write_text("Jazz Nix closure fixture\n", encoding="utf-8")
            added = subprocess.run(
                [
                    "nix-store",
                    "--store",
                    f"local?root={store_root}",
                    "--add",
                    str(payload),
                ],
                text=True,
                capture_output=True,
                check=True,
            )
            cls.nix_root = added.stdout.strip()
            exported = subprocess.run(
                [
                    "nix-store",
                    "--store",
                    f"local?root={store_root}",
                    "--export",
                    cls.nix_root,
                ],
                capture_output=True,
                check=True,
            )
            cls.nix_export = exported.stdout

    def setUp(self) -> None:
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.release_directory = Path(self.temporary_directory.name) / VERSION
        self.release_directory.mkdir()
        self.write_valid_artifact_set()

    def tearDown(self) -> None:
        self.temporary_directory.cleanup()

    def archive(self, name: str, files: dict[str, bytes]) -> Path:
        destination = self.release_directory / name
        with tarfile.open(destination, "w:gz") as archive:
            for member_name, contents in files.items():
                member = tarfile.TarInfo(member_name)
                member.size = len(contents)
                member.mode = 0o755 if member_name == "result/bin/jazz" else 0o644
                archive.addfile(member, io.BytesIO(contents))
        return destination

    def evidence_files(self) -> dict[str, bytes]:
        label = f"release-{VERSION}"
        run_id = "20260801T120000000000Z"
        files = {
            "corpus/pass-one.txt": b"PASS: corpus fixture\nAll ProgramCorpus tests passed.\n",
            "corpus/pass-two.txt": b"PASS: corpus fixture\nAll ProgramCorpus tests passed.\n",
            "determinism/stats-one.stdout": b"720\n",
            "determinism/stats-one.stderr": b'{"runtime":"stats"}\n',
            "determinism/stats-two.stdout": b"720\n",
            "determinism/stats-two.stderr": b'{"runtime":"stats"}\n',
            "determinism/profile-one.stdout": b"720\n",
            "determinism/profile-one.stderr": b"",
            "determinism/profile-one.speedscope.json": b'{"$schema":"speedscope"}\n',
            "determinism/profile-two.stdout": b"720\n",
            "determinism/profile-two.stderr": b"",
            "determinism/profile-two.speedscope.json": b'{"$schema":"speedscope"}\n',
            f"benchmarks/{label}/{run_id}/results.csv": b"case,mean_seconds\nfactorial,0.012\n",
            f"benchmarks/{label}/{run_id}/environment.json": (
                json.dumps(
                    {
                        "schema_version": 2,
                        "run_id": run_id,
                        "environment_label": label,
                        "time_mode": "cpu",
                        "run_timestamp": "2026-08-01T12:00:00Z",
                    },
                    sort_keys=True,
                ).encode("utf-8")
                + b"\n"
            ),
        }
        manifest = {
            "schema_version": 1,
            "artifacts": [
                {"path": path, "sha256": hashlib.sha256(contents).hexdigest()}
                for path, contents in sorted(files.items())
            ],
        }
        files["manifest.json"] = json.dumps(manifest, sort_keys=True).encode("utf-8") + b"\n"
        return files

    def refresh_evidence_manifest(self, files: dict[str, bytes]) -> None:
        manifest = {
            "schema_version": 1,
            "artifacts": [
                {"path": path, "sha256": hashlib.sha256(contents).hexdigest()}
                for path, contents in sorted(files.items())
                if path != "manifest.json"
            ],
        }
        files["manifest.json"] = json.dumps(manifest, sort_keys=True).encode("utf-8") + b"\n"

    def write_valid_artifact_set(self) -> None:
        self.archive(
            f"jazz-{VERSION}-source.tar.gz",
            {
                "jazz-0.1.0.0/jazz.cabal": b"name: jazz\nversion: 0.1.0.0\n",
                "jazz-0.1.0.0/src/Jazz/Compiler.hs": b"module Jazz.Compiler where\n",
            },
        )
        self.archive(
            f"jazz-{VERSION}-nix-{SYSTEM}.tar.gz",
            {
                "nix-closure/closure.nar": self.nix_export,
                "nix-closure/root-store-path": f"{self.nix_root}\n".encode(),
                "nix-closure/store-paths": f"{self.nix_root}\n".encode(),
                "nix-closure/system": f"{SYSTEM}\n".encode(),
            },
        )
        self.archive(
            f"jazz-{VERSION}-docs.tar.gz",
            {"index.html": b"<!doctype html><title>Jazz</title>\n", "assets/site.js": b";\n"},
        )
        self.archive(
            f"jazz-{VERSION}-benchmark-evidence.tar.gz",
            self.evidence_files(),
        )
        self.write_checksums()

    def write_checksums(self) -> None:
        archive_paths = sorted(self.release_directory.glob("*.tar.gz"), key=lambda path: path.name)
        lines = [f"{hashlib.sha256(path.read_bytes()).hexdigest()}  {path.name}" for path in archive_paths]
        (self.release_directory / "SHA256SUMS").write_text("\n".join(lines) + "\n", encoding="utf-8")

    def run_verifier(
        self, environment: dict[str, str] | None = None
    ) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [sys.executable, str(VERIFY), str(self.release_directory)],
            env=environment,
            text=True,
            capture_output=True,
            check=False,
        )

    def assert_rejected(self, expected: str) -> None:
        result = self.run_verifier()
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn(expected, result.stdout + result.stderr)

    def test_accepts_the_exact_valid_alpha_artifact_set(self) -> None:
        result = self.run_verifier()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stdout, f"Verified Jazz alpha artifacts for {VERSION} ({SYSTEM}).\n")

    def test_rejects_a_missing_artifact(self) -> None:
        (self.release_directory / f"jazz-{VERSION}-docs.tar.gz").unlink()
        self.assert_rejected("artifact set does not match the required files")

    def test_rejects_an_unexpected_filename(self) -> None:
        (self.release_directory / "notes.txt").write_text("not an artifact\n", encoding="utf-8")
        self.assert_rejected("artifact set does not match the required files")

    def test_rejects_duplicate_checksum_entries(self) -> None:
        checksums = self.release_directory / "SHA256SUMS"
        first_line = checksums.read_text(encoding="utf-8").splitlines()[0]
        checksums.write_text(checksums.read_text(encoding="utf-8") + first_line + "\n", encoding="utf-8")
        self.assert_rejected("duplicate SHA256SUMS entry")

    def test_rejects_an_incorrect_archive_hash(self) -> None:
        checksums = self.release_directory / "SHA256SUMS"
        lines = checksums.read_text(encoding="utf-8").splitlines()
        lines[0] = "0" * 64 + lines[0][64:]
        checksums.write_text("\n".join(lines) + "\n", encoding="utf-8")
        self.assert_rejected("SHA-256 mismatch")

    def test_rejects_unsafe_archive_paths(self) -> None:
        source_name = f"jazz-{VERSION}-source.tar.gz"
        self.archive(source_name, {"../escape": b"unsafe\n"})
        self.write_checksums()
        self.assert_rejected("unsafe archive member")

    def test_rejects_windows_style_archive_traversal(self) -> None:
        source_name = f"jazz-{VERSION}-source.tar.gz"
        self.archive(source_name, {r"..\escape": b"unsafe\n"})
        self.write_checksums()
        self.assert_rejected("unsafe archive member")

    def test_rejects_source_distribution_output_and_internal_paths(self) -> None:
        source_name = f"jazz-{VERSION}-source.tar.gz"
        self.archive(
            source_name,
            {
                "jazz-0.1.0.0/jazz.cabal": b"name: jazz\n",
                "jazz-0.1.0.0/.codex/execution/queue.md": b"internal\n",
            },
        )
        self.write_checksums()
        self.assert_rejected("source archive contains forbidden path")

    def test_rejects_docs_without_the_static_index(self) -> None:
        self.archive(f"jazz-{VERSION}-docs.tar.gz", {"assets/site.js": b";\n"})
        self.write_checksums()
        self.assert_rejected("docs archive is missing index.html")

    def test_rejects_nix_closure_without_its_recorded_root(self) -> None:
        absent_root = "/nix/store/cccccccccccccccccccccccccccccccc-jazz-0.1.0.0"
        self.archive(
            f"jazz-{VERSION}-nix-{SYSTEM}.tar.gz",
            {
                "nix-closure/closure.nar": self.nix_export,
                "nix-closure/root-store-path": f"{absent_root}\n".encode(),
                "nix-closure/store-paths": f"{self.nix_root}\n".encode(),
                "nix-closure/system": f"{SYSTEM}\n".encode(),
            },
        )
        self.write_checksums()
        self.assert_rejected("Nix closure root store path is absent")

    def test_rejects_a_corrupt_nix_closure_export(self) -> None:
        self.archive(
            f"jazz-{VERSION}-nix-{SYSTEM}.tar.gz",
            {
                "nix-closure/closure.nar": b"not a Nix store export\n",
                "nix-closure/root-store-path": f"{self.nix_root}\n".encode(),
                "nix-closure/store-paths": f"{self.nix_root}\n".encode(),
                "nix-closure/system": f"{SYSTEM}\n".encode(),
            },
        )
        self.write_checksums()
        self.assert_rejected("Nix runtime closure export cannot be imported")

    def test_rejects_nix_store_paths_not_present_in_the_export(self) -> None:
        advertised_paths = sorted((NIX_DEPENDENCY, self.nix_root))
        self.archive(
            f"jazz-{VERSION}-nix-{SYSTEM}.tar.gz",
            {
                "nix-closure/closure.nar": self.nix_export,
                "nix-closure/root-store-path": f"{self.nix_root}\n".encode(),
                "nix-closure/store-paths": ("\n".join(advertised_paths) + "\n").encode(),
                "nix-closure/system": f"{SYSTEM}\n".encode(),
            },
        )
        self.write_checksums()
        self.assert_rejected("Nix closure store paths do not match the imported export")

    def test_accepts_dependency_order_from_nix_import(self) -> None:
        advertised_paths = sorted((NIX_DEPENDENCY, NIX_ROOT))
        self.archive(
            f"jazz-{VERSION}-nix-{SYSTEM}.tar.gz",
            {
                "nix-closure/closure.nar": b"fixture accepted by fake importer\n",
                "nix-closure/root-store-path": f"{NIX_ROOT}\n".encode(),
                "nix-closure/store-paths": ("\n".join(advertised_paths) + "\n").encode(),
                "nix-closure/system": f"{SYSTEM}\n".encode(),
            },
        )
        self.write_checksums()
        fake_bin = Path(self.temporary_directory.name) / "fake-bin"
        fake_bin.mkdir()
        fake_nix_store = fake_bin / "nix-store"
        fake_nix_store.write_text(
            f"""#!/usr/bin/env bash
set -euo pipefail
if [[ "$1" == "--store" && "$3" == "--import" ]]; then
  printf '%s\\n' '{NIX_ROOT}' '{NIX_DEPENDENCY}'
elif [[ "$1" == "--store" && "$3" == "--query" && "$4" == "--requisites" ]]; then
  printf '%s\\n' '{NIX_ROOT}' '{NIX_DEPENDENCY}'
else
  exit 64
fi
""",
            encoding="utf-8",
        )
        os.chmod(fake_nix_store, 0o755)
        environment = os.environ.copy()
        environment["PATH"] = str(fake_bin) + os.pathsep + environment["PATH"]
        result = self.run_verifier(environment)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_rejects_nix_closure_for_a_different_system(self) -> None:
        self.archive(
            f"jazz-{VERSION}-nix-{SYSTEM}.tar.gz",
            {
                "nix-closure/closure.nar": self.nix_export,
                "nix-closure/root-store-path": f"{self.nix_root}\n".encode(),
                "nix-closure/store-paths": f"{self.nix_root}\n".encode(),
                "nix-closure/system": b"x86_64-linux\n",
            },
        )
        self.write_checksums()
        self.assert_rejected("Nix closure system does not match")

    def test_rejects_duplicate_evidence_manifest_entries(self) -> None:
        files = self.evidence_files()
        manifest = json.loads(files["manifest.json"])
        manifest["artifacts"].append(dict(manifest["artifacts"][0]))
        files["manifest.json"] = json.dumps(manifest).encode("utf-8")
        self.archive(f"jazz-{VERSION}-benchmark-evidence.tar.gz", files)
        self.write_checksums()
        self.assert_rejected("duplicate artifact manifest path")

    def test_rejects_incorrect_evidence_manifest_hashes(self) -> None:
        files = self.evidence_files()
        manifest = json.loads(files["manifest.json"])
        manifest["artifacts"][0]["sha256"] = "0" * 64
        files["manifest.json"] = json.dumps(manifest).encode("utf-8")
        self.archive(f"jazz-{VERSION}-benchmark-evidence.tar.gz", files)
        self.write_checksums()
        self.assert_rejected("artifact manifest SHA-256 mismatch")

    def test_rejects_invalid_benchmark_metadata(self) -> None:
        files = self.evidence_files()
        metadata_name = next(path for path in files if path.endswith("environment.json"))
        metadata = json.loads(files[metadata_name])
        metadata["environment_label"] = "wrong-label"
        files[metadata_name] = json.dumps(metadata).encode("utf-8")
        manifest = {
            "schema_version": 1,
            "artifacts": [
                {"path": path, "sha256": hashlib.sha256(contents).hexdigest()}
                for path, contents in sorted(files.items())
                if path != "manifest.json"
            ],
        }
        files["manifest.json"] = json.dumps(manifest).encode("utf-8")
        self.archive(f"jazz-{VERSION}-benchmark-evidence.tar.gz", files)
        self.write_checksums()
        self.assert_rejected("benchmark environment label does not match its path")

    def test_rejects_mismatched_determinism_evidence(self) -> None:
        pairs = (
            ("determinism/stats-two.stdout", b"721\n", "runtime statistics stdout differs"),
            ("determinism/stats-two.stderr", b'{"runtime":"different"}\n', "runtime statistics stderr differs"),
            ("determinism/profile-two.stdout", b"721\n", "runtime profile stdout differs"),
            ("determinism/profile-two.stderr", b"unexpected\n", "runtime profile stderr differs"),
            (
                "determinism/profile-two.speedscope.json",
                b'{"$schema":"speedscope","different":true}\n',
                "Speedscope profiles differ",
            ),
        )
        for path, replacement, expected in pairs:
            with self.subTest(path=path):
                files = self.evidence_files()
                files[path] = replacement
                self.refresh_evidence_manifest(files)
                self.archive(f"jazz-{VERSION}-benchmark-evidence.tar.gz", files)
                self.write_checksums()
                self.assert_rejected(expected)

    def test_rejects_empty_required_determinism_evidence(self) -> None:
        for path in (
            "determinism/stats-one.stdout",
            "determinism/stats-one.stderr",
            "determinism/profile-one.stdout",
            "determinism/profile-one.speedscope.json",
        ):
            with self.subTest(path=path):
                files = self.evidence_files()
                files[path] = b""
                paired_path = path.replace("one", "two")
                files[paired_path] = b""
                self.refresh_evidence_manifest(files)
                self.archive(f"jazz-{VERSION}-benchmark-evidence.tar.gz", files)
                self.write_checksums()
                self.assert_rejected("determinism evidence must be nonempty")

    def test_build_alpha_assembles_and_verifies_a_fixture_candidate(self) -> None:
        repository = Path(self.temporary_directory.name) / "fixture-repository"
        (repository / "scripts/release").mkdir(parents=True)
        (repository / "scripts/ci").mkdir(parents=True)
        (repository / "fixtures/docs").mkdir(parents=True)
        (repository / "fixtures/bin").mkdir(parents=True)
        (repository / "fixtures/nix-result/bin").mkdir(parents=True)
        (repository / "fixtures/evidence").mkdir(parents=True)
        shutil.copy2(BUILD, repository / "scripts/release/build-alpha.sh")
        shutil.copy2(VERIFY, repository / "scripts/release/verify-artifacts.py")
        (repository / ".gitignore").write_text(
            "/artifacts/\n/website/build/\n", encoding="utf-8"
        )
        (repository / "fixtures/docs/index.html").write_text(
            "<!doctype html><title>Jazz</title>\n", encoding="utf-8"
        )
        (repository / "fixtures/nix-result/bin/jazz").write_text(
            "#!/usr/bin/env bash\n", encoding="utf-8"
        )
        os.chmod(repository / "fixtures/nix-result/bin/jazz", 0o755)
        (repository / "fixtures/closure.nar").write_bytes(self.nix_export)
        (repository / "fixtures/bin/nix").write_text(
            "#!/usr/bin/env bash\nprintf 'aarch64-darwin'\n", encoding="utf-8"
        )
        os.chmod(repository / "fixtures/bin/nix", 0o755)
        (repository / "fixtures/bin/nix-store").write_text(
            f"""#!/usr/bin/env bash
set -euo pipefail
if [[ "$1" == "--query" && "$2" == "--outputs" ]]; then
  printf '%s\\n' '{self.nix_root}'
elif [[ "$1" == "--query" && "$2" == "--requisites" ]]; then
  printf '%s\\n' '{self.nix_root}'
elif [[ "$1" == "--export" ]]; then
  command cat fixtures/closure.nar
elif [[ "$1" == "--store" && "$3" == "--import" ]]; then
  printf '%s\\n' '{self.nix_root}'
elif [[ "$1" == "--store" && "$3" == "--query" && "$4" == "--requisites" ]]; then
  printf '%s\\n' '{self.nix_root}'
else
  exit 64
fi
""",
            encoding="utf-8",
        )
        os.chmod(repository / "fixtures/bin/nix-store", 0o755)
        (repository / "fixtures/bin/readlink").write_text(
            f"#!/usr/bin/env bash\nprintf '%s\\n' '{self.nix_root}'\n", encoding="utf-8"
        )
        os.chmod(repository / "fixtures/bin/readlink", 0o755)
        for relative_path, contents in self.evidence_files().items():
            destination = repository / "fixtures/evidence" / relative_path
            destination.parent.mkdir(parents=True, exist_ok=True)
            destination.write_bytes(contents)
        source_fixture = repository / "fixtures/jazz-0.1.0.0.tar.gz"
        with tarfile.open(source_fixture, "w:gz") as archive:
            contents = b"name: jazz\nversion: 0.1.0.0\n"
            member = tarfile.TarInfo("jazz-0.1.0.0/jazz.cabal")
            member.size = len(contents)
            archive.addfile(member, io.BytesIO(contents))

        candidate = repository / "scripts/ci/release-candidate.sh"
        candidate.write_text(
            """#!/usr/bin/env bash
set -euo pipefail
[[ ! -e "$JAZZ_ARTIFACT_ROOT" ]]
[[ "$JAZZ_ARTIFACT_ROOT" != "$JAZZ_RELEASE_OUTPUT_ROOT" ]]
mkdir -p "$JAZZ_ARTIFACT_ROOT" "$JAZZ_RELEASE_SDIST_ROOT" "$(dirname "$JAZZ_NIX_RESULT")" website/build
cp -R fixtures/evidence/. "$JAZZ_ARTIFACT_ROOT/"
cp fixtures/jazz-0.1.0.0.tar.gz "$JAZZ_RELEASE_SDIST_ROOT/"
cp -R fixtures/nix-result "$JAZZ_NIX_RESULT"
cp -R fixtures/docs/. website/build/
""",
            encoding="utf-8",
        )
        os.chmod(candidate, 0o755)
        for command in (
            ("git", "init", "-q"),
            ("git", "config", "user.email", "fixture@example.com"),
            ("git", "config", "user.name", "Fixture"),
            ("git", "add", "."),
            ("git", "commit", "-qm", "fixture"),
        ):
            subprocess.run(command, cwd=repository, check=True)

        environment = os.environ.copy()
        environment["JAZZ_RELEASE_VERSION"] = VERSION
        environment["PATH"] = str(repository / "fixtures/bin") + os.pathsep + environment["PATH"]
        result = subprocess.run(
            ["bash", "scripts/release/build-alpha.sh"],
            cwd=repository,
            env=environment,
            text=True,
            capture_output=True,
            check=False,
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        final_directory = repository / "artifacts/release" / VERSION
        verification = subprocess.run(
            [sys.executable, "scripts/release/verify-artifacts.py", str(final_directory)],
            cwd=repository,
            text=True,
            capture_output=True,
            check=False,
        )
        self.assertEqual(verification.returncode, 0, verification.stdout + verification.stderr)
        status = subprocess.run(
            ["git", "status", "--porcelain"],
            cwd=repository,
            text=True,
            capture_output=True,
            check=True,
        )
        self.assertEqual(status.stdout, "")


if __name__ == "__main__":
    unittest.main()
