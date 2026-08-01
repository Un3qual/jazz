#!/usr/bin/env python3
"""Behavior tests for the executable Jazz CI tier policy."""

from __future__ import annotations

import subprocess
import sys
import tempfile
import textwrap
import unittest
from pathlib import Path


CHECKER = Path(__file__).with_name("check-ci-policy.py")

FAST_COMPONENTS = (
    "cli-spec",
    "runtime-observation-spec",
    "warning-config-spec",
    "structured-error-diagnostics-spec",
    "diagnostic-catalog-spec",
    "signature-rendering-spec",
    "loader-spec",
    "module-resolution-spec",
    "module-exports-spec",
    "module-pipeline-contract-spec",
    "prelude-loading-spec",
    "stdlib-spec",
    "canonical-lexer-comparison-spec",
    "canonical-parser-comparison-spec",
    "canonical-core-comparison-spec",
    "jazz-lowered-ir-contract-spec",
    "jazz-typed-core-contract-spec",
    "jazz-typed-core-expression-direct-call-spec",
    "parser-core-spec",
    "jazz-parser-parity-spec",
    "jazz-parser-scale-spec",
    "jazz-lexer-parity-spec",
    "parser-foundation-spec",
    "binding-signature-coherence-spec",
    "purity-semantics-spec",
    "runtime-semantics-spec",
    "repository-audit-spec",
)


def script(body: str) -> str:
    return "#!/usr/bin/env bash\nset -euo pipefail\n" + textwrap.dedent(body).lstrip()


VALID_FAST = script(
    f"""
    cabal build all
    test_components=({' '.join(FAST_COMPONENTS)})
    cabal test "${{test_components[@]}}" --test-show-details=direct
    cabal check
    bash scripts/check-examples.sh
    git diff --check
    """
)

VALID_MAIN = script(
    """
    cabal build all
    cabal test all --test-show-details=direct
    cabal check
    bash scripts/check-docs.sh
    bash scripts/check-execution-queue.sh
    bash scripts/check-examples.sh
    nix flake check
    git diff --check
    """
)

VALID_DETERMINISM = script(
    """
    JAZZ_ARTIFACT_ROOT="${JAZZ_ARTIFACT_ROOT:-artifacts/determinism}"
    "$JAZZ_BIN" --run --runtime-stats=json examples/functions/factorial.jz >"$JAZZ_ARTIFACT_ROOT/stats-one.stdout" 2>"$JAZZ_ARTIFACT_ROOT/stats-one.stderr"
    "$JAZZ_BIN" --run --runtime-stats=json examples/functions/factorial.jz >"$JAZZ_ARTIFACT_ROOT/stats-two.stdout" 2>"$JAZZ_ARTIFACT_ROOT/stats-two.stderr"
    "$JAZZ_BIN" --run --runtime-profile="$JAZZ_ARTIFACT_ROOT/profile-one.speedscope.json" examples/functions/factorial.jz >"$JAZZ_ARTIFACT_ROOT/profile-one.stdout" 2>"$JAZZ_ARTIFACT_ROOT/profile-one.stderr"
    "$JAZZ_BIN" --run --runtime-profile="$JAZZ_ARTIFACT_ROOT/profile-two.speedscope.json" examples/functions/factorial.jz >"$JAZZ_ARTIFACT_ROOT/profile-two.stdout" 2>"$JAZZ_ARTIFACT_ROOT/profile-two.stderr"
    cmp "$JAZZ_ARTIFACT_ROOT/stats-one.stdout" "$JAZZ_ARTIFACT_ROOT/stats-two.stdout"
    cmp "$JAZZ_ARTIFACT_ROOT/stats-one.stderr" "$JAZZ_ARTIFACT_ROOT/stats-two.stderr"
    cmp "$JAZZ_ARTIFACT_ROOT/profile-one.stdout" "$JAZZ_ARTIFACT_ROOT/profile-two.stdout"
    cmp "$JAZZ_ARTIFACT_ROOT/profile-one.stderr" "$JAZZ_ARTIFACT_ROOT/profile-two.stderr"
    cmp "$JAZZ_ARTIFACT_ROOT/profile-one.speedscope.json" "$JAZZ_ARTIFACT_ROOT/profile-two.speedscope.json"
    """
)

VALID_EXTENDED = script(
    """
    JAZZ_ARTIFACT_ROOT="${JAZZ_ARTIFACT_ROOT:-artifacts/extended}"
    if [[ -d "$JAZZ_ARTIFACT_ROOT" && -n "$(find "$JAZZ_ARTIFACT_ROOT" -mindepth 1 -print -quit)" ]]; then exit 1; fi
    mkdir -p "$JAZZ_ARTIFACT_ROOT/corpus" "$JAZZ_ARTIFACT_ROOT/benchmarks"
    full_scale_components=(jazz-parser-scale-full-expression-spec jazz-parser-scale-full-declarations-spec jazz-parser-scale-full-control-flow-spec jazz-parser-scale-full-operator-spec)
    cabal test all "${full_scale_components[@]}" -ffull-parser-scale --test-show-details=always --test-log="$corpus_log_root/first/\\$test-suite.log"
    cabal test program-corpus-spec --test-show-details=always --test-log="$corpus_log_root/second/\\$test-suite.log"
    python3 - "$corpus_log_root/first/program-corpus-spec.log" "$corpus_log_root/second/program-corpus-spec.log" "$JAZZ_ARTIFACT_ROOT/corpus/pass-one.txt" "$JAZZ_ARTIFACT_ROOT/corpus/pass-two.txt" <<'PY'
    first_destination.write_text(normalize(first_log), encoding="utf-8")
    second_destination.write_text(normalize(second_log), encoding="utf-8")
    PY
    cmp "$JAZZ_ARTIFACT_ROOT/corpus/pass-one.txt" "$JAZZ_ARTIFACT_ROOT/corpus/pass-two.txt"
    JAZZ_ARTIFACT_ROOT="$JAZZ_ARTIFACT_ROOT/determinism" bash scripts/ci/determinism.sh
    cabal --project-file=cabal.project.profile-stages build all
    cabal --project-file=cabal.project.profile-hotspots build all
    cabal bench jazz-bench --benchmark-options="--environment-label=${JAZZ_BENCHMARK_LABEL} --result-root=${JAZZ_ARTIFACT_ROOT}/benchmarks"
    python3 - "$JAZZ_ARTIFACT_ROOT/benchmarks" "$JAZZ_BENCHMARK_LABEL" <<'PY'
    run_directories = [path for path in label_root.iterdir() if path.is_dir()]
    if len(run_directories) != 1: raise SystemExit(1)
    environment_path = run_directories[0] / "environment.json"
    results_path = run_directories[0] / "results.csv"
    if metadata["environment_label"] != expected_label: raise SystemExit(1)
    if metadata["schema_version"] != 2: raise SystemExit(1)
    if not results_path.is_file() or results_path.stat().st_size == 0: raise SystemExit(1)
    PY
    cabal test benchmark-metadata-spec --test-show-details=direct
    manifest_path="$JAZZ_ARTIFACT_ROOT/manifest.json"
    python3 - "$JAZZ_ARTIFACT_ROOT" "$manifest_path" <<'PY'
    for path in sorted(artifact_root.rglob("*")):
    artifacts.append({"path": path.relative_to(artifact_root).as_posix(), "sha256": hashlib.sha256(path.read_bytes()).hexdigest()})
    manifest_path.write_text(json.dumps({"schema_version": 1, "artifacts": artifacts}), encoding="utf-8")
    PY
    """
)

VALID_RELEASE = script(
    r"""
    : "${JAZZ_RELEASE_VERSION:?JAZZ_RELEASE_VERSION is required}"
    if [[ ! "$JAZZ_RELEASE_VERSION" =~ ^0\.[0-9]+\.[0-9]+-alpha\.[0-9]+$ ]]; then exit 1; fi
    bash scripts/ci/main-functional.sh
    bash scripts/ci/extended.sh
    bash scripts/check-docs.sh
    npm --prefix website ci
    npm --prefix website run build
    bash scripts/check-website.sh
    cabal sdist all
    nix build .#jazz
    require_path website/build/index.html
    require_path dist-newstyle/sdist
    require_path result
    validate_artifact_manifest "${JAZZ_ARTIFACT_ROOT}/manifest.json"
    required_artifacts = {
        "corpus/pass-one.txt", "corpus/pass-two.txt",
        "determinism/stats-one.stdout", "determinism/stats-two.stdout",
        "determinism/profile-one.speedscope.json", "determinism/profile-two.speedscope.json",
    }
    results_paths = [path for path in manifest_paths if path.startswith("benchmarks/") and path.endswith("/results.csv")]
    environment_paths = [path for path in manifest_paths if path.startswith("benchmarks/") and path.endswith("/environment.json")]
    if len(results_paths) != 1 or len(environment_paths) != 1: raise SystemExit(1)
    """
)

VALID_PR_WORKFLOW = textwrap.dedent(
    """
    name: Pull request checks
    on:
      pull_request:
    jobs:
      compiler:
        runs-on: ubuntu-latest
        steps:
          - run: bash scripts/ci/fast-compiler.sh
    """
).lstrip()


class CiPolicyCheckerTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary_directory.name)
        self.write("scripts/ci/fast-compiler.sh", VALID_FAST)
        self.write("scripts/ci/main-functional.sh", VALID_MAIN)
        self.write("scripts/ci/determinism.sh", VALID_DETERMINISM)
        self.write("scripts/ci/extended.sh", VALID_EXTENDED)
        self.write("scripts/ci/release-candidate.sh", VALID_RELEASE)
        self.write(".github/workflows/ci-pr.yml", VALID_PR_WORKFLOW)

    def tearDown(self) -> None:
        self.temporary_directory.cleanup()

    def write(self, relative_path: str, contents: str) -> None:
        path = self.root / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(contents, encoding="utf-8")

    def run_checker(self) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [sys.executable, str(CHECKER), str(self.root)],
            text=True,
            capture_output=True,
            check=False,
        )

    def assert_violation(self, expected: str) -> None:
        result = self.run_checker()
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn(expected, result.stdout)

    def test_accepts_a_complete_tier_policy(self) -> None:
        result = self.run_checker()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stdout, "CI policy checks passed.\n")

    def test_reports_missing_policy_files_in_stable_order(self) -> None:
        (self.root / "scripts/ci/fast-compiler.sh").unlink()
        (self.root / "scripts/ci/determinism.sh").unlink()

        first = self.run_checker()
        second = self.run_checker()

        self.assertNotEqual(first.returncode, 0)
        self.assertEqual(first.stdout, second.stdout)
        self.assertEqual(
            first.stdout.splitlines(),
            sorted(first.stdout.splitlines()),
        )
        self.assertIn("missing required CI policy file: scripts/ci/determinism.sh", first.stdout)
        self.assertIn("missing required CI policy file: scripts/ci/fast-compiler.sh", first.stdout)

    def test_fast_tier_requires_every_focused_component(self) -> None:
        self.write("scripts/ci/fast-compiler.sh", VALID_FAST.replace("stdlib-spec", ""))
        self.assert_violation("fast compiler tier is missing required token: stdlib-spec")

    def test_fast_tier_does_not_accept_an_inert_quoted_command(self) -> None:
        self.write(
            "scripts/ci/fast-compiler.sh",
            VALID_FAST.replace("cabal build all", ': "cabal build all"'),
        )
        self.assert_violation("fast compiler tier is missing required token: cabal build all")

    def test_fast_tier_rejects_long_running_work(self) -> None:
        for forbidden in (
            "cabal bench",
            "jazz-bench",
            "full-parser-scale",
            "profile-hotspots",
            "profile-stages",
            "program-corpus-spec",
        ):
            with self.subTest(forbidden=forbidden):
                self.write("scripts/ci/fast-compiler.sh", VALID_FAST + forbidden + "\n")
                self.assert_violation(f"fast compiler tier contains forbidden token: {forbidden}")

    def test_fast_tier_rejects_spaced_and_continued_benchmark_commands(self) -> None:
        for forbidden_command in ("cabal  bench jazz-bench\n", "cabal \\\n  bench jazz-bench\n"):
            with self.subTest(forbidden_command=forbidden_command):
                self.write("scripts/ci/fast-compiler.sh", VALID_FAST + forbidden_command)
                self.assert_violation("fast compiler tier contains forbidden token: cabal bench")

    def test_main_tier_requires_the_complete_ordinary_suite_and_validators(self) -> None:
        for required in (
            "cabal test all",
            "scripts/check-docs.sh",
            "scripts/check-execution-queue.sh",
            "scripts/check-examples.sh",
            "nix flake check",
        ):
            with self.subTest(required=required):
                self.write("scripts/ci/main-functional.sh", VALID_MAIN.replace(required, ""))
                self.assert_violation(f"main functional tier is missing required token: {required}")

    def test_main_tier_rejects_exhaustive_and_performance_work(self) -> None:
        for forbidden in (
            "cabal bench",
            "jazz-bench",
            "full-parser-scale",
            "profile-hotspots",
            "profile-stages",
        ):
            with self.subTest(forbidden=forbidden):
                self.write("scripts/ci/main-functional.sh", VALID_MAIN + forbidden + "\n")
                self.assert_violation(f"main functional tier contains forbidden token: {forbidden}")

    def test_main_tier_rejects_spaced_and_continued_benchmark_commands(self) -> None:
        for forbidden_command in ("cabal  bench jazz-bench\n", "cabal \\\n  bench jazz-bench\n"):
            with self.subTest(forbidden_command=forbidden_command):
                self.write("scripts/ci/main-functional.sh", VALID_MAIN + forbidden_command)
                self.assert_violation("main functional tier contains forbidden token: cabal bench")

    def test_main_tier_does_not_accept_an_inert_details_claim(self) -> None:
        self.write(
            "scripts/ci/main-functional.sh",
            VALID_MAIN.replace(
                "cabal test all --test-show-details=direct",
                'cabal test all\n: "--test-show-details=direct"',
            ),
        )
        self.assert_violation("main functional tier is missing required token: --test-show-details=direct")

    def test_policy_rejects_an_obvious_dead_code_guard(self) -> None:
        self.write(
            "scripts/ci/main-functional.sh",
            VALID_MAIN.replace(
                "cabal test all --test-show-details=direct",
                "if false; then\n"
                "  cabal test all --test-show-details=direct\n"
                "fi",
            ),
        )
        self.assert_violation("scripts/ci/main-functional.sh contains an obvious dead-code guard")

    def test_determinism_tier_requires_repeated_stats_profiles_and_byte_comparison(self) -> None:
        self.write(
            "scripts/ci/determinism.sh",
            VALID_DETERMINISM.replace(
                'cmp "$JAZZ_ARTIFACT_ROOT/profile-one.speedscope.json" "$JAZZ_ARTIFACT_ROOT/profile-two.speedscope.json"\n',
                "",
            ),
        )
        self.assert_violation("determinism tier must perform exactly five output comparisons with cmp")

    def test_determinism_tier_rejects_duplicate_destinations(self) -> None:
        self.write(
            "scripts/ci/determinism.sh",
            VALID_DETERMINISM.replace("stats-two.stdout", "stats-one.stdout"),
        )
        self.assert_violation("determinism tier must use distinct output destinations")

    def test_determinism_tier_rejects_a_self_comparison(self) -> None:
        self.write(
            "scripts/ci/determinism.sh",
            VALID_DETERMINISM.replace(
                'cmp "$JAZZ_ARTIFACT_ROOT/stats-one.stdout" "$JAZZ_ARTIFACT_ROOT/stats-two.stdout"',
                'cmp "$JAZZ_ARTIFACT_ROOT/stats-one.stdout" "$JAZZ_ARTIFACT_ROOT/stats-one.stdout"',
            ),
        )
        self.assert_violation("determinism tier must compare distinct output paths")

    def test_extended_tier_requires_exhaustive_components_and_second_corpus_run(self) -> None:
        for required in (
            "jazz-parser-scale-full-expression-spec",
            "jazz-parser-scale-full-declarations-spec",
            "jazz-parser-scale-full-control-flow-spec",
            "jazz-parser-scale-full-operator-spec",
            "cabal test program-corpus-spec",
        ):
            with self.subTest(required=required):
                self.write("scripts/ci/extended.sh", VALID_EXTENDED.replace(required, ""))
                self.assert_violation(f"extended tier is missing required token: {required}")

    def test_extended_tier_requires_owned_benchmark_arguments_and_manifest(self) -> None:
        for required in (
            "--environment-label",
            "--result-root",
            "JAZZ_ARTIFACT_ROOT",
            "manifest.json",
            "sha256",
        ):
            with self.subTest(required=required):
                self.write("scripts/ci/extended.sh", VALID_EXTENDED.replace(required, ""))
                self.assert_violation(f"extended tier is missing required token: {required}")

    def test_extended_tier_requires_owned_artifact_directories(self) -> None:
        self.write(
            "scripts/ci/extended.sh",
            VALID_EXTENDED.replace(
                'mkdir -p "$JAZZ_ARTIFACT_ROOT/corpus" "$JAZZ_ARTIFACT_ROOT/benchmarks"\n',
                "",
            ),
        )
        self.assert_violation("extended tier is missing required token: mkdir -p")

    def test_extended_tier_does_not_accept_an_inert_directory_claim(self) -> None:
        self.write(
            "scripts/ci/extended.sh",
            VALID_EXTENDED.replace(
                'mkdir -p "$JAZZ_ARTIFACT_ROOT/corpus" "$JAZZ_ARTIFACT_ROOT/benchmarks"',
                ': \'mkdir -p "$JAZZ_ARTIFACT_ROOT/corpus" "$JAZZ_ARTIFACT_ROOT/benchmarks"\'',
            ),
        )
        self.assert_violation("extended tier is missing required token: mkdir -p")

    def test_extended_tier_requires_a_fresh_artifact_root(self) -> None:
        self.write(
            "scripts/ci/extended.sh",
            VALID_EXTENDED.replace(
                'if [[ -d "$JAZZ_ARTIFACT_ROOT" && -n "$(find "$JAZZ_ARTIFACT_ROOT" -mindepth 1 -print -quit)" ]]; then exit 1; fi\n',
                "",
            ),
        )
        self.assert_violation("extended tier must reject a nonempty artifact root")

    def test_extended_tier_rejects_duplicate_corpus_destinations(self) -> None:
        self.write(
            "scripts/ci/extended.sh",
            VALID_EXTENDED.replace("corpus/pass-two.txt", "corpus/pass-one.txt"),
        )
        self.assert_violation("extended tier must capture two distinct corpus outputs")

    def test_extended_tier_rejects_a_corpus_self_comparison(self) -> None:
        self.write(
            "scripts/ci/extended.sh",
            VALID_EXTENDED.replace(
                'cmp "$JAZZ_ARTIFACT_ROOT/corpus/pass-one.txt" "$JAZZ_ARTIFACT_ROOT/corpus/pass-two.txt"',
                'cmp "$JAZZ_ARTIFACT_ROOT/corpus/pass-one.txt" "$JAZZ_ARTIFACT_ROOT/corpus/pass-one.txt"',
            ),
        )
        self.assert_violation("extended tier must compare distinct corpus outputs")

    def test_extended_tier_requires_generated_benchmark_validation(self) -> None:
        self.write(
            "scripts/ci/extended.sh",
            VALID_EXTENDED.replace(
                'if metadata["environment_label"] != expected_label: raise SystemExit(1)',
                'validation_claim = \'metadata["environment_label"] == expected_label\'',
            ),
        )
        self.assert_violation("extended tier must validate generated benchmark metadata")

    def test_extended_tier_rejects_fake_manifest_tokens(self) -> None:
        self.write(
            "scripts/ci/extended.sh",
            VALID_EXTENDED.replace(
                "manifest_path.write_text(json.dumps",
                'manifest_claim = "manifest_path.write_text(json.dumps" #',
            ),
        )
        self.assert_violation("extended tier must generate a SHA-256 artifact manifest")

    def test_extended_tier_rejects_timing_regression_thresholds(self) -> None:
        self.write(
            "scripts/ci/extended.sh",
            VALID_EXTENDED + "MAX_TIMING_REGRESSION_PERCENT=5\n",
        )
        self.assert_violation("extended tier must not fail on a timing regression threshold")

    def test_release_tier_requires_all_prerequisite_builds_and_validation(self) -> None:
        for required in (
            "scripts/ci/main-functional.sh",
            "scripts/ci/extended.sh",
            "scripts/check-docs.sh",
            "npm --prefix website ci",
            "npm --prefix website run build",
            "scripts/check-website.sh",
            "cabal sdist all",
            "nix build .#jazz",
            "validate_artifact_manifest",
        ):
            with self.subTest(required=required):
                self.write("scripts/ci/release-candidate.sh", VALID_RELEASE.replace(required, ""))
                self.assert_violation(f"release candidate tier is missing required token: {required}")

    def test_release_tier_requires_every_extended_artifact_category(self) -> None:
        self.write(
            "scripts/ci/release-candidate.sh",
            VALID_RELEASE.replace('"corpus/pass-one.txt"', '"corpus/claim.txt"'),
        )
        self.assert_violation("release candidate tier must validate every extended artifact category")

    def test_release_tier_does_not_accept_an_inert_category_claim(self) -> None:
        self.write(
            "scripts/ci/release-candidate.sh",
            VALID_RELEASE.replace(
                "required_artifacts = {",
                'category_claim = "required_artifacts = {"',
            ),
        )
        self.assert_violation("release candidate tier must validate every extended artifact category")

    def test_release_version_must_be_required_and_alpha_shaped(self) -> None:
        self.write(
            "scripts/ci/release-candidate.sh",
            VALID_RELEASE.replace(
                r"^0\.[0-9]+\.[0-9]+-alpha\.[0-9]+$",
                r"^[0-9]+\.[0-9]+\.[0-9]+$",
            ),
        )
        self.assert_violation("release candidate tier must enforce the 0.<minor>.<patch>-alpha.<n> version shape")

    def test_pull_request_workflow_cannot_inline_long_commands(self) -> None:
        for command in (
            "cabal test all --test-show-details=direct",
            "cabal bench jazz-bench",
            "cabal test program-corpus-spec",
        ):
            with self.subTest(command=command):
                self.write(".github/workflows/ci-pr.yml", VALID_PR_WORKFLOW + f"          - run: {command}\n")
                self.assert_violation("pull-request workflow inlines compiler or long-running CI work")


if __name__ == "__main__":
    unittest.main()
