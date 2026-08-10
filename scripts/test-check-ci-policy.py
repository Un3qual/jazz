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

REQUIRED_WORKFLOWS = {
    ".github/workflows/ci-pr.yml",
    ".github/workflows/ci-main.yml",
    ".github/workflows/ci-extended.yml",
    ".github/workflows/release.yml",
}
ACTION_PINS = (
    ("actions/checkout", "v4", "11d5960a326750d5838078e36cf38b85af677262"),
    ("cachix/install-nix-action", "v31", "630ae543ea3a38a9a4166f03376c02c50f408342"),
    ("actions/cache", "v4", "0057852bfaa89a56745cba8c7296529d2fc39830"),
    ("actions/upload-artifact", "v4", "ea165f8d65b6e75b540449e92b4886f43607fa02"),
    ("dorny/paths-filter", "v3", "0e4a8c6effa4802afeda77dc8d303f8176d7dfad"),
    ("pnpm/action-setup", "v4", "b906affcce14559ad1aafd4ab0e942779e9f58b1"),
    ("actions/setup-node", "v4", "49933ea5288caeca8642d1e84afbd3f7d6820020"),
)

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


def secured_workflow(contents: str) -> str:
    for action, version, revision in ACTION_PINS:
        contents = contents.replace(
            f"{action}@{version}", f"{action}@{revision} # {version}"
        )
    checkout = "actions/checkout@11d5960a326750d5838078e36cf38b85af677262 # v4"
    lines: list[str] = []
    for line in contents.splitlines():
        lines.append(line)
        if checkout in line:
            indent = line[: len(line) - len(line.lstrip())]
            lines.extend((f"{indent}with:", f"{indent}  persist-credentials: false"))
    return "\n".join(lines) + ("\n" if contents.endswith("\n") else "")


VALID_FAST = script(
    f"""
    cabal build all
    test_components=({' '.join(FAST_COMPONENTS)})
    cabal test "${{test_components[@]}}" --test-show-details=direct
    cabal check
    python3 scripts/release/test-verify-artifacts.py
    bash scripts/check-examples.sh
    if [[ -n "${{JAZZ_DIFF_BASE:-}}" ]]; then
      git diff --check "$JAZZ_DIFF_BASE...HEAD"
    else
      git diff --check
    fi
    """
)

VALID_MAIN = script(
    """
    cabal build all
    cabal test all --test-show-details=direct
    cabal check
    python3 scripts/test-check-ci-policy.py
    python3 scripts/release/test-verify-artifacts.py
    bash scripts/check-docs.sh
    bash scripts/check-execution-queue.sh
    bash scripts/check-examples.sh
    nix flake check
    if [[ -n "${JAZZ_DIFF_BASE:-}" ]]; then
      git diff --check "$JAZZ_DIFF_BASE...HEAD"
    else
      git diff --check
    fi
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
    if metadata.get("environment_label") != expected_label: raise SystemExit(1)
    if metadata.get("schema_version") != 2: raise SystemExit(1)
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
    NIX_CONFIG+='extra-experimental-features = nix-command flakes'
    export NIX_CONFIG
    : "${JAZZ_RELEASE_VERSION:?JAZZ_RELEASE_VERSION is required}"
    if [[ ! "$JAZZ_RELEASE_VERSION" =~ ^0\.[0-9]+\.[0-9]+-alpha\.[0-9]+$ ]]; then exit 1; fi
    JAZZ_ARTIFACT_ROOT="${JAZZ_ARTIFACT_ROOT:-artifacts/release-candidate/$JAZZ_RELEASE_VERSION/extended}"
    JAZZ_RELEASE_OUTPUT_ROOT="${JAZZ_RELEASE_OUTPUT_ROOT:-artifacts/release/$JAZZ_RELEASE_VERSION}"
    evidence_root, release_root = (os.path.realpath(path) for path in sys.argv[1:])
    common = os.path.commonpath((evidence_root, release_root))
    if evidence_root == release_root or common in (evidence_root, release_root): raise SystemExit(1)
    bash scripts/check-docs.sh
    find website -type f -name .DS_Store -delete
    pnpm --dir website install --frozen-lockfile
    pnpm --dir website run build
    bash scripts/check-website.sh
    bash scripts/ci/main-functional.sh
    bash scripts/ci/extended.sh
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

    permissions:
      contents: read
      pull-requests: read

    concurrency:
      group: ${{ github.workflow }}-pr-${{ github.event.pull_request.number }}
      cancel-in-progress: true

    jobs:
      changes:
        runs-on: ubuntu-latest
        outputs:
          compiler: ${{ steps.filter.outputs.compiler }}
        steps:
          - name: Check out repository
            uses: actions/checkout@v4
          - name: Detect compiler-relevant changes
            id: filter
            uses: dorny/paths-filter@v3
            with:
              predicate-quantifier: every
              filters: |
                compiler:
                  - '**'
                  - '!README.md'
                  - '!docs/**'
                  - '!rfcs/**'
                  - '!.codex/**'
                  - '!website/**'
                  - '!CONTRIBUTING.md'
                  - '!SECURITY.md'
                  - '!CHANGELOG.md'
                  - '!RELEASING.md'
                  - '!.github/ISSUE_TEMPLATE/**'
                  - '!.github/PULL_REQUEST_TEMPLATE.md'
      docs-and-site:
        runs-on: ubuntu-latest
        steps:
          - name: Check out repository
            uses: actions/checkout@v4
          - name: Install Nix
            uses: cachix/install-nix-action@v31
          - name: Set up pnpm
            uses: pnpm/action-setup@v4
            with:
              version: 11.18.0
          - name: Set up Node.js
            uses: actions/setup-node@v4
            with:
              node-version: 22
              cache: pnpm
              cache-dependency-path: website/pnpm-lock.yaml
          - name: Install website dependencies
            run: pnpm install --frozen-lockfile
            working-directory: website
          - name: Check documentation and RFCs
            run: nix develop .#docs --command bash scripts/check-docs.sh
          - name: Check website
            run: bash scripts/check-website.sh
          - name: Check CI policy
            run: python3 scripts/test-check-ci-policy.py
          - name: Check live CI policy
            run: python3 scripts/check-ci-policy.py
      compiler-fast:
        needs: changes
        if: needs.changes.outputs.compiler == 'true'
        runs-on: ubuntu-latest
        timeout-minutes: 30
        steps:
          - name: Check out repository
            uses: actions/checkout@v4
          - name: Install Nix
            uses: cachix/install-nix-action@v31
          - name: Cache Cabal dependencies and build output
            uses: actions/cache@v4
            with:
              path: |
                ~/.cabal/store
                dist-newstyle
              key: ${{ runner.os }}-cabal-${{ hashFiles('flake.lock', 'jazz.cabal', 'cabal.project') }}
          - name: Run the fast compiler tier
            run: nix develop --command bash scripts/ci/fast-compiler.sh
      pr-gate:
        name: Pull request gate
        if: always()
        needs:
          - changes
          - docs-and-site
          - compiler-fast
        runs-on: ubuntu-latest
        steps:
          - name: Require every applicable check
            env:
              CHANGES_RESULT: ${{ needs.changes.result }}
              DOCS_RESULT: ${{ needs.docs-and-site.result }}
              COMPILER_REQUIRED: ${{ needs.changes.outputs.compiler }}
              COMPILER_RESULT: ${{ needs.compiler-fast.result }}
            run: |
              [[ "$CHANGES_RESULT" == "success" ]]
              [[ "$DOCS_RESULT" == "success" ]]
              if [[ "$COMPILER_REQUIRED" == "true" ]]; then
                [[ "$COMPILER_RESULT" == "success" ]]
              else
                [[ "$COMPILER_REQUIRED" == "false" ]]
                [[ "$COMPILER_RESULT" == "skipped" ]]
              fi
    """
).lstrip()

VALID_MAIN_WORKFLOW = textwrap.dedent(
    """
    name: Main branch checks

    on:
      push:
        branches:
          - main
      workflow_dispatch:

    permissions:
      contents: read

    concurrency:
      group: ${{ github.workflow }}-${{ github.ref }}
      cancel-in-progress: true

    jobs:
      ordinary:
        name: Complete ordinary verification
        runs-on: ubuntu-latest
        timeout-minutes: 60
        steps:
          - name: Check out repository
            uses: actions/checkout@v4
          - name: Install Nix
            uses: cachix/install-nix-action@v31
          - name: Cache Cabal dependencies and build output
            uses: actions/cache@v4
            with:
              path: |
                ~/.cabal/store
                dist-newstyle
              key: ${{ runner.os }}-cabal-${{ hashFiles('flake.lock', 'jazz.cabal', 'cabal.project') }}
              restore-keys: |
                ${{ runner.os }}-cabal-
          - name: Remove cached test logs
            run: |
              if [[ -d dist-newstyle ]]; then
                find dist-newstyle -type f -name '*.log' -delete
              fi
          - name: Run complete ordinary verification
            id: ordinary
            run: nix develop --command bash scripts/ci/main-functional.sh
          - name: Collect ordinary test logs
            if: failure() && steps.ordinary.outcome == 'failure'
            run: |
              mkdir -p artifacts/ordinary-test-logs
              if [[ -d dist-newstyle ]]; then
                find dist-newstyle -type f -name '*.log' -exec cp --parents {} artifacts/ordinary-test-logs \\;
              fi
          - name: Upload ordinary test logs
            if: failure() && steps.ordinary.outcome == 'failure'
            uses: actions/upload-artifact@v4
            with:
              name: ordinary-test-logs-${{ github.run_id }}
              path: artifacts/ordinary-test-logs
              if-no-files-found: ignore
              retention-days: 7
    """
).lstrip()

VALID_EXTENDED_WORKFLOW = textwrap.dedent(
    """
    name: Extended verification

    on:
      schedule:
        - cron: '17 7 * * 0'
      workflow_dispatch:

    permissions:
      contents: read

    concurrency:
      group: extended
      cancel-in-progress: false

    jobs:
      extended:
        name: Weekly and manual extended verification
        runs-on: ubuntu-latest
        timeout-minutes: 360
        env:
          JAZZ_ARTIFACT_ROOT: artifacts/extended
          JAZZ_BENCHMARK_LABEL: github-actions-extended
        steps:
          - name: Check out repository
            uses: actions/checkout@v4
          - name: Install Nix
            uses: cachix/install-nix-action@v31
          - name: Cache Cabal dependencies and build output
            uses: actions/cache@v4
            with:
              path: |
                ~/.cabal/store
                dist-newstyle
              key: ${{ runner.os }}-cabal-${{ hashFiles('flake.lock', 'jazz.cabal', 'cabal.project') }}
              restore-keys: |
                ${{ runner.os }}-cabal-
          - name: Run extended verification
            id: extended
            run: nix develop --command bash scripts/ci/extended.sh
          - name: Upload extended verification evidence
            if: always()
            uses: actions/upload-artifact@v4
            with:
              name: extended-${{ github.sha }}-${{ github.run_id }}-${{ github.run_attempt }}
              path: artifacts/extended/
              if-no-files-found: warn
              retention-days: 30
          - name: Summarize extended verification
            if: always()
            env:
              EXTENDED_OUTCOME: ${{ steps.extended.outcome }}
            run: |
              {
                echo "## Extended verification"
                echo
                echo "Completion state: \\`$EXTENDED_OUTCOME\\`"
                echo
                echo "Artifact paths:"
                echo "- \\`artifacts/extended/benchmarks/**/results.csv\\`"
                echo "- \\`artifacts/extended/benchmarks/**/environment.json\\`"
                echo "- \\`artifacts/extended/determinism/profile-one.speedscope.json\\`"
                echo "- \\`artifacts/extended/determinism/profile-two.speedscope.json\\`"
                echo "- \\`artifacts/extended/corpus/pass-one.txt\\`"
                echo "- \\`artifacts/extended/corpus/pass-two.txt\\`"
                echo "- \\`artifacts/extended/manifest.json\\`"
              } >> "$GITHUB_STEP_SUMMARY"
    """
).lstrip()

VALID_ALPHA_BUILD = script(
    r"""
    NIX_CONFIG+='extra-experimental-features = nix-command flakes'
    export NIX_CONFIG
    : "${JAZZ_RELEASE_VERSION:?JAZZ_RELEASE_VERSION is required}"
    release_directory="artifacts/release/$JAZZ_RELEASE_VERSION"
    work_root="$(mktemp -d)"
    source_name="jazz-$JAZZ_RELEASE_VERSION-source.tar.gz"
    nix_name="jazz-$JAZZ_RELEASE_VERSION-nix-$system.tar.gz"
    docs_name="jazz-$JAZZ_RELEASE_VERSION-docs.tar.gz"
    evidence_name="jazz-$JAZZ_RELEASE_VERSION-benchmark-evidence.tar.gz"
    checksum_name="SHA256SUMS"
    JAZZ_ARTIFACT_ROOT="$work_root/extended" \
    JAZZ_BENCHMARK_LABEL="release-$JAZZ_RELEASE_VERSION" \
    JAZZ_RELEASE_OUTPUT_ROOT="$release_directory" \
      bash scripts/ci/release-candidate.sh
    nix-store --query --requisites "$nix_result" | LC_ALL=C sort -u > "$nix_closure_stage/store-paths"
    nix-store --export "${closure_paths[@]}" > "$nix_closure_stage/closure.nar"
    printf '%s\n' "$root_store_path" > "$nix_closure_stage/root-store-path"
    printf '%s\n' "$system" > "$nix_closure_stage/system"
    python3 scripts/release/verify-artifacts.py "$work_root/$JAZZ_RELEASE_VERSION"
    python3 scripts/release/verify-artifacts.py "$release_directory"
    """
)

VALID_RELEASE_WORKFLOW = textwrap.dedent(
    r"""
    name: Release candidate

    on:
      workflow_dispatch:
        inputs:
          version:
            description: Alpha version without the v prefix
            required: true
            type: string
      push:
        tags:
          - 'v*'

    permissions:
      contents: read

    concurrency:
      group: release-${{ github.ref }}-${{ inputs.version || github.ref_name }}
      cancel-in-progress: false

    jobs:
      release:
        name: Build verified alpha artifacts
        runs-on: ubuntu-latest
        timeout-minutes: 480
        env:
          NIX_CONFIG: |
            extra-experimental-features = nix-command flakes
        steps:
          - name: Check out repository
            uses: actions/checkout@v4
          - name: Install Nix
            uses: cachix/install-nix-action@v31
          - name: Set up pnpm
            uses: pnpm/action-setup@v4
            with:
              version: 11.18.0
          - name: Set up Node.js
            uses: actions/setup-node@v4
            with:
              node-version: 22
              cache: pnpm
              cache-dependency-path: website/pnpm-lock.yaml
          - name: Resolve alpha version
            env:
              EVENT_NAME: ${{ github.event_name }}
              DISPATCH_VERSION: ${{ inputs.version }}
              TAG_NAME: ${{ github.ref_name }}
            run: |
              if [[ "$EVENT_NAME" == "workflow_dispatch" ]]; then
                version="$DISPATCH_VERSION"
              else
                version="${TAG_NAME#v}"
              fi
              [[ "$version" =~ ^0\.[0-9]+\.[0-9]+-alpha\.[0-9]+$ ]]
              echo "JAZZ_RELEASE_VERSION=$version" >> "$GITHUB_ENV"
          - name: Build and verify alpha artifacts
            id: release
            run: nix develop --command bash scripts/release/build-alpha.sh
          - name: Upload verified alpha artifacts
            uses: actions/upload-artifact@v4
            with:
              name: jazz-${{ env.JAZZ_RELEASE_VERSION }}-${{ github.sha }}-${{ github.run_id }}
              path: artifacts/release/${{ env.JAZZ_RELEASE_VERSION }}/
              if-no-files-found: error
              retention-days: 30
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
        self.write("scripts/release/build-alpha.sh", VALID_ALPHA_BUILD)
        self.write(".github/workflows/ci-pr.yml", VALID_PR_WORKFLOW)
        self.write(".github/workflows/ci-main.yml", VALID_MAIN_WORKFLOW)
        self.write(".github/workflows/ci-extended.yml", VALID_EXTENDED_WORKFLOW)
        self.write(".github/workflows/release.yml", VALID_RELEASE_WORKFLOW)
        self.write(".gitignore", "/artifacts/\n/result\n")

    def tearDown(self) -> None:
        self.temporary_directory.cleanup()

    def write(self, relative_path: str, contents: str) -> None:
        path = self.root / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        if relative_path in REQUIRED_WORKFLOWS:
            contents = secured_workflow(contents)
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

    def test_every_workflow_action_uses_an_immutable_revision(self) -> None:
        self.write(
            ".github/workflows/auxiliary.yml",
            "name: Auxiliary\n"
            "on: workflow_dispatch\n"
            "permissions: {}\n"
            "jobs:\n"
            "  check:\n"
            "    runs-on: ubuntu-latest\n"
            "    steps:\n"
            "      - uses: actions/cache@v4\n",
        )
        self.assert_violation("workflow action must use an immutable commit")

    def test_every_checkout_disables_persisted_credentials(self) -> None:
        self.write(
            ".github/workflows/auxiliary.yml",
            "name: Auxiliary\n"
            "on: workflow_dispatch\n"
            "permissions: {}\n"
            "jobs:\n"
            "  check:\n"
            "    runs-on: ubuntu-latest\n"
            "    steps:\n"
            "      - uses: actions/checkout@11d5960a326750d5838078e36cf38b85af677262\n",
        )
        self.assert_violation("checkout must set persist-credentials: false")

    def test_every_checkout_uses_the_triggering_repository_and_revision(self) -> None:
        self.write(
            ".github/workflows/auxiliary.yml",
            "name: Auxiliary\n"
            "on: workflow_dispatch\n"
            "permissions: {}\n"
            "jobs:\n"
            "  check:\n"
            "    runs-on: ubuntu-latest\n"
            "    steps:\n"
            "      - uses: actions/checkout@11d5960a326750d5838078e36cf38b85af677262\n"
            "        with:\n"
            "          persist-credentials: false\n"
            "          ref: main\n",
        )
        self.assert_violation("checkout must use the triggering repository and revision")

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

    def test_fast_tier_runs_release_verifier_behavior_tests(self) -> None:
        self.write(
            "scripts/ci/fast-compiler.sh",
            VALID_FAST.replace("python3 scripts/release/test-verify-artifacts.py\n", ""),
        )
        self.assert_violation(
            "fast compiler tier must run release verifier behavior tests"
        )

    def test_fast_tier_checks_the_committed_pull_request_diff(self) -> None:
        self.write(
            "scripts/ci/fast-compiler.sh",
            VALID_FAST.replace('git diff --check "$JAZZ_DIFF_BASE...HEAD"', "git diff --check"),
        )
        self.assert_violation(
            "fast compiler tier must check the committed diff when JAZZ_DIFF_BASE is set"
        )

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

    def test_main_tier_runs_validator_behavior_tests(self) -> None:
        for command, expected in (
            (
                "python3 scripts/test-check-ci-policy.py\n",
                "main functional tier must run CI policy behavior tests",
            ),
            (
                "python3 scripts/release/test-verify-artifacts.py\n",
                "main functional tier must run release verifier behavior tests",
            ),
        ):
            with self.subTest(command=command):
                self.write(
                    "scripts/ci/main-functional.sh",
                    VALID_MAIN.replace(command, ""),
                )
                self.assert_violation(expected)

    def test_main_tier_checks_the_committed_push_diff(self) -> None:
        self.write(
            "scripts/ci/main-functional.sh",
            VALID_MAIN.replace('git diff --check "$JAZZ_DIFF_BASE...HEAD"', "git diff --check"),
        )
        self.assert_violation(
            "main functional tier must check the committed diff when JAZZ_DIFF_BASE is set"
        )

    def test_main_tier_rejects_indirect_extended_and_release_entry_points(self) -> None:
        for forbidden in (
            "scripts/ci/extended.sh",
            "scripts/ci/release-candidate.sh",
            "scripts/ci/determinism.sh",
            "scripts/release/build-alpha.sh",
        ):
            with self.subTest(forbidden=forbidden):
                self.write(
                    "scripts/ci/main-functional.sh",
                    VALID_MAIN + f"bash {forbidden}\n",
                )
                self.assert_violation(
                    f"main functional tier contains forbidden token: {forbidden}"
                )

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
                'if metadata.get("environment_label") != expected_label: raise SystemExit(1)',
                'validation_claim = \'metadata.get("environment_label") == expected_label\'',
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
            "pnpm --dir website install --frozen-lockfile",
            "pnpm --dir website run build",
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

    def test_release_candidate_owns_fresh_evidence_outside_final_artifacts(self) -> None:
        for token in (
            'artifacts/release-candidate/$JAZZ_RELEASE_VERSION/extended',
            'evidence_root == release_root or common in (evidence_root, release_root)',
        ):
            with self.subTest(token=token):
                self.write("scripts/ci/release-candidate.sh", VALID_RELEASE.replace(token, "removed", 1))
                self.assert_violation(
                    "release candidate tier must use a fresh evidence root outside final artifacts"
                )

    def test_release_scripts_enable_required_nix_features_before_nested_nix(self) -> None:
        fixtures = (
            (
                "scripts/ci/release-candidate.sh",
                VALID_RELEASE.replace("extra-experimental-features = nix-command flakes", "removed", 1),
            ),
            (
                "scripts/release/build-alpha.sh",
                VALID_ALPHA_BUILD.replace("extra-experimental-features = nix-command flakes", "removed", 1),
            ),
        )
        for path, fixture in fixtures:
            with self.subTest(path=path):
                self.write(path, fixture)
                self.assert_violation(
                    f"{path} must enable nix-command and flakes before nested Nix invocations"
                )

    def test_pull_request_workflow_cannot_inline_long_commands(self) -> None:
        for command in (
            "cabal test all --test-show-details=direct",
            "cabal bench jazz-bench",
            "cabal test program-corpus-spec",
        ):
            with self.subTest(command=command):
                self.write(".github/workflows/ci-pr.yml", VALID_PR_WORKFLOW + f"          - run: {command}\n")
                self.assert_violation("pull-request workflow inlines compiler or long-running CI work")

    def test_pull_request_workflow_is_required(self) -> None:
        (self.root / ".github/workflows/ci-pr.yml").unlink()
        self.assert_violation("missing required pull-request workflow: .github/workflows/ci-pr.yml")

    def test_main_workflow_is_required(self) -> None:
        (self.root / ".github/workflows/ci-main.yml").unlink()
        self.assert_violation("missing required main workflow: .github/workflows/ci-main.yml")

    def test_extended_workflow_is_required(self) -> None:
        (self.root / ".github/workflows/ci-extended.yml").unlink()
        self.assert_violation(
            "missing required extended workflow: .github/workflows/ci-extended.yml"
        )

    def test_release_workflow_is_required(self) -> None:
        (self.root / ".github/workflows/release.yml").unlink()
        self.assert_violation("missing required release workflow: .github/workflows/release.yml")

    def test_release_workflow_requires_manual_and_tag_alpha_inputs(self) -> None:
        fixtures = (
            (
                VALID_RELEASE_WORKFLOW.replace("  workflow_dispatch:\n", "  repository_dispatch:\n"),
                "release workflow must support workflow_dispatch with a required version input",
            ),
            (
                VALID_RELEASE_WORKFLOW.replace("        required: true", "        required: false"),
                "release workflow must support workflow_dispatch with a required version input",
            ),
            (
                VALID_RELEASE_WORKFLOW.replace("      - 'v*'", "      - '*'"),
                "release workflow tag trigger must be restricted to v*",
            ),
        )
        for fixture, expected in fixtures:
            with self.subTest(expected=expected):
                self.write(".github/workflows/release.yml", fixture)
                self.assert_violation(expected)

    def test_release_workflow_is_read_only_and_never_publishes(self) -> None:
        fixtures = (
            VALID_RELEASE_WORKFLOW.replace("contents: read", "contents: write"),
            VALID_RELEASE_WORKFLOW.replace(
                "  release:\n    name:",
                "  release:\n    permissions:\n      contents: write\n    name:",
            ),
            VALID_RELEASE_WORKFLOW + "          - run: gh release create v0.1.0-alpha.1\n",
        )
        for fixture in fixtures:
            with self.subTest(fixture=fixture):
                self.write(".github/workflows/release.yml", fixture)
                self.assert_violation("release workflow must be read-only and must not publish")

    def test_release_workflow_serializes_each_version(self) -> None:
        self.write(
            ".github/workflows/release.yml",
            VALID_RELEASE_WORKFLOW.replace(
                "-${{ inputs.version || github.ref_name }}", ""
            ),
        )
        self.assert_violation(
            "release workflow concurrency must include the requested version"
        )

    def test_release_workflow_requires_toolchains_timeout_owned_script_and_upload(self) -> None:
        requirements = (
            ("timeout-minutes: 480", "release job must have a 480-minute timeout"),
            ("cachix/install-nix-action@v31", "release job must install Nix"),
            ("pnpm/action-setup@v4", "release job must use pnpm/action-setup@v4"),
            ("version: 11.18.0", "release job must use pnpm 11.18.0"),
            ("actions/setup-node@v4", "release job must set up Node.js"),
            ("node-version: 22", "release job must use Node 22"),
            ("cache: pnpm", "release job must use the pnpm cache"),
            (
                "cache-dependency-path: website/pnpm-lock.yaml",
                "release job must key the pnpm cache from website/pnpm-lock.yaml",
            ),
            (
                "nix develop --command bash scripts/release/build-alpha.sh",
                "release job must invoke scripts/release/build-alpha.sh",
            ),
            ("actions/upload-artifact@v4", "release workflow must upload verified artifacts with actions/upload-artifact@v4"),
            ("if-no-files-found: error", "release artifact upload must fail when files are missing"),
            ("retention-days: 30", "release artifact upload must retain artifacts for 30 days"),
        )
        for token, expected in requirements:
            with self.subTest(token=token):
                self.write(".github/workflows/release.yml", VALID_RELEASE_WORKFLOW.replace(token, "removed", 1))
                self.assert_violation(expected)

    def test_release_workflow_propagates_required_nix_features(self) -> None:
        self.write(
            ".github/workflows/release.yml",
            VALID_RELEASE_WORKFLOW.replace(
                "extra-experimental-features = nix-command flakes", "removed", 1
            ),
        )
        self.assert_violation(
            "release job must propagate nix-command and flakes to every nested Nix invocation"
        )

    def test_release_workflow_cannot_mask_or_skip_the_release_gates(self) -> None:
        for injection in (
            "        continue-on-error: true\n",
            "        if: always()\n",
            "        run: nix develop --command bash scripts/release/build-alpha.sh || true\n",
        ):
            with self.subTest(injection=injection):
                fixture = VALID_RELEASE_WORKFLOW.replace(
                    "        id: release\n        run: nix develop --command bash scripts/release/build-alpha.sh\n",
                    "        id: release\n" + injection,
                )
                self.write(".github/workflows/release.yml", fixture)
                self.assert_violation("release workflow must not mask or skip release verification")

    def test_alpha_builder_requires_release_candidate_evidence_and_final_verification(self) -> None:
        for token, expected in (
            (
                "bash scripts/ci/release-candidate.sh",
                "alpha artifact builder must invoke the complete release-candidate tier",
            ),
            (
                'JAZZ_ARTIFACT_ROOT="$work_root/extended"',
                "alpha artifact builder must use a fresh evidence root outside the final release directory",
            ),
            (
                'python3 scripts/release/verify-artifacts.py "$release_directory"',
                "alpha artifact builder must verify the final release directory",
            ),
        ):
            with self.subTest(token=token):
                self.write("scripts/release/build-alpha.sh", VALID_ALPHA_BUILD.replace(token, "removed", 1))
                self.assert_violation(expected)

    def test_alpha_builder_exports_a_sorted_same_system_nix_runtime_closure(self) -> None:
        for token in (
            'nix-store --query --requisites "$nix_result"',
            "LC_ALL=C sort -u",
            'nix-store --export "${closure_paths[@]}"',
            '"$nix_closure_stage/root-store-path"',
            '"$nix_closure_stage/system"',
        ):
            with self.subTest(token=token):
                self.write(
                    "scripts/release/build-alpha.sh",
                    VALID_ALPHA_BUILD.replace(token, "removed", 1),
                )
                self.assert_violation(
                    "alpha artifact builder must export a sorted same-system Nix runtime closure"
                )

    def test_generated_release_outputs_are_ignored_at_narrow_root_paths(self) -> None:
        for token in ("/artifacts/", "/result"):
            with self.subTest(token=token):
                self.write(".gitignore", "/artifacts/\n/result\n".replace(token + "\n", ""))
                self.assert_violation(
                    f"generated release output must be ignored at the repository root: {token}"
                )

    def test_extended_workflow_requires_the_weekly_and_manual_triggers(self) -> None:
        fixtures = (
            (
                VALID_EXTENDED_WORKFLOW.replace("  workflow_dispatch:\n", ""),
                "extended workflow must support workflow_dispatch",
            ),
            (
                VALID_EXTENDED_WORKFLOW.replace("'17 7 * * 0'", "'0 0 * * *'"),
                "extended workflow must run at 17 7 * * 0",
            ),
        )
        for fixture, expected in fixtures:
            with self.subTest(expected=expected):
                self.write(".github/workflows/ci-extended.yml", fixture)
                self.assert_violation(expected)

    def test_extended_workflow_rejects_hidden_ordinary_triggers(self) -> None:
        for trigger in (
            "  push:\n    branches:\n      - main\n",
            "  pull_request:\n",
            "  repository_dispatch:\n",
        ):
            with self.subTest(trigger=trigger):
                self.write(
                    ".github/workflows/ci-extended.yml",
                    VALID_EXTENDED_WORKFLOW.replace(
                        "  workflow_dispatch:\n",
                        "  workflow_dispatch:\n" + trigger,
                    ),
                )
                self.assert_violation(
                    "extended workflow must trigger only on its weekly schedule and manual dispatch"
                )

    def test_extended_workflow_requires_read_only_contents_without_overrides(self) -> None:
        fixtures = (
            VALID_EXTENDED_WORKFLOW.replace("contents: read", "contents: write"),
            VALID_EXTENDED_WORKFLOW.replace(
                "  extended:\n    name:",
                "  extended:\n    permissions:\n      contents: write\n    name:",
            ),
            VALID_EXTENDED_WORKFLOW.replace(
                "  contents: read\n", "  contents: read\n  pages: write\n"
            ),
        )
        for fixture in fixtures:
            with self.subTest(fixture=fixture):
                self.write(".github/workflows/ci-extended.yml", fixture)
                self.assert_violation(
                    "extended workflow must grant only read access to contents"
                )

    def test_extended_workflow_requires_non_cancelling_extended_concurrency(self) -> None:
        for old, expected in (
            (
                "group: extended",
                "extended workflow concurrency group must be extended",
            ),
            (
                "cancel-in-progress: false",
                "extended workflow must not cancel an in-progress evidence run",
            ),
        ):
            with self.subTest(old=old):
                self.write(
                    ".github/workflows/ci-extended.yml",
                    VALID_EXTENDED_WORKFLOW.replace(old, "removed"),
                )
                self.assert_violation(expected)

    def test_extended_job_requires_timeout_nix_safe_caches_and_owned_invocation(self) -> None:
        for old, expected in (
            (
                "timeout-minutes: 360",
                "extended job must have a 360-minute timeout",
            ),
            (
                "cachix/install-nix-action@v31",
                "extended job must use cachix/install-nix-action@v31",
            ),
            ("actions/cache@v4", "extended job must use actions/cache@v4"),
            ("~/.cabal/store", "extended cache must include ~/.cabal/store"),
            ("dist-newstyle", "extended cache must include dist-newstyle"),
            (
                "runner.os",
                "extended cache key must include runner.os",
            ),
            (
                "hashFiles('flake.lock', 'jazz.cabal', 'cabal.project')",
                "extended cache key must include flake.lock, jazz.cabal, and cabal.project",
            ),
            (
                "restore-keys: |\n            ${{ runner.os }}-cabal-",
                "extended cache must restore only the operating-system Cabal prefix",
            ),
            (
                "JAZZ_ARTIFACT_ROOT: artifacts/extended",
                "extended job must own JAZZ_ARTIFACT_ROOT=artifacts/extended",
            ),
            (
                "JAZZ_BENCHMARK_LABEL: github-actions-extended",
                "extended job must own JAZZ_BENCHMARK_LABEL=github-actions-extended",
            ),
            (
                "nix develop --command bash scripts/ci/extended.sh",
                "extended job must invoke the owned extended script",
            ),
        ):
            with self.subTest(old=old):
                self.write(
                    ".github/workflows/ci-extended.yml",
                    VALID_EXTENDED_WORKFLOW.replace(old, "removed", 1),
                )
                self.assert_violation(expected)

    def test_extended_cache_rejects_unowned_paths(self) -> None:
        self.write(
            ".github/workflows/ci-extended.yml",
            VALID_EXTENDED_WORKFLOW.replace(
                "            dist-newstyle\n",
                "            dist-newstyle\n            /tmp/extended-cache\n",
            ),
        )
        self.assert_violation(
            "extended cache must contain only the Cabal store and ordinary build output"
        )

    def test_extended_run_step_exposes_outcome_without_masking_failure(self) -> None:
        fixtures = (
            (
                VALID_EXTENDED_WORKFLOW.replace("        id: extended\n", ""),
                "extended verification step must expose the extended id",
            ),
            (
                VALID_EXTENDED_WORKFLOW.replace(
                    "        id: extended\n",
                    "        id: extended\n        continue-on-error: true\n",
                ),
                "extended workflow must not mask verification or evidence failures",
            ),
            (
                VALID_EXTENDED_WORKFLOW.replace(
                    "nix develop --command bash scripts/ci/extended.sh",
                    "nix develop --command bash scripts/ci/extended.sh || true",
                ),
                "extended workflow must not mask verification or evidence failures",
            ),
        )
        for fixture, expected in fixtures:
            with self.subTest(expected=expected):
                self.write(".github/workflows/ci-extended.yml", fixture)
                self.assert_violation(expected)

    def test_extended_workflow_always_uploads_owned_evidence_with_provenance(self) -> None:
        for old, expected in (
            (
                "if: always()",
                "extended evidence upload must run on success or failure",
            ),
            (
                "actions/upload-artifact@v4",
                "extended evidence upload must use actions/upload-artifact@v4",
            ),
            (
                "extended-${{ github.sha }}-${{ github.run_id }}-${{ github.run_attempt }}",
                "extended evidence artifact name must include commit and run provenance",
            ),
            (
                "path: artifacts/extended/",
                "extended evidence upload must include the owned artifact root",
            ),
            (
                "if-no-files-found: warn",
                "extended evidence upload must tolerate a failed run without evidence",
            ),
            (
                "retention-days: 30",
                "extended evidence upload must retain evidence for 30 days",
            ),
        ):
            with self.subTest(old=old):
                self.write(
                    ".github/workflows/ci-extended.yml",
                    VALID_EXTENDED_WORKFLOW.replace(old, "removed", 1),
                )
                self.assert_violation(expected)

    def test_extended_summary_reports_state_and_every_evidence_category(self) -> None:
        required_paths = (
            "benchmarks/**/results.csv",
            "benchmarks/**/environment.json",
            "determinism/profile-one.speedscope.json",
            "determinism/profile-two.speedscope.json",
            "corpus/pass-one.txt",
            "corpus/pass-two.txt",
            "manifest.json",
        )
        for required_path in required_paths:
            with self.subTest(required_path=required_path):
                self.write(
                    ".github/workflows/ci-extended.yml",
                    VALID_EXTENDED_WORKFLOW.replace(required_path, "removed", 1),
                )
                self.assert_violation(
                    f"extended summary is missing evidence path: artifacts/extended/{required_path}"
                )
        for old, expected in (
            (
                "      - name: Summarize extended verification\n        if: always()",
                "extended summary must run on success or failure",
            ),
            (
                "EXTENDED_OUTCOME: ${{ steps.extended.outcome }}",
                "extended summary must bind the verification completion state",
            ),
            (
                '>> "$GITHUB_STEP_SUMMARY"',
                "extended summary must write to GITHUB_STEP_SUMMARY",
            ),
        ):
            with self.subTest(old=old):
                self.write(
                    ".github/workflows/ci-extended.yml",
                    VALID_EXTENDED_WORKFLOW.replace(old, "removed", 1),
                )
                self.assert_violation(expected)

    def test_extended_workflow_rejects_timing_threshold_expressions(self) -> None:
        self.write(
            ".github/workflows/ci-extended.yml",
            VALID_EXTENDED_WORKFLOW.replace(
                "Completion state:", "Timing regression percent threshold:"
            ),
        )
        self.assert_violation(
            "extended workflow must not classify timing changes with a threshold"
        )

    def test_extended_workflow_rejects_flow_form_ordinary_triggers(self) -> None:
        self.write(
            ".github/workflows/ci-extended.yml",
            VALID_EXTENDED_WORKFLOW.replace(
                "  workflow_dispatch:\n",
                "  workflow_dispatch:\n  pull_request: {}\n  push: {}\n",
            ),
        )
        self.assert_violation(
            "extended workflow must trigger only on its weekly schedule and manual dispatch"
        )

    def test_extended_workflow_rejects_quoted_job_permission_overrides(self) -> None:
        self.write(
            ".github/workflows/ci-extended.yml",
            VALID_EXTENDED_WORKFLOW.replace(
                "  extended:\n    name:",
                '  extended:\n    "permissions":\n      contents: write\n    name:',
            ),
        )
        self.assert_violation(
            "extended workflow must grant only read access to contents"
        )

    def test_extended_run_step_rejects_owned_environment_overrides(self) -> None:
        self.write(
            ".github/workflows/ci-extended.yml",
            VALID_EXTENDED_WORKFLOW.replace(
                "        id: extended\n        run:",
                "        id: extended\n"
                "        env:\n"
                "          JAZZ_ARTIFACT_ROOT: artifacts/elsewhere\n"
                "          JAZZ_BENCHMARK_LABEL: untrusted-label\n"
                "        run:",
            ),
        )
        self.assert_violation(
            "extended verification step must not override the owned environment"
        )

    def test_extended_run_step_rejects_a_success_masking_custom_shell(self) -> None:
        self.write(
            ".github/workflows/ci-extended.yml",
            VALID_EXTENDED_WORKFLOW.replace(
                "        id: extended\n        run:",
                '        id: extended\n        shell: \'bash -c "{0}; exit 0"\'\n        run:',
            ),
        )
        self.assert_violation(
            "extended workflow must not mask verification or evidence failures"
        )

    def test_checked_in_repository_policy_is_valid(self) -> None:
        repository_root = CHECKER.parents[1]
        result = subprocess.run(
            [sys.executable, str(CHECKER), str(repository_root)],
            text=True,
            capture_output=True,
            check=False,
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stdout, "CI policy checks passed.\n")

    def test_main_workflow_requires_main_push_and_manual_triggers(self) -> None:
        fixtures = (
            (
                VALID_MAIN_WORKFLOW.replace("      - main\n", "      - feature\n"),
                "main workflow push trigger must be restricted to main",
            ),
            (
                VALID_MAIN_WORKFLOW.replace("  workflow_dispatch:\n", ""),
                "main workflow must support workflow_dispatch",
            ),
        )
        for fixture, expected in fixtures:
            with self.subTest(expected=expected):
                self.write(".github/workflows/ci-main.yml", fixture)
                self.assert_violation(expected)

    def test_main_workflow_rejects_non_main_triggers(self) -> None:
        for trigger in ("  pull_request:\n", "  schedule:\n    - cron: '17 7 * * 0'\n"):
            with self.subTest(trigger=trigger):
                self.write(
                    ".github/workflows/ci-main.yml",
                    VALID_MAIN_WORKFLOW.replace(
                        "  workflow_dispatch:\n",
                        "  workflow_dispatch:\n" + trigger,
                    ),
                )
                self.assert_violation("main workflow must trigger only on main pushes and manual dispatch")

    def test_main_workflow_rejects_quoted_non_main_triggers(self) -> None:
        self.write(
            ".github/workflows/ci-main.yml",
            VALID_MAIN_WORKFLOW.replace(
                "  workflow_dispatch:\n",
                "  workflow_dispatch:\n  \"pull_request\":\n",
            ),
        )
        self.assert_violation(
            "main workflow must trigger only on main pushes and manual dispatch"
        )

    def test_main_workflow_requires_read_only_permissions_without_overrides(self) -> None:
        fixtures = (
            VALID_MAIN_WORKFLOW.replace("contents: read", "contents: write"),
            VALID_MAIN_WORKFLOW.replace(
                "  ordinary:\n    name:",
                "  ordinary:\n    permissions:\n      contents: write\n    name:",
            ),
        )
        for fixture in fixtures:
            with self.subTest(fixture=fixture):
                self.write(".github/workflows/ci-main.yml", fixture)
                self.assert_violation("main workflow must grant only read access to contents")

    def test_main_workflow_rejects_quoted_job_permission_overrides(self) -> None:
        self.write(
            ".github/workflows/ci-main.yml",
            VALID_MAIN_WORKFLOW.replace(
                "  ordinary:\n    name:",
                '  ordinary:\n    "permissions":\n      contents: write\n    name:',
            ),
        )
        self.assert_violation("main workflow must grant only read access to contents")

    def test_main_workflow_requires_branch_scoped_cancellation(self) -> None:
        for old, expected in (
            (
                "${{ github.workflow }}-${{ github.ref }}",
                "main workflow concurrency must include workflow and branch ref",
            ),
            (
                "cancel-in-progress: true",
                "main workflow must cancel superseded branch runs",
            ),
        ):
            with self.subTest(old=old):
                self.write(
                    ".github/workflows/ci-main.yml",
                    VALID_MAIN_WORKFLOW.replace(old, "removed"),
                )
                self.assert_violation(expected)

    def test_main_job_requires_timeout_nix_safe_caches_and_owned_script(self) -> None:
        for old, expected in (
            ("timeout-minutes: 60", "main ordinary job must have a 60-minute timeout"),
            ("cachix/install-nix-action@v31", "main ordinary job must use cachix/install-nix-action@v31"),
            ("actions/cache@v4", "main ordinary job must use actions/cache@v4"),
            ("~/.cabal/store", "main ordinary cache must include ~/.cabal/store"),
            ("dist-newstyle", "main ordinary cache must include dist-newstyle"),
            ("runner.os", "main ordinary cache key must include runner.os"),
            (
                "hashFiles('flake.lock', 'jazz.cabal', 'cabal.project')",
                "main ordinary cache key must include flake.lock, jazz.cabal, and cabal.project",
            ),
            (
                "restore-keys: |\n            ${{ runner.os }}-cabal-",
                "main ordinary cache must restore only the operating-system Cabal prefix",
            ),
            (
                "nix develop --command bash scripts/ci/main-functional.sh",
                "main ordinary job must invoke the complete ordinary script",
            ),
        ):
            with self.subTest(old=old):
                self.write(
                    ".github/workflows/ci-main.yml",
                    VALID_MAIN_WORKFLOW.replace(old, "removed", 1),
                )
                self.assert_violation(expected)

    def test_main_workflow_collects_only_logs_without_filename_collisions(self) -> None:
        for old, expected in (
            (
                "find dist-newstyle -type f -name '*.log' -exec cp --parents {} artifacts/ordinary-test-logs \\;",
                "main workflow must stage only ordinary logs with their source paths",
            ),
            (
                "cp --parents",
                "main workflow must stage only ordinary logs with their source paths",
            ),
        ):
            with self.subTest(old=old):
                self.write(
                    ".github/workflows/ci-main.yml",
                    VALID_MAIN_WORKFLOW.replace(old, "removed", 1),
                )
                self.assert_violation(expected)

    def test_main_workflow_uploads_only_staged_logs_on_failure_for_seven_days(self) -> None:
        for old, replacement, expected in (
            (
                "      - name: Collect ordinary test logs\n        if: failure() && steps.ordinary.outcome == 'failure'",
                "      - name: Collect ordinary test logs\n        if: always() && steps.ordinary.outcome == 'failure'",
                "main workflow must collect ordinary test logs only for ordinary failure",
            ),
            (
                "      - name: Upload ordinary test logs\n        if: failure() && steps.ordinary.outcome == 'failure'",
                "      - name: Upload ordinary test logs\n        if: always() && steps.ordinary.outcome == 'failure'",
                "main workflow must upload ordinary test logs only for ordinary failure",
            ),
            (
                "uses: actions/upload-artifact@v4",
                "uses: actions/upload-artifact@v3",
                "main workflow must use actions/upload-artifact@v4 for ordinary logs",
            ),
            (
                "path: artifacts/ordinary-test-logs",
                "path: dist-newstyle",
                "main workflow must upload only staged ordinary test logs",
            ),
            (
                "retention-days: 7",
                "retention-days: 30",
                "main workflow ordinary logs must have seven-day retention",
            ),
        ):
            with self.subTest(old=old):
                self.write(
                    ".github/workflows/ci-main.yml",
                    VALID_MAIN_WORKFLOW.replace(old, replacement, 1),
                )
                self.assert_violation(expected)

    def test_main_workflow_ties_failure_logs_to_the_ordinary_step(self) -> None:
        fixtures = (
            (
                VALID_MAIN_WORKFLOW.replace("        id: ordinary\n", ""),
                "main ordinary step must expose the ordinary id",
            ),
            (
                VALID_MAIN_WORKFLOW.replace(
                    "if: failure() && steps.ordinary.outcome == 'failure'",
                    "if: failure()",
                    1,
                ),
                "main workflow must collect ordinary test logs only for ordinary failure",
            ),
            (
                VALID_MAIN_WORKFLOW.replace(
                    "if: failure() && steps.ordinary.outcome == 'failure'",
                    "if: failure()",
                    2,
                ),
                "main workflow must upload ordinary test logs only for ordinary failure",
            ),
            (
                VALID_MAIN_WORKFLOW.replace(
                    "        id: ordinary\n",
                    "        id: ordinary\n        if: always()\n",
                ),
                "main ordinary step must retain implicit success gating",
            ),
        )
        for fixture, expected in fixtures:
            with self.subTest(expected=expected):
                self.write(".github/workflows/ci-main.yml", fixture)
                self.assert_violation(expected)

    def test_main_workflow_uploads_logs_after_collection(self) -> None:
        collect_marker = "      - name: Collect ordinary test logs\n"
        upload_marker = "      - name: Upload ordinary test logs\n"
        collect_start = VALID_MAIN_WORKFLOW.index(collect_marker)
        upload_start = VALID_MAIN_WORKFLOW.index(upload_marker)
        reordered = (
            VALID_MAIN_WORKFLOW[:collect_start]
            + VALID_MAIN_WORKFLOW[upload_start:]
            + VALID_MAIN_WORKFLOW[collect_start:upload_start]
        )
        self.write(".github/workflows/ci-main.yml", reordered)
        self.assert_violation("main workflow must upload ordinary logs after collection")

    def test_main_workflow_never_uploads_build_or_dependency_caches(self) -> None:
        for forbidden_path in ("dist-newstyle", "~/.cabal/store"):
            with self.subTest(forbidden_path=forbidden_path):
                self.write(
                    ".github/workflows/ci-main.yml",
                    VALID_MAIN_WORKFLOW.replace(
                        "path: artifacts/ordinary-test-logs",
                        f"path: {forbidden_path}",
                    ),
                )
                self.assert_violation(
                    f"main workflow must not upload build or dependency cache: {forbidden_path}"
                )

    def test_main_workflow_rejects_a_second_build_cache_artifact(self) -> None:
        for forbidden_path in ("dist-newstyle", "~/.cabal/store"):
            with self.subTest(forbidden_path=forbidden_path):
                self.write(
                    ".github/workflows/ci-main.yml",
                    VALID_MAIN_WORKFLOW
                    + "      - name: Upload build cache\n"
                    + "        if: failure()\n"
                    + "        uses: actions/upload-artifact@v4\n"
                    + "        with:\n"
                    + "          name: forbidden-build-cache\n"
                    + f"          path: {forbidden_path}\n",
                )
                self.assert_violation(
                    f"main workflow must not upload build or dependency cache: {forbidden_path}"
                )

    def test_main_workflow_rejects_exhaustive_and_performance_work(self) -> None:
        for token in (
            "cabal bench",
            "jazz-bench",
            "full-parser-scale",
            "profile-hotspots",
            "profile-stages",
        ):
            with self.subTest(token=token):
                self.write(".github/workflows/ci-main.yml", VALID_MAIN_WORKFLOW + token + "\n")
                self.assert_violation(f"main workflow contains forbidden token: {token}")

    def test_main_workflow_rejects_indirect_extended_and_release_entry_points(self) -> None:
        for forbidden in (
            "scripts/ci/extended.sh",
            "scripts/ci/release-candidate.sh",
            "scripts/ci/determinism.sh",
            "scripts/release/build-alpha.sh",
        ):
            with self.subTest(forbidden=forbidden):
                self.write(
                    ".github/workflows/ci-main.yml",
                    VALID_MAIN_WORKFLOW + f"      - run: bash {forbidden}\n",
                )
                self.assert_violation(f"main workflow contains forbidden token: {forbidden}")

    def test_pull_request_workflow_requires_read_only_permissions(self) -> None:
        for old in ("contents: read", "pull-requests: read"):
            with self.subTest(old=old):
                self.write(
                    ".github/workflows/ci-pr.yml",
                    VALID_PR_WORKFLOW.replace(old, old.replace("read", "write")),
                )
                self.assert_violation(
                    "pull-request workflow must grant only read access to contents and pull requests"
                )

    def test_pull_request_workflow_grants_paths_filter_pr_read_access(self) -> None:
        self.write(
            ".github/workflows/ci-pr.yml",
            VALID_PR_WORKFLOW.replace("  pull-requests: read\n", ""),
        )
        self.assert_violation(
            "pull-request workflow must grant only read access to contents and pull requests"
        )

    def test_pull_request_workflow_rejects_job_permission_overrides(self) -> None:
        self.write(
            ".github/workflows/ci-pr.yml",
            VALID_PR_WORKFLOW.replace(
                "  changes:\n    runs-on:",
                "  changes:\n    permissions:\n      contents: write\n    runs-on:",
            ),
        )
        self.assert_violation("pull-request workflow must not override permissions in a job")

    def test_pull_request_workflow_rejects_quoted_job_permission_overrides(self) -> None:
        self.write(
            ".github/workflows/ci-pr.yml",
            VALID_PR_WORKFLOW.replace(
                "  changes:\n    runs-on:",
                '  changes:\n    "permissions":\n      contents: write\n    runs-on:',
            ),
        )
        self.assert_violation("pull-request workflow must not override permissions in a job")

    def test_pull_request_workflow_requires_pr_scoped_cancellation(self) -> None:
        for old, replacement, expected in (
            (
                "  cancel-in-progress: true\n",
                "",
                "pull-request workflow must cancel superseded runs",
            ),
            (
                "${{ github.workflow }}-pr-${{ github.event.pull_request.number }}",
                "static-group",
                "pull-request workflow concurrency must include workflow and pull-request number",
            ),
        ):
            with self.subTest(old=old):
                self.write(
                    ".github/workflows/ci-pr.yml",
                    VALID_PR_WORKFLOW.replace(old, replacement),
                )
                self.assert_violation(expected)

    def test_changes_job_requires_paths_filter_and_compiler_output(self) -> None:
        for old, expected in (
            ("dorny/paths-filter@v3", "changes job must use dorny/paths-filter@v3"),
            ("predicate-quantifier: every", "changes job must apply every docs-only exclusion"),
            (
                "compiler: ${{ steps.filter.outputs.compiler }}",
                "changes job must publish the compiler path-filter output",
            ),
        ):
            with self.subTest(old=old):
                self.write(
                    ".github/workflows/ci-pr.yml",
                    VALID_PR_WORKFLOW.replace(old, "removed"),
                )
                self.assert_violation(expected)

    def test_changes_job_treats_only_the_documented_paths_as_docs_only(self) -> None:
        for exclusion in (
            "!README.md",
            "!docs/**",
            "!rfcs/**",
            "!.codex/**",
            "!website/**",
            "!CONTRIBUTING.md",
            "!SECURITY.md",
            "!CHANGELOG.md",
            "!RELEASING.md",
            "!.github/ISSUE_TEMPLATE/**",
            "!.github/PULL_REQUEST_TEMPLATE.md",
        ):
            with self.subTest(exclusion=exclusion):
                self.write(
                    ".github/workflows/ci-pr.yml",
                    VALID_PR_WORKFLOW.replace(f"          - '{exclusion}'\n", ""),
                )
                self.assert_violation(f"changes job is missing docs-only exclusion: {exclusion}")

    def test_changes_job_treats_unclassified_infrastructure_as_compiler_relevant(self) -> None:
        self.write(
            ".github/workflows/ci-pr.yml",
            VALID_PR_WORKFLOW.replace("          - '**'\n", "          - 'src/**'\n"),
        )
        self.assert_violation("changes job must default unclassified paths to compiler-relevant")

    def test_docs_job_requires_pinned_tools_node_cache_install_and_all_checks(self) -> None:
        for old, expected in (
            ("cachix/install-nix-action@v31", "docs-and-site job must install the pinned Nix documentation toolchain"),
            ("pnpm/action-setup@v4", "docs-and-site job must use pnpm/action-setup@v4"),
            ("version: 11.18.0", "docs-and-site job must use pnpm 11.18.0"),
            ("node-version: 22", "docs-and-site job must use Node 22"),
            ("cache: pnpm", "docs-and-site job must use the pnpm cache"),
            ("cache-dependency-path: website/pnpm-lock.yaml", "docs-and-site job must key the pnpm cache from website/pnpm-lock.yaml"),
            ("run: pnpm install --frozen-lockfile\n        working-directory: website", "docs-and-site job must install only website dependencies with pnpm --frozen-lockfile"),
            ("nix develop .#docs --command bash scripts/check-docs.sh", "docs-and-site job must run documentation checks in the pinned docs shell"),
            ("bash scripts/check-website.sh", "docs-and-site job is missing required check: scripts/check-website.sh"),
            ("python3 scripts/test-check-ci-policy.py", "docs-and-site job must run CI policy behavior tests"),
            ("python3 scripts/check-ci-policy.py", "docs-and-site job is missing required check: scripts/check-ci-policy.py"),
        ):
            with self.subTest(old=old):
                self.write(
                    ".github/workflows/ci-pr.yml",
                    VALID_PR_WORKFLOW.replace(old, "removed"),
                )
                self.assert_violation(expected)

    def test_docs_job_is_unconditional(self) -> None:
        for injected in ("    if: false\n", "    needs: compiler-fast\n"):
            with self.subTest(injected=injected):
                self.write(
                    ".github/workflows/ci-pr.yml",
                    VALID_PR_WORKFLOW.replace(
                        "  docs-and-site:\n",
                        "  docs-and-site:\n" + injected,
                    ),
                )
                self.assert_violation("docs-and-site job must run for every pull request")

    def test_docs_job_rejects_compiler_toolchain_and_unscoped_nix_commands(self) -> None:
        injection_point = "        run: nix develop .#docs --command bash scripts/check-docs.sh\n"
        for command in (
            "cabal check",
            "nix develop --command true",
            "ghcup/setup@v1",
        ):
            with self.subTest(command=command):
                injected = f"      - run: {command}\n" if "/" not in command or command.startswith("nix") else f"      - uses: {command}\n"
                self.write(
                    ".github/workflows/ci-pr.yml",
                    VALID_PR_WORKFLOW.replace(injection_point, injected + injection_point),
                )
                self.assert_violation("docs-and-site job must not install or invoke the compiler toolchain")

    def test_compiler_job_requires_path_condition_timeout_nix_cache_and_fast_script(self) -> None:
        for old, expected in (
            ("if: needs.changes.outputs.compiler == 'true'", "compiler-fast job must run only for compiler-relevant changes"),
            ("timeout-minutes: 30", "compiler-fast job must have a 30-minute timeout"),
            ("cachix/install-nix-action@v31", "compiler-fast job must use cachix/install-nix-action@v31"),
            ("actions/cache@v4", "compiler-fast job must use actions/cache@v4"),
            ("~/.cabal/store", "compiler-fast cache must include ~/.cabal/store"),
            ("dist-newstyle", "compiler-fast cache must include dist-newstyle"),
            ("runner.os", "compiler-fast cache key must include runner.os"),
            ("hashFiles('flake.lock', 'jazz.cabal', 'cabal.project')", "compiler-fast cache key must include flake.lock, jazz.cabal, and cabal.project"),
            ("nix develop --command bash scripts/ci/fast-compiler.sh", "compiler-fast job must invoke the fast compiler script"),
        ):
            with self.subTest(old=old):
                self.write(
                    ".github/workflows/ci-pr.yml",
                    VALID_PR_WORKFLOW.replace(old, "removed"),
                )
                self.assert_violation(expected)

    def test_compiler_job_rejects_a_timeout_too_short_for_cold_nix_setup(self) -> None:
        self.write(
            ".github/workflows/ci-pr.yml",
            VALID_PR_WORKFLOW.replace(
                "timeout-minutes: 30",
                "timeout-minutes: 12",
            ),
        )
        self.assert_violation(
            "compiler-fast job must have a 30-minute timeout"
        )

    def test_pull_request_workflow_rejects_every_extended_token(self) -> None:
        for token in (
            "cabal bench",
            "jazz-bench",
            "full-parser-scale",
            "profile-hotspots",
            "profile-stages",
            "program-corpus-spec",
        ):
            with self.subTest(token=token):
                self.write(".github/workflows/ci-pr.yml", VALID_PR_WORKFLOW + f"# benign section\n{token}\n")
                self.assert_violation(f"pull-request workflow contains forbidden extended token: {token}")

    def test_pr_gate_requires_all_dependencies_and_always_runs(self) -> None:
        for old, expected in (
            ("name: Pull request gate", "pr-gate job must expose the stable name: Pull request gate"),
            ("if: always()", "pr-gate job must run with if: always()"),
            ("      - changes\n", "pr-gate job must depend on changes"),
            ("      - docs-and-site\n", "pr-gate job must depend on docs-and-site"),
            ("      - compiler-fast\n", "pr-gate job must depend on compiler-fast"),
        ):
            with self.subTest(old=old):
                self.write(
                    ".github/workflows/ci-pr.yml",
                    VALID_PR_WORKFLOW.replace(old, ""),
                )
                self.assert_violation(expected)

    def test_pr_gate_rejects_failed_dependencies(self) -> None:
        for assertion, expected in (
            ('[[ "$CHANGES_RESULT" == "success" ]]', "pr-gate job must reject a failed changes dependency"),
            ('[[ "$DOCS_RESULT" == "success" ]]', "pr-gate job must reject a failed docs-and-site dependency"),
            ('[[ "$COMPILER_RESULT" == "success" ]]', "pr-gate job must require compiler-fast success for compiler changes"),
        ):
            with self.subTest(assertion=assertion):
                self.write(
                    ".github/workflows/ci-pr.yml",
                    VALID_PR_WORKFLOW.replace(assertion, "true"),
                )
                self.assert_violation(expected)

    def test_pr_gate_accepts_skip_only_for_docs_only_changes(self) -> None:
        for assertion, expected in (
            ('[[ "$COMPILER_REQUIRED" == "false" ]]', "pr-gate job must prove a compiler skip was documentation-only"),
            ('[[ "$COMPILER_RESULT" == "skipped" ]]', "pr-gate job must require a legitimate compiler-fast skip for docs-only changes"),
        ):
            with self.subTest(assertion=assertion):
                self.write(
                    ".github/workflows/ci-pr.yml",
                    VALID_PR_WORKFLOW.replace(assertion, "true"),
                )
                self.assert_violation(expected)

    def test_pr_gate_ties_compiler_success_and_skip_to_path_classification(self) -> None:
        self.write(
            ".github/workflows/ci-pr.yml",
            VALID_PR_WORKFLOW.replace(
                'if [[ "$COMPILER_REQUIRED" == "true" ]]; then',
                "if true; then",
            ),
        )
        self.assert_violation("pr-gate job must tie compiler result to path classification")

    def test_pr_gate_rejects_disabled_fail_fast_with_trailing_success(self) -> None:
        masked_gate = VALID_PR_WORKFLOW.replace(
            '[[ "$CHANGES_RESULT" == "success" ]]',
            'set +e\n'
            '          [[ "$CHANGES_RESULT" == "success" ]]',
        ).replace(
            "          fi\n",
            "          fi\n"
            "          true\n",
        )
        self.write(".github/workflows/ci-pr.yml", masked_gate)
        self.assert_violation(
            "pr-gate job must not disable fail-fast or mask assertion failures"
        )

    def test_pr_gate_rejects_pragmatic_result_masking_forms(self) -> None:
        fixtures = (
            VALID_PR_WORKFLOW.replace(
                '[[ "$DOCS_RESULT" == "success" ]]',
                '[[ "$DOCS_RESULT" == "success" ]] || true',
            ),
            VALID_PR_WORKFLOW.replace(
                "      - name: Require every applicable check\n",
                "      - name: Require every applicable check\n"
                "        continue-on-error: true\n",
            ),
            VALID_PR_WORKFLOW.replace(
                '[[ "$CHANGES_RESULT" == "success" ]]',
                'set +o errexit\n'
                '          [[ "$CHANGES_RESULT" == "success" ]]',
            ),
        )
        for fixture in fixtures:
            with self.subTest(fixture=fixture):
                self.write(".github/workflows/ci-pr.yml", fixture)
                self.assert_violation(
                    "pr-gate job must not disable fail-fast or mask assertion failures"
                )


if __name__ == "__main__":
    unittest.main()
