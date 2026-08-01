#!/usr/bin/env python3
"""Check that Jazz verification tiers preserve their workload boundaries."""

from __future__ import annotations

import re
import sys
from pathlib import Path


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

FAST_FORBIDDEN = (
    "cabal bench",
    "jazz-bench",
    "full-parser-scale",
    "profile-hotspots",
    "profile-stages",
    "program-corpus-spec",
)

MAIN_FORBIDDEN = (
    "cabal bench",
    "jazz-bench",
    "full-parser-scale",
    "profile-hotspots",
    "profile-stages",
)

POLICY_PATHS = (
    "scripts/ci/determinism.sh",
    "scripts/ci/extended.sh",
    "scripts/ci/fast-compiler.sh",
    "scripts/ci/main-functional.sh",
    "scripts/ci/release-candidate.sh",
)


def active_text(contents: str) -> str:
    """Return non-comment lines used to enforce executable policy tokens."""
    return "\n".join(
        line for line in contents.splitlines() if not line.lstrip().startswith("#")
    )


def joined_text(contents: str) -> str:
    """Join shell continuation lines so recognizable commands stay checkable."""
    return re.sub(r"\\\s*\n\s*", " ", contents)


def has_command(contents: str, pattern: str) -> bool:
    return re.search(rf"(?m)^\s*{pattern}(?:\s|$)", joined_text(contents)) is not None


def require_command(
    violations: list[str], tier: str, contents: str, token: str, pattern: str
) -> None:
    if not has_command(contents, pattern):
        violations.append(f"{tier} is missing required token: {token}")


def array_body(contents: str, name: str) -> str:
    match = re.search(rf"(?ms)^\s*{re.escape(name)}=\((.*?)\)", contents)
    return match.group(1) if match else ""


def require_tokens(
    violations: list[str], tier: str, contents: str, tokens: tuple[str, ...]
) -> None:
    for token in tokens:
        if token not in contents:
            violations.append(f"{tier} is missing required token: {token}")


def reject_tokens(
    violations: list[str], tier: str, contents: str, tokens: tuple[str, ...]
) -> None:
    executable_text = joined_text(contents)
    for token in tokens:
        if token == "cabal bench":
            found = has_command(executable_text, r"cabal[ \t]+bench")
        else:
            found = token in executable_text
        if found:
            violations.append(f"{tier} contains forbidden token: {token}")


def load_policy_files(root: Path, violations: list[str]) -> dict[str, str]:
    policies: dict[str, str] = {}
    for relative_path in POLICY_PATHS:
        path = root / relative_path
        if not path.is_file():
            violations.append(f"missing required CI policy file: {relative_path}")
            continue
        contents = active_text(path.read_text(encoding="utf-8"))
        policies[relative_path] = contents
        if "set -euo pipefail" not in contents:
            violations.append(f"{relative_path} must use set -euo pipefail")
        if re.search(
            r"(?m)^\s*if\s+(?:false\b|\[\[\s*1\s+-eq\s+0\s*\]\])",
            contents,
        ):
            violations.append(f"{relative_path} contains an obvious dead-code guard")
    return policies


def check_fast(contents: str, violations: list[str]) -> None:
    tier = "fast compiler tier"
    require_command(violations, tier, contents, "cabal build all", r"cabal\s+build\s+all")
    require_command(
        violations,
        tier,
        contents,
        "--test-show-details=direct",
        r'cabal\s+test\s+"\$\{test_components\[@\]\}".*--test-show-details=direct',
    )
    require_command(violations, tier, contents, "cabal check", r"cabal\s+check")
    require_command(
        violations,
        tier,
        contents,
        "scripts/check-examples.sh",
        r"bash\s+scripts/check-examples\.sh",
    )
    require_command(violations, tier, contents, "git diff --check", r"git\s+diff\s+--check")
    components = array_body(contents, "test_components")
    for component in FAST_COMPONENTS:
        if not re.search(rf"(?<![a-z0-9-]){re.escape(component)}(?![a-z0-9-])", components):
            violations.append(f"{tier} is missing required token: {component}")
    reject_tokens(violations, "fast compiler tier", contents, FAST_FORBIDDEN)


def check_main(contents: str, violations: list[str]) -> None:
    tier = "main functional tier"
    commands = (
        ("cabal build all", r"cabal\s+build\s+all"),
        (
            "cabal test all",
            r"cabal\s+test\s+all",
        ),
        ("cabal check", r"cabal\s+check"),
        ("scripts/check-docs.sh", r"bash\s+scripts/check-docs\.sh"),
        (
            "scripts/check-execution-queue.sh",
            r"bash\s+scripts/check-execution-queue\.sh",
        ),
        ("scripts/check-examples.sh", r"bash\s+scripts/check-examples\.sh"),
        ("nix flake check", r"nix\s+flake\s+check"),
        ("git diff --check", r"git\s+diff\s+--check"),
    )
    for token, pattern in commands:
        require_command(violations, tier, contents, token, pattern)
    require_command(
        violations,
        tier,
        contents,
        "--test-show-details=direct",
        r"cabal\s+test\s+all\s+--test-show-details=direct",
    )
    reject_tokens(violations, "main functional tier", contents, MAIN_FORBIDDEN)


def check_determinism(contents: str, violations: list[str]) -> None:
    require_tokens(
        violations,
        "determinism tier",
        contents,
        (
            'JAZZ_ARTIFACT_ROOT="${JAZZ_ARTIFACT_ROOT:-artifacts/determinism}"',
            "examples/functions/factorial.jz",
            "--runtime-stats=json",
            "--runtime-profile=",
        ),
    )
    command_lines = [
        match.group(0)
        for match in re.finditer(r'(?m)^\s*"\$JAZZ_BIN"\s+.*$', joined_text(contents))
    ]
    if sum("--runtime-stats=json" in line for line in command_lines) != 2:
        violations.append("determinism tier must run runtime statistics twice")
    if sum("--runtime-profile=" in line for line in command_lines) != 2:
        violations.append("determinism tier must write two separate Speedscope profiles")

    destinations: list[str] = []
    for line in command_lines:
        destinations.extend(
            re.findall(
                r'(?:--runtime-profile=|2>|>)"\$JAZZ_ARTIFACT_ROOT/([^"]+)"',
                line,
            )
        )
    if len(destinations) != 10 or len(set(destinations)) != 10:
        violations.append("determinism tier must use distinct output destinations")

    comparisons = re.findall(
        r'(?m)^\s*cmp\s+"([^"]+)"\s+"([^"]+)"', joined_text(contents)
    )
    if len(comparisons) != 5:
        violations.append("determinism tier must perform exactly five output comparisons with cmp")
    if any(left == right for left, right in comparisons):
        violations.append("determinism tier must compare distinct output paths")


def check_extended(contents: str, violations: list[str]) -> None:
    tier = "extended tier"
    require_tokens(
        violations,
        tier,
        contents,
        ("JAZZ_BENCHMARK_LABEL", "JAZZ_ARTIFACT_ROOT", "mkdir -p"),
    )
    require_command(
        violations,
        tier,
        contents,
        "mkdir -p",
        r'mkdir\s+-p\s+"\$JAZZ_ARTIFACT_ROOT/corpus"\s+"\$JAZZ_ARTIFACT_ROOT/benchmarks"',
    )

    full_components = array_body(contents, "full_scale_components")
    for component in (
        "jazz-parser-scale-full-expression-spec",
        "jazz-parser-scale-full-declarations-spec",
        "jazz-parser-scale-full-control-flow-spec",
        "jazz-parser-scale-full-operator-spec",
    ):
        if component not in full_components:
            violations.append(f"{tier} is missing required token: {component}")

    require_command(
        violations,
        tier,
        contents,
        "cabal test all",
        r'cabal\s+test\s+all\s+"\$\{full_scale_components\[@\]\}".*-ffull-parser-scale.*--test-log=.*first.*',
    )
    require_command(
        violations,
        tier,
        contents,
        "cabal test program-corpus-spec",
        r"cabal\s+test\s+program-corpus-spec.*--test-log=.*second.*",
    )
    if "-ffull-parser-scale" not in joined_text(contents):
        violations.append(f"{tier} is missing required token: -ffull-parser-scale")

    freshness_tokens = (
        'if [[ -d "$JAZZ_ARTIFACT_ROOT"',
        'find "$JAZZ_ARTIFACT_ROOT" -mindepth 1 -print -quit',
    )
    if not all(token in joined_text(contents) for token in freshness_tokens):
        violations.append("extended tier must reject a nonempty artifact root")

    corpus_one = "corpus/pass-one.txt"
    corpus_two = "corpus/pass-two.txt"
    if corpus_one not in contents or corpus_two not in contents:
        violations.append("extended tier must capture two distinct corpus outputs")
    if not (
        re.search(r"(?m)^\s*first_destination\.write_text\(", contents)
        and re.search(r"(?m)^\s*second_destination\.write_text\(", contents)
    ):
        violations.append("extended tier must normalize both corpus outputs")
    corpus_comparisons = re.findall(
        r'(?m)^\s*cmp\s+"([^"]*corpus/[^"]+)"\s+"([^"]*corpus/[^"]+)"',
        joined_text(contents),
    )
    expected_corpus_pair = (
        "$JAZZ_ARTIFACT_ROOT/corpus/pass-one.txt",
        "$JAZZ_ARTIFACT_ROOT/corpus/pass-two.txt",
    )
    if corpus_comparisons != [expected_corpus_pair]:
        violations.append("extended tier must compare distinct corpus outputs")

    commands = (
        (
            "scripts/ci/determinism.sh",
            r'JAZZ_ARTIFACT_ROOT="\$JAZZ_ARTIFACT_ROOT/determinism"\s+bash\s+scripts/ci/determinism\.sh',
        ),
        (
            "cabal.project.profile-stages",
            r"cabal\s+--project-file=cabal\.project\.profile-stages\s+build\s+all",
        ),
        (
            "cabal.project.profile-hotspots",
            r"cabal\s+--project-file=cabal\.project\.profile-hotspots\s+build\s+all",
        ),
        (
            "cabal bench jazz-bench",
            r"cabal\s+bench\s+jazz-bench.*--environment-label.*--result-root(?:=|\s).*",
        ),
        (
            "benchmark-metadata-spec",
            r"cabal\s+test\s+benchmark-metadata-spec",
        ),
    )
    for token, pattern in commands:
        require_command(violations, tier, contents, token, pattern)
    require_tokens(
        violations,
        tier,
        contents,
        ("--environment-label", "--result-root", "manifest.json", "sha256"),
    )

    benchmark_validation = (
        '"environment.json"',
        '"results.csv"',
        "len(run_directories) != 1",
        'metadata["environment_label"] != expected_label',
        'metadata["schema_version"] != 2',
        "results_path.stat().st_size == 0",
    )
    if not all(token in contents for token in benchmark_validation):
        violations.append("extended tier must validate generated benchmark metadata")

    manifest_structure = (
        'manifest_path="$JAZZ_ARTIFACT_ROOT/manifest.json"',
        'for path in sorted(artifact_root.rglob("*")):',
        '"path": path.relative_to(artifact_root).as_posix()',
        "hashlib.sha256(path.read_bytes()).hexdigest()",
    )
    if not (
        all(token in contents for token in manifest_structure)
        and re.search(r"(?m)^\s*manifest_path\.write_text\(", contents)
    ):
        violations.append("extended tier must generate a SHA-256 artifact manifest")

    timing_threshold = re.compile(
        r"(?:timing|benchmark).*(?:threshold|regression[_-]?percent)"
        r"|(?:threshold|regression[_-]?percent).*(?:timing|benchmark)",
        re.IGNORECASE,
    )
    if timing_threshold.search(contents):
        violations.append("extended tier must not fail on a timing regression threshold")


def check_release(contents: str, violations: list[str]) -> None:
    tier = "release candidate tier"
    require_tokens(violations, tier, contents, ("JAZZ_RELEASE_VERSION",))
    commands = (
        ("scripts/ci/main-functional.sh", r"bash\s+scripts/ci/main-functional\.sh"),
        ("scripts/ci/extended.sh", r"bash\s+scripts/ci/extended\.sh"),
        ("scripts/check-docs.sh", r"bash\s+scripts/check-docs\.sh"),
        ("npm --prefix website ci", r"npm\s+--prefix\s+website\s+ci"),
        (
            "npm --prefix website run build",
            r"npm\s+--prefix\s+website\s+run\s+build",
        ),
        ("scripts/check-website.sh", r"bash\s+scripts/check-website\.sh"),
        ("cabal sdist all", r"cabal\s+sdist\s+all"),
        ("nix build .#jazz", r"nix\s+build\s+\.\#jazz"),
        (
            "validate_artifact_manifest",
            r'validate_artifact_manifest\s+"\$\{?JAZZ_ARTIFACT_ROOT\}?/manifest\.json"',
        ),
    )
    for token, pattern in commands:
        require_command(violations, tier, contents, token, pattern)

    category_tokens = (
        "required_artifacts = {",
        '"corpus/pass-one.txt"',
        '"corpus/pass-two.txt"',
        '"determinism/stats-one.stdout"',
        '"determinism/stats-two.stdout"',
        '"determinism/profile-one.speedscope.json"',
        '"determinism/profile-two.speedscope.json"',
        'path.startswith("benchmarks/")',
        'path.endswith("/results.csv")',
        'path.endswith("/environment.json")',
        "len(results_paths) != 1",
        "len(environment_paths) != 1",
    )
    if not (
        all(token in contents for token in category_tokens)
        and re.search(r"(?m)^\s*required_artifacts\s*=\s*\{", contents)
    ):
        violations.append("release candidate tier must validate every extended artifact category")
    version_pattern = r"^0\.[0-9]+\.[0-9]+-alpha\.[0-9]+$"
    if version_pattern not in contents:
        violations.append(
            "release candidate tier must enforce the 0.<minor>.<patch>-alpha.<n> version shape"
        )
    if "scripts/release/build-alpha.sh" in contents:
        violations.append("release candidate tier must not invoke scripts/release/build-alpha.sh")


def check_pull_request_workflows(root: Path, violations: list[str]) -> None:
    workflow_root = root / ".github/workflows"
    if not workflow_root.is_dir():
        return
    for path in sorted((*workflow_root.glob("*.yml"), *workflow_root.glob("*.yaml"))):
        contents = active_text(path.read_text(encoding="utf-8"))
        if not re.search(r"(?m)^\s*pull_request\s*:", contents):
            continue
        if re.search(r"(?m)^\s*(?:run:\s*)?.*\bcabal\s+(?:build|test|bench|run)\b", contents):
            violations.append(
                f"pull-request workflow inlines compiler or long-running CI work: {path.relative_to(root)}"
            )


def check_repository(root: Path) -> list[str]:
    violations: list[str] = []
    policies = load_policy_files(root, violations)

    checks = (
        ("scripts/ci/fast-compiler.sh", check_fast),
        ("scripts/ci/main-functional.sh", check_main),
        ("scripts/ci/determinism.sh", check_determinism),
        ("scripts/ci/extended.sh", check_extended),
        ("scripts/ci/release-candidate.sh", check_release),
    )
    for relative_path, checker in checks:
        if relative_path in policies:
            checker(policies[relative_path], violations)

    check_pull_request_workflows(root, violations)
    return sorted(set(violations))


def main(arguments: list[str]) -> int:
    if len(arguments) > 1:
        print("usage: check-ci-policy.py [repository-root]", file=sys.stderr)
        return 2

    root = Path(arguments[0]).resolve() if arguments else Path(__file__).resolve().parents[1]
    violations = check_repository(root)
    if violations:
        for violation in violations:
            print(f"FAIL: {violation}")
        return 1

    print("CI policy checks passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
