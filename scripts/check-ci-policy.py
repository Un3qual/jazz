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
    "scripts/ci/extended.sh",
    "scripts/ci/release-candidate.sh",
    "scripts/ci/determinism.sh",
    "scripts/release/build-alpha.sh",
)

POLICY_PATHS = (
    "scripts/ci/determinism.sh",
    "scripts/ci/extended.sh",
    "scripts/ci/fast-compiler.sh",
    "scripts/ci/main-functional.sh",
    "scripts/ci/release-candidate.sh",
    "scripts/release/build-alpha.sh",
)

PR_WORKFLOW_PATH = ".github/workflows/ci-pr.yml"
MAIN_WORKFLOW_PATH = ".github/workflows/ci-main.yml"
EXTENDED_WORKFLOW_PATH = ".github/workflows/ci-extended.yml"
RELEASE_WORKFLOW_PATH = ".github/workflows/release.yml"

PINNED_ACTIONS = (
    ("actions/checkout", "v4", "11d5960a326750d5838078e36cf38b85af677262"),
    ("cachix/install-nix-action", "v31", "630ae543ea3a38a9a4166f03376c02c50f408342"),
    ("actions/cache", "v4", "0057852bfaa89a56745cba8c7296529d2fc39830"),
    ("actions/upload-artifact", "v4", "ea165f8d65b6e75b540449e92b4886f43607fa02"),
    ("dorny/paths-filter", "v3", "0e4a8c6effa4802afeda77dc8d303f8176d7dfad"),
    ("pnpm/action-setup", "v4", "b906affcce14559ad1aafd4ab0e942779e9f58b1"),
    ("actions/setup-node", "v4", "49933ea5288caeca8642d1e84afbd3f7d6820020"),
)
ACTION_USE_RE = re.compile(
    r"(?m)^\s*(?:-\s+)?uses:\s*([^@\s]+)@([^\s#]+)"
)
IMMUTABLE_REVISION_RE = re.compile(r"[0-9a-f]{40}")

PR_FORBIDDEN = (
    "cabal bench",
    "jazz-bench",
    "full-parser-scale",
    "profile-hotspots",
    "profile-stages",
    "program-corpus-spec",
)

DOCS_ONLY_EXCLUSIONS = (
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
)

TIMING_THRESHOLD_PATTERN = re.compile(
    r"(?:timing|benchmark).*(?:threshold|regression[_ -]?percent)"
    r"|(?:threshold|regression[_ -]?percent).*(?:timing|benchmark)",
    re.IGNORECASE,
)


def active_text(contents: str) -> str:
    """Return non-comment lines used to enforce executable policy tokens."""
    return "\n".join(
        line for line in contents.splitlines() if not line.lstrip().startswith("#")
    )


def normalized_action_versions(contents: str) -> str:
    """Expose reviewed action versions to the existing semantic checks."""
    for action, version, revision in PINNED_ACTIONS:
        contents = re.sub(
            rf"{re.escape(action)}@{revision}(?:\s+#\s*{re.escape(version)})?",
            f"{action}@{version}",
            contents,
        )
    return contents


def check_workflow_supply_chain(root: Path, violations: list[str]) -> None:
    workflow_root = root / ".github/workflows"
    if not workflow_root.is_dir():
        return
    for path in sorted((*workflow_root.glob("*.yml"), *workflow_root.glob("*.yaml"))):
        contents = path.read_text(encoding="utf-8")
        label = path.relative_to(root)
        for match in ACTION_USE_RE.finditer(contents):
            action, revision = match.groups()
            if action.startswith("./"):
                continue
            if IMMUTABLE_REVISION_RE.fullmatch(revision) is None:
                violations.append(
                    f"{label}: workflow action must use an immutable commit: {action}@{revision}"
                )

        lines = contents.splitlines()
        for index, line in enumerate(lines):
            if not re.search(r"\buses:\s*actions/checkout@", line):
                continue
            uses_indent = len(line) - len(line.lstrip(" "))
            step_indent = max(0, uses_indent - 2)
            block: list[str] = []
            for candidate in lines[index + 1 :]:
                candidate_indent = len(candidate) - len(candidate.lstrip(" "))
                if candidate.strip() and candidate_indent <= step_indent:
                    break
                block.append(candidate)
            if not re.search(
                r"(?m)^\s*persist-credentials:\s*false\s*$", "\n".join(block)
            ):
                violations.append(
                    f"{label}: checkout must set persist-credentials: false"
                )
            if re.search(r"(?m)^\s*(?:repository|ref):", "\n".join(block)):
                violations.append(
                    f"{label}: checkout must use the triggering repository and revision"
                )


def joined_text(contents: str) -> str:
    """Join shell continuation lines so recognizable commands stay checkable."""
    return re.sub(r"\\\s*\n\s*", " ", contents)


def indented_block(contents: str, header: str, indent: int) -> str:
    """Return one trusted-repository YAML block without parsing general YAML."""
    lines = contents.splitlines()
    header_line = f"{' ' * indent}{header}:"
    for index, line in enumerate(lines):
        if line.rstrip() != header_line:
            continue
        block: list[str] = []
        for candidate in lines[index + 1 :]:
            if not candidate.strip():
                block.append(candidate)
                continue
            candidate_indent = len(candidate) - len(candidate.lstrip(" "))
            if candidate_indent <= indent:
                break
            block.append(candidate)
        return "\n".join(block)
    return ""


def yaml_literal_block(contents: str, header: str, indent: int) -> str:
    """Return one trusted-repository YAML literal block body."""
    lines = contents.splitlines()
    header_line = re.compile(
        rf"^{re.escape(' ' * indent)}{re.escape(header)}:\s*[|>]\s*$"
    )
    for index, line in enumerate(lines):
        if not header_line.match(line):
            continue
        block: list[str] = []
        for candidate in lines[index + 1 :]:
            if not candidate.strip():
                block.append(candidate)
                continue
            candidate_indent = len(candidate) - len(candidate.lstrip(" "))
            if candidate_indent <= indent:
                break
            block.append(candidate)
        return "\n".join(block)
    return ""


def yaml_mapping_keys(contents: str, indent: int) -> list[str]:
    """Return keys declared at one indentation level in trusted workflow YAML."""
    pattern = re.compile(
        rf"^{re.escape(' ' * indent)}(?:'([^']+)'|\"([^\"]+)\"|([A-Za-z_][A-Za-z0-9_-]*))\s*:",
        re.MULTILINE,
    )
    return [next(group for group in match.groups() if group) for match in pattern.finditer(contents)]


def workflow_job(contents: str, job_name: str) -> str:
    jobs = indented_block(contents, "jobs", 0)
    return indented_block(jobs, job_name, 2)


def workflow_step(contents: str, step_name: str) -> str:
    """Return one named workflow step from trusted repository YAML."""
    lines = contents.splitlines()
    target = re.compile(
        rf"^(?P<indent>\s*)-\s+name:\s*['\"]?{re.escape(step_name)}['\"]?\s*$"
    )
    for index, line in enumerate(lines):
        match = target.match(line)
        if not match:
            continue
        indent = len(match.group("indent"))
        block = [line]
        for candidate in lines[index + 1 :]:
            if not candidate.strip():
                block.append(candidate)
                continue
            candidate_indent = len(candidate) - len(candidate.lstrip(" "))
            if candidate_indent < indent or (
                candidate_indent == indent and candidate.lstrip().startswith("- ")
            ):
                break
            block.append(candidate)
        return "\n".join(block)
    return ""


def has_yaml_list_item(contents: str, item: str) -> bool:
    return (
        re.search(
            rf"(?m)^\s*-\s+['\"]?{re.escape(item)}['\"]?\s*$",
            contents,
        )
        is not None
    )


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


def require_nix_features_before(
    contents: str,
    relative_path: str,
    before_token: str,
    violations: list[str],
) -> None:
    feature_token = "extra-experimental-features = nix-command flakes"
    executable = joined_text(contents)
    feature_index = executable.find(feature_token)
    export_index = executable.find("export NIX_CONFIG")
    invocation_index = executable.find(before_token)
    if (
        feature_index < 0
        or export_index < feature_index
        or invocation_index < 0
        or feature_index > invocation_index
        or export_index > invocation_index
    ):
        violations.append(
            f"{relative_path} must enable nix-command and flakes before nested Nix invocations"
        )


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
    if not has_command(
        contents, r"python3\s+scripts/release/test-verify-artifacts\.py"
    ):
        violations.append("fast compiler tier must run release verifier behavior tests")
    require_command(
        violations,
        tier,
        contents,
        "scripts/check-examples.sh",
        r"bash\s+scripts/check-examples\.sh",
    )
    require_command(violations, tier, contents, "git diff --check", r"git\s+diff\s+--check")
    if 'git diff --check "$JAZZ_DIFF_BASE...HEAD"' not in contents:
        violations.append(
            "fast compiler tier must check the committed diff when JAZZ_DIFF_BASE is set"
        )
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
    if not has_command(contents, r"python3\s+scripts/test-check-ci-policy\.py"):
        violations.append("main functional tier must run CI policy behavior tests")
    if not has_command(
        contents, r"python3\s+scripts/release/test-verify-artifacts\.py"
    ):
        violations.append("main functional tier must run release verifier behavior tests")
    if 'git diff --check "$JAZZ_DIFF_BASE...HEAD"' not in contents:
        violations.append(
            "main functional tier must check the committed diff when JAZZ_DIFF_BASE is set"
        )
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
        'metadata.get("environment_label") != expected_label',
        'metadata.get("schema_version") != 2',
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

    if TIMING_THRESHOLD_PATTERN.search(contents):
        violations.append("extended tier must not fail on a timing regression threshold")


def check_release(contents: str, violations: list[str]) -> None:
    tier = "release candidate tier"
    require_tokens(violations, tier, contents, ("JAZZ_RELEASE_VERSION",))
    commands = (
        ("scripts/ci/main-functional.sh", r"bash\s+scripts/ci/main-functional\.sh"),
        ("scripts/ci/extended.sh", r"bash\s+scripts/ci/extended\.sh"),
        ("scripts/check-docs.sh", r"bash\s+scripts/check-docs\.sh"),
        (
            "pnpm --dir website install --frozen-lockfile",
            r"pnpm\s+--dir\s+website\s+install\s+--frozen-lockfile",
        ),
        (
            "pnpm --dir website run build",
            r"pnpm\s+--dir\s+website\s+run\s+build",
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
    fresh_evidence_tokens = (
        'artifacts/release-candidate/$JAZZ_RELEASE_VERSION/extended',
        "os.path.commonpath((evidence_root, release_root))",
        "evidence_root == release_root or common in (evidence_root, release_root)",
    )
    if not all(token in contents for token in fresh_evidence_tokens):
        violations.append(
            "release candidate tier must use a fresh evidence root outside final artifacts"
        )
    require_nix_features_before(
        contents,
        "scripts/ci/release-candidate.sh",
        "bash scripts/ci/main-functional.sh",
        violations,
    )


def check_alpha_build(contents: str, violations: list[str]) -> None:
    tier = "alpha artifact builder"
    if re.search(
        r"(?m)\bbash\s+scripts/ci/release-candidate\.sh(?:\s|$)",
        joined_text(contents),
    ) is None:
        violations.append(
            "alpha artifact builder must invoke the complete release-candidate tier"
        )
    executable = joined_text(contents)
    owns_fresh_evidence = 'JAZZ_ARTIFACT_ROOT="$work_root/extended"' in executable or (
        'evidence_root="$work_root/extended"' in contents
        and 'JAZZ_ARTIFACT_ROOT="$evidence_root"' in executable
    )
    if not owns_fresh_evidence or (
        'JAZZ_RELEASE_OUTPUT_ROOT="$release_directory"' not in executable
    ):
        violations.append(
            "alpha artifact builder must use a fresh evidence root outside the final release directory"
        )
    if not has_command(
        contents,
        r'python3\s+scripts/release/verify-artifacts\.py\s+"\$release_directory"',
    ):
        violations.append("alpha artifact builder must verify the final release directory")
    required_artifact_tokens = (
        "-source.tar.gz",
        "-nix-$system.tar.gz",
        "-docs.tar.gz",
        "-benchmark-evidence.tar.gz",
        "SHA256SUMS",
    )
    for token in required_artifact_tokens:
        if token not in contents:
            violations.append(f"{tier} is missing required token: {token}")
    closure_tokens = (
        'nix-store --query --requisites "$nix_result"',
        "LC_ALL=C sort -u",
        'nix-store --export "${closure_paths[@]}"',
        '"$nix_closure_stage/root-store-path"',
        '"$nix_closure_stage/system"',
    )
    if not all(token in executable for token in closure_tokens):
        violations.append(
            "alpha artifact builder must export a sorted same-system Nix runtime closure"
        )
    if re.search(r"(?m)^\s*JAZZ_ARTIFACT_ROOT=\"\$release_directory", contents):
        violations.append(
            "alpha artifact builder must use a fresh evidence root outside the final release directory"
        )
    if re.search(r"(?m)\|\|\s*(?:true|:)(?:\s*(?:#.*)?)?$", contents):
        violations.append("alpha artifact builder must not mask release-candidate failure")
    require_nix_features_before(
        contents,
        "scripts/release/build-alpha.sh",
        "bash scripts/ci/release-candidate.sh",
        violations,
    )


def check_pr_changes_job(contents: str, violations: list[str]) -> None:
    job = workflow_job(contents, "changes")
    if not job:
        violations.append("pull-request workflow is missing the changes job")
        return
    if not re.search(r"(?m)^\s*(?:-\s+)?uses:\s+dorny/paths-filter@v3\s*$", job):
        violations.append("changes job must use dorny/paths-filter@v3")
    if not re.search(r"(?m)^\s*predicate-quantifier:\s*every\s*$", job):
        violations.append("changes job must apply every docs-only exclusion")
    if not re.search(r"(?m)^\s*id:\s*filter\s*$", job):
        violations.append("changes job must give the paths filter the id filter")
    if not re.search(
        r"(?m)^\s*compiler:\s*\$\{\{\s*steps\.filter\.outputs\.compiler\s*\}\}\s*$",
        job,
    ):
        violations.append("changes job must publish the compiler path-filter output")
    if not has_yaml_list_item(job, "**"):
        violations.append("changes job must default unclassified paths to compiler-relevant")
    for exclusion in DOCS_ONLY_EXCLUSIONS:
        if not has_yaml_list_item(job, exclusion):
            violations.append(f"changes job is missing docs-only exclusion: {exclusion}")


def check_pr_docs_job(contents: str, violations: list[str]) -> None:
    job = workflow_job(contents, "docs-and-site")
    if not job:
        violations.append("pull-request workflow is missing the docs-and-site job")
        return
    if re.search(r"(?m)^    (?:if|needs):", job):
        violations.append("docs-and-site job must run for every pull request")

    required = (
        (
            r"(?m)^\s*(?:-\s+)?uses:\s*cachix/install-nix-action@v31\s*$",
            "docs-and-site job must install the pinned Nix documentation toolchain",
        ),
        (
            r"(?m)^\s*(?:-\s+)?uses:\s*pnpm/action-setup@v4\s*$",
            "docs-and-site job must use pnpm/action-setup@v4",
        ),
        (
            r"(?m)^\s*version:\s*11\.18\.0\s*$",
            "docs-and-site job must use pnpm 11.18.0",
        ),
        (
            r"(?m)^\s*(?:-\s+)?uses:\s*actions/setup-node@v4\s*$",
            "docs-and-site job must use actions/setup-node@v4",
        ),
        (
            r"(?m)^\s*node-version:\s*22\s*$",
            "docs-and-site job must use Node 22",
        ),
        (
            r"(?m)^\s*cache:\s*pnpm\s*$",
            "docs-and-site job must use the pnpm cache",
        ),
        (
            r"(?m)^\s*cache-dependency-path:\s*website/pnpm-lock\.yaml\s*$",
            "docs-and-site job must key the pnpm cache from website/pnpm-lock.yaml",
        ),
        (
            r"(?m)^\s*(?:-\s+)?run:\s*pnpm\s+install\s+--frozen-lockfile\s*\n\s+working-directory:\s*website\s*$",
            "docs-and-site job must install only website dependencies with pnpm --frozen-lockfile",
        ),
        (
            r"(?m)^\s*(?:-\s+)?run:\s*nix\s+develop\s+\.\#docs\s+--command\s+bash\s+scripts/check-docs\.sh\s*$",
            "docs-and-site job must run documentation checks in the pinned docs shell",
        ),
    )
    for pattern, message in required:
        if not re.search(pattern, job):
            violations.append(message)

    pnpm_installs = re.findall(r"(?m)^\s*(?:-\s+)?run:\s*pnpm\s+install\b.*$", job)
    if len(pnpm_installs) != 1 or not re.search(
        r"\bpnpm\s+install\s+--frozen-lockfile\s*$", pnpm_installs[0]
    ):
        violations.append(
            "docs-and-site job must install only website dependencies with pnpm --frozen-lockfile"
        )

    checks = (
        ("scripts/check-website.sh", r"bash\s+scripts/check-website\.sh"),
        ("scripts/check-ci-policy.py", r"python3\s+scripts/check-ci-policy\.py"),
    )
    for token, command in checks:
        if not re.search(rf"(?m)^\s*(?:-\s+)?run:\s*{command}\s*$", job):
            violations.append(f"docs-and-site job is missing required check: {token}")
    if not re.search(
        r"(?m)^\s*(?:-\s+)?run:\s*python3\s+scripts/test-check-ci-policy\.py\s*$",
        job,
    ):
        violations.append("docs-and-site job must run CI policy behavior tests")

    compiler_toolchain = re.compile(r"(?i)\bcabal\b|\bghc(?:up)?\b|setup-haskell")
    nix_commands = re.findall(r"(?m)^\s*(?:-\s+)?run:\s*(nix\b.*)$", job)
    if compiler_toolchain.search(job) or nix_commands != [
        "nix develop .#docs --command bash scripts/check-docs.sh"
    ]:
        violations.append("docs-and-site job must not install or invoke the compiler toolchain")


def check_pr_compiler_job(contents: str, violations: list[str]) -> None:
    job = workflow_job(contents, "compiler-fast")
    if not job:
        violations.append("pull-request workflow is missing the compiler-fast job")
        return
    requirements = (
        (
            r"(?m)^\s*needs:\s*changes\s*$",
            "compiler-fast job must depend on changes",
        ),
        (
            r"(?m)^\s*if:\s*needs\.changes\.outputs\.compiler\s*==\s*'true'\s*$",
            "compiler-fast job must run only for compiler-relevant changes",
        ),
        (
            r"(?m)^\s*timeout-minutes:\s*30\s*$",
            "compiler-fast job must have a 30-minute timeout",
        ),
        (
            r"(?m)^\s*(?:-\s+)?uses:\s*cachix/install-nix-action@v31\s*$",
            "compiler-fast job must use cachix/install-nix-action@v31",
        ),
        (
            r"(?m)^\s*(?:-\s+)?uses:\s*actions/cache@v4\s*$",
            "compiler-fast job must use actions/cache@v4",
        ),
        (
            r"(?m)^\s*~/.cabal/store\s*$",
            "compiler-fast cache must include ~/.cabal/store",
        ),
        (
            r"(?m)^\s*dist-newstyle\s*$",
            "compiler-fast cache must include dist-newstyle",
        ),
        (
            r"(?m)^\s*key:\s*.*\$\{\{\s*runner\.os\s*\}\}.*$",
            "compiler-fast cache key must include runner.os",
        ),
        (
            r"hashFiles\(\s*'flake\.lock'\s*,\s*'jazz\.cabal'\s*,\s*'cabal\.project'\s*\)",
            "compiler-fast cache key must include flake.lock, jazz.cabal, and cabal.project",
        ),
        (
            r"(?m)^\s*(?:-\s+)?run:\s*nix\s+develop\s+--command\s+bash\s+scripts/ci/fast-compiler\.sh\s*$",
            "compiler-fast job must invoke the fast compiler script",
        ),
    )
    for pattern, message in requirements:
        if not re.search(pattern, job):
            violations.append(message)


def check_pr_gate_job(contents: str, violations: list[str]) -> None:
    job = workflow_job(contents, "pr-gate")
    if not job:
        violations.append("pull-request workflow is missing the pr-gate job")
        return
    requirements = (
        (
            r"(?m)^\s*name:\s*Pull request gate\s*$",
            "pr-gate job must expose the stable name: Pull request gate",
        ),
        (
            r"(?m)^\s*if:\s*always\(\)\s*$",
            "pr-gate job must run with if: always()",
        ),
    )
    for pattern, message in requirements:
        if not re.search(pattern, job):
            violations.append(message)
    for dependency in ("changes", "docs-and-site", "compiler-fast"):
        if not has_yaml_list_item(job, dependency):
            violations.append(f"pr-gate job must depend on {dependency}")

    bindings = (
        "CHANGES_RESULT: ${{ needs.changes.result }}",
        "DOCS_RESULT: ${{ needs.docs-and-site.result }}",
        "COMPILER_REQUIRED: ${{ needs.changes.outputs.compiler }}",
        "COMPILER_RESULT: ${{ needs.compiler-fast.result }}",
    )
    if not all(binding in job for binding in bindings):
        violations.append("pr-gate job must bind every dependency result")

    gate_assertions = (
        (
            '[[ "$CHANGES_RESULT" == "success" ]]',
            "pr-gate job must reject a failed changes dependency",
        ),
        (
            '[[ "$DOCS_RESULT" == "success" ]]',
            "pr-gate job must reject a failed docs-and-site dependency",
        ),
        (
            '[[ "$COMPILER_RESULT" == "success" ]]',
            "pr-gate job must require compiler-fast success for compiler changes",
        ),
        (
            '[[ "$COMPILER_REQUIRED" == "false" ]]',
            "pr-gate job must prove a compiler skip was documentation-only",
        ),
        (
            '[[ "$COMPILER_RESULT" == "skipped" ]]',
            "pr-gate job must require a legitimate compiler-fast skip for docs-only changes",
        ),
    )
    for assertion, message in gate_assertions:
        if assertion not in job:
            violations.append(message)

    failure_masking = (
        r"(?m)^\s*continue-on-error:\s*true\s*$",
        r"(?m)^\s*set\s+\+e(?:\s|;|$)",
        r"(?m)^\s*set\s+\+o\s+errexit(?:\s|;|$)",
        r"(?m)\|\|\s*(?:true|:)(?:\s*(?:#.*)?)?$",
    )
    if any(re.search(pattern, job) for pattern in failure_masking):
        violations.append(
            "pr-gate job must not disable fail-fast or mask assertion failures"
        )

    tied_results = re.compile(
        r'if\s+\[\[\s+"\$COMPILER_REQUIRED"\s+==\s+"true"\s+\]\];\s+then\s*'
        r'\[\[\s+"\$COMPILER_RESULT"\s+==\s+"success"\s+\]\]\s*'
        r'else\s*'
        r'\[\[\s+"\$COMPILER_REQUIRED"\s+==\s+"false"\s+\]\]\s*'
        r'\[\[\s+"\$COMPILER_RESULT"\s+==\s+"skipped"\s+\]\]\s*'
        r'fi',
        re.MULTILINE,
    )
    if not tied_results.search(job):
        violations.append("pr-gate job must tie compiler result to path classification")


def check_pr_workflow(root: Path, violations: list[str]) -> None:
    path = root / PR_WORKFLOW_PATH
    if not path.is_file():
        violations.append(f"missing required pull-request workflow: {PR_WORKFLOW_PATH}")
        return
    contents = normalized_action_versions(active_text(path.read_text(encoding="utf-8")))
    trigger_block = indented_block(contents, "on", 0)
    if "pull_request" not in yaml_mapping_keys(trigger_block, 2):
        violations.append("pull-request workflow must trigger on pull_request")

    permissions = [line.strip() for line in indented_block(contents, "permissions", 0).splitlines() if line.strip()]
    if permissions != ["contents: read", "pull-requests: read"]:
        violations.append(
            "pull-request workflow must grant only read access to contents and pull requests"
        )
    if "permissions" in yaml_mapping_keys(contents, 4):
        violations.append("pull-request workflow must not override permissions in a job")

    concurrency = indented_block(contents, "concurrency", 0)
    if "${{ github.workflow }}" not in concurrency or "${{ github.event.pull_request.number }}" not in concurrency:
        violations.append(
            "pull-request workflow concurrency must include workflow and pull-request number"
        )
    if not re.search(r"(?m)^\s*cancel-in-progress:\s*true\s*$", concurrency):
        violations.append("pull-request workflow must cancel superseded runs")

    for token in PR_FORBIDDEN:
        if token in joined_text(contents):
            violations.append(f"pull-request workflow contains forbidden extended token: {token}")

    check_pr_changes_job(contents, violations)
    check_pr_docs_job(contents, violations)
    check_pr_compiler_job(contents, violations)
    check_pr_gate_job(contents, violations)


def check_main_workflow(root: Path, violations: list[str]) -> None:
    path = root / MAIN_WORKFLOW_PATH
    if not path.is_file():
        violations.append(f"missing required main workflow: {MAIN_WORKFLOW_PATH}")
        return

    contents = normalized_action_versions(active_text(path.read_text(encoding="utf-8")))
    trigger_block = indented_block(contents, "on", 0)
    events = yaml_mapping_keys(trigger_block, 2)
    if "workflow_dispatch" not in events:
        violations.append("main workflow must support workflow_dispatch")
    if any(event not in {"push", "workflow_dispatch"} for event in events):
        violations.append(
            "main workflow must trigger only on main pushes and manual dispatch"
        )

    push = indented_block(trigger_block, "push", 2)
    branches = indented_block(push, "branches", 4)
    branch_names = re.findall(
        r"(?m)^\s*-\s+['\"]?([^'\"\s]+)['\"]?\s*$",
        branches,
    )
    if branch_names != ["main"]:
        violations.append("main workflow push trigger must be restricted to main")

    permissions = [
        line.strip()
        for line in indented_block(contents, "permissions", 0).splitlines()
        if line.strip()
    ]
    if permissions != ["contents: read"] or "permissions" in yaml_mapping_keys(
        contents, 4
    ):
        violations.append("main workflow must grant only read access to contents")

    concurrency = indented_block(contents, "concurrency", 0)
    if "${{ github.workflow }}" not in concurrency or "${{ github.ref }}" not in concurrency:
        violations.append(
            "main workflow concurrency must include workflow and branch ref"
        )
    if not re.search(r"(?m)^\s*cancel-in-progress:\s*true\s*$", concurrency):
        violations.append("main workflow must cancel superseded branch runs")

    job = workflow_job(contents, "ordinary")
    if not job:
        violations.append("main workflow is missing the ordinary job")
        return

    requirements = (
        (
            r"(?m)^\s*timeout-minutes:\s*60\s*$",
            "main ordinary job must have a 60-minute timeout",
        ),
        (
            r"(?m)^\s*(?:-\s+)?uses:\s*cachix/install-nix-action@v31\s*$",
            "main ordinary job must use cachix/install-nix-action@v31",
        ),
        (
            r"(?m)^\s*(?:-\s+)?uses:\s*actions/cache@v4\s*$",
            "main ordinary job must use actions/cache@v4",
        ),
        (
            r"(?m)^\s*~/.cabal/store\s*$",
            "main ordinary cache must include ~/.cabal/store",
        ),
        (
            r"(?m)^\s*dist-newstyle\s*$",
            "main ordinary cache must include dist-newstyle",
        ),
        (
            r"(?m)^\s*key:\s*.*\$\{\{\s*runner\.os\s*\}\}.*$",
            "main ordinary cache key must include runner.os",
        ),
        (
            r"hashFiles\(\s*'flake\.lock'\s*,\s*'jazz\.cabal'\s*,\s*'cabal\.project'\s*\)",
            "main ordinary cache key must include flake.lock, jazz.cabal, and cabal.project",
        ),
        (
            r"(?m)^\s*restore-keys:\s*\|\s*\n\s*\$\{\{\s*runner\.os\s*\}\}-cabal-\s*$",
            "main ordinary cache must restore only the operating-system Cabal prefix",
        ),
        (
            r"(?m)^\s*(?:-\s+)?run:\s*nix\s+develop\s+--command\s+bash\s+scripts/ci/main-functional\.sh\s*$",
            "main ordinary job must invoke the complete ordinary script",
        ),
    )
    for pattern, message in requirements:
        if not re.search(pattern, job):
            violations.append(message)

    ordinary_step = workflow_step(job, "Run complete ordinary verification")
    if not re.search(r"(?m)^\s*id:\s*ordinary\s*$", ordinary_step):
        violations.append("main ordinary step must expose the ordinary id")
    if re.search(r"(?m)^\s*if\s*:", ordinary_step):
        violations.append("main ordinary step must retain implicit success gating")

    collect_step = workflow_step(job, "Collect ordinary test logs")
    ordinary_failure = (
        r"(?m)^\s*if:\s*failure\(\)\s*&&\s*"
        r"steps\.ordinary\.outcome\s*==\s*'failure'\s*$"
    )
    if not collect_step or not re.search(ordinary_failure, collect_step):
        violations.append(
            "main workflow must collect ordinary test logs only for ordinary failure"
        )
    collection_command = (
        "find dist-newstyle -type f -name '*.log' -exec cp --parents {} "
        "artifacts/ordinary-test-logs \\;"
    )
    if collection_command not in collect_step:
        violations.append(
            "main workflow must stage only ordinary logs with their source paths"
        )

    upload_step = workflow_step(job, "Upload ordinary test logs")
    if not upload_step or not re.search(ordinary_failure, upload_step):
        violations.append(
            "main workflow must upload ordinary test logs only for ordinary failure"
        )
    if not re.search(
        r"(?m)^\s*uses:\s*actions/upload-artifact@v4\s*$", upload_step
    ):
        violations.append(
            "main workflow must use actions/upload-artifact@v4 for ordinary logs"
        )
    if not re.search(
        r"(?m)^\s*path:\s*artifacts/ordinary-test-logs\s*$", upload_step
    ):
        violations.append("main workflow must upload only staged ordinary test logs")
    if not re.search(r"(?m)^\s*retention-days:\s*7\s*$", upload_step):
        violations.append("main workflow ordinary logs must have seven-day retention")
    for forbidden_path in ("dist-newstyle", "~/.cabal/store"):
        if re.search(
            rf"(?m)^\s*path:\s*{re.escape(forbidden_path)}/?\s*$",
            job,
        ):
            violations.append(
                f"main workflow must not upload build or dependency cache: {forbidden_path}"
            )

    collect_marker = "- name: Collect ordinary test logs"
    upload_marker = "- name: Upload ordinary test logs"
    if job.find(collect_marker) > job.find(upload_marker):
        violations.append("main workflow must upload ordinary logs after collection")

    reject_tokens(violations, "main workflow", contents, MAIN_FORBIDDEN)


def check_extended_workflow(root: Path, violations: list[str]) -> None:
    path = root / EXTENDED_WORKFLOW_PATH
    if not path.is_file():
        violations.append(
            f"missing required extended workflow: {EXTENDED_WORKFLOW_PATH}"
        )
        return

    contents = normalized_action_versions(active_text(path.read_text(encoding="utf-8")))
    trigger_block = indented_block(contents, "on", 0)
    events = yaml_mapping_keys(trigger_block, 2)
    if "workflow_dispatch" not in events:
        violations.append("extended workflow must support workflow_dispatch")
    if len(events) != 2 or set(events) != {"schedule", "workflow_dispatch"}:
        violations.append(
            "extended workflow must trigger only on its weekly schedule and manual dispatch"
        )

    schedule = indented_block(trigger_block, "schedule", 2)
    crons = re.findall(
        r"(?m)^\s*-\s+cron:\s*['\"]?([^'\"]+?)['\"]?\s*$", schedule
    )
    if crons != ["17 7 * * 0"]:
        violations.append("extended workflow must run at 17 7 * * 0")

    permissions = [
        line.strip()
        for line in indented_block(contents, "permissions", 0).splitlines()
        if line.strip()
    ]
    if permissions != ["contents: read"] or "permissions" in yaml_mapping_keys(
        contents, 4
    ):
        violations.append("extended workflow must grant only read access to contents")

    concurrency = indented_block(contents, "concurrency", 0)
    if not re.search(r"(?m)^\s*group:\s*extended\s*$", concurrency):
        violations.append("extended workflow concurrency group must be extended")
    if not re.search(r"(?m)^\s*cancel-in-progress:\s*false\s*$", concurrency):
        violations.append(
            "extended workflow must not cancel an in-progress evidence run"
        )

    job = workflow_job(contents, "extended")
    if not job:
        violations.append("extended workflow is missing the extended job")
        return

    requirements = (
        (
            r"(?m)^\s*timeout-minutes:\s*360\s*$",
            "extended job must have a 360-minute timeout",
        ),
    )
    for pattern, message in requirements:
        if not re.search(pattern, job):
            violations.append(message)

    job_environment = indented_block(job, "env", 4)
    owned_environment = (
        (
            r"(?m)^\s*JAZZ_ARTIFACT_ROOT:\s*artifacts/extended\s*$",
            "extended job must own JAZZ_ARTIFACT_ROOT=artifacts/extended",
        ),
        (
            r"(?m)^\s*JAZZ_BENCHMARK_LABEL:\s*github-actions-extended\s*$",
            "extended job must own JAZZ_BENCHMARK_LABEL=github-actions-extended",
        ),
    )
    for pattern, message in owned_environment:
        if not re.search(pattern, job_environment):
            violations.append(message)

    cache_step = workflow_step(job, "Cache Cabal dependencies and build output")
    cache_requirements = (
        (
            r"(?m)^\s*uses:\s*actions/cache@v4\s*$",
            "extended job must use actions/cache@v4",
        ),
        (
            r"(?m)^\s*key:\s*.*\$\{\{\s*runner\.os\s*\}\}.*$",
            "extended cache key must include runner.os",
        ),
        (
            r"hashFiles\(\s*'flake\.lock'\s*,\s*'jazz\.cabal'\s*,\s*'cabal\.project'\s*\)",
            "extended cache key must include flake.lock, jazz.cabal, and cabal.project",
        ),
        (
            r"(?m)^\s*restore-keys:\s*\|\s*\n\s*\$\{\{\s*runner\.os\s*\}\}-cabal-\s*$",
            "extended cache must restore only the operating-system Cabal prefix",
        ),
    )
    for pattern, message in cache_requirements:
        if not re.search(pattern, cache_step):
            violations.append(message)
    cache_with = indented_block(cache_step, "with", 8)
    cache_paths = [
        line.strip()
        for line in yaml_literal_block(cache_with, "path", 10).splitlines()
        if line.strip()
    ]
    if "~/.cabal/store" not in cache_paths:
        violations.append("extended cache must include ~/.cabal/store")
    if "dist-newstyle" not in cache_paths:
        violations.append("extended cache must include dist-newstyle")
    if cache_paths and cache_paths != ["~/.cabal/store", "dist-newstyle"]:
        violations.append(
            "extended cache must contain only the Cabal store and ordinary build output"
        )
    if not re.search(
        r"(?m)^\s*(?:-\s+)?uses:\s*cachix/install-nix-action@v31\s*$", job
    ):
        violations.append("extended job must use cachix/install-nix-action@v31")

    run_step = workflow_step(job, "Run extended verification")
    if not re.search(r"(?m)^\s*id:\s*extended\s*$", run_step):
        violations.append("extended verification step must expose the extended id")
    if not re.search(
        r"(?m)^\s*run:\s*nix\s+develop\s+--command\s+bash\s+scripts/ci/extended\.sh\s*$",
        run_step,
    ):
        violations.append("extended job must invoke the owned extended script")
    if re.search(r"(?m)^\s*if\s*:", run_step):
        violations.append("extended verification step must retain implicit success gating")
    run_step_keys = yaml_mapping_keys(run_step, 8)
    if "shell" in run_step_keys:
        violations.append(
            "extended workflow must not mask verification or evidence failures"
        )
    run_step_environment_keys = set(yaml_mapping_keys(run_step, 10))
    if run_step_environment_keys.intersection(
        {"JAZZ_ARTIFACT_ROOT", "JAZZ_BENCHMARK_LABEL"}
    ):
        violations.append(
            "extended verification step must not override the owned environment"
        )

    upload_step = workflow_step(job, "Upload extended verification evidence")
    upload_requirements = (
        (
            r"(?m)^\s*if:\s*always\(\)\s*$",
            "extended evidence upload must run on success or failure",
        ),
        (
            r"(?m)^\s*uses:\s*actions/upload-artifact@v4\s*$",
            "extended evidence upload must use actions/upload-artifact@v4",
        ),
        (
            r"(?m)^\s*name:\s*extended-\$\{\{\s*github\.sha\s*\}\}-\$\{\{\s*github\.run_id\s*\}\}-\$\{\{\s*github\.run_attempt\s*\}\}\s*$",
            "extended evidence artifact name must include commit and run provenance",
        ),
        (
            r"(?m)^\s*path:\s*artifacts/extended/\s*$",
            "extended evidence upload must include the owned artifact root",
        ),
        (
            r"(?m)^\s*if-no-files-found:\s*warn\s*$",
            "extended evidence upload must tolerate a failed run without evidence",
        ),
        (
            r"(?m)^\s*retention-days:\s*30\s*$",
            "extended evidence upload must retain evidence for 30 days",
        ),
    )
    for pattern, message in upload_requirements:
        if not re.search(pattern, upload_step):
            violations.append(message)

    summary_step = workflow_step(job, "Summarize extended verification")
    if not re.search(r"(?m)^\s*if:\s*always\(\)\s*$", summary_step):
        violations.append("extended summary must run on success or failure")
    if not re.search(
        r"(?m)^\s*EXTENDED_OUTCOME:\s*\$\{\{\s*steps\.extended\.outcome\s*\}\}\s*$",
        summary_step,
    ):
        violations.append("extended summary must bind the verification completion state")
    if '>> "$GITHUB_STEP_SUMMARY"' not in summary_step:
        violations.append("extended summary must write to GITHUB_STEP_SUMMARY")
    evidence_paths = (
        "artifacts/extended/benchmarks/**/results.csv",
        "artifacts/extended/benchmarks/**/environment.json",
        "artifacts/extended/determinism/profile-one.speedscope.json",
        "artifacts/extended/determinism/profile-two.speedscope.json",
        "artifacts/extended/corpus/pass-one.txt",
        "artifacts/extended/corpus/pass-two.txt",
        "artifacts/extended/manifest.json",
    )
    for evidence_path in evidence_paths:
        if evidence_path not in summary_step:
            violations.append(
                f"extended summary is missing evidence path: {evidence_path}"
            )

    failure_masking = (
        r"(?m)^\s*continue-on-error\s*:",
        r"(?m)^\s*set\s+\+e(?:\s|;|$)",
        r"(?m)^\s*set\s+\+o\s+errexit(?:\s|;|$)",
        r"(?m)\|\|\s*(?:true|:)(?:\s*(?:#.*)?)?$",
    )
    if any(re.search(pattern, job) for pattern in failure_masking):
        violations.append(
            "extended workflow must not mask verification or evidence failures"
        )

    if TIMING_THRESHOLD_PATTERN.search(contents):
        violations.append(
            "extended workflow must not classify timing changes with a threshold"
        )


def check_release_workflow(root: Path, violations: list[str]) -> None:
    path = root / RELEASE_WORKFLOW_PATH
    if not path.is_file():
        violations.append(f"missing required release workflow: {RELEASE_WORKFLOW_PATH}")
        return

    contents = normalized_action_versions(active_text(path.read_text(encoding="utf-8")))
    trigger_block = indented_block(contents, "on", 0)
    events = yaml_mapping_keys(trigger_block, 2)
    if set(events) != {"workflow_dispatch", "push"} or len(events) != 2:
        violations.append("release workflow must trigger only on alpha tags and manual dispatch")
    dispatch = indented_block(trigger_block, "workflow_dispatch", 2)
    version_input = indented_block(indented_block(dispatch, "inputs", 4), "version", 6)
    if not (
        re.search(r"(?m)^\s*required:\s*true\s*$", version_input)
        and re.search(r"(?m)^\s*type:\s*string\s*$", version_input)
    ):
        violations.append(
            "release workflow must support workflow_dispatch with a required version input"
        )
    push = indented_block(trigger_block, "push", 2)
    tags = indented_block(push, "tags", 4)
    tag_patterns = re.findall(
        r"(?m)^\s*-\s+['\"]?([^'\"\s]+)['\"]?\s*$",
        tags,
    )
    if tag_patterns != ["v*"]:
        violations.append("release workflow tag trigger must be restricted to v*")

    permissions = [
        line.strip()
        for line in indented_block(contents, "permissions", 0).splitlines()
        if line.strip()
    ]
    publication_tokens = (
        "contents: write",
        "packages: write",
        "gh release",
        "action-gh-release",
        "actions/create-release",
        "cabal upload",
        "npm publish",
    )
    if (
        permissions != ["contents: read"]
        or "permissions" in yaml_mapping_keys(contents, 4)
        or any(token in joined_text(contents) for token in publication_tokens)
    ):
        violations.append("release workflow must be read-only and must not publish")

    concurrency = indented_block(contents, "concurrency", 0)
    if "${{ inputs.version || github.ref_name }}" not in concurrency:
        violations.append(
            "release workflow concurrency must include the requested version"
        )

    job = workflow_job(contents, "release")
    if not job:
        violations.append("release workflow is missing the release job")
        return
    requirements = (
        (
            r"(?m)^\s*timeout-minutes:\s*480\s*$",
            "release job must have a 480-minute timeout",
        ),
        (
            r"(?m)^\s*(?:-\s+)?uses:\s*cachix/install-nix-action@v31\s*$",
            "release job must install Nix",
        ),
        (
            r"(?m)^\s*(?:-\s+)?uses:\s*pnpm/action-setup@v4\s*$",
            "release job must use pnpm/action-setup@v4",
        ),
        (
            r"(?m)^\s*version:\s*11\.18\.0\s*$",
            "release job must use pnpm 11.18.0",
        ),
        (
            r"(?m)^\s*(?:-\s+)?uses:\s*actions/setup-node@v4\s*$",
            "release job must set up Node.js",
        ),
        (
            r"(?m)^\s*node-version:\s*22\s*$",
            "release job must use Node 22",
        ),
        (
            r"(?m)^\s*cache:\s*pnpm\s*$",
            "release job must use the pnpm cache",
        ),
        (
            r"(?m)^\s*cache-dependency-path:\s*website/pnpm-lock\.yaml\s*$",
            "release job must key the pnpm cache from website/pnpm-lock.yaml",
        ),
    )
    for pattern, message in requirements:
        if not re.search(pattern, job):
            violations.append(message)
    job_environment = indented_block(job, "env", 4)
    nix_config = yaml_literal_block(job_environment, "NIX_CONFIG", 6)
    if "extra-experimental-features = nix-command flakes" not in nix_config:
        violations.append(
            "release job must propagate nix-command and flakes to every nested Nix invocation"
        )

    resolve_step = workflow_step(job, "Resolve alpha version")
    resolve_tokens = (
        "${{ inputs.version }}",
        "${{ github.ref_name }}",
        "${TAG_NAME#v}",
        r"^0\.[0-9]+\.[0-9]+-alpha\.[0-9]+$",
        'JAZZ_RELEASE_VERSION=$version',
        '>> "$GITHUB_ENV"',
    )
    if not resolve_step or not all(token in resolve_step for token in resolve_tokens):
        violations.append("release workflow must derive and validate the alpha version from input or tag")

    run_step = workflow_step(job, "Build and verify alpha artifacts")
    if not re.search(
        r"(?m)^\s*run:\s*nix\s+develop\s+--command\s+bash\s+scripts/release/build-alpha\.sh\s*$",
        run_step,
    ):
        violations.append("release job must invoke scripts/release/build-alpha.sh")
    if not re.search(r"(?m)^\s*id:\s*release\s*$", run_step):
        violations.append("release job must expose the release step outcome")

    upload_step = workflow_step(job, "Upload verified alpha artifacts")
    upload_requirements = (
        (
            r"(?m)^\s*uses:\s*actions/upload-artifact@v4\s*$",
            "release workflow must upload verified artifacts with actions/upload-artifact@v4",
        ),
        (
            r"(?m)^\s*path:\s*artifacts/release/\$\{\{\s*env\.JAZZ_RELEASE_VERSION\s*\}\}/\s*$",
            "release artifact upload must use the verified version directory",
        ),
        (
            r"(?m)^\s*if-no-files-found:\s*error\s*$",
            "release artifact upload must fail when files are missing",
        ),
        (
            r"(?m)^\s*retention-days:\s*30\s*$",
            "release artifact upload must retain artifacts for 30 days",
        ),
    )
    for pattern, message in upload_requirements:
        if not re.search(pattern, upload_step):
            violations.append(message)
    if not all(
        token in upload_step
        for token in ("${{ env.JAZZ_RELEASE_VERSION }}", "${{ github.sha }}", "${{ github.run_id }}")
    ):
        violations.append("release artifact name must include version and run provenance")
    if job.find("- name: Build and verify alpha artifacts") > job.find(
        "- name: Upload verified alpha artifacts"
    ):
        violations.append("release artifact upload must follow successful verification")

    failure_masking = (
        r"(?m)^\s*continue-on-error\s*:",
        r"(?m)^\s*if:\s*always\(\)\s*$",
        r"(?m)\|\|\s*(?:true|:)(?:\s*(?:#.*)?)?$",
        r"(?m)^\s*set\s+\+e(?:\s|;|$)",
    )
    if any(re.search(pattern, job) for pattern in failure_masking):
        violations.append("release workflow must not mask or skip release verification")


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


def check_generated_release_ignores(root: Path, violations: list[str]) -> None:
    path = root / ".gitignore"
    lines = (
        {line.strip() for line in path.read_text(encoding="utf-8").splitlines()}
        if path.is_file()
        else set()
    )
    for required in ("/artifacts/", "/result"):
        if required not in lines:
            violations.append(
                f"generated release output must be ignored at the repository root: {required}"
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
        ("scripts/release/build-alpha.sh", check_alpha_build),
    )
    for relative_path, checker in checks:
        if relative_path in policies:
            checker(policies[relative_path], violations)

    check_pr_workflow(root, violations)
    check_main_workflow(root, violations)
    check_extended_workflow(root, violations)
    check_release_workflow(root, violations)
    check_workflow_supply_chain(root, violations)
    check_pull_request_workflows(root, violations)
    check_generated_release_ignores(root, violations)
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
