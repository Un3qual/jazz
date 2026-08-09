#!/usr/bin/env python3
"""Validate the intentionally small Jazz documentation Pages workflow."""

from __future__ import annotations

import re
import sys
from pathlib import Path


WORKFLOW = Path(".github/workflows/docs-pages.yml")
EXPECTED_PATHS = (
    "docs/**",
    "website/**",
    "examples/functions/factorial.jz",
    "scripts/example-cases.tsv",
    "scripts/check-website.sh",
    "scripts/check-website-boundary.py",
    "scripts/check-public-docs.py",
    "scripts/example_cases.py",
    "scripts/markdown_targets.py",
    "scripts/markdown_visibility.py",
    "scripts/public-doc-fragments.tsv",
    "README.md",
    ".github/workflows/docs-pages.yml",
)
EXPECTED_JOB_PERMISSIONS = {
    "build": {"contents": "read"},
    "deploy": {"pages": "write", "id-token": "write"},
}
CHECKOUT_ACTION = "actions/checkout@11d5960a326750d5838078e36cf38b85af677262"
SETUP_NODE_ACTION = "actions/setup-node@49933ea5288caeca8642d1e84afbd3f7d6820020"
CONFIGURE_PAGES_ACTION = (
    "actions/configure-pages@983d7736d9b0ae728b81ab479565c72886d7745b"
)
UPLOAD_PAGES_ACTION = (
    "actions/upload-pages-artifact@56afc609e74202658d3ffba0e8f6dda462b719fa"
)
DEPLOY_PAGES_ACTION = (
    "actions/deploy-pages@d6db90164ac5ed86f2b6aed7e0febac5b3c0c03e"
)
REQUIRED_ACTIONS = {
    "build": (
        CHECKOUT_ACTION,
        SETUP_NODE_ACTION,
        CONFIGURE_PAGES_ACTION,
        UPLOAD_PAGES_ACTION,
    ),
    "deploy": (DEPLOY_PAGES_ACTION,),
}
REQUIRED_COMMANDS = (
    "npm ci",
    "npm run test:brand",
    "npm run test:experience",
    "npm run typecheck",
    "python3 scripts/check-public-docs.py",
    "npm run build",
)
BOUNDARY_COMMAND = "python3 scripts/check-website-boundary.py"
BANNED_WORK_RE = re.compile(
    r"\b(?:cabal|ghc|compiler|benchmark|performance|profil(?:e|ing)?|"
    r"full-parser-scale|parser-scale)\b",
    re.IGNORECASE,
)


def scalar(value: str) -> str:
    value = value.strip()
    if len(value) >= 2 and value[0] == value[-1] and value[0] in "\"'":
        return value[1:-1]
    return re.split(r"\s+#", value, maxsplit=1)[0].rstrip()


def lines_at_indent(block: str, indent: int) -> list[str]:
    prefix = " " * indent
    return [line[len(prefix) :] for line in block.splitlines() if line.startswith(prefix) and not line.startswith(prefix + " ")]


def section(text: str, key: str, indent: int = 0) -> str | None:
    lines = text.splitlines()
    prefix = " " * indent
    start = next(
        (
            index
            for index, line in enumerate(lines)
            if line == f"{prefix}{key}:"
        ),
        None,
    )
    if start is None:
        return None
    end = len(lines)
    for index in range(start + 1, len(lines)):
        line = lines[index]
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        current_indent = len(line) - len(line.lstrip(" "))
        if current_indent <= indent:
            end = index
            break
    return "\n".join(lines[start + 1 : end])


def child_keys(block: str, indent: int) -> list[str]:
    keys: list[str] = []
    for line in lines_at_indent(block, indent):
        match = re.fullmatch(r"([A-Za-z0-9_-]+):(?:\s+.*)?", line)
        if match:
            keys.append(match.group(1))
    return keys


def list_values(block: str, indent: int) -> list[str]:
    values: list[str] = []
    prefix = " " * indent + "- "
    for line in block.splitlines():
        if line.startswith(prefix):
            values.append(scalar(line[len(prefix) :]))
    return values


def mapping_values(block: str, indent: int) -> dict[str, str]:
    values: dict[str, str] = {}
    for line in lines_at_indent(block, indent):
        match = re.fullmatch(r"([A-Za-z0-9_-]+):\s+(.+)", line)
        if match:
            values[match.group(1)] = scalar(match.group(2))
    return values


def step_blocks(block: str) -> list[str]:
    lines = block.splitlines()
    starts = [
        index
        for index, line in enumerate(lines)
        if line.startswith("      - ")
    ]
    return [
        "\n".join(lines[start : starts[position + 1] if position + 1 < len(starts) else len(lines)])
        for position, start in enumerate(starts)
    ]


def step_values(step: str, key: str) -> list[str]:
    pattern = re.compile(rf"^ {{8}}{re.escape(key)}:\s+(.+)$")
    return [
        scalar(match.group(1))
        for line in step.splitlines()
        if (match := pattern.fullmatch(line)) is not None
    ]


def step_for(block: str, key: str, value: str) -> str | None:
    for candidate in step_blocks(block):
        if value in step_values(candidate, key):
            return candidate
    return None


def step_positions(steps: list[str], key: str, value: str) -> list[int]:
    return [
        index
        for index, step in enumerate(steps)
        if value in step_values(step, key)
    ]


def validate_triggers(text: str, violations: list[str]) -> None:
    on_block = section(text, "on")
    if on_block is None:
        violations.append("workflow trigger block is required")
        return

    triggers = child_keys(on_block, 2)
    if "workflow_dispatch" not in triggers:
        violations.append("workflow_dispatch trigger is required")
    if "pull_request" in triggers or re.search(
        r"(?m)^\s*pull_request(?:_target)?:", on_block
    ):
        violations.append("pull_request triggers are forbidden")
    if set(triggers) != {"push", "workflow_dispatch"}:
        violations.append("workflow triggers must be exactly push and workflow_dispatch")

    push_block = section(on_block, "push", 2)
    if push_block is None:
        violations.append("push trigger is required")
        return
    branches = list_values(section(push_block, "branches", 4) or "", 6)
    if branches != ["main"]:
        violations.append("push branch must be exactly main")
    paths = list_values(section(push_block, "paths", 4) or "", 6)
    if tuple(paths) != EXPECTED_PATHS:
        violations.append("push paths must be exactly: " + ", ".join(EXPECTED_PATHS))


def validate_workflow_scope(text: str, violations: list[str]) -> None:
    if re.search(r"(?m)^permissions:\s*\{\}\s*(?:#.*)?$", text) is None:
        violations.append("workflow permissions must be empty")
    concurrency = mapping_values(section(text, "concurrency") or "", 2)
    if (
        concurrency.get("group") != "pages"
        or concurrency.get("cancel-in-progress") != "true"
    ):
        violations.append("concurrency must use group pages with cancel-in-progress true")
    if BANNED_WORK_RE.search(text):
        violations.append("compiler or performance work is forbidden in the Pages workflow")


def workflow_job_blocks(text: str, violations: list[str]) -> tuple[str, str]:
    jobs_block = section(text, "jobs")
    if child_keys(jobs_block or "", 2) != ["build", "deploy"]:
        violations.append("jobs must be exactly build followed by deploy")
    build_block = section(jobs_block or "", "build", 2)
    deploy_block = section(jobs_block or "", "deploy", 2)
    if build_block is None:
        violations.append("build job is required")
    if deploy_block is None:
        violations.append("deploy job is required")
    return build_block or "", deploy_block or ""


def validate_job_shape(
    build_block: str, deploy_block: str, violations: list[str]
) -> None:
    for job_name, job_block in (("build", build_block), ("deploy", deploy_block)):
        permissions = mapping_values(section(job_block, "permissions", 4) or "", 6)
        expected_permissions = EXPECTED_JOB_PERMISSIONS[job_name]
        if permissions != expected_permissions:
            expected_label = ", ".join(
                f"{key}:{value}" for key, value in expected_permissions.items()
            )
            violations.append(
                f"{job_name} permissions must be exactly {expected_label}"
            )
    if "    runs-on: ubuntu-latest" not in build_block:
        violations.append("build job must run on ubuntu-latest")
    if "    runs-on: ubuntu-latest" not in deploy_block:
        violations.append("deploy job must run on ubuntu-latest")


def validate_actions(job_steps: dict[str, list[str]], violations: list[str]) -> None:
    for job_name, steps in job_steps.items():
        uses = [
            action
            for step in steps
            for action in step_values(step, "uses")
        ]
        required_actions = REQUIRED_ACTIONS[job_name]
        for action in required_actions:
            if uses.count(action) != 1:
                violations.append(
                    f"required action is missing from {job_name} job: {action}"
                )
        for action in sorted(set(uses) - set(required_actions)):
            violations.append(
                f"unexpected or unpinned action in {job_name} job: {action}"
            )


def validate_build_setup(build_block: str, violations: list[str]) -> None:
    setup_step = step_for(build_block, "uses", SETUP_NODE_ACTION) or ""
    if re.search(r"(?m)^\s+node-version:\s+['\"]?22['\"]?\s*$", setup_step) is None:
        violations.append("setup-node must use Node.js 22")
    if re.search(r"(?m)^\s+cache:\s+['\"]?npm['\"]?\s*$", setup_step) is None:
        violations.append("setup-node must enable the npm cache")
    if re.search(
        r"(?m)^\s+cache-dependency-path:\s+['\"]?website/package-lock\.json['\"]?\s*$",
        setup_step,
    ) is None:
        violations.append("npm cache must use website/package-lock.json")

    checkout_step = step_for(build_block, "uses", CHECKOUT_ACTION) or ""
    checkout_with = section(checkout_step, "with", 8) or ""
    if mapping_values(checkout_with, 10).get("persist-credentials") != "false":
        violations.append("checkout must disable credential persistence")


def validate_required_commands(
    build_steps: list[str], violations: list[str]
) -> None:
    command_positions: list[int] = []
    for command in REQUIRED_COMMANDS:
        matches = [
            (index, step)
            for index, step in enumerate(build_steps)
            if command in step_values(step, "run")
        ]
        if not matches:
            violations.append(f"required command is missing: {command}")
        elif len(matches) > 1:
            violations.append(f"required command appears more than once: {command}")
        else:
            position, step = matches[0]
            command_positions.append(position)
            if command.startswith("npm ") and re.search(
                r"(?m)^\s+working-directory:\s+website\s*$", step
            ) is None:
                violations.append(f"website command must run in website/: {command}")
    if (
        len(command_positions) == len(REQUIRED_COMMANDS)
        and command_positions != sorted(command_positions)
    ):
        violations.append(
            "required website commands must appear exactly once and in order"
        )


def validate_publication_order(
    build_steps: list[str], violations: list[str]
) -> None:
    build_positions = step_positions(build_steps, "run", "npm run build")
    build_position = build_positions[0] if len(build_positions) == 1 else -1
    public_docs_positions = step_positions(
        build_steps, "run", "python3 scripts/check-public-docs.py"
    )
    if len(public_docs_positions) != 1 or public_docs_positions[0] >= build_position:
        violations.append("public documentation check is required before build")

    boundary_positions = step_positions(build_steps, "run", BOUNDARY_COMMAND)
    if len(boundary_positions) != 2 or not (
        boundary_positions[0] < build_position < boundary_positions[1]
    ):
        violations.append("source publication boundary check is required before build")

    configure_positions = step_positions(build_steps, "uses", CONFIGURE_PAGES_ACTION)
    upload_positions = step_positions(build_steps, "uses", UPLOAD_PAGES_ACTION)
    postbuild_positions = (
        [boundary_positions[1], configure_positions[0], upload_positions[0]]
        if len(boundary_positions) == 2
        and len(configure_positions) == 1
        and len(upload_positions) == 1
        else []
    )
    if (
        build_position < 0
        or len(postbuild_positions) != 3
        or [build_position, *postbuild_positions]
        != sorted([build_position, *postbuild_positions])
    ):
        violations.append(
            "generated publication boundary check is required after build and "
            "before Pages upload"
        )


def validate_deployment(
    build_block: str, deploy_block: str, violations: list[str]
) -> None:
    upload_step = step_for(build_block, "uses", UPLOAD_PAGES_ACTION) or ""
    if re.search(
        r"(?m)^\s+path:\s+['\"]?website/build['\"]?\s*$", upload_step
    ) is None:
        violations.append("Pages artifact path must be website/build")
    if re.search(r"(?m)^\s{4}needs:\s+build\s*$", deploy_block) is None:
        violations.append("deploy job must depend on build")
    environment = mapping_values(section(deploy_block, "environment", 4) or "", 6)
    if environment.get("name") != "github-pages":
        violations.append("deploy environment must be github-pages")
    if environment.get("url") != "${{ steps.deployment.outputs.page_url }}":
        violations.append("deploy environment URL must use the deployment page_url")
    deploy_step = step_for(deploy_block, "uses", DEPLOY_PAGES_ACTION) or ""
    if re.search(r"(?m)^\s+id:\s+deployment\s*$", deploy_step) is None:
        violations.append("deploy-pages step must use id deployment")


def validate(root: Path) -> list[str]:
    workflow_path = root / WORKFLOW
    try:
        text = workflow_path.read_text(encoding="utf-8")
    except (OSError, UnicodeError) as exc:
        return [f"{WORKFLOW}: cannot read workflow: {exc}"]

    violations: list[str] = []
    validate_triggers(text, violations)
    validate_workflow_scope(text, violations)
    build_block, deploy_block = workflow_job_blocks(text, violations)
    validate_job_shape(build_block, deploy_block, violations)
    job_steps = {
        "build": step_blocks(build_block),
        "deploy": step_blocks(deploy_block),
    }
    validate_actions(job_steps, violations)
    validate_build_setup(build_block, violations)
    validate_required_commands(job_steps["build"], violations)
    validate_publication_order(job_steps["build"], violations)
    validate_deployment(build_block, deploy_block, violations)

    return sorted(set(violations))


def main(argv: list[str]) -> int:
    if len(argv) > 2:
        print("usage: check-docs-pages-workflow.py [repository-root]", file=sys.stderr)
        return 2
    root = Path(argv[1]).resolve() if len(argv) == 2 else Path(__file__).resolve().parent.parent
    violations = validate(root)
    if violations:
        for violation in violations:
            print(f"FAIL: {violation}")
        return 1
    print("Documentation Pages workflow checks passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
