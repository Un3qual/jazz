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
    "README.md",
    ".github/workflows/docs-pages.yml",
)
EXPECTED_PERMISSIONS = {
    "contents": "read",
    "pages": "write",
    "id-token": "write",
}
REQUIRED_ACTIONS = (
    "actions/checkout@v4",
    "actions/setup-node@v4",
    "actions/configure-pages@v5",
    "actions/upload-pages-artifact@v3",
    "actions/deploy-pages@v4",
)
REQUIRED_COMMANDS = (
    "npm ci",
    "npm run test:brand",
    "npm run test:experience",
    "npm run typecheck",
    "npm run build",
    "python3 scripts/check-website-boundary.py",
)
BANNED_WORK_RE = re.compile(
    r"\b(?:cabal|ghc|compiler|benchmark|performance|profil(?:e|ing)?|"
    r"full-parser-scale|parser-scale)\b",
    re.IGNORECASE,
)


def scalar(value: str) -> str:
    value = value.strip()
    if len(value) >= 2 and value[0] == value[-1] and value[0] in "\"'":
        return value[1:-1]
    return value


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


def step_for(block: str, token: str) -> str | None:
    lines = block.splitlines()
    starts = [
        index
        for index, line in enumerate(lines)
        if line.startswith("      - ")
    ]
    for position, start in enumerate(starts):
        end = starts[position + 1] if position + 1 < len(starts) else len(lines)
        candidate = "\n".join(lines[start:end])
        if token in candidate:
            return candidate
    return None


def validate(root: Path) -> list[str]:
    workflow_path = root / WORKFLOW
    try:
        text = workflow_path.read_text(encoding="utf-8")
    except (OSError, UnicodeError) as exc:
        return [f"{WORKFLOW}: cannot read workflow: {exc}"]

    violations: list[str] = []
    on_block = section(text, "on")
    if on_block is None:
        violations.append("workflow trigger block is required")
    else:
        triggers = child_keys(on_block, 2)
        if "workflow_dispatch" not in triggers:
            violations.append("workflow_dispatch trigger is required")
        if "pull_request" in triggers or re.search(r"(?m)^\s*pull_request(?:_target)?:", on_block):
            violations.append("pull_request triggers are forbidden")
        if set(triggers) != {"push", "workflow_dispatch"}:
            violations.append("workflow triggers must be exactly push and workflow_dispatch")

        push_block = section(on_block, "push", 2)
        if push_block is None:
            violations.append("push trigger is required")
        else:
            branches_block = section(push_block, "branches", 4)
            branches = list_values(branches_block or "", 6)
            if branches != ["main"]:
                violations.append("push branch must be exactly main")
            paths_block = section(push_block, "paths", 4)
            paths = list_values(paths_block or "", 6)
            if tuple(paths) != EXPECTED_PATHS:
                violations.append(
                    "push paths must be exactly: " + ", ".join(EXPECTED_PATHS)
                )

    permissions_block = section(text, "permissions")
    permissions = mapping_values(permissions_block or "", 2)
    if permissions != EXPECTED_PERMISSIONS:
        violations.append("permissions must be exactly contents:read, pages:write, id-token:write")

    concurrency_block = section(text, "concurrency")
    concurrency = mapping_values(concurrency_block or "", 2)
    if concurrency.get("group") != "pages" or concurrency.get("cancel-in-progress") != "true":
        violations.append("concurrency must use group pages with cancel-in-progress true")

    jobs_block = section(text, "jobs")
    job_keys = child_keys(jobs_block or "", 2)
    if job_keys != ["build", "deploy"]:
        violations.append("jobs must be exactly build followed by deploy")
    build_block = section(jobs_block or "", "build", 2)
    deploy_block = section(jobs_block or "", "deploy", 2)
    if build_block is None:
        violations.append("build job is required")
        build_block = ""
    if deploy_block is None:
        violations.append("deploy job is required")
        deploy_block = ""

    if "    runs-on: ubuntu-latest" not in build_block:
        violations.append("build job must run on ubuntu-latest")
    if "    runs-on: ubuntu-latest" not in deploy_block:
        violations.append("deploy job must run on ubuntu-latest")

    uses = re.findall(r"(?m)^\s+uses:\s+([^\s#]+)", text)
    for action in REQUIRED_ACTIONS:
        if uses.count(action) != 1:
            violations.append(f"required action is missing: {action}")
    unexpected_actions = sorted(set(uses) - set(REQUIRED_ACTIONS))
    for action in unexpected_actions:
        violations.append(f"unexpected or unpinned action: {action}")

    setup_step = step_for(build_block, "actions/setup-node@v4") or ""
    if re.search(r"(?m)^\s+node-version:\s+['\"]?22['\"]?\s*$", setup_step) is None:
        violations.append("setup-node must use Node.js 22")
    if re.search(r"(?m)^\s+cache:\s+['\"]?npm['\"]?\s*$", setup_step) is None:
        violations.append("setup-node must enable the npm cache")
    if re.search(
        r"(?m)^\s+cache-dependency-path:\s+['\"]?website/package-lock\.json['\"]?\s*$",
        setup_step,
    ) is None:
        violations.append("npm cache must use website/package-lock.json")

    command_positions: list[int] = []
    for command in REQUIRED_COMMANDS:
        pattern = rf"(?m)^\s+run:\s+{re.escape(command)}\s*$"
        matches = list(re.finditer(pattern, build_block))
        if not matches:
            violations.append(f"required command is missing: {command}")
        elif len(matches) > 1:
            violations.append(f"required command appears more than once: {command}")
        else:
            command_positions.append(matches[0].start())
            step = step_for(build_block, command) or ""
            if command.startswith("npm ") and re.search(
                r"(?m)^\s+working-directory:\s+website\s*$", step
            ) is None:
                violations.append(f"website command must run in website/: {command}")
    if len(command_positions) == len(REQUIRED_COMMANDS) and command_positions != sorted(command_positions):
        violations.append("required website commands must appear exactly once and in order")

    order_tokens = (
        "npm run build",
        "python3 scripts/check-website-boundary.py",
        "actions/configure-pages@v5",
        "actions/upload-pages-artifact@v3",
    )
    order_positions = [build_block.find(token) for token in order_tokens]
    if any(position < 0 for position in order_positions) or order_positions != sorted(order_positions):
        violations.append("generated publication boundary check is required after build and before Pages upload")

    upload_step = step_for(build_block, "actions/upload-pages-artifact@v3") or ""
    if re.search(r"(?m)^\s+path:\s+['\"]?website/build['\"]?\s*$", upload_step) is None:
        violations.append("Pages artifact path must be website/build")

    if re.search(r"(?m)^\s{4}needs:\s+build\s*$", deploy_block) is None:
        violations.append("deploy job must depend on build")
    environment_block = section(deploy_block, "environment", 4) or ""
    environment = mapping_values(environment_block, 6)
    if environment.get("name") != "github-pages":
        violations.append("deploy environment must be github-pages")
    if environment.get("url") != "${{ steps.deployment.outputs.page_url }}":
        violations.append("deploy environment URL must use the deployment page_url")
    deploy_step = step_for(deploy_block, "actions/deploy-pages@v4") or ""
    if re.search(r"(?m)^\s+id:\s+deployment\s*$", deploy_step) is None:
        violations.append("deploy-pages step must use id deployment")

    if BANNED_WORK_RE.search(text):
        violations.append("compiler or performance work is forbidden in the Pages workflow")

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
