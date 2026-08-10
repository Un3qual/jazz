#!/usr/bin/env python3
"""Check Jazz-specific safety invariants in the Pages workflow.

actionlint owns YAML and GitHub Actions syntax. This file intentionally checks
only the small deployment contract that is specific to this repository.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path


WORKFLOW = Path(".github/workflows/docs-pages.yml")
SHA = r"[0-9a-f]{40}"
USES_RE = re.compile(r"(?m)^\s*uses:\s+([^\s#]+)")

CHECKOUT_ACTION = "actions/checkout@11d5960a326750d5838078e36cf38b85af677262"
PNPM_ACTION = "pnpm/action-setup@b906affcce14559ad1aafd4ab0e942779e9f58b1"
SETUP_NODE_ACTION = "actions/setup-node@49933ea5288caeca8642d1e84afbd3f7d6820020"
CONFIGURE_PAGES_ACTION = "actions/configure-pages@983d7736d9b0ae728b81ab479565c72886d7745b"
UPLOAD_PAGES_ACTION = "actions/upload-pages-artifact@56afc609e74202658d3ffba0e8f6dda462b719fa"
DEPLOY_PAGES_ACTION = "actions/deploy-pages@d6db90164ac5ed86f2b6aed7e0febac5b3c0c03e"

EXPECTED_ACTIONS = (
    CHECKOUT_ACTION,
    PNPM_ACTION,
    SETUP_NODE_ACTION,
    CONFIGURE_PAGES_ACTION,
    UPLOAD_PAGES_ACTION,
    DEPLOY_PAGES_ACTION,
)

REQUIRED_FRAGMENTS = (
    "on:\n  push:\n    branches:\n      - main",
    "      - \"docs/**\"",
    "      - \"website/**\"",
    "      - \"editors/vscode-jazz/syntaxes/jazz.tmLanguage.json\"",
    "  workflow_dispatch:",
    "permissions: {}",
    "  build:\n    permissions:\n      contents: read",
    "  deploy:\n    permissions:\n      pages: write",
    "      id-token: write",
    "          version: 11.18.0",
    "          node-version: 22",
    "          cache: pnpm",
    "          cache-dependency-path: website/pnpm-lock.yaml",
    "          path: website/build",
    "    needs: build",
    "      name: github-pages",
    "      url: ${{ steps.deployment.outputs.page_url }}",
)

ORDERED_STEPS = (
    "      - name: Check out repository",
    "      - name: Set up pnpm",
    "      - name: Set up Node.js",
    "      - name: Install website dependencies",
    "      - name: Check brand assets",
    "      - name: Check website experience",
    "      - name: Type-check website",
    "      - name: Check Pages workflow contract",
    "      - name: Check public documentation",
    "      - name: Build website",
    "      - name: Check generated publication boundary",
    "      - name: Configure GitHub Pages",
    "      - name: Upload GitHub Pages artifact",
    "  deploy:",
    "      - name: Deploy GitHub Pages",
)

REQUIRED_COMMANDS = (
    "        run: pnpm install --frozen-lockfile",
    "        run: pnpm run test:brand",
    "        run: pnpm run test:experience",
    "        run: pnpm run typecheck",
    "        run: python3 scripts/check-docs-pages-workflow.py",
    "        run: python3 scripts/check-public-docs.py",
    "        run: python3 scripts/check-website-boundary.py --build-directory website/build",
    "        run: pnpm run build",
)

CRITICAL_STEPS = (
    "Install website dependencies",
    "Check brand assets",
    "Check website experience",
    "Type-check website",
    "Check Pages workflow contract",
    "Check public documentation",
    "Build website",
    "Check generated publication boundary",
)


def step_block(source: str, name: str) -> str:
    marker = f"      - name: {name}\n"
    start = source.find(marker)
    if start < 0:
        return ""
    boundaries = (
        source.find("\n      - name:", start + len(marker)),
        source.find("\n  deploy:", start + len(marker)),
    )
    end = min((boundary for boundary in boundaries if boundary >= 0), default=len(source))
    return source[start:end]


def validate(root: Path) -> list[str]:
    path = root / WORKFLOW
    try:
        source = path.read_text(encoding="utf-8")
    except (OSError, UnicodeError) as error:
        return [f"{WORKFLOW}: cannot read workflow: {error}"]

    violations: list[str] = []
    actions = tuple(USES_RE.findall(source))
    for action in actions:
        if action.startswith("./"):
            continue
        reference = action.rsplit("@", 1)[-1]
        if re.fullmatch(SHA, reference) is None:
            violations.append(f"action must use an immutable commit: {action}")
    if actions != EXPECTED_ACTIONS:
        violations.append("Pages workflow actions must be the approved pinned actions in order")

    checkout = step_block(source, "Check out repository")
    if re.search(r"(?m)^\s*(?:repository|ref):", checkout):
        violations.append("checkout must use the triggering repository and revision")
    if re.search(r"(?m)^\s*persist-credentials:\s*false\s*$", checkout) is None:
        violations.append("checkout must disable credential persistence")

    for fragment in REQUIRED_FRAGMENTS:
        if fragment not in source:
            if fragment == "permissions: {}":
                violations.append("workflow permissions must be empty")
            else:
                violations.append(f"required workflow setting is missing: {fragment.strip()}")

    for command in REQUIRED_COMMANDS:
        if command not in source:
            violations.append(f"required workflow step is missing: {command.strip()}")

    for name in CRITICAL_STEPS:
        block = step_block(source, name)
        if not block:
            violations.append(f"required workflow step is missing: {name}")
            continue
        if re.search(r"(?m)^\s*(?:continue-on-error|if):", block):
            violations.append(f"critical workflow step must fail the job: {name}")

    positions = [source.find(step) for step in ORDERED_STEPS]
    if any(position < 0 for position in positions):
        violations.append("required workflow step is missing")
    elif positions != sorted(positions):
        violations.append("workflow steps are out of publication order")

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
