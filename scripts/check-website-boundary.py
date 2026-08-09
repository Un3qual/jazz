#!/usr/bin/env python3
"""Validate the Jazz-specific Docusaurus publication boundary.

Docusaurus owns rendering and link resolution. This checker only preserves the
repository's configured publication root and scans emitted text for ordinary
accidental internal references or remote runtime dependencies.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path


CONFIG = Path("website/docusaurus.config.ts")
BUILD = Path("website/build")
TEXT_SUFFIXES = {".css", ".html", ".js", ".svg"}
INTERNAL_TERMS = (
    ".codex",
    "docs/execution",
    "docs/superpowers",
    "jazz-hs",
    "jazz-next",
    "jazz2",
    "jazznext",
    "rfcs/",
)
REMOTE_URL_RE = re.compile(r"(?:https?:)?//[^\s\"'`<>()\\]+", re.IGNORECASE)
RESOURCE_ATTRIBUTE_RE = re.compile(
    r"(?i)\b(?:src|srcset|poster)\s*=\s*(['\"])(.*?)\1"
)
LINK_RESOURCE_RE = re.compile(
    r"(?is)<(?:link|image|use)\b[^>]*\b(?:xlink:)?href\s*=\s*(['\"])(.*?)\1"
)
CSS_RESOURCE_RE = re.compile(
    r"(?is)(?:url|image-set)\(\s*(.*?)\s*\)|@import\s+(['\"])(.*?)\2"
)
SCRIPT_RESOURCE_RE = re.compile(
    r"(?i)\b(?:fetch|import)\(\s*(['\"])(.*?)\1"
)
PRODUCTION_PREFIXES = (
    "https://un3qual.github.io/jazz/",
    "//un3qual.github.io/jazz/",
)

CONFIG_REQUIREMENTS = (
    ("url: 'https://un3qual.github.io'", "Docusaurus must use the production origin"),
    ("baseUrl: '/jazz/'", "Docusaurus must use the /jazz/ base route"),
    ("onBrokenLinks: 'throw'", "Docusaurus must fail broken links"),
    (
        "onBrokenMarkdownLinks: 'throw'",
        "Docusaurus must fail broken Markdown links",
    ),
    ("format: 'md'", "Docusaurus must publish plain Markdown"),
    ("path: '../docs'", "Docusaurus must publish only ../docs"),
    ("routeBasePath: 'docs'", "Docusaurus must publish docs at /docs"),
    ("blog: false", "Docusaurus blog publication must remain disabled"),
)


def read_text(path: Path) -> str | None:
    try:
        return path.read_text(encoding="utf-8")
    except (OSError, UnicodeError):
        return None


def allowed_remote_url(url: str) -> bool:
    cleaned = url.rstrip(".,;:!?")
    return cleaned.startswith(PRODUCTION_PREFIXES)


def resource_targets(source: str) -> list[str]:
    targets = [match.group(2) for match in RESOURCE_ATTRIBUTE_RE.finditer(source)]
    targets.extend(match.group(2) for match in LINK_RESOURCE_RE.finditer(source))
    targets.extend(
        match.group(1) or match.group(3)
        for match in CSS_RESOURCE_RE.finditer(source)
    )
    targets.extend(match.group(2) for match in SCRIPT_RESOURCE_RE.finditer(source))
    return targets


def check_config(root: Path, violations: list[str]) -> None:
    path = root / CONFIG
    source = read_text(path)
    if source is None:
        violations.append(f"{CONFIG}: required UTF-8 Docusaurus configuration is missing")
        return
    for fragment, message in CONFIG_REQUIREMENTS:
        if fragment not in source:
            violations.append(f"{CONFIG}: {message}")


def check_built_output(root: Path, violations: list[str]) -> None:
    build = root / BUILD
    if not build.exists():
        return
    for path in sorted(build.rglob("*")):
        if not path.is_file() or path.suffix.casefold() not in TEXT_SUFFIXES:
            continue
        source = read_text(path)
        relative = path.relative_to(root).as_posix()
        if source is None:
            violations.append(f"{relative}: generated text is not readable UTF-8")
            continue
        folded = source.casefold()
        for term in INTERNAL_TERMS:
            if term.casefold() in folded:
                violations.append(
                    f"{relative}: generated output contains internal-only material: {term}"
                )
        for target in resource_targets(source):
            if target.lstrip(" \t\r\n\"'").casefold().startswith("data:"):
                continue
            for url in REMOTE_URL_RE.findall(target):
                if not allowed_remote_url(url):
                    violations.append(
                        f"{relative}: generated output contains a non-allowlisted remote URL: {url}"
                    )


def validate(root: Path) -> list[str]:
    violations: list[str] = []
    check_config(root, violations)
    check_built_output(root, violations)
    return sorted(set(violations))


def main(argv: list[str]) -> int:
    if len(argv) > 2:
        print("usage: check-website-boundary.py [repository-root]", file=sys.stderr)
        return 2
    root = Path(argv[1]).resolve() if len(argv) == 2 else Path(__file__).resolve().parent.parent
    violations = validate(root)
    if violations:
        print("Website boundary checks failed:")
        for violation in violations:
            print(f"- {violation}")
        return 1
    print("Website boundary checks passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
