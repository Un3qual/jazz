#!/usr/bin/env python3
"""Validate the Jazz-specific built Docusaurus publication boundary.

Docusaurus owns configuration, rendering, and link resolution. This checker
only scans emitted text for accidental internal references or remote runtime
dependencies.
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path


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
    r"(?i)\b(?:src|srcset|poster|data)\s*=\s*(['\"])(.*?)\1"
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


def check_output_tree(build: Path, label_root: Path, violations: list[str]) -> None:
    if not build.is_dir():
        violations.append(f"{build}: build directory is missing")
        return
    for path in sorted(build.rglob("*")):
        if not path.is_file() or path.suffix.casefold() not in TEXT_SUFFIXES:
            continue
        source = read_text(path)
        relative = path.relative_to(label_root).as_posix()
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


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--build-directory", required=True)
    arguments = parser.parse_args(argv[1:])
    build = Path(arguments.build_directory).resolve()
    violations: list[str] = []
    check_output_tree(build, build, violations)
    violations = sorted(set(violations))
    if violations:
        print("Website boundary checks failed:")
        for violation in violations:
            print(f"- {violation}")
        return 1
    print("Website boundary checks passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
