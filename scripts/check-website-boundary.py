#!/usr/bin/env python3
"""Enforce the publication boundary for the Jazz Docusaurus website."""

from __future__ import annotations

import os
import re
import sys
from pathlib import Path


CONFIG_PATH = "website/docusaurus.config.ts"
IGNORED_DIRECTORIES = {"node_modules", "build", ".docusaurus"}
TEXT_SOURCE_SUFFIXES = {
    ".css",
    ".html",
    ".js",
    ".jsx",
    ".json",
    ".md",
    ".mdx",
    ".mjs",
    ".scss",
    ".svg",
    ".ts",
    ".tsx",
    ".txt",
}
AUTHORED_URL_SUFFIXES = {".css", ".md", ".mdx", ".scss", ".tsx"}
FORBIDDEN_SOURCE_REFERENCES = (
    ".codex",
    "docs/execution",
    "docs/superpowers",
    "rfcs",
)
FORBIDDEN_BUILD_STRINGS = (
    ".codex",
    "docs/execution",
    "docs/superpowers",
    "jazz-hs",
    "jazz-next",
    "jazz2",
    "JazzNext",
    "rfcs",
)
REMOTE_URL_RE = re.compile(r"https?://[^\s\"'<>)}]+")
GITHUB_REPOSITORY_URL = "https://github.com/un3qual/jazz"


def relative(root: Path, path: Path) -> str:
    try:
        return path.relative_to(root).as_posix()
    except ValueError:
        return str(path)


def read_utf8(path: Path, root: Path, violations: list[str]) -> str | None:
    try:
        return path.read_bytes().decode("utf-8")
    except (OSError, UnicodeError) as exc:
        violations.append(f"{relative(root, path)}: cannot read UTF-8 text: {exc}")
        return None


def strip_javascript_comments(source: str) -> str:
    """Remove JavaScript comments without treating comment markers in strings as syntax."""

    result: list[str] = []
    index = 0
    quote: str | None = None
    escaped = False
    while index < len(source):
        char = source[index]
        following = source[index + 1] if index + 1 < len(source) else ""
        if quote is not None:
            result.append(char)
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == quote:
                quote = None
            index += 1
            continue
        if char in {"'", '"', "`"}:
            quote = char
            result.append(char)
            index += 1
            continue
        if char == "/" and following == "/":
            result.extend("  ")
            index += 2
            while index < len(source) and source[index] not in "\r\n":
                result.append(" ")
                index += 1
            continue
        if char == "/" and following == "*":
            result.extend("  ")
            index += 2
            while index < len(source):
                if source[index] == "*" and index + 1 < len(source) and source[index + 1] == "/":
                    result.extend("  ")
                    index += 2
                    break
                result.append("\n" if source[index] == "\n" else " ")
                index += 1
            continue
        result.append(char)
        index += 1
    return "".join(result)


def property_values(source: str, property_name: str) -> list[str]:
    pattern = re.compile(
        rf"\b{re.escape(property_name)}\s*:\s*(['\"])(.*?)\1",
        re.DOTALL,
    )
    return [match.group(2) for match in pattern.finditer(source)]


def has_boolean_property(source: str, property_name: str, expected: str) -> bool:
    return bool(
        re.search(
            rf"\b{re.escape(property_name)}\s*:\s*{re.escape(expected)}\b",
            source,
        )
    )


def object_property_body(source: str, property_name: str) -> str | None:
    opening = re.search(
        rf"\b{re.escape(property_name)}\s*:\s*\{{",
        source,
    )
    if opening is None:
        return None
    start = opening.end()
    depth = 1
    quote: str | None = None
    escaped = False
    for index in range(start, len(source)):
        char = source[index]
        if quote is not None:
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == quote:
                quote = None
            continue
        if char in {"'", '"', "`"}:
            quote = char
        elif char == "{":
            depth += 1
        elif char == "}":
            depth -= 1
            if depth == 0:
                return source[start:index]
    return None


def check_config(root: Path, violations: list[str]) -> None:
    config_path = root / CONFIG_PATH
    if not config_path.is_file():
        violations.append(f"{CONFIG_PATH}: required Docusaurus configuration is missing")
        return
    raw_config = read_utf8(config_path, root, violations)
    if raw_config is None:
        return
    config = strip_javascript_comments(raw_config)

    requirements = (
        (
            "path",
            "../docs",
            "docs path must be exactly ../docs",
        ),
        (
            "url",
            "https://un3qual.github.io",
            "production URL must be https://un3qual.github.io",
        ),
        (
            "baseUrl",
            "/jazz/",
            "base URL must be /jazz/",
        ),
        (
            "onBrokenLinks",
            "throw",
            "broken links must throw",
        ),
    )
    for property_name, expected, message in requirements:
        values = property_values(config, property_name)
        if values != [expected]:
            violations.append(f"{CONFIG_PATH}: {message}")

    if not has_boolean_property(config, "blog", "false"):
        violations.append(f"{CONFIG_PATH}: classic preset blog must be disabled")

    markdown = object_property_body(config, "markdown")
    hooks = object_property_body(markdown or "", "hooks")
    if hooks is None or property_values(hooks, "onBrokenMarkdownLinks") != ["throw"]:
        violations.append(
            f"{CONFIG_PATH}: broken Markdown links must throw through markdown hooks"
        )


def website_entries(website: Path) -> list[Path]:
    entries: list[Path] = []
    for directory, directory_names, file_names in os.walk(
        website,
        topdown=True,
        followlinks=False,
    ):
        directory_names[:] = sorted(
            name for name in directory_names if name not in IGNORED_DIRECTORIES
        )
        current = Path(directory)
        entries.extend(current / name for name in directory_names)
        entries.extend(current / name for name in sorted(file_names))
    return sorted(entries, key=lambda path: path.as_posix())


def check_authored_sources(root: Path, violations: list[str]) -> None:
    website = root / "website"
    if not website.is_dir():
        violations.append("website: required website directory is missing")
        return

    for path in website_entries(website):
        path_label = relative(root, path)
        if path.is_symlink():
            violations.append(f"{path_label}: symlink is not allowed in website sources")
            continue
        relative_parts = tuple(part.casefold() for part in path.relative_to(website).parts)
        normalized_path = "/".join(relative_parts)
        for forbidden in FORBIDDEN_SOURCE_REFERENCES:
            if forbidden.casefold() in normalized_path:
                violations.append(
                    f"{path_label}: forbidden publication source reference: {forbidden}"
                )
        if not path.is_file() or path.name == "package-lock.json":
            continue
        if path.suffix.casefold() not in TEXT_SOURCE_SUFFIXES:
            continue
        text = read_utf8(path, root, violations)
        if text is None:
            continue
        folded_text = text.casefold()
        for forbidden in FORBIDDEN_SOURCE_REFERENCES:
            if forbidden.casefold() in folded_text:
                violations.append(
                    f"{path_label}: forbidden publication source reference: {forbidden}"
                )
        if path.suffix.casefold() in AUTHORED_URL_SUFFIXES:
            remote_urls = REMOTE_URL_RE.findall(text)
            if any(not url.startswith(GITHUB_REPOSITORY_URL) for url in remote_urls):
                violations.append(f"{path_label}: remote authored URL is not allowed")


def check_generated_output(root: Path, violations: list[str]) -> None:
    build = root / "website/build"
    if not build.exists():
        return
    if build.is_symlink() or not build.is_dir():
        violations.append("website/build: generated output must be a real directory")
        return
    for directory, directory_names, file_names in os.walk(build, followlinks=False):
        directory_names[:] = sorted(directory_names)
        current = Path(directory)
        for name in directory_names:
            path = current / name
            if path.is_symlink():
                violations.append(
                    f"{relative(root, path)}: symlink is not allowed in generated output"
                )
        for name in sorted(file_names):
            path = current / name
            path_label = relative(root, path)
            if path.is_symlink():
                violations.append(f"{path_label}: symlink is not allowed in generated output")
                continue
            try:
                contents = path.read_bytes()
            except OSError as exc:
                violations.append(f"{path_label}: cannot read generated output: {exc}")
                continue
            folded_contents = contents.lower()
            for forbidden in FORBIDDEN_BUILD_STRINGS:
                if forbidden.encode("utf-8").lower() in folded_contents:
                    violations.append(
                        f"{path_label}: generated output contains forbidden string: {forbidden}"
                    )


def main(argv: list[str]) -> int:
    if len(argv) > 2:
        print("usage: check-website-boundary.py [repository-root]")
        return 2
    root = (
        Path(argv[1]).expanduser().resolve()
        if len(argv) == 2
        else Path(__file__).resolve().parent.parent
    )
    violations: list[str] = []
    if not root.is_dir():
        violations.append(f"{root}: repository root is not a directory")
    else:
        check_config(root, violations)
        check_authored_sources(root, violations)
        check_generated_output(root, violations)

    if violations:
        print("Website boundary checks failed:")
        for violation in sorted(set(violations)):
            print(f"- {violation}")
        return 1
    print("Website boundary checks passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
