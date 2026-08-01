#!/usr/bin/env python3
"""Enforce the publication boundary for the Jazz Docusaurus website."""

from __future__ import annotations

import os
import re
import sys
from pathlib import Path
from urllib.parse import unquote, urlsplit


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
REMOTE_URL_RE = re.compile(r"https?://[^\s\"'<>)}]+", re.IGNORECASE)
MARKDOWN_NAVIGATION_RE = re.compile(
    r"(?<!!)\[[^\]]*\]\(\s*<?(?P<url>https?://[^\s)>]+)>?(?:\s+[^)]*)?\)",
    re.IGNORECASE,
)
MARKUP_NAVIGATION_RE = re.compile(
    r"<(?:Link|a)\b[^>]*?\b(?:to|href)\s*=\s*"
    r"(?P<quote>['\"])(?P<url>https?://[^'\"]+)(?P=quote)",
    re.DOTALL,
)
PROTOCOL_RELATIVE_CSS_RE = re.compile(
    r"(?:url\s*\(\s*|@import\s+)(?:['\"])?(?P<url>//[^\s\"')]+)",
    re.IGNORECASE,
)
PROTOCOL_RELATIVE_MARKUP_RE = re.compile(
    r"\b(?:src|srcSet|poster|href|to)\s*=\s*"
    r"(?P<quote>['\"])(?P<url>//[^'\"]+)(?P=quote)",
    re.IGNORECASE | re.DOTALL,
)
PROTOCOL_RELATIVE_MARKDOWN_RE = re.compile(
    r"!?\[[^\]]*\]\(\s*<?(?P<url>//[^\s)>]+)",
    re.IGNORECASE,
)
PROTOCOL_RELATIVE_IMPORT_RE = re.compile(
    r"\b(?:from|import)\s*(?P<quote>['\"])(?P<url>//[^'\"]+)(?P=quote)",
    re.IGNORECASE,
)


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


def delimited_body(
    source: str,
    opening_index: int,
    opener: str,
    closer: str,
) -> tuple[str, int] | None:
    if opening_index >= len(source) or source[opening_index] != opener:
        return None
    start = opening_index + 1
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
        elif char == opener:
            depth += 1
        elif char == closer:
            depth -= 1
            if depth == 0:
                return source[start:index], index + 1
    return None


def top_level_elements(source: str) -> list[str]:
    elements: list[str] = []
    start = 0
    depths = {"(": 0, "[": 0, "{": 0}
    closing_to_opening = {")": "(", "]": "[", "}": "{"}
    quote: str | None = None
    escaped = False
    for index, char in enumerate(source):
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
        elif char in depths:
            depths[char] += 1
        elif char in closing_to_opening:
            opener = closing_to_opening[char]
            depths[opener] = max(0, depths[opener] - 1)
        elif char == "," and all(depth == 0 for depth in depths.values()):
            element = source[start:index].strip()
            if element:
                elements.append(element)
            start = index + 1
    final = source[start:].strip()
    if final:
        elements.append(final)
    return elements


def property_expressions(source: str, property_name: str) -> list[str]:
    expressions: list[str] = []
    pattern = re.compile(rf"^{re.escape(property_name)}\s*:\s*(.*)$", re.DOTALL)
    for element in top_level_elements(source):
        match = pattern.match(element)
        if match:
            expressions.append(match.group(1).strip())
    return expressions


def container_expression_body(
    expression: str,
    opener: str,
    closer: str,
) -> str | None:
    stripped = expression.strip()
    result = delimited_body(stripped, 0, opener, closer)
    if result is None:
        return None
    body, end = result
    if stripped[end:].strip():
        return None
    return body


def string_expression_value(expression: str) -> str | None:
    match = re.fullmatch(r"(['\"])(.*?)\1", expression.strip(), re.DOTALL)
    return match.group(2) if match else None


def mask_javascript_strings(source: str) -> str:
    masked = list(source)
    quote: str | None = None
    escaped = False
    for index, char in enumerate(source):
        if quote is not None:
            masked[index] = "\n" if char == "\n" else " "
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == quote:
                quote = None
            continue
        if char in {"'", '"', "`"}:
            quote = char
            masked[index] = " "
    return "".join(masked)


def literal_statement_trailing_is_valid(source: str, body_end: int) -> bool:
    return bool(
        re.match(
            r"\s*(?:satisfies\s+Config)?\s*;",
            source[body_end:],
        )
    )


def default_export_config_body(source: str) -> str | None:
    code = mask_javascript_strings(source)
    exports = list(re.finditer(r"\bexport\s+default\b", code))
    if len(exports) != 1:
        return None
    export_end = exports[0].end()
    position = export_end
    while position < len(code) and code[position].isspace():
        position += 1

    if position < len(source) and source[position] == "{":
        result = delimited_body(source, position, "{", "}")
        if result is None:
            return None
        body, body_end = result
        return body if literal_statement_trailing_is_valid(source, body_end) else None

    identifier = re.match(r"[A-Za-z_$][A-Za-z0-9_$]*", code[position:])
    if identifier is None:
        return None
    identifier_name = identifier.group(0)
    identifier_end = position + identifier.end()
    if re.match(r"\s*;", code[identifier_end:]) is None:
        return None

    declaration_pattern = re.compile(
        rf"\bconst\s+{re.escape(identifier_name)}"
        r"(?:\s*:\s*Config)?\s*=\s*\{"
    )
    declarations = list(declaration_pattern.finditer(code))
    if len(declarations) != 1:
        return None
    result = delimited_body(source, declarations[0].end() - 1, "{", "}")
    if result is None:
        return None
    body, body_end = result
    return body if literal_statement_trailing_is_valid(source, body_end) else None


def has_dynamic_object_entries(source: str) -> bool:
    return any(
        element.lstrip().startswith(("...", "["))
        for element in top_level_elements(source)
    )


def has_array_spread(source: str) -> bool:
    return any(
        element.lstrip().startswith("...")
        for element in top_level_elements(source)
    )


def presets_array_body(config: str) -> str | None:
    presets = property_expressions(config, "presets")
    if len(presets) != 1:
        return None
    return container_expression_body(presets[0], "[", "]")


def classic_preset_options(config: str) -> list[str]:
    presets_body = presets_array_body(config)
    if presets_body is None:
        return []

    classic_options: list[str] = []
    for preset_expression in top_level_elements(presets_body):
        preset_body = container_expression_body(preset_expression, "[", "]")
        if preset_body is None:
            continue
        fields = top_level_elements(preset_body)
        if not fields or string_expression_value(fields[0]) != "classic":
            continue
        if len(fields) != 2:
            continue
        options = fields[1].strip()
        if not options.startswith("{"):
            continue
        result = delimited_body(options, 0, "{", "}")
        if result is None:
            continue
        body, end = result
        trailing = options[end:].strip()
        if trailing and not re.fullmatch(r"satisfies\s+Preset\.Options", trailing):
            continue
        classic_options.append(body)
    return classic_options


def object_property_body(source: str, property_name: str) -> str | None:
    expressions = property_expressions(source, property_name)
    if len(expressions) != 1:
        return None
    return container_expression_body(expressions[0], "{", "}")


def is_github_repository_navigation(url: str) -> bool:
    parsed = urlsplit(url)
    if parsed.scheme != "https" or parsed.netloc.casefold() != "github.com":
        return False
    path = parsed.path
    if unquote(path) != path:
        return False
    if any(segment in {".", ".."} for segment in path.split("/")):
        return False
    return path == "/un3qual/jazz" or path.startswith("/un3qual/jazz/")


def navigation_url_spans(text: str, suffix: str) -> set[tuple[int, int]]:
    spans: set[tuple[int, int]] = set()
    patterns = [MARKUP_NAVIGATION_RE]
    if suffix in {".md", ".mdx"}:
        patterns.append(MARKDOWN_NAVIGATION_RE)
    for pattern in patterns:
        for match in pattern.finditer(text):
            url = match.group("url")
            if is_github_repository_navigation(url):
                spans.add(match.span("url"))
    return spans


def has_protocol_relative_resource(text: str, suffix: str) -> bool:
    patterns: list[re.Pattern[str]] = []
    if suffix in {".css", ".scss"}:
        patterns.append(PROTOCOL_RELATIVE_CSS_RE)
    if suffix == ".tsx":
        patterns.extend((PROTOCOL_RELATIVE_MARKUP_RE, PROTOCOL_RELATIVE_IMPORT_RE))
    if suffix in {".md", ".mdx"}:
        patterns.extend((PROTOCOL_RELATIVE_MARKDOWN_RE, PROTOCOL_RELATIVE_MARKUP_RE))
    return any(pattern.search(text) is not None for pattern in patterns)


def has_forbidden_remote_url(text: str, suffix: str) -> bool:
    if has_protocol_relative_resource(text, suffix):
        return True
    allowed_spans = navigation_url_spans(text, suffix)
    return any(match.span() not in allowed_spans for match in REMOTE_URL_RE.finditer(text))


def check_config(root: Path, violations: list[str]) -> None:
    config_path = root / CONFIG_PATH
    if not config_path.is_file():
        violations.append(f"{CONFIG_PATH}: required Docusaurus configuration is missing")
        return
    raw_config = read_utf8(config_path, root, violations)
    if raw_config is None:
        return
    config = strip_javascript_comments(raw_config)
    config_body = default_export_config_body(config)
    if config_body is None:
        violations.append(
            f"{CONFIG_PATH}: default export must resolve unambiguously to a literal Config object"
        )
        return

    if has_dynamic_object_entries(config_body):
        violations.append(
            f"{CONFIG_PATH}: config object must not contain spreads or computed properties"
        )

    requirements = (
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
        values = [
            string_expression_value(expression)
            for expression in property_expressions(config_body, property_name)
        ]
        if values != [expected]:
            violations.append(f"{CONFIG_PATH}: {message}")

    presets_body = presets_array_body(config_body)
    if presets_body is not None and has_array_spread(presets_body):
        violations.append(f"{CONFIG_PATH}: presets array must not contain spreads")

    classic_options = classic_preset_options(config_body)
    if len(classic_options) == 1 and has_dynamic_object_entries(classic_options[0]):
        violations.append(
            f"{CONFIG_PATH}: classic preset options must not contain spreads or computed properties"
        )
    docs_body = (
        object_property_body(classic_options[0], "docs")
        if len(classic_options) == 1
        else None
    )
    if docs_body is not None and has_dynamic_object_entries(docs_body):
        violations.append(
            f"{CONFIG_PATH}: classic preset docs options must not contain spreads or computed properties"
        )
    docs_paths = (
        [
            string_expression_value(expression)
            for expression in property_expressions(docs_body, "path")
        ]
        if docs_body is not None
        else []
    )
    if docs_paths != ["../docs"]:
        violations.append(
            f"{CONFIG_PATH}: classic preset docs path must be exactly ../docs"
        )

    blog_values = (
        property_expressions(classic_options[0], "blog")
        if len(classic_options) == 1
        else []
    )
    if blog_values != ["false"]:
        violations.append(f"{CONFIG_PATH}: classic preset blog must be disabled")

    markdown = object_property_body(config_body, "markdown")
    if markdown is not None and has_dynamic_object_entries(markdown):
        violations.append(
            f"{CONFIG_PATH}: markdown options must not contain spreads or computed properties"
        )
    hooks = object_property_body(markdown or "", "hooks")
    if hooks is not None and has_dynamic_object_entries(hooks):
        violations.append(
            f"{CONFIG_PATH}: markdown hooks must not contain spreads or computed properties"
        )
    markdown_link_values = (
        [
            string_expression_value(expression)
            for expression in property_expressions(hooks, "onBrokenMarkdownLinks")
        ]
        if hooks is not None
        else []
    )
    if markdown_link_values != ["throw"]:
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
            if has_forbidden_remote_url(text, path.suffix.casefold()):
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
