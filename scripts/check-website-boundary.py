#!/usr/bin/env python3
"""Enforce the publication boundary for the Jazz Docusaurus website."""

from __future__ import annotations

import os
import re
import sys
from dataclasses import dataclass
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
AUTHORED_URL_SUFFIXES = {
    ".css",
    ".js",
    ".jsx",
    ".md",
    ".mdx",
    ".scss",
    ".tsx",
}
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
INLINE_MARKDOWN_TARGET_RE = re.compile(
    r"(?P<image>!)?\[[^\]]*\]\(\s*"
    r"(?:<(?P<angle>[^>\n]+)>|(?P<bare>[^\s)\n]+))"
)
REFERENCE_DEFINITION_RE = re.compile(
    r"^[ \t]{0,3}\[(?P<label>[^\]]+)\]:[ \t]*"
    r"(?:<(?P<angle>[^>\n]+)>|(?P<bare>\S+))",
    re.MULTILINE,
)
FULL_REFERENCE_USAGE_RE = re.compile(
    r"(?P<image>!)?\[(?P<text>[^\]]+)\]\[(?P<label>[^\]]*)\]"
)
SHORTCUT_REFERENCE_USAGE_RE = re.compile(
    r"(?P<image>!)?\[(?P<label>[^\]]+)\](?![\[(])"
)
MARKDOWN_AUTOLINK_RE = re.compile(r"<(?P<url>https?://[^>\s]+)>", re.IGNORECASE)
HTML_ELEMENT_RE = re.compile(
    r"<(?P<tag>[A-Za-z][A-Za-z0-9.:_-]*)\b(?P<attributes>[^>]*)>",
    re.DOTALL,
)
HTML_TARGET_ATTRIBUTE_RE = re.compile(
    r"\b(?P<attribute>src|href|to)\s*=\s*"
    r"(?:"
    r"(?P<quote>['\"])(?P<quoted_url>[^'\"]+)(?P=quote)"
    r"|\{\s*(?P<expression_quote>['\"])(?P<expression_url>[^'\"]+)"
    r"(?P=expression_quote)\s*\}"
    r"|(?P<bare_url>[^\s\"'=<>`{}]+)"
    r")",
    re.IGNORECASE | re.DOTALL,
)
STATIC_MDX_IMPORT_RE = re.compile(
    r"^[ \t]*import\b"
    r"(?:[ \t\r\n]+(?:(?!;)[\s\S])*?\bfrom)?"
    r"[ \t\r\n]*(?P<quote>['\"])(?P<url>[^'\"\r\n]+)(?P=quote)"
    r"[ \t]*;?",
    re.MULTILINE,
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


def mask_range(characters: list[str], start: int, end: int) -> None:
    for index in range(start, end):
        if characters[index] not in "\r\n":
            characters[index] = " "


def mask_markdown_noncontent(source: str) -> str:
    characters = list(source)
    for match in re.finditer(r"<!--.*?-->", source, re.DOTALL):
        mask_range(characters, *match.span())

    offset = 0
    fence_character: str | None = None
    fence_length = 0
    for line in source.splitlines(keepends=True):
        content = line.rstrip("\r\n")
        indentation = len(content) - len(content.lstrip(" "))
        stripped = content[indentation:] if indentation <= 3 else ""
        marker = re.match(r"(?P<marker>`{3,}|~{3,})", stripped)
        if fence_character is None and marker:
            marker_text = marker.group("marker")
            fence_character = marker_text[0]
            fence_length = len(marker_text)
            mask_range(characters, offset, offset + len(line))
        elif fence_character is not None:
            mask_range(characters, offset, offset + len(line))
            closing = re.match(
                rf"{re.escape(fence_character)}{{{fence_length},}}\s*$",
                stripped,
            )
            if closing:
                fence_character = None
                fence_length = 0
        offset += len(line)

    masked = "".join(characters)
    for match in re.finditer(r"(?P<ticks>`+).*?(?P=ticks)", masked, re.DOTALL):
        mask_range(characters, *match.span())
    return "".join(characters)


def strip_css_comments(source: str) -> str:
    characters = list(source)
    quote: str | None = None
    escaped = False
    index = 0
    while index < len(source):
        char = source[index]
        following = source[index + 1] if index + 1 < len(source) else ""
        if quote is not None:
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == quote:
                quote = None
            index += 1
            continue
        if char in {"'", '"'}:
            quote = char
            index += 1
            continue
        if char == "/" and following == "*":
            end = source.find("*/", index + 2)
            end = len(source) if end == -1 else end + 2
            mask_range(characters, index, end)
            index = end
            continue
        index += 1
    return "".join(characters)


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


@dataclass(frozen=True)
class MarkdownTarget:
    target: str
    is_asset: bool
    start: int
    end: int
    context: str = "Markdown"


def normalize_reference_label(label: str) -> str:
    return re.sub(r"\s+", " ", label.strip()).casefold()


def markdown_targets(source: str) -> list[MarkdownTarget]:
    targets: list[MarkdownTarget] = []
    for match in INLINE_MARKDOWN_TARGET_RE.finditer(source):
        group = "angle" if match.group("angle") is not None else "bare"
        targets.append(
            MarkdownTarget(
                target=match.group(group),
                is_asset=match.group("image") is not None,
                start=match.start(group),
                end=match.end(group),
            )
        )
    for match in MARKDOWN_AUTOLINK_RE.finditer(source):
        targets.append(
            MarkdownTarget(
                target=match.group("url"),
                is_asset=False,
                start=match.start("url"),
                end=match.end("url"),
            )
        )

    definitions: dict[str, list[tuple[str, int, int]]] = {}
    definition_spans: list[tuple[int, int]] = []
    for match in REFERENCE_DEFINITION_RE.finditer(source):
        group = "angle" if match.group("angle") is not None else "bare"
        label = normalize_reference_label(match.group("label"))
        definitions.setdefault(label, []).append(
            (match.group(group), match.start(group), match.end(group))
        )
        definition_spans.append(match.span())

    usage_is_asset: dict[str, bool] = {}
    for match in FULL_REFERENCE_USAGE_RE.finditer(source):
        label = normalize_reference_label(match.group("label") or match.group("text"))
        usage_is_asset[label] = usage_is_asset.get(label, False) or (
            match.group("image") is not None
        )
    for match in SHORTCUT_REFERENCE_USAGE_RE.finditer(source):
        if any(start <= match.start() < end for start, end in definition_spans):
            continue
        label = normalize_reference_label(match.group("label"))
        if label not in definitions:
            continue
        usage_is_asset[label] = usage_is_asset.get(label, False) or (
            match.group("image") is not None
        )

    for label, entries in definitions.items():
        for target, start, end in entries:
            targets.append(
                MarkdownTarget(
                    target=target,
                    is_asset=usage_is_asset.get(label, False),
                    start=start,
                    end=end,
                )
            )
    return sorted(targets, key=lambda target: (target.start, target.end, target.target))


def html_targets(source: str) -> list[MarkdownTarget]:
    targets: list[MarkdownTarget] = []
    for element in HTML_ELEMENT_RE.finditer(source):
        tag = element.group("tag")
        attributes = element.group("attributes")
        attributes_start = element.start("attributes")
        for attribute in HTML_TARGET_ATTRIBUTE_RE.finditer(attributes):
            name = attribute.group("attribute").casefold()
            url_group = next(
                group
                for group in ("quoted_url", "expression_url", "bare_url")
                if attribute.group(group) is not None
            )
            is_navigation = (
                tag.casefold() == "a" and name == "href"
            ) or (
                tag == "Link" and name in {"href", "to"}
            )
            targets.append(
                MarkdownTarget(
                    target=attribute.group(url_group),
                    is_asset=not is_navigation,
                    start=attributes_start + attribute.start(url_group),
                    end=attributes_start + attribute.end(url_group),
                    context="HTML",
                )
            )
    return targets


def published_document_targets(source: str, suffix: str) -> list[MarkdownTarget]:
    targets = markdown_targets(source)
    targets.extend(html_targets(source))
    if suffix == ".mdx":
        for match in STATIC_MDX_IMPORT_RE.finditer(source):
            targets.append(
                MarkdownTarget(
                    target=match.group("url"),
                    is_asset=True,
                    start=match.start("url"),
                    end=match.end("url"),
                    context="MDX import",
                )
            )
    return sorted(
        targets,
        key=lambda target: (target.start, target.end, target.target, target.context),
    )


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


def literal_statement_end(source: str, body_end: int) -> int | None:
    match = re.match(
        r"\s*(?:satisfies\s+Config)?\s*;",
        source[body_end:],
    )
    return body_end + match.end() if match else None


def contains_only_imports(source: str) -> bool:
    position = 0
    while position < len(source):
        while position < len(source) and source[position].isspace():
            position += 1
        if position == len(source):
            return True
        import_keyword = re.match(r"import\b", source[position:])
        if import_keyword is None:
            return False
        after_keyword = position + import_keyword.end()
        while after_keyword < len(source) and source[after_keyword].isspace():
            after_keyword += 1
        if after_keyword < len(source) and source[after_keyword] == "(":
            return False
        semicolon = source.find(";", position)
        if semicolon == -1:
            return False
        position = semicolon + 1
    return True


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
        statement_end = literal_statement_end(source, body_end)
        if statement_end is None:
            return None
        if not contains_only_imports(code[: exports[0].start()]):
            return None
        return body if not code[statement_end:].strip() else None

    identifier = re.match(r"[A-Za-z_$][A-Za-z0-9_$]*", code[position:])
    if identifier is None:
        return None
    identifier_name = identifier.group(0)
    identifier_end = position + identifier.end()
    export_trailing = re.match(r"\s*;", code[identifier_end:])
    if export_trailing is None:
        return None
    export_statement_end = identifier_end + export_trailing.end()
    if code[export_statement_end:].strip():
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
    declaration_statement_end = literal_statement_end(source, body_end)
    if declaration_statement_end is None:
        return None
    if not contains_only_imports(code[: declarations[0].start()]):
        return None
    if declaration_statement_end > exports[0].start():
        return None
    if code[declaration_statement_end : exports[0].start()].strip():
        return None
    return body


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


def is_exact_https_navigation_path(
    url: str,
    host: str,
    path_root: str,
    *,
    include_root_without_slash: bool,
) -> bool:
    parsed = urlsplit(url)
    if parsed.scheme != "https" or parsed.netloc.casefold() != host:
        return False
    path = parsed.path
    if unquote(path) != path or "\\" in path:
        return False
    if any(segment in {".", ".."} for segment in path.split("/")):
        return False
    root_without_slash = path_root.rstrip("/")
    return (
        (include_root_without_slash and path == root_without_slash)
        or path == path_root
        or path.startswith(path_root)
    )


def is_allowed_navigation(url: str) -> bool:
    return is_exact_https_navigation_path(
        url,
        "github.com",
        "/un3qual/jazz/",
        include_root_without_slash=True,
    ) or is_exact_https_navigation_path(
        url,
        "un3qual.github.io",
        "/jazz/",
        include_root_without_slash=False,
    )


def navigation_url_spans(text: str, suffix: str) -> set[tuple[int, int]]:
    spans: set[tuple[int, int]] = set()
    for target in html_targets(text):
        if not target.is_asset and is_allowed_navigation(target.target):
            spans.add((target.start, target.end))
    if suffix in {".md", ".mdx"}:
        for target in markdown_targets(text):
            if not target.is_asset and is_allowed_navigation(target.target):
                spans.add((target.start, target.end))
    return spans


def has_protocol_relative_resource(text: str, suffix: str) -> bool:
    patterns: list[re.Pattern[str]] = []
    if suffix in {".css", ".js", ".jsx", ".scss", ".tsx"}:
        patterns.append(PROTOCOL_RELATIVE_CSS_RE)
    if suffix in {".js", ".jsx", ".tsx"}:
        patterns.extend((PROTOCOL_RELATIVE_MARKUP_RE, PROTOCOL_RELATIVE_IMPORT_RE))
    if suffix in {".md", ".mdx"}:
        patterns.extend((PROTOCOL_RELATIVE_MARKDOWN_RE, PROTOCOL_RELATIVE_MARKUP_RE))
        if any(target.target.startswith("//") for target in markdown_targets(text)):
            return True
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
        suffix = path.suffix.casefold()
        if suffix in AUTHORED_URL_SUFFIXES:
            url_source = text
            if suffix in {".js", ".jsx", ".tsx"}:
                url_source = strip_javascript_comments(url_source)
            elif suffix in {".css", ".scss"}:
                url_source = strip_css_comments(url_source)
            elif suffix in {".md", ".mdx"}:
                url_source = mask_markdown_noncontent(url_source)
            if has_forbidden_remote_url(url_source, suffix):
                violations.append(f"{path_label}: remote authored URL is not allowed")


def check_local_markdown_target(
    root: Path,
    docs_root: Path,
    page: Path,
    target: MarkdownTarget,
    violations: list[str],
) -> None:
    raw_target = target.target.strip()
    parsed = urlsplit(raw_target)
    is_site_alias = target.context == "MDX import" and raw_target.startswith(
        "@site/"
    )
    if (
        target.context == "MDX import"
        and not raw_target.startswith((".", "/"))
        and not is_site_alias
        and not (parsed.scheme or parsed.netloc)
    ):
        return
    if raw_target.startswith("//") or parsed.scheme in {"http", "https"}:
        return
    if parsed.scheme or parsed.netloc:
        violations.append(f"{relative(root, page)}: remote authored URL is not allowed")
        return
    if not parsed.path:
        return

    path_text = unquote(parsed.path)
    page_label = relative(root, page)
    absolute_target = path_text.startswith("/")
    if is_site_alias:
        candidate = root / "website" / path_text.removeprefix("@site/")
        containment_root = docs_root.resolve()
    elif absolute_target:
        if target.context == "MDX import":
            candidate = docs_root / path_text.lstrip("/")
            containment_root = docs_root.resolve()
        elif not target.is_asset:
            if path_text in {"/", "/docs"} or path_text.startswith("/docs/"):
                return
            violations.append(
                f"{page_label}: local Markdown target is outside published routes: {raw_target}"
            )
            return
        else:
            candidate = root / "website/static" / path_text.lstrip("/")
            containment_root = (root / "website/static").resolve()
    else:
        candidate = page.parent / path_text
        containment_root = docs_root.resolve()

    resolved_candidate = candidate.resolve()
    target_description = (
        target.context
        if target.context == "MDX import"
        else f"{target.context} {'asset' if target.is_asset else 'link'}"
    )
    if not resolved_candidate.is_relative_to(containment_root):
        escape_description = (
            "Markdown target" if target.context == "Markdown" else target_description
        )
        violations.append(
            f"{page_label}: local {escape_description} escapes published docs: {raw_target}"
        )
        return
    if not resolved_candidate.is_file():
        violations.append(
            f"{page_label}: local {target_description} does not exist: {raw_target}"
        )


def published_doc_paths(docs_root: Path) -> tuple[list[Path], list[Path]]:
    pages: list[Path] = []
    symlinks: list[Path] = []
    for directory, directory_names, file_names in os.walk(
        docs_root,
        topdown=True,
        followlinks=False,
    ):
        directory_names[:] = sorted(directory_names)
        current = Path(directory)
        for name in directory_names:
            path = current / name
            if path.is_symlink():
                symlinks.append(path)
        for name in sorted(file_names):
            path = current / name
            if path.is_symlink():
                symlinks.append(path)
            elif path.suffix.casefold() in {".md", ".mdx"}:
                pages.append(path)
    return (
        sorted(pages, key=lambda path: path.as_posix()),
        sorted(symlinks, key=lambda path: path.as_posix()),
    )


def check_published_docs(root: Path, violations: list[str]) -> None:
    docs_root = root / "docs"
    if not docs_root.is_dir():
        violations.append("docs: configured public documentation directory is missing")
        return
    pages, symlinks = published_doc_paths(docs_root)
    for symlink in symlinks:
        violations.append(
            f"{relative(root, symlink)}: symlink is not allowed in published docs"
        )
    for page in pages:
        page_label = relative(root, page)
        text = read_utf8(page, root, violations)
        if text is None:
            continue
        authored = mask_markdown_noncontent(text)
        folded_authored = authored.casefold()
        for forbidden in FORBIDDEN_SOURCE_REFERENCES:
            if forbidden.casefold() in folded_authored:
                violations.append(
                    f"{page_label}: forbidden publication source reference: {forbidden}"
                )
        suffix = page.suffix.casefold()
        if has_forbidden_remote_url(authored, suffix):
            violations.append(f"{page_label}: remote authored URL is not allowed")
        for target in published_document_targets(authored, suffix):
            check_local_markdown_target(
                root,
                docs_root,
                page,
                target,
                violations,
            )


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
        check_published_docs(root, violations)
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
