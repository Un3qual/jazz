#!/usr/bin/env python3
"""Enforce the publication boundary for the Jazz Docusaurus website."""

from __future__ import annotations

import os
import re
import sys
from dataclasses import dataclass
from html import unescape
from pathlib import Path
from urllib.parse import unquote, urlsplit


CONFIG_PATH = "website/docusaurus.config.ts"
IGNORED_DIRECTORIES = {"node_modules", "build", ".docusaurus"}
AUTHORED_SITE_SUFFIXES = {
    ".css",
    ".cjs",
    ".html",
    ".js",
    ".jsx",
    ".md",
    ".mdx",
    ".mjs",
    ".ts",
    ".tsx",
}
UNSUPPORTED_STYLESHEET_SUFFIXES = {".less", ".sass", ".scss"}
TEXT_SOURCE_SUFFIXES = AUTHORED_SITE_SUFFIXES | UNSUPPORTED_STYLESHEET_SUFFIXES | {
    ".json", ".svg", ".txt"
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
NONLOCAL_URI_TOKEN_RE = re.compile(
    r"(?:\b(?:https?|ftp|file|data|javascript|vbscript|ws|wss|blob):"
    r"[^\s\"'<>)}]*|(?<!:)//[A-Za-z0-9][^\s\"'<>)}]*)",
    re.IGNORECASE,
)
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
MARKDOWN_AUTOLINK_RE = re.compile(
    r"<(?P<url>[A-Za-z][A-Za-z0-9+.-]*:[^>\s]+)>"
)
RESOURCE_ATTRIBUTE_RE = re.compile(
    r"\b(?P<attribute>src|href|to|poster|srcSet|action|formAction|cite|data|background|manifest)\s*=\s*",
    re.IGNORECASE,
)
STATIC_IMPORT_RE = re.compile(
    r"^[ \t]*import\b"
    r"(?:[ \t\r\n]+(?:(?!;)[\s\S])*?\bfrom)?"
    r"[ \t\r\n]*(?:"
    r"\"(?P<double_url>(?:\\.|[^\"\\\r\n])+)\""
    r"|'(?P<single_url>(?:\\.|[^'\\\r\n])+)'"
    r")"
    r"[ \t]*;?",
    re.MULTILINE,
)
DYNAMIC_IMPORT_RE = re.compile(
    r"\bimport\s*\(\s*(?:"
    r"\"(?P<double_url>(?:\\.|[^\"\\\r\n])+)\""
    r"|'(?P<single_url>(?:\\.|[^'\\\r\n])+)'"
    r")\s*\)",
    re.MULTILINE,
)
CONFIG_RESOURCE_PROPERTY_RE = re.compile(
    r"\b(?P<property>favicon|image|socialCard|src|poster|href|to|content|customCss|sidebarPath)\s*:",
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


def strip_javascript_comments(source: str) -> str:
    """Mask comments while leaving quoted strings untouched and offsets stable."""

    characters = list(source)
    tokens = re.compile(
        r'"(?:\\.|[^"\\])*"|\'(?:\\.|[^\'\\])*\'|`(?:\\.|[^`\\])*`'
        r"|(?P<comment>//[^\r\n]*|/\*.*?\*/)",
        re.DOTALL,
    )
    for match in tokens.finditer(source):
        if match.group("comment") is not None:
            mask_range(characters, *match.span())
    return "".join(characters)


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


def quoted_value(source: str, opening: int) -> tuple[str, int, int] | None:
    """Read one quote-delimited value, preserving escapes for validation."""

    quote = source[opening]
    cursor = opening + 1
    while cursor < len(source):
        if source[cursor] == "\\":
            cursor += 2
            continue
        if source[cursor] == quote:
            return source[opening + 1 : cursor], opening + 1, cursor
        cursor += 1
    return None


def srcset_targets(value: str) -> list[str]:
    """Return URL tokens from a literal srcSet value."""

    decoded = unescape(value).strip()
    if decoded.startswith("//") or urlsplit(decoded).scheme:
        return [decoded]
    return [
        candidate.strip().split(maxsplit=1)[0]
        for candidate in decoded.split(",")
        if candidate.strip()
    ]


def html_resource_analysis(
    source: str,
    *,
    allow_bare: bool,
) -> tuple[list[MarkdownTarget], bool, bool]:
    targets: list[MarkdownTarget] = []
    dynamic_value, has_spread = False, False
    for tag, attributes, attributes_start in html_elements(source):
        rel_match = re.search(
            r"\brel\s*=\s*(?:\"([^\"]*)\"|'([^']*)'|([^\s\"'=<>`{}]+))",
            attributes,
            re.IGNORECASE,
        )
        rel_value = (
            next(value for value in rel_match.groups() if value is not None).casefold()
            if rel_match else ""
        )
        has_spread |= bool(re.search(r"\{\s*\.\.\.", attributes))
        for attribute in RESOURCE_ATTRIBUTE_RE.finditer(attributes):
            name = attribute.group("attribute")
            value_start = attribute.end()
            if value_start >= len(attributes):
                dynamic_value = True
                continue
            if attributes[value_start] in {'"', "'"}:
                result = quoted_value(attributes, value_start)
                if result is None:
                    dynamic_value = True
                    continue
                value, local_start, local_end = result
            elif allow_bare and attributes[value_start] not in "{`":
                bare = re.match(r"[^\s\"'=<>`{}]+", attributes[value_start:])
                if bare is None:
                    dynamic_value = True
                    continue
                value = bare.group(0)
                local_start = value_start
                local_end = value_start + bare.end()
            else:
                dynamic_value = True
                continue
            folded_tag = tag.casefold()
            folded_name = name.casefold()
            is_navigation = (folded_tag == "a" and folded_name == "href") or (
                tag == "Link" and folded_name in {"href", "to"}
            ) or (
                folded_tag == "link"
                and folded_name == "href"
                and bool({"alternate", "canonical"} & set(rel_value.split()))
            )
            values = srcset_targets(value) if folded_name == "srcset" else [value]
            targets.extend(
                MarkdownTarget(
                    target,
                    not is_navigation,
                    attributes_start + local_start,
                    attributes_start + local_end,
                    "HTML",
                )
                for target in values
            )
    return targets, dynamic_value, has_spread


def html_elements(source: str) -> list[tuple[str, str, int]]:
    """Return start tags without treating quoted or braced `>` as terminators."""

    elements: list[tuple[str, str, int]] = []
    index = 0
    while index < len(source):
        opening = source.find("<", index)
        if opening == -1:
            break
        tag_match = re.match(r"[A-Za-z][A-Za-z0-9.:_-]*", source[opening + 1 :])
        if tag_match is None:
            index = opening + 1
            continue

        tag = tag_match.group(0)
        attributes_start = opening + 1 + tag_match.end()
        quote: str | None = None
        escaped = False
        brace_depth = 0
        cursor = attributes_start
        while cursor < len(source):
            char = source[cursor]
            if quote is not None:
                if escaped:
                    escaped = False
                elif char == "\\":
                    escaped = True
                elif char == quote:
                    quote = None
            elif char in {"'", '"', "`"}:
                quote = char
            elif char == "{":
                brace_depth += 1
            elif char == "}" and brace_depth:
                brace_depth -= 1
            elif char == ">" and brace_depth == 0:
                elements.append(
                    (tag, source[attributes_start:cursor], attributes_start)
                )
                cursor += 1
                break
            cursor += 1
        index = max(opening + 1, cursor)
    return elements


def css_resource_targets(source: str) -> list[str]:
    """Scan CSS url() and @import literals with quote/escape awareness."""

    targets: list[str] = []
    cursor = 0
    while cursor < len(source):
        token = re.search(r"(?i)(?<![-\w])url\s*\(|@import\b", source[cursor:])
        if token is None:
            break
        start = cursor + token.start()
        cursor += token.end()
        is_url = source[start:cursor].lstrip().casefold().startswith("url")
        while cursor < len(source) and source[cursor].isspace():
            cursor += 1
        if not is_url and source[cursor : cursor + 3].casefold() == "url":
            continue
        if cursor >= len(source):
            break
        if source[cursor] in {'"', "'"}:
            result = quoted_value(source, cursor)
            if result is None:
                break
            value, _, end = result
            targets.append(value)
            cursor = end + 1
            continue
        if not is_url:
            start = cursor
            while cursor < len(source) and source[cursor] not in ";\r\n\t ":
                cursor += 1
            if cursor > start:
                targets.append(source[start:cursor])
            continue
        start = cursor
        escaped = False
        while cursor < len(source):
            char = source[cursor]
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == ")" or char.isspace():
                break
            cursor += 1
        if cursor > start:
            targets.append(source[start:cursor])
    return targets


def import_occurrences(source: str) -> list[tuple[str, int, int]]:
    occurrences: list[tuple[str, int, int]] = []
    for pattern in (STATIC_IMPORT_RE, DYNAMIC_IMPORT_RE):
        for match in pattern.finditer(source):
            group = "double_url" if match.group("double_url") else "single_url"
            occurrences.append((match.group(group), *match.span(group)))
    return occurrences


def config_resource_targets(source: str) -> tuple[list[MarkdownTarget], bool]:
    code = mask_javascript_strings(source)
    targets = [
        MarkdownTarget(value, True, start, end, "import")
        for value, start, end in import_occurrences(source)
    ]
    invalid = False
    for match in CONFIG_RESOURCE_PROPERTY_RE.finditer(code):
        opening = match.end()
        while opening < len(source) and source[opening].isspace():
            opening += 1
        if opening >= len(source) or source[opening] not in {'"', "'", "`"}:
            invalid = True
            continue
        result = quoted_value(source, opening)
        if result is None:
            invalid = True
            continue
        value, start, end = result
        trailing = end + 1 + len(source[end + 1 :]) - len(source[end + 1 :].lstrip())
        if (source[opening] == "`" and "${" in value) or (
            trailing < len(source) and source[trailing] not in ",}]"
        ):
            invalid = True
            continue
        is_navigation = match.group("property").casefold() in {"href", "to"}
        targets.append(
            MarkdownTarget(value, not is_navigation, start, end, "config")
        )
    return targets, invalid


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


def target_violation(
    target: MarkdownTarget,
    *,
    site_source: bool,
    root: Path | None = None,
) -> str | None:
    raw_target = target.target.strip()
    decoded = unescape(raw_target)
    percent_decoded = unquote(decoded)
    if "\\" in raw_target or "\\" in decoded or "\\" in percent_decoded:
        return f"local target contains a backslash: {raw_target}"

    if decoded.startswith("@site/"):
        if not site_source or target.context != "import":
            return f"@site is authorized only for site source imports: {raw_target}"
        alias_path = urlsplit(percent_decoded).path
        if not alias_path.startswith("@site/static/") or any(
            segment in {".", ".."} for segment in alias_path.split("/")
        ):
            return f"site import uses an unauthorized @site root: {raw_target}"
        if root is not None:
            static_root = (root / "website/static").resolve()
            candidate = (root / "website" / alias_path.removeprefix("@site/")).resolve()
            if not candidate.is_relative_to(static_root) or not candidate.is_file():
                return f"site import does not resolve to a contained static asset: {raw_target}"
        return None

    parsed = urlsplit(decoded)
    if not (decoded.startswith("//") or parsed.scheme or parsed.netloc):
        return None
    if not target.is_asset and is_allowed_navigation(decoded):
        return None
    return "remote authored URL is not allowed"


def authored_target_analysis(
    text: str, suffix: str, *, root: Path
) -> tuple[list[str], list[MarkdownTarget]]:
    targets = [
        MarkdownTarget(value, True, -1, -1, "CSS")
        for value in css_resource_targets(text)
    ]
    if suffix in {".cjs", ".js", ".jsx", ".mjs", ".ts", ".tsx", ".mdx"}:
        targets.extend(
            MarkdownTarget(value, True, -1, -1, "import")
            for value, _, _ in import_occurrences(text)
        )
    if suffix in {".md", ".mdx"}:
        targets.extend(markdown_targets(text))
    violations: list[str] = []
    if suffix != ".css":
        html, dynamic_value, has_spread = html_resource_analysis(
            text, allow_bare=suffix in {".html", ".md", ".mdx"}
        )
        targets.extend(html)
        if dynamic_value:
            violations.append("resource attributes must use direct static literals")
        if has_spread:
            violations.append(
                "JSX/HTML spreads are forbidden by the static resource profile"
            )
    allowed_spans = {
        (target.start, target.end)
        for target in targets
        if not target.is_asset and is_allowed_navigation(unescape(target.target.strip()))
    }
    if any(
        match.span() not in allowed_spans
        for match in NONLOCAL_URI_TOKEN_RE.finditer(text)
    ):
        violations.append("nonlocal URI scheme tokens are forbidden")
    for target in targets:
        violation = target_violation(target, site_source=True, root=root)
        if violation is not None:
            violations.append(violation)
            decoded = unescape(target.target.strip())
            if "backslash" in violation and (
                decoded.startswith("//") or urlsplit(decoded).scheme
            ):
                violations.append("remote authored URL is not allowed")
    return sorted(set(violations)), targets


def check_config(root: Path, violations: list[str]) -> None:
    config_path = root / CONFIG_PATH
    if not config_path.is_file():
        violations.append(f"{CONFIG_PATH}: required Docusaurus configuration is missing")
        return
    raw_config = read_utf8(config_path, root, violations)
    if raw_config is None:
        return
    config = strip_javascript_comments(raw_config)
    resource_targets, dynamic_resource = config_resource_targets(config)
    allowed_nonlocal_spans = {
        (target.start, target.end)
        for target in resource_targets
        if not target.is_asset and is_allowed_navigation(unescape(target.target.strip()))
    }
    if any(
        match.span() not in allowed_nonlocal_spans
        and not (
            match.group() == "https://un3qual.github.io"
            and re.search(r"\burl\s*:\s*['\"]$", config[: match.start()])
        )
        for match in NONLOCAL_URI_TOKEN_RE.finditer(raw_config)
    ):
        violations.append(f"{CONFIG_PATH}: nonlocal URI scheme tokens are forbidden")
    if dynamic_resource:
        violations.append(f"{CONFIG_PATH}: config resource properties must use direct static literals")
    if "..." in mask_javascript_strings(config):
        violations.append(f"{CONFIG_PATH}: config resource spreads are forbidden")
    config_body = default_export_config_body(config)
    if config_body is None:
        violations.append(
            f"{CONFIG_PATH}: default export must resolve unambiguously to a literal Config object"
        )
        return

    for target in resource_targets:
        violation = target_violation(target, site_source=True, root=root)
        if violation is not None:
            violations.append(f"{CONFIG_PATH}: {violation}")

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
    markdown_format_values = [
        string_expression_value(expression)
        for expression in property_expressions(markdown or "", "format")
    ]
    if markdown_format_values != ["md"]:
        violations.append(f"{CONFIG_PATH}: markdown format must be exactly md")
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
        if suffix in UNSUPPORTED_STYLESHEET_SUFFIXES:
            violations.append(
                f"{path_label}: unsupported stylesheet dialect; author plain .css"
            )
        if suffix in AUTHORED_SITE_SUFFIXES and path_label != CONFIG_PATH:
            url_source = text
            if suffix in {".md", ".mdx"}:
                url_source = mask_markdown_noncontent(url_source)
            found, _ = authored_target_analysis(
                url_source,
                suffix,
                root=root,
            )
            for violation in found:
                violations.append(f"{path_label}: {violation}")


def check_local_markdown_target(
    root: Path,
    docs_root: Path,
    page: Path,
    target: MarkdownTarget,
    violations: list[str],
) -> None:
    raw_target = target.target.strip()
    page_label = relative(root, page)
    boundary_violation = target_violation(target, site_source=False)
    if boundary_violation is not None:
        violations.append(f"{page_label}: {boundary_violation}")
        return

    decoded = unescape(raw_target)
    parsed = urlsplit(decoded)
    if decoded.startswith("//") or parsed.scheme or parsed.netloc:
        return
    if not parsed.path:
        return

    path_text = unquote(parsed.path)
    absolute_target = path_text.startswith("/")
    if absolute_target:
        if not target.is_asset:
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
    target_description = f"{target.context} {'asset' if target.is_asset else 'link'}"
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


def published_doc_paths(docs_root: Path) -> tuple[list[Path], list[Path], list[Path]]:
    pages, mdx_pages, symlinks = [], [], []
    for directory, directory_names, file_names in os.walk(docs_root, followlinks=False):
        current = Path(directory)
        for name in directory_names + file_names:
            path = current / name
            if path.is_symlink():
                symlinks.append(path)
            elif path.is_file() and path.suffix.casefold() == ".md":
                pages.append(path)
            elif path.is_file() and path.suffix.casefold() == ".mdx":
                mdx_pages.append(path)
    key = lambda path: path.as_posix()
    return sorted(pages, key=key), sorted(mdx_pages, key=key), sorted(symlinks, key=key)


def front_matter_selects_mdx(source: str) -> bool:
    block = re.match(r"\A---[ \t]*\r?\n(.*?)\r?\n---[ \t]*(?:\r?\n|\Z)", source, re.DOTALL)
    if block is None:
        return False
    for match in re.finditer(
        r"^\s*([A-Za-z0-9_.-]+)\s*:\s*(.*?)\s*$", block.group(1), re.MULTILINE
    ):
        key = match.group(1).casefold()
        value = match.group(2).strip().strip("'\"").casefold()
        if "mdx" in key or (key in {"format", "markdown.format"} and value != "md"):
            return True
    return False


def check_published_docs(root: Path, violations: list[str]) -> None:
    docs_root = root / "docs"
    if not docs_root.is_dir():
        violations.append("docs: configured public documentation directory is missing")
        return
    pages, mdx_pages, symlinks = published_doc_paths(docs_root)
    for symlink in symlinks:
        violations.append(
            f"{relative(root, symlink)}: symlink is not allowed in published docs"
        )
    for page in mdx_pages:
        violations.append(
            f"{relative(root, page)}: published documentation must use plain .md files; .mdx is forbidden"
        )
    for page in pages:
        page_label = relative(root, page)
        text = read_utf8(page, root, violations)
        if text is None:
            continue
        if front_matter_selects_mdx(text):
            violations.append(
                f"{page_label}: front matter must not enable or detect MDX"
            )
        authored = mask_markdown_noncontent(text)
        folded_authored = authored.casefold()
        for forbidden in FORBIDDEN_SOURCE_REFERENCES:
            if forbidden.casefold() in folded_authored:
                violations.append(
                    f"{page_label}: forbidden publication source reference: {forbidden}"
                )
        found, targets = authored_target_analysis(authored, ".md", root=root)
        for violation in found:
            violations.append(f"{page_label}: {violation}")
        for target in (target for target in targets if target.context in {"Markdown", "HTML"}):
            check_local_markdown_target(
                root,
                docs_root,
                page,
                target,
                violations,
            )


def generated_target_loads_remote(target: str) -> bool:
    decoded = unescape(target.strip())
    parsed = urlsplit(decoded)
    return decoded.startswith("//") or bool(parsed.scheme and parsed.scheme.casefold() not in {"data", "blob"})


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
            suffix = path.suffix.casefold()
            text = contents.decode("utf-8", errors="replace")
            if suffix == ".css":
                targets = css_resource_targets(text)
            elif suffix == ".html":
                html = html_resource_analysis(text, allow_bare=True)[0]
                targets = [target.target for target in html if target.is_asset]
            else:
                continue
            if any(generated_target_loads_remote(target) for target in targets):
                violations.append(
                    f"{path_label}: generated output loads a remote resource"
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
