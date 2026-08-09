#!/usr/bin/env python3
"""Extract rendered link, image, HTML, and heading targets from Markdown."""

from __future__ import annotations

import re
import sys
from dataclasses import dataclass
from html import unescape as html_unescape
from html.parser import HTMLParser

from markdown_visibility import (
    blank_range,
    container_relative_markdown,
    is_thematic_break,
    line_end_offset,
    rendered_html_source_markdown,
    rendered_markdown,
    without_indented_code_blocks,
)


FULL_REFERENCE_RE = re.compile(r"\[([^\]]+)\]\[([^\]]*)\]")
INERT_HTML_CONTENT_TAGS = frozenset({"script", "style", "template", "textarea"})
VOID_HTML_TAGS = frozenset(
    {
        "area",
        "base",
        "br",
        "col",
        "embed",
        "hr",
        "img",
        "input",
        "link",
        "meta",
        "param",
        "source",
        "track",
        "wbr",
    }
)
ATX_HEADING_RE = re.compile(r"^[ \t]{0,3}#{1,6}(?:[ \t]+|$)(.*?)[ \t]*$", re.MULTILINE)
ATX_HEADING_LEVEL_RE = re.compile(
    r"^[ \t]{0,3}(#{1,6})(?:[ \t]+|$)", re.MULTILINE
)
SETEXT_HEADING_UNDERLINE_RE = re.compile(r"^[ \t]{0,3}(=+|-+)[ \t]*$")
EXPLICIT_HEADING_ID_RE = re.compile(r"[ \t]+\{#([A-Za-z][A-Za-z0-9_.:-]*)\}[ \t]*$")
MARKDOWN_AUTOLINK_RE = re.compile(
    r"<((?:[A-Za-z][A-Za-z0-9+.-]{1,31}:[^<>\s]*|"
    r"[A-Za-z0-9.!#$%&'*+/=?^_`{|}~-]+@"
    r"[A-Za-z0-9](?:[A-Za-z0-9.-]*[A-Za-z0-9])?))>"
)
_ESCAPABLE_MARKDOWN_PUNCTUATION = frozenset("!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")
_UNDERSCORE_EMPHASIS_RE = re.compile(
    r"(?<!\w)(_{1,2})(?=\S)(.+?)(?<=\S)\1(?!\w)", re.DOTALL
)
_CODE_SPAN_RE = re.compile(r"(?<!`)(`+)(?!`)(.+?)(?<!`)\1(?!`)", re.DOTALL)


def unescape_markdown_punctuation(text: str) -> str:
    """Decode CommonMark backslash escapes without changing other slashes."""
    unescaped: list[str] = []
    index = 0
    while index < len(text):
        if (
            text[index] == "\\"
            and index + 1 < len(text)
            and text[index + 1] in _ESCAPABLE_MARKDOWN_PUNCTUATION
        ):
            unescaped.append(text[index + 1])
            index += 2
            continue
        unescaped.append(text[index])
        index += 1
    return "".join(unescaped)


def decode_markdown_escapes_and_html_entities(text: str) -> str:
    """Decode source in the order used when Markdown produces rendered HTML."""
    return html_unescape(unescape_markdown_punctuation(text))


class _HtmlVisibleTextParser(HTMLParser):
    def __init__(self) -> None:
        super().__init__(convert_charrefs=True)
        self.parts: list[str] = []

    def handle_data(self, data: str) -> None:
        self.parts.append(data)


def html_visible_text(text: str) -> str:
    parser = _HtmlVisibleTextParser()
    parser.feed(MARKDOWN_AUTOLINK_RE.sub(r"\1", text))
    parser.close()
    return "".join(parser.parts)


def _decode_css_escapes(value: str) -> str:
    """Decode CSS escapes before comparing identifier-shaped declarations."""

    def replacement(match: re.Match[str]) -> str:
        hexadecimal = match.group(1)
        if hexadecimal is not None:
            codepoint = int(hexadecimal, 16)
            if (
                codepoint == 0
                or codepoint > sys.maxunicode
                or 0xD800 <= codepoint <= 0xDFFF
            ):
                return "\N{REPLACEMENT CHARACTER}"
            return chr(codepoint)
        if match.group(2) is not None:
            return ""
        return match.group(3)

    return re.sub(
        r"\\(?:([0-9a-fA-F]{1,6})(?:\r\n|[ \t\r\n\f])?|" r"(\r\n|[\r\n\f])|(.))",
        replacement,
        value,
        flags=re.DOTALL,
    )


def _html_attributes_make_contract_inert(
    attributes: list[tuple[str, str | None]],
) -> bool:
    """Keep required contracts out of hidden or presentation-dependent HTML."""
    for name, value in attributes:
        folded_name = name.casefold()
        if folded_name == "hidden":
            return True
        if folded_name == "aria-hidden" and (value or "").casefold() == "true":
            return True
        if folded_name != "style":
            continue
        style = re.sub(r"/\*.*?\*/", "", value or "", flags=re.DOTALL)
        for declaration in style.split(";"):
            property_name, separator, property_value = declaration.partition(":")
            if not separator:
                continue
            property_name = _decode_css_escapes(property_name).strip().casefold()
            property_value = _decode_css_escapes(property_value)
            property_value = (
                re.sub(r"\s*!important\s*$", "", property_value, flags=re.IGNORECASE)
                .strip()
                .casefold()
            )
            if property_name == "display" and property_value == "none":
                return True
            if property_name == "visibility" and property_value == "hidden":
                return True
    return False


class _InertHtmlSubtreeMasker(HTMLParser):
    def __init__(self) -> None:
        super().__init__(convert_charrefs=False)
        self.parts: list[str] = []
        self.inert_depth = 0
        self.open_elements: list[tuple[str, bool]] = []

    def append(self, source: str, *, force_mask: bool = False) -> None:
        if not self.inert_depth and not force_mask:
            self.parts.append(source)
            return
        self.parts.append(
            "".join(character if character in "\r\n" else " " for character in source)
        )

    def handle_starttag(
        self, tag: str, attributes: list[tuple[str, str | None]]
    ) -> None:
        source = self.get_starttag_text() or f"<{tag}>"
        folded_tag = tag.casefold()
        introduces_inertness = (
            folded_tag in INERT_HTML_CONTENT_TAGS
            or _html_attributes_make_contract_inert(attributes)
        )
        if introduces_inertness:
            self.inert_depth += 1
        self.append(source)
        if folded_tag in VOID_HTML_TAGS:
            if introduces_inertness:
                self.inert_depth -= 1
            return
        self.open_elements.append((folded_tag, introduces_inertness))

    def handle_startendtag(
        self, tag: str, attributes: list[tuple[str, str | None]]
    ) -> None:
        source = self.get_starttag_text() or f"<{tag} />"
        introduces_inertness = (
            tag.casefold() in INERT_HTML_CONTENT_TAGS
            or _html_attributes_make_contract_inert(attributes)
        )
        self.append(source, force_mask=introduces_inertness)

    def handle_endtag(self, tag: str) -> None:
        self.append(f"</{tag}>")
        folded_tag = tag.casefold()
        matching_index = next(
            (
                index
                for index in range(len(self.open_elements) - 1, -1, -1)
                if self.open_elements[index][0] == folded_tag
            ),
            None,
        )
        if matching_index is None:
            return
        closed_elements = self.open_elements[matching_index:]
        del self.open_elements[matching_index:]
        self.inert_depth -= sum(
            introduces_inertness
            for _element_tag, introduces_inertness in closed_elements
        )

    def handle_data(self, data: str) -> None:
        self.append(data)

    def handle_entityref(self, name: str) -> None:
        self.append(f"&{name};")

    def handle_charref(self, name: str) -> None:
        self.append(f"&#{name};")


def without_inert_html_subtrees(text: str) -> str:
    parser = _InertHtmlSubtreeMasker()
    parser.feed(text)
    parser.close()
    return "".join(parser.parts)


def _normalize_reference_label(label: str) -> str:
    unescaped = unescape_markdown_punctuation(label)
    return re.sub(r"\s+", " ", unescaped.strip()).casefold()


def _reference_label_at(
    text: str, start: int, *, allow_empty: bool = False
) -> tuple[str, int] | None:
    """Parse one bracketed CommonMark reference label with backslash escapes."""
    if start >= len(text) or text[start] != "[":
        return None
    index = start + 1
    raw_label: list[str] = []
    while index < len(text):
        character = text[index]
        if (
            character == "\\"
            and index + 1 < len(text)
            and text[index + 1] in _ESCAPABLE_MARKDOWN_PUNCTUATION
        ):
            raw_label.extend((character, text[index + 1]))
            index += 2
            continue
        if character == "]":
            label = "".join(raw_label)
            if not allow_empty and not _normalize_reference_label(label):
                return None
            return label, index + 1
        if character == "[":
            return None
        raw_label.append(character)
        index += 1
    return None


def _skip_space_tabs(text: str, position: int) -> int:
    while position < len(text) and text[position] in " \t":
        position += 1
    return position


def _line_ending_end(text: str, position: int) -> int | None:
    if text.startswith("\r\n", position):
        return position + 2
    if position < len(text) and text[position] in "\r\n":
        return position + 1
    return None


def _link_destination_at(text: str, start: int) -> tuple[str, int] | None:
    if start >= len(text):
        return None
    if text[start] == "<":
        position = start + 1
        while position < len(text):
            character = text[position]
            if character in "\r\n<":
                return None
            if character == ">":
                return text[start + 1 : position], position + 1
            if character == "\\" and position + 1 < len(text):
                position += 2
            else:
                position += 1
        return None

    position = start
    parenthesis_depth = 0
    while position < len(text):
        character = text[position]
        if character == "\\" and position + 1 < len(text):
            position += 2
            continue
        if character.isspace() or ord(character) < 0x20:
            break
        if character == "<":
            return None
        if character == "(":
            parenthesis_depth += 1
        elif character == ")":
            if parenthesis_depth == 0:
                break
            parenthesis_depth -= 1
        position += 1
    if position == start or parenthesis_depth:
        return None
    return text[start:position], position


def _link_title_end(text: str, start: int) -> int | None:
    if start >= len(text) or text[start] not in "\"'(":
        return None
    opener = text[start]
    closer = ")" if opener == "(" else opener
    position = start + 1
    while position < len(text):
        character = text[position]
        if character == "\\" and position + 1 < len(text):
            position += 2
            continue
        if character == closer:
            return position + 1
        if opener == "(" and character == "(":
            return None
        if character in "\r\n":
            line_end = _line_ending_end(text, position)
            assert line_end is not None
            next_line_end = line_end_offset(text, line_end)
            if not text[line_end:next_line_end].strip(" \t\r\n"):
                return None
            position = line_end
            continue
        position += 1
    return None


@dataclass(frozen=True)
class _ParsedReferenceDefinition:
    label: str
    target: str
    end: int


def _reference_definition_at(
    text: str, line_start: int
) -> _ParsedReferenceDefinition | None:
    first_line_end = line_end_offset(text, line_start)
    position = line_start
    while (
        position < first_line_end
        and text[position] == " "
        and position - line_start < 3
    ):
        position += 1
    parsed_label = _reference_label_at(text, position)
    if parsed_label is None:
        return None
    raw_label, position = parsed_label
    if position >= len(text) or text[position] != ":":
        return None

    position = _skip_space_tabs(text, position + 1)
    line_break_end = _line_ending_end(text, position)
    if line_break_end is not None:
        position = _skip_space_tabs(text, line_break_end)
    destination = _link_destination_at(text, position)
    if destination is None:
        return None
    target, destination_end = destination

    after_destination = _skip_space_tabs(text, destination_end)
    title_start = (
        after_destination
        if after_destination > destination_end
        and after_destination < len(text)
        and text[after_destination] in "\"'("
        else None
    )
    if title_start is None:
        title_line_start = _line_ending_end(text, after_destination)
        if title_line_start is not None:
            continued_title = _skip_space_tabs(text, title_line_start)
            if continued_title < len(text) and text[continued_title] in "\"'(":
                title_start = continued_title

    if title_start is None:
        if (
            after_destination < len(text)
            and _line_ending_end(text, after_destination) is None
        ):
            return None
        definition_end = line_end_offset(text, destination_end)
    else:
        title_end = _link_title_end(text, title_start)
        if title_end is None:
            return None
        after_title = _skip_space_tabs(text, title_end)
        if after_title < len(text) and _line_ending_end(text, after_title) is None:
            return None
        definition_end = line_end_offset(text, title_end)

    return _ParsedReferenceDefinition(
        label=_normalize_reference_label(raw_label),
        target=target,
        end=definition_end,
    )


def _reference_definitions(
    text: str,
) -> tuple[dict[str, str], list[tuple[int, int]]]:
    """Collect first-wins definitions and every complete definition span."""
    definitions: dict[str, str] = {}
    spans: list[tuple[int, int]] = []
    line_start = 0
    while line_start < len(text):
        parsed = _reference_definition_at(text, line_start)
        if parsed is None:
            line_start = line_end_offset(text, line_start)
            continue
        definitions.setdefault(parsed.label, parsed.target)
        spans.append((line_start, parsed.end))
        line_start = parsed.end
    return definitions, spans


def _is_escaped_markdown_character(text: str, position: int) -> bool:
    backslashes = 0
    position -= 1
    while position >= 0 and text[position] == "\\":
        backslashes += 1
        position -= 1
    return backslashes % 2 == 1


def _inline_link_end(text: str, start: int) -> int | None:
    if start >= len(text) or text[start] != "(":
        return None
    position = _skip_space_tabs(text, start + 1)
    first_line_break = _line_ending_end(text, position)
    if first_line_break is not None:
        position = _skip_space_tabs(text, first_line_break)

    if position < len(text) and text[position] != ")":
        destination = _link_destination_at(text, position)
        if destination is None:
            return None
        _target, position = destination

    separator_start = position
    position = _skip_space_tabs(text, position)
    title_line_break = _line_ending_end(text, position)
    if title_line_break is not None:
        position = _skip_space_tabs(text, title_line_break)
    has_title_separator = position != separator_start
    if has_title_separator and position < len(text) and text[position] in "\"'(":
        title_end = _link_title_end(text, position)
        if title_end is None:
            return None
        position = _skip_space_tabs(text, title_end)
        closing_line_break = _line_ending_end(text, position)
        if closing_line_break is not None:
            position = _skip_space_tabs(text, closing_line_break)
    return position + 1 if position < len(text) and text[position] == ")" else None


def _used_reference_target_usages(text: str) -> list[tuple[str, bool]]:
    structural_text = container_relative_markdown(text)
    definitions, definition_spans = _reference_definitions(structural_text)

    usage_text = list(structural_text)
    for start, end in definition_spans:
        blank_range(usage_text, start, end)
    usages = "".join(usage_text)

    used_labels: set[tuple[str, bool]] = set()
    position = 0
    while position < len(usages):
        if usages[position] != "[" or _is_escaped_markdown_character(usages, position):
            position += 1
            continue
        is_image = (
            position > 0
            and usages[position - 1] == "!"
            and not _is_escaped_markdown_character(usages, position - 1)
        )
        first_label = _reference_label_at(usages, position)
        if first_label is None:
            position += 1
            continue
        raw_first_label, first_end = first_label
        label = raw_first_label
        reference_end = first_end
        if first_end < len(usages) and usages[first_end] == "[":
            second_label = _reference_label_at(usages, first_end, allow_empty=True)
            if second_label is None:
                position = first_end
                continue
            raw_second_label, reference_end = second_label
            if raw_second_label:
                label = raw_second_label
        elif first_end < len(usages) and usages[first_end] == "(":
            inline_end = _inline_link_end(usages, first_end)
            position = inline_end if inline_end is not None else first_end + 1
            continue
        normalized_label = _normalize_reference_label(label)
        if normalized_label in definitions:
            used_labels.add((normalized_label, is_image))
        position = reference_end

    return sorted((definitions[label], is_image) for label, is_image in used_labels)


def used_reference_targets(text: str) -> list[str]:
    """Resolve link targets used by full, collapsed, and shortcut references."""
    return [
        target
        for target, is_image in _used_reference_target_usages(text)
        if not is_image
    ]


def used_reference_image_targets(text: str) -> list[str]:
    """Resolve image targets used by full, collapsed, and shortcut references."""
    return [
        target for target, is_image in _used_reference_target_usages(text) if is_image
    ]


def _markdown_heading_text(markup: str) -> str:
    text = re.sub(r"!?\[([^\]]*)\]\([^)]+\)", r"\1", markup)
    text = re.sub(r"\[([^\]]+)\]\[[^\]]*\]", r"\1", text)
    text = re.sub(r"\[([^\]]+)\]", r"\1", text)
    protected_literals: list[tuple[str, str]] = []

    def protect_literal(value: str) -> str:
        token = f"\ue000{len(protected_literals)}\ue001"
        protected_literals.append((token, value))
        return token

    text = _CODE_SPAN_RE.sub(lambda match: protect_literal(match.group(2)), text)

    def protect_escaped_underscore(match: re.Match[str]) -> str:
        backslash_count = len(match.group()) - 1
        return "\\" * (backslash_count // 2) + protect_literal("_")

    text = re.sub(
        r"(?<!\\)(?:\\\\)*\\_",
        protect_escaped_underscore,
        text,
    )
    while True:
        without_emphasis = _UNDERSCORE_EMPHASIS_RE.sub(r"\2", text)
        if without_emphasis == text:
            break
        text = without_emphasis
    for token, value in protected_literals:
        text = text.replace(token, value)
    text = text.replace("`", "")
    text = MARKDOWN_AUTOLINK_RE.sub(r"\1", text)
    return html_visible_text(text)


def _markdown_heading_slug(markup: str) -> str:
    heading = _markdown_heading_text(markup).lower()
    heading = re.sub(r"[^\w _-]", "", heading)
    heading = re.sub(r"\s+", "-", heading)
    return heading.strip("-")


class _HtmlHeadingParser(HTMLParser):
    def __init__(self) -> None:
        super().__init__(convert_charrefs=True)
        self.levels: set[int] = set()
        self.fragments: set[str] = set()

    def collect_heading(
        self, tag: str, attributes: list[tuple[str, str | None]]
    ) -> None:
        match = re.fullmatch(r"h([1-6])", tag.casefold())
        if match is None:
            return
        self.levels.add(int(match.group(1)))
        for name, value in attributes:
            if name.casefold() == "id" and value:
                self.fragments.add(value)
                break

    def handle_starttag(
        self, tag: str, attributes: list[tuple[str, str | None]]
    ) -> None:
        self.collect_heading(tag, attributes)

    def handle_startendtag(
        self, tag: str, attributes: list[tuple[str, str | None]]
    ) -> None:
        self.collect_heading(tag, attributes)


def rendered_heading_levels(text: str) -> set[int]:
    """Return heading levels produced by visible Markdown and raw HTML."""
    rendered = container_relative_markdown(
        without_indented_code_blocks(
            without_inert_html_subtrees(rendered_markdown(text))
        )
    )
    levels = {
        len(match.group(1)) for match in ATX_HEADING_LEVEL_RE.finditer(rendered)
    }
    lines = rendered.splitlines()
    for index, line in enumerate(lines[1:], 1):
        underline = SETEXT_HEADING_UNDERLINE_RE.fullmatch(line)
        previous = lines[index - 1]
        if underline is not None and previous.strip():
            levels.add(1 if underline.group(1).startswith("=") else 2)

    html_parser = _HtmlHeadingParser()
    html_parser.feed(
        without_inert_html_subtrees(rendered_html_source_markdown(text))
    )
    html_parser.close()
    levels.update(html_parser.levels)
    return levels


def _add_markdown_heading_fragment(
    heading: str,
    fragments: set[str],
    slug_counts: dict[str, int],
) -> None:
    explicit_id = EXPLICIT_HEADING_ID_RE.search(heading)
    if explicit_id is not None:
        fragments.add(explicit_id.group(1))
        return
    slug = _markdown_heading_slug(heading)
    if not slug:
        return
    duplicate_index = slug_counts.get(slug, 0)
    slug_counts[slug] = duplicate_index + 1
    fragments.add(slug if duplicate_index == 0 else f"{slug}-{duplicate_index}")


def _markdown_heading_fragments(rendered: str) -> set[str]:
    characters = list(rendered)
    _definitions, definition_spans = _reference_definitions(rendered)
    for start, end in definition_spans:
        blank_range(characters, start, end)

    fragments: set[str] = set()
    slug_counts: dict[str, int] = {}
    paragraph_lines: list[str] = []
    for line in "".join(characters).splitlines():
        atx_heading = ATX_HEADING_RE.fullmatch(line)
        if atx_heading is not None:
            paragraph_lines.clear()
            heading = re.sub(r"[ \t]+#+[ \t]*$", "", atx_heading.group(1))
            _add_markdown_heading_fragment(heading, fragments, slug_counts)
            continue

        setext_underline = SETEXT_HEADING_UNDERLINE_RE.fullmatch(line)
        if setext_underline is not None and paragraph_lines:
            _add_markdown_heading_fragment(
                " ".join(part.strip() for part in paragraph_lines),
                fragments,
                slug_counts,
            )
            paragraph_lines.clear()
            continue

        if not line.strip() or is_thematic_break(line):
            paragraph_lines.clear()
            continue
        paragraph_lines.append(line)
    return fragments


def rendered_heading_fragments(text: str) -> set[str]:
    rendered = container_relative_markdown(
        without_indented_code_blocks(
            without_inert_html_subtrees(rendered_markdown(text))
        )
    )
    fragments = _markdown_heading_fragments(rendered)

    html_parser = _HtmlHeadingParser()
    html_parser.feed(
        without_inert_html_subtrees(rendered_html_source_markdown(text))
    )
    html_parser.close()
    fragments.update(html_parser.fragments)
    return fragments


def _srcset_targets(value: str) -> list[str]:
    """Extract URL tokens from an HTML srcset candidate list."""
    targets: list[str] = []
    position = 0
    while position < len(value):
        while position < len(value) and (
            value[position].isspace() or value[position] == ","
        ):
            position += 1
        if position >= len(value):
            break

        start = position
        while position < len(value) and not value[position].isspace():
            position += 1
        target_with_separator = value[start:position]
        target = target_with_separator.rstrip(",")
        if target:
            targets.append(target)
        if target != target_with_separator:
            continue

        parenthesis_depth = 0
        while position < len(value):
            character = value[position]
            position += 1
            if character == "(":
                parenthesis_depth += 1
            elif character == ")" and parenthesis_depth:
                parenthesis_depth -= 1
            elif character == "," and not parenthesis_depth:
                break
    return targets


class _HtmlReferenceTargetParser(HTMLParser):
    def __init__(self, *, include_links: bool, include_images: bool) -> None:
        super().__init__(convert_charrefs=True)
        self.include_links = include_links
        self.include_images = include_images
        self.targets: list[str] = []
        self.inert_depth = 0
        self.open_elements: list[tuple[str, bool]] = []

    def handle_starttag(
        self, tag: str, attributes: list[tuple[str, str | None]]
    ) -> None:
        folded_tag = tag.casefold()
        introduces_inertness = (
            folded_tag in INERT_HTML_CONTENT_TAGS
            or _html_attributes_make_contract_inert(attributes)
        )
        if introduces_inertness:
            self.inert_depth += 1
        if not self.inert_depth:
            self.collect_target(folded_tag, attributes)
        if folded_tag in VOID_HTML_TAGS:
            if introduces_inertness:
                self.inert_depth -= 1
            return
        self.open_elements.append((folded_tag, introduces_inertness))

    def handle_startendtag(
        self, tag: str, attributes: list[tuple[str, str | None]]
    ) -> None:
        if self.inert_depth or _html_attributes_make_contract_inert(attributes):
            return
        self.collect_target(tag.casefold(), attributes)

    def collect_target(
        self, folded_tag: str, attributes: list[tuple[str, str | None]]
    ) -> None:
        for name, value in attributes:
            folded_name = name.casefold()
            if self.include_links and folded_tag == "a" and folded_name == "href":
                self.targets.append(value or "")
            if not self.include_images:
                continue
            if folded_tag == "img" and folded_name == "src":
                self.targets.append(value or "")
            if folded_tag in {"img", "source"} and folded_name == "srcset":
                self.targets.extend(_srcset_targets(value or ""))

    def handle_endtag(self, tag: str) -> None:
        folded_tag = tag.casefold()
        matching_index = next(
            (
                index
                for index in range(len(self.open_elements) - 1, -1, -1)
                if self.open_elements[index][0] == folded_tag
            ),
            None,
        )
        if matching_index is None:
            return
        closed_elements = self.open_elements[matching_index:]
        del self.open_elements[matching_index:]
        self.inert_depth -= sum(
            introduces_inertness
            for _element_tag, introduces_inertness in closed_elements
        )


def html_reference_targets(text: str, *, include_images: bool = True) -> list[str]:
    """Collect link and optional image targets from rendered raw HTML."""
    parser = _HtmlReferenceTargetParser(
        include_links=True,
        include_images=include_images,
    )
    parser.feed(text)
    parser.close()
    return parser.targets


def html_image_targets(text: str) -> list[str]:
    """Collect image targets from rendered raw HTML."""
    parser = _HtmlReferenceTargetParser(
        include_links=False,
        include_images=True,
    )
    parser.feed(text)
    parser.close()
    return parser.targets
