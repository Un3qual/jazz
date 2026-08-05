#!/usr/bin/env python3
"""CommonMark fence and rendered-content visibility helpers."""

from __future__ import annotations

import re
import sys
from dataclasses import dataclass
from html.parser import HTMLParser
from pathlib import Path


HTML_BLOCK_TAG_NAMES = """
address article aside base basefont blockquote body caption center col colgroup
dd details dialog dir div dl dt fieldset figcaption figure footer form frame
frameset h1 h2 h3 h4 h5 h6 head header hr html iframe legend li link main menu
menuitem nav noframes ol optgroup option p param search section summary table
tbody td tfoot th thead title tr track ul
""".split()
HTML_BLOCK_TAG_RE = re.compile(
    r"^</?(" + "|".join(HTML_BLOCK_TAG_NAMES) + r")(?=[ \t]|/?>|$)",
    re.IGNORECASE,
)
HTML_OPEN_TAG_RE = re.compile(
    r"^<[A-Za-z][A-Za-z0-9-]*"
    r"(?:[ \t]+[A-Za-z_:][A-Za-z0-9_.:-]*"
    r"(?:[ \t]*=[ \t]*(?:[^ \t\n\"'=<>`]+|'[^']*'|\"[^\"]*\"))?)*"
    r"[ \t]*/?>[ \t]*$"
)
HTML_CLOSING_TAG_RE = re.compile(
    r"^</[A-Za-z][A-Za-z0-9-]*[ \t]*>[ \t]*$"
)
INERT_HTML_CONTENT_TAGS = frozenset({"script", "style", "template", "textarea"})


@dataclass(frozen=True)
class MarkdownFence:
    start: int
    end: int
    source: str
    info: str | None
    closed: bool

    @property
    def is_jazz(self) -> bool:
        return self.info == "jazz"


def without_line_ending(line: str) -> str:
    if line.endswith("\r\n"):
        return line[:-2]
    if line.endswith(("\n", "\r")):
        return line[:-1]
    return line


def leading_spaces(line: str) -> int:
    return len(line) - len(line.lstrip(" "))


def leading_indentation_columns(line: str) -> int:
    columns = 0
    for character in line:
        if character == " ":
            columns += 1
        elif character == "\t":
            columns += 4 - (columns % 4)
        else:
            break
    return columns


@dataclass(frozen=True)
class _MarkdownContainer:
    kind: str
    continuation_indent: int = 0


@dataclass(frozen=True)
class _ContainerLine:
    content: str
    prefix_length: int
    containers: tuple[_MarkdownContainer, ...]


LIST_MARKER_RE = re.compile(r"(?:[*+-]|[0-9]{1,9}[.)])")
FULL_REFERENCE_RE = re.compile(r"\[([^\]]+)\]\[([^\]]*)\]")
_ESCAPABLE_MARKDOWN_PUNCTUATION = frozenset(
    "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
)


def is_thematic_break(line: str) -> bool:
    content = without_line_ending(line)
    return (
        re.fullmatch(
            r" {0,3}(?:(?:\*[ \t]*){3,}|(?:_[ \t]*){3,}|(?:-[ \t]*){3,})",
            content,
        )
        is not None
    )


def blockquote_prefix_end(line: str, start: int) -> int | None:
    content = without_line_ending(line)
    indent = leading_spaces(content[start:])
    if indent > 3:
        return None
    marker = start + indent
    if marker >= len(content) or content[marker] != ">":
        return None
    end = marker + 1
    if end < len(content) and content[end] in " \t":
        end += 1
    return end


def list_container_opener(
    line: str, start: int
) -> tuple[int, _MarkdownContainer] | None:
    content = without_line_ending(line)
    indent = leading_spaces(content[start:])
    if indent > 3:
        return None
    marker_start = start + indent
    marker = LIST_MARKER_RE.match(content, marker_start)
    if marker is None:
        return None
    marker_end = marker.end()
    if marker_end < len(content) and content[marker_end] not in " \t":
        return None

    marker_width = marker_end - marker_start
    if marker_end == len(content):
        padding = 1
        item_content_start = marker_end
    elif content[marker_end] == "\t":
        padding = 1
        item_content_start = marker_end + 1
    else:
        whitespace_end = marker_end
        while (
            whitespace_end < len(content)
            and content[whitespace_end] == " "
        ):
            whitespace_end += 1
        whitespace_width = whitespace_end - marker_end
        if whitespace_end == len(content):
            padding = 1
            item_content_start = whitespace_end
        elif whitespace_width <= 4:
            padding = whitespace_width
            item_content_start = whitespace_end
        else:
            padding = 1
            item_content_start = marker_end + 1

    return (
        item_content_start,
        _MarkdownContainer(
            kind="list",
            continuation_indent=indent + marker_width + padding,
        ),
    )


def match_container_prefix(
    line: str, start: int, container: _MarkdownContainer
) -> int | None:
    if container.kind == "blockquote":
        return blockquote_prefix_end(line, start)
    content = without_line_ending(line)
    end = start + container.continuation_indent
    if (
        end > len(content)
        or content[start:end] != " " * container.continuation_indent
    ):
        return None
    return end


class _MarkdownContainerScanner:
    def __init__(self) -> None:
        self.containers: list[_MarkdownContainer] = []

    def scan(self, line: str) -> _ContainerLine:
        content = without_line_ending(line)
        if not content.strip(" \t"):
            return _ContainerLine(line, 0, tuple(self.containers))

        position = 0
        matched = 0
        while matched < len(self.containers):
            next_position = match_container_prefix(
                line, position, self.containers[matched]
            )
            if next_position is None:
                del self.containers[matched:]
                break
            position = next_position
            matched += 1

        while True:
            blockquote_end = blockquote_prefix_end(line, position)
            if blockquote_end is not None:
                self.containers.append(_MarkdownContainer(kind="blockquote"))
                position = blockquote_end
                continue
            if is_thematic_break(line[position:]):
                break
            list_opener = list_container_opener(line, position)
            if list_opener is None:
                break
            position, list_container = list_opener
            self.containers.append(list_container)

        return _ContainerLine(
            line[position:], position, tuple(self.containers)
        )

    @staticmethod
    def scan_fence_line(
        line: str, containers: tuple[_MarkdownContainer, ...]
    ) -> _ContainerLine | None:
        content = without_line_ending(line)
        if not content.strip(" \t"):
            position = 0
            for container in containers:
                next_position = match_container_prefix(line, position, container)
                if next_position is None:
                    break
                position = next_position
            return _ContainerLine(line[position:], position, containers)

        position = 0
        for container in containers:
            next_position = match_container_prefix(line, position, container)
            if next_position is None:
                return None
            position = next_position
        return _ContainerLine(line[position:], position, containers)


def fence_opener(line: str) -> tuple[str, int, int, str | None] | None:
    content = without_line_ending(line)
    indent = leading_spaces(content)
    if indent > 3:
        return None
    candidate = content[indent:]
    if not candidate or candidate[0] not in ("`", "~"):
        return None

    delimiter = candidate[0]
    length = len(candidate) - len(candidate.lstrip(delimiter))
    if length < 3:
        return None

    info = candidate[length:]
    if delimiter == "`" and "`" in info:
        return None
    first_info_token = re.match(r"([^ \t]+)", info.strip(" \t"))
    return (
        delimiter,
        length,
        indent,
        first_info_token.group(1) if first_info_token is not None else None,
    )


def is_fence_closer(line: str, delimiter: str, minimum_length: int) -> bool:
    content = without_line_ending(line)
    indent = leading_spaces(content)
    if indent > 3:
        return False
    candidate = content[indent:]
    length = len(candidate) - len(candidate.lstrip(delimiter))
    return length >= minimum_length and candidate[length:].strip(" \t") == ""


def strip_fence_indent(line: str, indent: int) -> str:
    removable = min(leading_spaces(line), indent)
    return line[removable:]


def markdown_fences(text: str) -> list[MarkdownFence]:
    """Parse CommonMark-style fenced code blocks while preserving source text."""
    lines = text.splitlines(keepends=True)
    offsets: list[int] = []
    offset = 0
    for line in lines:
        offsets.append(offset)
        offset += len(line)

    fences: list[MarkdownFence] = []
    container_scanner = _MarkdownContainerScanner()
    line_index = 0
    while line_index < len(lines):
        container_line = container_scanner.scan(lines[line_index])
        opener = fence_opener(container_line.content)
        if opener is None:
            line_index += 1
            continue

        delimiter, minimum_length, indent, info = opener
        start = offsets[line_index] + container_line.prefix_length
        fence_containers = container_line.containers
        source_lines: list[str] = []
        line_index += 1
        closed = False
        while line_index < len(lines):
            fence_line = container_scanner.scan_fence_line(
                lines[line_index], fence_containers
            )
            if fence_line is None:
                break
            if is_fence_closer(
                fence_line.content, delimiter, minimum_length
            ):
                line_index += 1
                closed = True
                break
            source_lines.append(strip_fence_indent(fence_line.content, indent))
            line_index += 1

        end = offsets[line_index] if line_index < len(lines) else len(text)
        fences.append(
            MarkdownFence(
                start=start,
                end=end,
                source="".join(source_lines),
                info=info,
                closed=closed,
            )
        )

    return fences


def blank_range(characters: list[str], start: int, end: int) -> None:
    for index in range(start, end):
        if characters[index] not in "\r\n":
            characters[index] = " "


def raw_html_block_spec(
    candidate: str, *, type_seven_can_start: bool
) -> tuple[str, str | re.Pattern[str]] | None:
    type_one = re.match(
        r"^<(script|pre|style|textarea)(?=[ \t]|>|$)",
        candidate,
        re.IGNORECASE,
    )
    if type_one is not None:
        return (
            "marker",
            re.compile(rf"</{re.escape(type_one.group(1))}>", re.IGNORECASE),
        )
    if candidate.startswith("<?"):
        return "marker", "?>"
    if candidate.startswith("<![CDATA["):
        return "marker", "]]>"
    if re.match(r"^<![A-Za-z]", candidate) is not None:
        return "marker", ">"
    if HTML_BLOCK_TAG_RE.match(candidate) is not None:
        return "blank", ""
    if type_seven_can_start and (
        HTML_OPEN_TAG_RE.fullmatch(candidate) is not None
        or HTML_CLOSING_TAG_RE.fullmatch(candidate) is not None
    ):
        return "blank", ""
    return None


def line_end_offset(text: str, start: int) -> int:
    cursor = start
    while cursor < len(text):
        if text[cursor] == "\n":
            return cursor + 1
        if text[cursor] == "\r":
            return cursor + 2 if text[cursor : cursor + 2] == "\r\n" else cursor + 1
        cursor += 1
    return len(text)


def previous_line_content(text: str, line_start: int) -> str | None:
    if line_start == 0:
        return None
    previous_end = line_start - 1
    if previous_end > 0 and text[previous_end - 1 : line_start] == "\r\n":
        previous_end -= 1
    previous_start = max(
        text.rfind("\n", 0, previous_end),
        text.rfind("\r", 0, previous_end),
    ) + 1
    return text[previous_start:previous_end]


def type_seven_html_block_can_start(
    text: str,
    line_start: int,
    containers: tuple[_MarkdownContainer, ...] = (),
) -> bool:
    previous = previous_line_content(text, line_start)
    if previous is not None and containers:
        container_line = _MarkdownContainerScanner.scan_fence_line(
            previous, containers
        )
        previous = None if container_line is None else container_line.content
    if previous is None or not previous.strip():
        return True
    if re.match(r"^ {0,3}#{1,6}(?:[ \t]+|$)", previous) is not None:
        return True
    if fence_opener(previous) is not None:
        return True
    if is_thematic_break(previous):
        return True
    if re.fullmatch(r" {0,3}(?:=+|-+)[ \t]*", previous) is not None:
        return True
    if previous.startswith("\t") or leading_spaces(previous) >= 4:
        return True
    candidate = previous[leading_spaces(previous) :]
    return (
        leading_spaces(previous) <= 3
        and HTML_CLOSING_TAG_RE.fullmatch(candidate) is not None
    )


def raw_html_block_end(text: str, opener_start: int) -> tuple[int, int] | None:
    line_start = max(
        text.rfind("\n", 0, opener_start),
        text.rfind("\r", 0, opener_start),
    ) + 1
    first_line_end = line_end_offset(text, line_start)
    first_physical_line = text[line_start:first_line_end]
    container_line = _MarkdownContainerScanner().scan(first_physical_line)
    content_start = line_start + container_line.prefix_length
    prefix = text[content_start:opener_start]
    if len(prefix) > 3 or prefix.strip(" "):
        return None
    candidate = without_line_ending(text[opener_start:first_line_end])
    block_spec = raw_html_block_spec(
        candidate,
        type_seven_can_start=type_seven_html_block_can_start(
            text, line_start, container_line.containers
        ),
    )
    if block_spec is None:
        return None

    mode, terminator = block_spec
    cursor = line_start
    while cursor < len(text):
        current_end = line_end_offset(text, cursor)
        physical_line = text[cursor:current_end]
        if cursor == line_start:
            line = without_line_ending(
                physical_line[container_line.prefix_length :]
            )
        else:
            current_container_line = _MarkdownContainerScanner.scan_fence_line(
                physical_line, container_line.containers
            )
            if current_container_line is None:
                return line_start, cursor
            line = without_line_ending(current_container_line.content)
        if mode == "blank":
            if cursor != line_start and not line.strip():
                return line_start, cursor
        else:
            found = (
                terminator.search(line) is not None
                if isinstance(terminator, re.Pattern)
                else terminator in line
            )
            if found:
                return line_start, current_end
        cursor = current_end
    return line_start, len(text)


def markdown_visibility(
    text: str,
    *,
    mask_fenced_code: bool,
    mask_html_comments: bool = True,
    mask_inline_code: bool,
    mask_raw_html_blocks: bool = True,
) -> str:
    """Mask non-rendered Markdown while respecting delimiter nesting."""
    characters = list(text)
    fences = markdown_fences(text)
    fence_ends = {fence.start: fence.end for fence in fences}
    if mask_fenced_code:
        for fence in fences:
            blank_range(characters, fence.start, fence.end)

    partially_visible = "".join(characters)
    index = 0
    while index < len(partially_visible):
        fence_end = fence_ends.get(index)
        if fence_end is not None:
            index = fence_end
            continue
        if mask_html_comments and partially_visible.startswith("<!--", index):
            closer = partially_visible.find("-->", index + 4)
            comment_end = len(partially_visible) if closer < 0 else closer + 3
            blank_range(characters, index, comment_end)
            index = comment_end
            continue
        if mask_raw_html_blocks and partially_visible[index] == "<":
            html_block = raw_html_block_end(partially_visible, index)
            if html_block is not None:
                block_start, block_end = html_block
                blank_range(characters, block_start, block_end)
                index = block_end
                continue
        if partially_visible[index] != "`":
            index += 1
            continue
        preceding_backslashes = 0
        cursor = index - 1
        while cursor >= 0 and partially_visible[cursor] == "\\":
            preceding_backslashes += 1
            cursor -= 1
        if preceding_backslashes % 2 == 1:
            index += 1
            continue

        delimiter_length = 1
        while (
            index + delimiter_length < len(partially_visible)
            and partially_visible[index + delimiter_length] == "`"
        ):
            delimiter_length += 1
        closer_start = index + delimiter_length
        while closer_start < len(partially_visible):
            if partially_visible[closer_start] != "`":
                closer_start += 1
                continue
            closer_length = 1
            while (
                closer_start + closer_length < len(partially_visible)
                and partially_visible[closer_start + closer_length] == "`"
            ):
                closer_length += 1
            if closer_length == delimiter_length:
                closer_end = closer_start + closer_length
                if mask_inline_code:
                    blank_range(characters, index, closer_end)
                index = closer_end
                break
            closer_start += closer_length
        else:
            index += delimiter_length
    return "".join(characters)


def without_indented_code_blocks(text: str) -> str:
    """Mask CommonMark indented code while preserving source offsets."""
    characters = list(text)
    scanner = _MarkdownContainerScanner()
    offset = 0
    in_code_block = False
    follows_blank_line = True
    for line in text.splitlines(keepends=True):
        relative_line = without_line_ending(
            scanner.scan(line.expandtabs(4)).content
        )
        if not relative_line.strip(" \t"):
            if in_code_block:
                blank_range(characters, offset, offset + len(line))
            follows_blank_line = True
        elif leading_indentation_columns(relative_line) >= 4 and (
            in_code_block or follows_blank_line
        ):
            blank_range(characters, offset, offset + len(line))
            in_code_block = True
            follows_blank_line = False
        else:
            in_code_block = False
            follows_blank_line = False
        offset += len(line)
    return "".join(characters)


def _normalize_reference_label(label: str) -> str:
    unescaped: list[str] = []
    index = 0
    while index < len(label):
        if (
            label[index] == "\\"
            and index + 1 < len(label)
            and label[index + 1] in _ESCAPABLE_MARKDOWN_PUNCTUATION
        ):
            unescaped.append(label[index + 1])
            index += 2
            continue
        unescaped.append(label[index])
        index += 1
    return re.sub(r"\s+", " ", "".join(unescaped).strip()).casefold()


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


def _reference_definitions(
    text: str,
) -> tuple[dict[str, str], list[tuple[int, int]]]:
    """Collect first-wins definitions and every definition span."""
    definitions: dict[str, str] = {}
    spans: list[tuple[int, int]] = []
    line_start = 0
    while line_start < len(text):
        line_end = line_end_offset(text, line_start)
        position = line_start
        while (
            position < line_end
            and text[position] == " "
            and position - line_start < 3
        ):
            position += 1
        parsed_label = _reference_label_at(text, position)
        if parsed_label is None:
            line_start = line_end
            continue
        raw_label, position = parsed_label
        if position >= len(text) or text[position] != ":":
            line_start = line_end
            continue
        position += 1
        while position < len(text) and text[position] in " \t":
            position += 1
        if position < len(text) and text[position] in "\r\n":
            position = line_end
            while position < len(text) and text[position] in " \t":
                position += 1
        if position >= len(text):
            line_start = line_end
            continue
        if text[position] == "<":
            target_end = text.find(">", position + 1)
            if target_end < 0 or any(
                character in "\r\n"
                for character in text[position + 1 : target_end]
            ):
                line_start = line_end
                continue
            target = text[position + 1 : target_end]
            span_end = target_end + 1
        else:
            target_match = re.match(r"\S+", text[position:])
            if target_match is None:
                line_start = line_end
                continue
            target = target_match.group(0)
            span_end = position + len(target)
        label = _normalize_reference_label(raw_label)
        definitions.setdefault(label, target)
        spans.append((line_start, span_end))
        line_start = line_end
    return definitions, spans


def _is_escaped_markdown_character(text: str, position: int) -> bool:
    backslashes = 0
    position -= 1
    while position >= 0 and text[position] == "\\":
        backslashes += 1
        position -= 1
    return backslashes % 2 == 1


def used_reference_targets(text: str) -> list[str]:
    """Resolve targets used by full, collapsed, and shortcut references."""
    structural_text = container_relative_markdown(text)
    definitions, definition_spans = _reference_definitions(structural_text)

    usage_text = list(structural_text)
    for start, end in definition_spans:
        usage_text[start:end] = " " * (end - start)
    usages = "".join(usage_text)

    used_labels: set[str] = set()
    position = 0
    while position < len(usages):
        if usages[position] != "[" or _is_escaped_markdown_character(
            usages, position
        ):
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
            second_label = _reference_label_at(
                usages, first_end, allow_empty=True
            )
            if second_label is None:
                position = first_end
                continue
            raw_second_label, reference_end = second_label
            if raw_second_label:
                label = raw_second_label
        elif first_end < len(usages) and usages[first_end] == "(":
            position = first_end + 1
            continue
        normalized_label = _normalize_reference_label(label)
        if not is_image and normalized_label in definitions:
            used_labels.add(normalized_label)
        position = reference_end

    return sorted(definitions[label] for label in used_labels)


def visible_markdown(text: str) -> str:
    """Blank fenced/inline code and HTML comments before structural checks."""
    return markdown_visibility(
        text, mask_fenced_code=True, mask_inline_code=True
    )


def rendered_markdown(text: str) -> str:
    """Blank fenced code and comments while preserving visible inline code text."""
    return markdown_visibility(
        text, mask_fenced_code=True, mask_inline_code=False
    )


def container_relative_markdown(text: str) -> str:
    """Strip active CommonMark container prefixes from each physical line."""
    scanner = _MarkdownContainerScanner()
    return "".join(
        scanner.scan(line.expandtabs(4)).content
        for line in text.splitlines(keepends=True)
    )


def rendered_markdown_with_code(text: str) -> str:
    """Blank comments while preserving rendered prose and code content."""
    return markdown_visibility(
        text, mask_fenced_code=False, mask_inline_code=False
    )


def _example_metadata_source_markdown(text: str) -> str:
    """Mask comments except standalone example metadata outside code fences."""
    characters = list(text)
    fence_ends = {fence.start: fence.end for fence in markdown_fences(text)}
    position = 0
    while position < len(text):
        fence_end = fence_ends.get(position)
        if fence_end is not None:
            position = fence_end
            continue
        if not text.startswith("<!--", position):
            position += 1
            continue

        comment_start = position
        cursor = position + 4
        depth = 1
        maximum_depth = 1
        while depth:
            nested_start = text.find("<!--", cursor)
            closer = text.find("-->", cursor)
            if closer < 0:
                cursor = len(text)
                break
            if 0 <= nested_start < closer:
                depth += 1
                maximum_depth = max(maximum_depth, depth)
                cursor = nested_start + 4
                continue
            depth -= 1
            cursor = closer + 3

        comment = text[comment_start:cursor]
        is_standalone_metadata = maximum_depth == 1 and (
            "jazz-example:" in comment
            or "jazz-example-output:" in comment
        )
        if not is_standalone_metadata:
            blank_range(characters, comment_start, cursor)
        position = cursor
    return "".join(characters)


def renderable_source_markdown(text: str) -> str:
    """Mask raw HTML blocks while preserving rendered fences and metadata comments."""
    return _example_metadata_source_markdown(
        markdown_visibility(
            text,
            mask_fenced_code=False,
            mask_html_comments=False,
            mask_inline_code=False,
        )
    )


def html_source_markdown(text: str) -> str:
    """Preserve HTML tags while masking Markdown code and comment decoys."""
    return markdown_visibility(
        text,
        mask_fenced_code=True,
        mask_inline_code=True,
        mask_raw_html_blocks=False,
    )


def rendered_html_source_markdown(text: str) -> str:
    """Preserve rendered raw HTML while masking every Markdown code form."""
    return without_indented_code_blocks(html_source_markdown(text))


class _HtmlReferenceTargetParser(HTMLParser):
    def __init__(self, *, include_images: bool) -> None:
        super().__init__(convert_charrefs=True)
        self.include_images = include_images
        self.targets: list[str] = []
        self.inert_depth = 0

    def handle_starttag(
        self, tag: str, attributes: list[tuple[str, str | None]]
    ) -> None:
        folded_tag = tag.casefold()
        if folded_tag in INERT_HTML_CONTENT_TAGS:
            self.inert_depth += 1
            return
        if self.inert_depth:
            return
        target_attribute = "href" if folded_tag == "a" else None
        if self.include_images and folded_tag == "img":
            target_attribute = "src"
        if target_attribute is None:
            return
        for name, value in attributes:
            if name.casefold() == target_attribute:
                self.targets.append(value or "")

    def handle_endtag(self, tag: str) -> None:
        if tag.casefold() in INERT_HTML_CONTENT_TAGS and self.inert_depth:
            self.inert_depth -= 1


def html_reference_targets(
    text: str, *, include_images: bool = True
) -> list[str]:
    """Collect link and optional image targets from rendered raw HTML."""
    parser = _HtmlReferenceTargetParser(include_images=include_images)
    parser.feed(text)
    parser.close()
    return parser.targets


def main(argv: list[str]) -> int:
    modes = {
        "--preserve-inline-code": rendered_markdown,
        "--preserve-code": rendered_markdown_with_code,
    }
    if len(argv) != 3 or argv[1] not in modes:
        print(
            "usage: markdown_visibility.py "
            "(--preserve-inline-code|--preserve-code) MARKDOWN_FILE",
            file=sys.stderr,
        )
        return 2
    path = Path(argv[2])
    try:
        text = path.read_text(encoding="utf-8")
    except (OSError, UnicodeError) as exc:
        print(f"cannot read Markdown file {path}: {exc}", file=sys.stderr)
        return 1
    sys.stdout.write(modes[argv[1]](text))
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
