#!/usr/bin/env python3
"""CommonMark fence and rendered-content visibility helpers."""

from __future__ import annotations

import re
import sys
from dataclasses import dataclass
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
            return _ContainerLine(line, 0, containers)

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


def type_seven_html_block_can_start(text: str, line_start: int) -> bool:
    previous = previous_line_content(text, line_start)
    if previous is None or not previous.strip():
        return True
    if re.match(r"^ {0,3}#{1,6}(?:[ \t]+|$)", previous) is not None:
        return True
    if fence_opener(previous) is not None:
        return True
    if re.fullmatch(
        r" {0,3}(?:(?:\*[ \t]*){3,}|(?:_[ \t]*){3,}|(?:-[ \t]*){3,})",
        previous,
    ) is not None:
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
    prefix = text[line_start:opener_start]
    if len(prefix) > 3 or prefix.strip(" "):
        return None
    first_line_end = line_end_offset(text, line_start)
    candidate = without_line_ending(text[opener_start:first_line_end])
    block_spec = raw_html_block_spec(
        candidate,
        type_seven_can_start=type_seven_html_block_can_start(text, line_start),
    )
    if block_spec is None:
        return None

    mode, terminator = block_spec
    cursor = line_start
    while cursor < len(text):
        current_end = line_end_offset(text, cursor)
        line = without_line_ending(text[cursor:current_end])
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


def rendered_markdown_with_code(text: str) -> str:
    """Blank comments while preserving rendered prose and code content."""
    return markdown_visibility(
        text, mask_fenced_code=False, mask_inline_code=False
    )


def renderable_source_markdown(text: str) -> str:
    """Mask raw HTML blocks while preserving rendered fences and metadata comments."""
    return markdown_visibility(
        text,
        mask_fenced_code=False,
        mask_html_comments=False,
        mask_inline_code=False,
    )


def html_source_markdown(text: str) -> str:
    """Preserve HTML tags while masking Markdown code and comment decoys."""
    return markdown_visibility(
        text,
        mask_fenced_code=True,
        mask_inline_code=True,
        mask_raw_html_blocks=False,
    )


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
