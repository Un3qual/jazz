#!/usr/bin/env python3
"""CommonMark fence and rendered-content visibility helpers."""

from __future__ import annotations

import re
from dataclasses import dataclass


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
    line_index = 0
    while line_index < len(lines):
        opener = fence_opener(lines[line_index])
        if opener is None:
            line_index += 1
            continue

        delimiter, minimum_length, indent, info = opener
        start = offsets[line_index]
        source_lines: list[str] = []
        line_index += 1
        closed = False
        while line_index < len(lines):
            if is_fence_closer(lines[line_index], delimiter, minimum_length):
                line_index += 1
                closed = True
                break
            source_lines.append(strip_fence_indent(lines[line_index], indent))
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


def visible_markdown(text: str) -> str:
    """Blank fenced/inline code and HTML comments before rendered checks."""
    hidden_ranges = [(fence.start, fence.end) for fence in markdown_fences(text)]
    hidden_ranges.extend(
        match.span()
        for match in re.finditer(r"<!--.*?(?:-->|\Z)", text, re.DOTALL)
    )
    characters = list(text)
    for start, end in hidden_ranges:
        for index in range(start, end):
            if characters[index] not in "\r\n":
                characters[index] = " "

    partially_visible = "".join(characters)
    index = 0
    while index < len(partially_visible):
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
                for hidden_index in range(index, closer_end):
                    if characters[hidden_index] not in "\r\n":
                        characters[hidden_index] = " "
                index = closer_end
                break
            closer_start += closer_length
        else:
            index += delimiter_length
    return "".join(characters)
