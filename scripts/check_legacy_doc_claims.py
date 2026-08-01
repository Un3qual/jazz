#!/usr/bin/env python3

import argparse
from collections.abc import Iterator, Sequence
from pathlib import Path
import re
import sys


_FENCE_PATTERN = re.compile(r"^\s*(```|~~~)")
_HEADING_PATTERN = re.compile(r"^\s*#{1,6}\s+")
_LIST_ITEM_PATTERN = re.compile(r"^\s*(?:[-*+]|\d+[.)])\s+(.*)$")
_LEGACY_TREE_PATTERN = re.compile(
    r"\b(?:legacy|deleted|removed|former|pre-root-canonicalization)\s+"
    r"(?:(?:implementation|source|compiler|repository)\s+)?"
    r"(?:directories?|trees?|paths?)\b",
    re.IGNORECASE,
)
_OBSOLETE_IDENTITIES = ("jazz-" + "next", "jazz-" + "hs", "jazz" + "2")
_LIVE_READ_ONLY_PATTERN = re.compile(
    r"(?:\b(?:remain|remains|stay|stays|are|is)\b.*\bread[- ]only\b)"
    r"|(?:\bread[- ]only\b.*\b(?:remain|remains|stay|stays|are|is)\b)",
    re.IGNORECASE,
)
_ARCHIVE_CONTEXT_PATTERN = re.compile(
    r"\b(?:archive|archived|historical|history|tag|snapshot|snapshots)\b",
    re.IGNORECASE,
)


def _markdown_prose_blocks(source: str) -> Iterator[tuple[int, str]]:
    block_start = 0
    block_lines: list[str] = []
    in_fence = False

    def flush() -> tuple[int, str] | None:
        nonlocal block_start, block_lines
        if not block_lines:
            return None
        normalized = " ".join(" ".join(block_lines).split())
        result = (block_start, normalized)
        block_start = 0
        block_lines = []
        return result

    for line_number, raw_line in enumerate(source.splitlines(), start=1):
        if _FENCE_PATTERN.match(raw_line):
            block = flush()
            if block is not None:
                yield block
            in_fence = not in_fence
            continue
        if in_fence:
            continue

        stripped = raw_line.strip()
        if not stripped or _HEADING_PATTERN.match(raw_line):
            block = flush()
            if block is not None:
                yield block
            continue

        list_item = _LIST_ITEM_PATTERN.match(raw_line)
        if list_item is not None:
            block = flush()
            if block is not None:
                yield block
            block_start = line_number
            block_lines.append(list_item.group(1))
            continue

        if not block_lines:
            block_start = line_number
        block_lines.append(stripped.lstrip("> "))

    block = flush()
    if block is not None:
        yield block


def find_live_legacy_tree_claims(source: str) -> list[tuple[int, str]]:
    claims: list[tuple[int, str]] = []
    for line_number, block in _markdown_prose_blocks(source):
        lowered = block.casefold()
        has_legacy_subject = _LEGACY_TREE_PATTERN.search(block) is not None or any(
            identity in lowered for identity in _OBSOLETE_IDENTITIES
        )
        if (
            has_legacy_subject
            and _LIVE_READ_ONLY_PATTERN.search(block) is not None
            and _ARCHIVE_CONTEXT_PATTERN.search(block) is None
        ):
            claims.append((line_number, block))
    return claims


def _markdown_files(paths: Sequence[Path]) -> Iterator[Path]:
    for path in paths:
        if path.is_dir():
            yield from sorted(path.rglob("*.md"))
        elif path.suffix == ".md":
            yield path


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Reject live read-only claims about removed implementation trees."
    )
    parser.add_argument("paths", nargs="+", type=Path)
    args = parser.parse_args(argv)

    failed = False
    for path in _markdown_files(args.paths):
        for line_number, claim in find_live_legacy_tree_claims(
            path.read_text(encoding="utf-8")
        ):
            failed = True
            print(
                f"{path}:{line_number}: live legacy-tree claim: {claim}",
                file=sys.stderr,
            )
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
