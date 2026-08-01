#!/usr/bin/env python3

import argparse
from collections.abc import Iterator, Sequence
from pathlib import Path
import re
import sys


_HEADING_PATTERN = re.compile(r"^\s*#{1,6}\s+")
_LIST_ITEM_PATTERN = re.compile(r"^\s*(?:[-*+]|\d+[.)])\s+(.*)$")
_CLAUSE_BOUNDARY_PATTERN = re.compile(
    r"(?<=[.!?;])\s+|,\s+(?=(?:but|however)\b)", re.IGNORECASE
)
_LEGACY_TREE_PATTERN = re.compile(
    r"\b(?:legacy|deleted|removed|former|pre-root-canonicalization)\s+"
    r"(?:(?:implementation|source|compiler|repository)\s+)?"
    r"(?:directories?|trees?|paths?)\b",
    re.IGNORECASE,
)
_OBSOLETE_IDENTITIES = ("jazz-" + "next", "jazz-" + "hs", "jazz" + "2")
_POSITIVE_CLAIM_PATTERNS = (
    re.compile(
        r"\b(?:remain|remains|stay|stays|are|is)\b.*?\bread[- ]only\b",
        re.IGNORECASE,
    ),
    re.compile(
        r"\bread[- ]only\b.*?\b(?:remain|remains|stay|stays|are|is)\b",
        re.IGNORECASE,
    ),
    re.compile(r"\b(?:currently|still)\s+(?:exist|exists)\b", re.IGNORECASE),
    re.compile(
        r"\b(?:remain|remains|exist|exists|are present|is present)\b.*?"
        r"\b(?:current|live)\s+(?:checkout|workspace|repository)\b",
        re.IGNORECASE,
    ),
)
_FALSE_CLAIM_PREFIX_PATTERN = re.compile(
    r"\b(?:it is false that|it is not true that)\b", re.IGNORECASE
)
_NEGATED_STATUS_PATTERNS = (
    re.compile(
        r"\b(?:do|does|did|are|is|was|were)\s+not\b.*"
        r"\b(?:remain|remains|stay|stays|exist|exists|present|read[- ]only)\b",
        re.IGNORECASE,
    ),
    re.compile(
        r"\b(?:no longer|never)\b.*"
        r"\b(?:remain|remains|stay|stays|exist|exists|present|read[- ]only)\b",
        re.IGNORECASE,
    ),
)


def _fence_run(line: str) -> tuple[str, str] | None:
    indentation = len(line) - len(line.lstrip(" "))
    if indentation > 3:
        return None
    candidate = line[indentation:]
    if not candidate or candidate[0] not in "`~":
        return None
    fence_character = candidate[0]
    fence_length = len(candidate) - len(candidate.lstrip(fence_character))
    if fence_length < 3:
        return None
    return fence_character * fence_length, candidate[fence_length:]


def _opening_fence(line: str) -> str | None:
    fence_run = _fence_run(line)
    return None if fence_run is None else fence_run[0]


def _closes_fence(line: str, opening_fence: str) -> bool:
    fence_run = _fence_run(line)
    if fence_run is None:
        return False
    closing_fence, remainder = fence_run
    return (
        closing_fence[0] == opening_fence[0]
        and len(closing_fence) >= len(opening_fence)
        and not remainder.strip()
    )


def _markdown_prose_blocks(source: str) -> Iterator[tuple[int, str]]:
    block_start = 0
    block_lines: list[str] = []
    opening_fence: str | None = None

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
        if opening_fence is not None:
            if _closes_fence(raw_line, opening_fence):
                opening_fence = None
            continue

        opening_fence = _opening_fence(raw_line)
        if opening_fence is not None:
            block = flush()
            if block is not None:
                yield block
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
        if any(
            _clause_has_live_legacy_tree_claim(clause)
            for clause in _CLAUSE_BOUNDARY_PATTERN.split(block)
        ):
            claims.append((line_number, block))
    return claims


def _clause_has_live_legacy_tree_claim(clause: str) -> bool:
    lowered = clause.casefold()
    subject_positions = [
        match.start() for match in _LEGACY_TREE_PATTERN.finditer(clause)
    ]
    subject_positions.extend(
        match.start()
        for identity in _OBSOLETE_IDENTITIES
        for match in re.finditer(re.escape(identity), lowered)
    )
    if not subject_positions:
        return False

    positive_claims = sorted(
        (
            match
            for pattern in _POSITIVE_CLAIM_PATTERNS
            for match in pattern.finditer(clause)
        ),
        key=lambda match: (match.start(), match.end()),
    )
    for positive_claim in positive_claims:
        candidate_subjects = [
            position for position in subject_positions if position <= positive_claim.end()
        ]
        if not candidate_subjects:
            continue
        subject_start = max(candidate_subjects)
        false_prefixes = [
            prefix
            for prefix in _FALSE_CLAIM_PREFIX_PATTERN.finditer(clause)
            if prefix.end() <= subject_start
        ]
        prefixed_as_false = bool(false_prefixes) and not clause[
            false_prefixes[-1].end() : subject_start
        ].strip(" `*_\"'")
        directly_negated = any(
            negation.start() <= positive_claim.start() < negation.end()
            for pattern in _NEGATED_STATUS_PATTERNS
            for negation in pattern.finditer(clause)
        )
        if not prefixed_as_false and not directly_negated:
            return True
    return False


def _markdown_files(paths: Sequence[Path]) -> tuple[list[Path], list[str]]:
    markdown_files: list[Path] = []
    errors: list[str] = []
    for path in paths:
        if not path.exists():
            errors.append(f"input path does not exist: {path}")
        elif path.is_dir():
            directory_files = sorted(path.rglob("*.md"))
            if directory_files:
                markdown_files.extend(directory_files)
            else:
                errors.append(f"input directory contains no Markdown files: {path}")
        elif not path.is_file() or path.suffix.lower() != ".md":
            errors.append(f"unsupported input file (expected .md): {path}")
        else:
            markdown_files.append(path)
    return markdown_files, errors


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Reject live read-only claims about removed implementation trees."
    )
    parser.add_argument("paths", nargs="+", type=Path)
    args = parser.parse_args(argv)

    markdown_files, selection_errors = _markdown_files(args.paths)
    if selection_errors:
        for error in selection_errors:
            print(error, file=sys.stderr)
        return 2

    failed = False
    for path in markdown_files:
        try:
            source = path.read_text(encoding="utf-8")
        except OSError as error:
            failed = True
            print(f"could not read {path}: {error}", file=sys.stderr)
            continue
        for line_number, claim in find_live_legacy_tree_claims(source):
            failed = True
            print(f"{path}:{line_number}: live legacy-tree claim: {claim}", file=sys.stderr)
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
