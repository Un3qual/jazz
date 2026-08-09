#!/usr/bin/env python3
"""Validate RFC placement, metadata headers, and Markdown section structure."""

from __future__ import annotations

import posixpath
import re
import sys
from dataclasses import dataclass
from datetime import date
from pathlib import Path
from urllib.parse import unquote, urlsplit

from markdown_targets import (
    html_visible_text,
    html_reference_targets,
    rendered_heading_fragments,
    unescape_markdown_punctuation,
    used_reference_targets,
    without_inert_html_subtrees,
)
from markdown_visibility import (
    rendered_html_source_markdown,
    rendered_prose_markdown,
    visible_markdown,
    without_indented_code_blocks,
)


RFC_NAME_RE = re.compile(r"^(\d{4})-[a-z0-9]+(?:-[a-z0-9]+)*\.md$")
TITLE_RE = re.compile(r"^# RFC (\d{4}): .+$")
DATE_RE = re.compile(r"^Date: (\d{4}-\d{2}-\d{2})$")
REQUIRED_HEADINGS = ("## Decision", "## Context", "## Consequences")
REFERENCE_DEFINITION_RE = re.compile(
    r"^ {0,3}\[[^\]\n]+\]:[ \t]*(?:\n[ \t]*)?"
    r"(?:<[^>\n]+>|\S+)"
    r"(?:[ \t]+|\n[ \t]*)?"
    r"(?:\"[^\"\n]*\"|'[^'\n]*'|\([^\)\n]*\))?[ \t]*$",
    re.MULTILINE,
)
MARKDOWN_LINK_RE = re.compile(
    r"(?<!!)\[[^\]]+\]\((?:<([^>]+)>|([^\s)]+))(?:\s+[^)]*)?\)"
)


def visible_heading_lines(text: str) -> list[str]:
    return [
        line
        for line in visible_markdown(text).splitlines()
        if line.startswith("## ")
    ]


def required_section_body(text: str, heading: str) -> str | None:
    structural_lines = visible_markdown(text).splitlines()
    heading_lines = [
        index for index, line in enumerate(structural_lines) if line == heading
    ]
    if len(heading_lines) != 1:
        return None
    start = heading_lines[0] + 1
    end = next(
        (
            index
            for index in range(start, len(structural_lines))
            if structural_lines[index].startswith("## ")
        ),
        len(structural_lines),
    )
    rendered_body = "\n".join(
        rendered_prose_markdown(text).splitlines()[start:end]
    )
    visible_body = without_inert_html_subtrees(rendered_body)
    without_definitions = REFERENCE_DEFINITION_RE.sub("", visible_body)
    return html_visible_text(without_definitions).strip()


def visible_link_targets(text: str) -> set[str]:
    visible = without_indented_code_blocks(
        without_inert_html_subtrees(visible_markdown(text))
    )
    targets = {
        match.group(1) or match.group(2)
        for match in MARKDOWN_LINK_RE.finditer(visible)
    }
    targets.update(used_reference_targets(visible))
    targets.update(
        html_reference_targets(
            rendered_html_source_markdown(text), include_images=False
        )
    )
    return targets


@dataclass(frozen=True)
class IndexTarget:
    path: str
    fragment: str
    raw: str


def normalized_index_target(target: str) -> IndexTarget | None:
    target = unescape_markdown_punctuation(target)
    parsed = urlsplit(target)
    if parsed.scheme or parsed.netloc or not parsed.path:
        return None
    decoded_path = unquote(parsed.path)
    normalized = posixpath.normpath(decoded_path)
    if normalized in {".", ".."} or normalized.startswith(("../", "/")):
        return None
    return IndexTarget(
        path=normalized,
        fragment=unquote(parsed.fragment),
        raw=target,
    )


def index_fragment_violation(root: Path, target: IndexTarget) -> str | None:
    if not target.fragment:
        return None
    rfc_root = (root / "rfcs").resolve()
    candidate = (rfc_root / target.path).resolve()
    try:
        candidate.relative_to(rfc_root)
    except ValueError:
        return None
    if candidate.suffix.casefold() not in {".md", ".mdx"} or not candidate.is_file():
        return None
    try:
        target_text = candidate.read_text(encoding="utf-8")
    except (OSError, UnicodeError):
        return None
    if target.fragment in rendered_heading_fragments(target_text):
        return None
    return f"rfcs/README.md: link fragment does not exist: {target.raw}"


def validate_metadata(path: str, lines: list[str], status: str) -> list[str]:
    violations: list[str] = []
    if len(lines) < 7:
        return [f"{path}: metadata must be a contiguous header"]
    title = TITLE_RE.fullmatch(lines[0])
    expected_number = Path(path).name[:4]
    if title is None or title.group(1) != expected_number:
        violations.append(f"{path}: title must match the RFC filename number")
    if lines[1] != "":
        violations.append(f"{path}: metadata must be a contiguous header")
        return violations
    metadata = lines[2:5]
    if metadata[0] != f"Status: {status}":
        violations.append(f"{path}: metadata must be a contiguous header")
    date_match = DATE_RE.fullmatch(metadata[1])
    if date_match is None:
        violations.append(f"{path}: decision date must use YYYY-MM-DD")
    else:
        try:
            date.fromisoformat(date_match.group(1))
        except ValueError:
            violations.append(f"{path}: decision date is not a calendar date")
    if re.fullmatch(r"Supersedes: .+", metadata[2]) is None:
        violations.append(f"{path}: Supersedes metadata must name a decision or None")
    if lines[5] != "":
        violations.append(f"{path}: metadata must be a contiguous header")
    return violations


def validate_rfc(root: Path, candidate: Path, status: str) -> list[str]:
    display = candidate.relative_to(root).as_posix()
    if candidate.is_symlink() or not candidate.is_file():
        return [f"{display}: RFC must be a regular file"]
    try:
        text = candidate.read_text(encoding="utf-8")
    except (OSError, UnicodeError) as exc:
        return [f"{display}: cannot read UTF-8 RFC: {exc}"]
    violations = validate_metadata(display, text.splitlines(), status)
    headings = visible_heading_lines(text)
    positions: list[int] = []
    for heading in REQUIRED_HEADINGS:
        if headings.count(heading) != 1:
            violations.append(f"{display}: missing required heading: {heading}")
        else:
            positions.append(headings.index(heading))
            if not required_section_body(text, heading):
                violations.append(
                    f"{display}: required section is empty: {heading}"
                )
    if len(positions) == len(REQUIRED_HEADINGS) and positions != sorted(positions):
        violations.append(f"{display}: required headings are out of order")
    return violations


def validate(root: Path) -> list[str]:
    violations: list[str] = []
    rfc_number_owners: dict[str, str] = {}
    accepted_targets: set[str] = set()
    accepted_index = root / "rfcs/README.md"
    try:
        normalized_index_targets = {
            normalized
            for target in visible_link_targets(
                accepted_index.read_text(encoding="utf-8")
            )
            if (normalized := normalized_index_target(target)) is not None
        }
        accepted_index_targets = {
            target.path for target in normalized_index_targets
        }
        for target in normalized_index_targets:
            if (violation := index_fragment_violation(root, target)) is not None:
                violations.append(violation)
    except (OSError, UnicodeError) as exc:
        violations.append(f"rfcs/README.md: cannot read RFC index: {exc}")
        accepted_index_targets = set()

    for directory_name, status in (
        ("rfcs/accepted", "Accepted"),
        ("rfcs/proposed", "Proposed"),
    ):
        directory = root / directory_name
        if not directory.is_dir():
            violations.append(f"{directory_name}: missing RFC directory")
            continue
        for nested in sorted(directory.glob("**/*.md")):
            if nested.parent != directory:
                violations.append(
                    f"{nested.relative_to(root).as_posix()}: RFC files must live directly below {directory_name}/"
                )
        for candidate in sorted(directory.iterdir()):
            if candidate.name == "README.md" and directory_name == "rfcs/proposed":
                continue
            name_match = RFC_NAME_RE.fullmatch(candidate.name)
            if name_match is None:
                violations.append(
                    f"{candidate.relative_to(root).as_posix()}: invalid RFC filename"
                )
                continue
            display = candidate.relative_to(root).as_posix()
            number = name_match.group(1)
            previous_owner = rfc_number_owners.get(number)
            if previous_owner is None:
                rfc_number_owners[number] = display
            else:
                violations.append(
                    f"{display}: duplicate RFC number {number}; "
                    f"already used by {previous_owner}"
                )
            violations.extend(validate_rfc(root, candidate, status))
            if status == "Accepted":
                target = f"accepted/{candidate.name}"
                accepted_targets.add(target)
                if target not in accepted_index_targets:
                    violations.append(
                        f"rfcs/README.md: missing accepted RFC index entry: {target}"
                    )
    for target in sorted(accepted_index_targets):
        if target.startswith("accepted/") and target not in accepted_targets:
            violations.append(
                f"rfcs/README.md: stale accepted RFC index entry: {target}"
            )
    return sorted(set(violations))


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        print("usage: check-rfcs.py REPOSITORY_ROOT", file=sys.stderr)
        return 2
    violations = validate(Path(argv[1]).resolve())
    if violations:
        for violation in violations:
            print(f"FAIL: {violation}")
        return 1
    print("RFC structure checks passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
