#!/usr/bin/env python3
"""Validate the boundary between public Jazz docs and internal project records."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path, PurePosixPath
from urllib.parse import unquote, urlsplit

from example_cases import case_source_binding_violation
from markdown_targets import (
    FULL_REFERENCE_RE,
    decode_markdown_escapes_and_html_entities,
    html_image_targets,
    html_visible_text,
    html_reference_targets,
    rendered_heading_fragments,
    unescape_markdown_punctuation,
    used_reference_image_targets,
    used_reference_targets,
    without_inert_html_subtrees,
)
from markdown_visibility import (
    markdown_fences,
    renderable_source_markdown,
    rendered_markdown,
    rendered_html_source_markdown,
    rendered_markdown_with_code,
    visible_markdown,
    without_indented_code_blocks,
)


ALLOWED_DOCS_ENTRIES = {
    "getting-started",
    "language",
    "standard-library",
    "reference",
    "compiler",
    "project",
    "index.md",
}

PUBLIC_IDENTITY_BANNED_TERMS = (
    "docs/superpowers",
    "docs/execution",
    ".codex/",
    "jazz-next",
    "JazzNext",
    "jazz-hs",
    "jazz2",
)
PUBLIC_GENERATED_OUTPUT_BANNED_TERMS = (
    "JavaScript output",
    "JavaScript artifact",
)
PUBLIC_PRIVATE_RUNTIME_BANNED_TERMS = ("__kernel_",)
BANNED_REFERENCES = (
    *PUBLIC_IDENTITY_BANNED_TERMS,
    *PUBLIC_GENERATED_OUTPUT_BANNED_TERMS,
    *PUBLIC_PRIVATE_RUNTIME_BANNED_TERMS,
)

README_TAGLINE = "A statically typed functional language with practical syntax"
README_MATURITY_NOTICE = "Experimental / pre-1.0"
README_FACTORIAL_PATH = "examples/functions/factorial.jz"
README_FACTORIAL_MARKER = (
    f"<!-- jazz-example: executable path={README_FACTORIAL_PATH} -->"
)
README_FACTORIAL_OUTPUT_MARKER = "<!-- jazz-example-output: case=factorial -->"
README_REQUIRED_LINKS = (
    "docs/getting-started/overview.md",
    "docs/language/overview.md",
    "docs/standard-library/overview.md",
    "docs/reference/expression-grammar.md",
    "docs/compiler/architecture.md",
    "docs/project/status.md",
    "docs/project/roadmap.md",
    "docs/project/contributing.md",
    "https://github.com/un3qual/jazz/issues",
    "https://un3qual.github.io/jazz/",
)
README_BANNED_TERMS = (
    *PUBLIC_IDENTITY_BANNED_TERMS,
    *PUBLIC_PRIVATE_RUNTIME_BANNED_TERMS,
    "rfcs/",
    "docs/spec",
    "superpowers",
    "Spec Authority",
    "Repository Governance",
    "implementation snapshot",
    "Planned / Aspirational",
    "JavaScript",
    "category theory",
    "monad is just",
    "### Story",
)
README_REQUIRED_SECTIONS = (
    "## Quick start",
    "## Available today",
    "## In development",
    "## Documentation",
    "## Contributing",
    "## License",
)
README_ORDERED_TOKENS = (
    "jazz_logo.png",
    README_TAGLINE,
    README_MATURITY_NOTICE,
    README_FACTORIAL_MARKER,
    README_FACTORIAL_OUTPUT_MARKER,
    "## Quick start",
    "## Available today",
    "## In development",
    "## Documentation",
    "## Contributing",
    "## License",
)

REQUIRED_PAGES = (
    "index.md",
    "getting-started/overview.md",
    "getting-started/installation.md",
    "getting-started/first-program.md",
    "getting-started/cli.md",
    "language/overview.md",
    "language/source-and-blocks.md",
    "language/bindings-and-functions.md",
    "language/types-and-signatures.md",
    "language/algebraic-data-types-and-patterns.md",
    "language/control-flow.md",
    "language/modules.md",
    "language/operators.md",
    "language/capabilities.md",
    "language/purity.md",
    "standard-library/overview.md",
    "standard-library/prelude.md",
    "standard-library/list.md",
    "standard-library/maybe-result-nonempty.md",
    "standard-library/dictionary.md",
    "standard-library/queue.md",
    "standard-library/map-and-set.md",
    "standard-library/char-and-text.md",
    "standard-library/io.md",
    "reference/lexical-grammar.md",
    "reference/expression-grammar.md",
    "reference/module-resolution.md",
    "reference/cli.md",
    "reference/diagnostics.md",
    "reference/runtime-values.md",
    "compiler/architecture.md",
    "compiler/pipeline.md",
    "compiler/bootstrapping.md",
    "project/status.md",
    "project/roadmap.md",
    "project/governance.md",
    "project/contributing.md",
)

LINK_RE = re.compile(r"!?\[[^\]]*\]\(([^)]+)\)")
MARKDOWN_IMAGE_RE = re.compile(r"!\[[^\]]*\]\(([^)]+)\)")
REFERENCE_DEFINITION_BLOCK_RE = re.compile(
    r"^[ \t]{0,3}\[[^\]\r\n]+\]:[^\r\n]*(?:\r?\n|$)"
    r"(?:[ \t]{1,3}(?:\"[^\"\r\n]*\"|'[^'\r\n]*'|\([^()\r\n]*\))"
    r"[ \t]*(?:\r?\n|$))?",
    re.MULTILINE,
)
JAZZ_EXAMPLE_MARKER_RE = re.compile(
    r"<!--\s*jazz-example:.*?-->", re.DOTALL
)
EXECUTABLE_MARKER_RE = re.compile(
    r"<!--\s*jazz-example:\s*executable\s+path=([^\s]+)\s*-->"
)
FRAGMENT_MARKER_RE = re.compile(
    r"<!--\s*jazz-example:\s*fragment\s*-->"
)
EXAMPLE_OUTPUT_MARKER_RE = re.compile(
    r"<!--\s*jazz-example-output:\s*case=([A-Za-z0-9][A-Za-z0-9_-]*)\s*-->"
)
EXAMPLE_CASES_PATH = "scripts/example-cases.tsv"
EXAMPLE_CASE_HEADER = ("name", "sources", "expected", "args")
EXAMPLE_CASE_NAME_RE = re.compile(r"[A-Za-z0-9][A-Za-z0-9_-]*")
FRAGMENT_INVENTORY_PATH = "scripts/public-doc-fragments.tsv"
FRAGMENT_INVENTORY_HEADER = ("document", "ordinal", "sha256")
SHA256_RE = re.compile(r"[0-9a-f]{64}")
DIAGNOSTIC_CODE_RE = re.compile(
    r"^(?:error|warning): (E[0-9]{4}|W[0-9]{4})\b", re.MULTILINE
)
CLI_OVERRIDE_ENV = (
    "JAZZ_PRELUDE",
    "JAZZ_WARNING_FLAGS",
    "JAZZ_WARNING_ERROR_FLAGS",
    "JAZZ_WARNING_CONFIG",
)
FRAGMENT_SYNTAX_ERROR_CODES = frozenset({"E0001"})
FRAGMENT_CHECK_TIMEOUT_SECONDS = 30.0


@dataclass(frozen=True)
class ExampleCase:
    sources: frozenset[str]
    expected: str


def blank_markdown_metadata(text: str) -> str:
    characters = list(text)
    spans = [
        match.span()
        for match in REFERENCE_DEFINITION_BLOCK_RE.finditer(text)
    ]
    spans.extend(match.span(1) for match in LINK_RE.finditer(text))
    spans.extend(match.span(2) for match in FULL_REFERENCE_RE.finditer(text))
    for start, end in spans:
        for index in range(start, end):
            if characters[index] not in "\r\n":
                characters[index] = " "
    return "".join(characters)


def rendered_contract_text_with_code(text: str) -> str:
    rendered_source = without_inert_html_subtrees(
        rendered_markdown_with_code(text)
    )
    fence_sources = [
        fence.source
        for fence in markdown_fences(rendered_source)
        if fence.closed
    ]
    rendered_prose = without_inert_html_subtrees(rendered_markdown(text))
    visible_prose = html_visible_text(blank_markdown_metadata(rendered_prose))
    return "\n".join([visible_prose, *fence_sources])


def exact_visible_line_positions(text: str) -> dict[str, int]:
    visible_source = without_inert_html_subtrees(visible_markdown(text))
    positions: dict[str, int] = {}
    offset = 0
    for line_with_ending in visible_source.splitlines(keepends=True):
        line = line_with_ending.rstrip("\r\n")
        positions.setdefault(line, offset)
        offset += len(line_with_ending)
    return positions


def relative(root: Path, path: Path) -> str:
    return path.relative_to(root).as_posix()


def read_text(path: Path, root: Path, violations: list[str]) -> str | None:
    try:
        return path.read_bytes().decode("utf-8")
    except (OSError, UnicodeError) as exc:
        violations.append(f"{relative(root, path)}: cannot read UTF-8 Markdown: {exc}")
        return None


def supported_yaml_scalar(value: str) -> bool:
    """Accept the one-line scalar subset used by public documentation metadata."""
    if not value:
        return False
    if value[0] == '"':
        try:
            return isinstance(json.loads(value), str)
        except json.JSONDecodeError:
            return False
    if value[0] == "'":
        if len(value) < 2 or value[-1] != "'":
            return False
        return "'" not in value[1:-1].replace("''", "")
    if value[0] in "-?:[]{}#&*!|>%@`":
        return False
    if any(character in value for character in "[]{}"):
        return False
    if re.search(r":(?:[ \t]|$)|(?:^|[ \t])#", value) is not None:
        return False
    return True


def front_matter(text: str) -> tuple[set[str], str] | None:
    lines = text.splitlines(keepends=True)
    if not lines or lines[0].strip() != "---":
        return None
    try:
        end = next(index for index, line in enumerate(lines[1:], 1) if line.strip() == "---")
    except StopIteration:
        return None
    fields: set[str] = set()
    for line in lines[1:end]:
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        if line.startswith((" ", "\t")) or "\t" in line:
            return None
        match = re.match(r"^([A-Za-z][A-Za-z0-9_-]*):(?:\s*(.*))?$", line)
        if match is None or not (match.group(2) or "").strip():
            return None
        field = match.group(1)
        if field in fields:
            return None
        value = (match.group(2) or "").strip()
        if not supported_yaml_scalar(value):
            return None
        fields.add(field)
    return fields, "".join(lines[end + 1 :])


def markdown_link_target(raw_target: str) -> str:
    target = raw_target.strip()
    if not target:
        return ""
    if target.startswith("<") and ">" in target:
        destination = target[1 : target.index(">")]
    else:
        # Markdown permits an optional title after a whitespace-delimited target.
        destination = target.split(maxsplit=1)[0]
    return unescape_markdown_punctuation(destination)


def decoded_public_policy_text(text: str) -> str:
    return decode_markdown_escapes_and_html_entities(text).casefold()


def local_markdown_fragment_violation(
    candidate: Path, fragment: str, raw_target: str
) -> str | None:
    if not fragment or candidate.suffix.casefold() not in {".md", ".mdx"}:
        return None
    try:
        target_text = candidate.read_text(encoding="utf-8")
    except (OSError, UnicodeError):
        return None
    if fragment not in rendered_heading_fragments(target_text):
        return (
            "link fragment does not exist: "
            f"{markdown_link_target(raw_target)}"
        )
    return None


def internal_repository_path_label(candidate: Path, root: Path) -> str | None:
    for internal_name in (".codex", "rfcs"):
        internal_root = root / internal_name
        if candidate == internal_root or internal_root in candidate.parents:
            return f"{internal_name}/"
    return None


def resolves_within(path: Path, directory: Path) -> bool:
    try:
        path.relative_to(directory)
    except ValueError:
        return False
    return True


def internal_escape_label(doc_path: Path, raw_target: str, docs_root: Path) -> str | None:
    target = unquote(markdown_link_target(raw_target))
    parsed = urlsplit(target)
    if parsed.scheme or parsed.netloc or not parsed.path:
        return None
    candidate = (doc_path.parent / parsed.path).resolve()
    return internal_repository_path_label(candidate, docs_root.parent.resolve())


def local_docs_link_violation(
    doc_path: Path, raw_target: str, docs_root: Path
) -> str | None:
    target = unquote(markdown_link_target(raw_target))
    parsed = urlsplit(target)
    if parsed.scheme or parsed.netloc:
        return None
    candidate = (
        (doc_path.parent / parsed.path).resolve()
        if parsed.path
        else doc_path.resolve()
    )
    if not resolves_within(candidate, docs_root.resolve()):
        return f"public link leaves docs/: {markdown_link_target(raw_target)}"
    if not candidate.is_file():
        return f"public link target does not exist: {markdown_link_target(raw_target)}"
    fragment_violation = local_markdown_fragment_violation(
        candidate, parsed.fragment, raw_target
    )
    if fragment_violation is not None:
        return f"public {fragment_violation}"
    return None


def local_readme_link_violation(root: Path, raw_target: str) -> str | None:
    target = unquote(markdown_link_target(raw_target))
    parsed = urlsplit(target)
    if parsed.scheme or parsed.netloc or (not parsed.path and not parsed.fragment):
        return None
    canonical_root = root.resolve()
    candidate = (
        (root / parsed.path).resolve()
        if parsed.path
        else (root / "README.md").resolve()
    )
    if not resolves_within(candidate, canonical_root):
        return f"local link leaves repository: {markdown_link_target(raw_target)}"
    internal_label = internal_repository_path_label(candidate, canonical_root)
    if internal_label is not None:
        return (
            f"local link targets internal tree {internal_label}: "
            f"{markdown_link_target(raw_target)}"
        )
    if not candidate.is_file():
        return f"local link target does not exist: {markdown_link_target(raw_target)}"
    fragment_violation = local_markdown_fragment_violation(
        candidate, parsed.fragment, raw_target
    )
    if fragment_violation is not None:
        return f"local {fragment_violation}"
    return None


def resolve_jazz_binary(
    explicit: str | None,
) -> tuple[Path | None, str | None]:
    if explicit is None:
        return None, None
    binary = Path(explicit).resolve()
    if not binary.is_file():
        return None, f"Jazz executable does not exist: {binary}"
    return binary, None


def load_fragment_inventory(
    root: Path, violations: list[str]
) -> set[tuple[str, int, str]]:
    path = root / FRAGMENT_INVENTORY_PATH
    try:
        text = path.read_bytes().decode("utf-8")
    except (OSError, UnicodeError) as exc:
        violations.append(f"{FRAGMENT_INVENTORY_PATH}: cannot read inventory: {exc}")
        return set()
    if not text.endswith("\n"):
        violations.append(f"{FRAGMENT_INVENTORY_PATH}: file must end with a newline")
    rows = text.splitlines()
    if not rows or tuple(rows[0].split("\t")) != FRAGMENT_INVENTORY_HEADER:
        violations.append(f"{FRAGMENT_INVENTORY_PATH}: invalid header")
        return set()

    inventory: set[tuple[str, int, str]] = set()
    for line_number, row in enumerate(rows[1:], 2):
        fields = row.split("\t")
        if len(fields) != len(FRAGMENT_INVENTORY_HEADER):
            violations.append(
                f"{FRAGMENT_INVENTORY_PATH}:{line_number}: "
                "expected three tab-separated fields"
            )
            continue
        document, raw_ordinal, digest = fields
        pure_document = PurePosixPath(document)
        if (
            pure_document.is_absolute()
            or ".." in pure_document.parts
            or pure_document.suffix != ".md"
            or not (
                document == "README.md"
                or (pure_document.parts and pure_document.parts[0] == "docs")
            )
        ):
            violations.append(
                f"{FRAGMENT_INVENTORY_PATH}:{line_number}: invalid document path"
            )
            continue
        try:
            ordinal = int(raw_ordinal)
        except ValueError:
            ordinal = 0
        if ordinal <= 0:
            violations.append(
                f"{FRAGMENT_INVENTORY_PATH}:{line_number}: invalid fragment ordinal"
            )
            continue
        if SHA256_RE.fullmatch(digest) is None:
            violations.append(
                f"{FRAGMENT_INVENTORY_PATH}:{line_number}: invalid SHA-256 digest"
            )
            continue
        entry = (document, ordinal, digest)
        if entry in inventory:
            violations.append(
                f"{FRAGMENT_INVENTORY_PATH}:{line_number}: duplicate fragment entry"
            )
            continue
        inventory.add(entry)
    return inventory


def checked_jazz_environment() -> dict[str, str]:
    environment = os.environ.copy()
    for name in CLI_OVERRIDE_ENV:
        environment.pop(name, None)
    environment["JAZZ_WARNING_CONFIG"] = os.devnull
    return environment


def fragment_programs(source: str) -> tuple[str, ...]:
    authored = source.rstrip() + "\n"
    if authored.rstrip().endswith("."):
        return (authored,)
    return authored, authored.rstrip() + "\n.\n"


def run_fragment_compiler(
    root: Path,
    jazz_binary: Path,
    display: str,
    source: str,
    violations: list[str],
) -> subprocess.CompletedProcess[str] | None:
    try:
        return subprocess.run(
            [str(jazz_binary), "--no-prelude"],
            cwd=root,
            env=checked_jazz_environment(),
            input=source,
            check=False,
            capture_output=True,
            text=True,
            timeout=FRAGMENT_CHECK_TIMEOUT_SECONDS,
        )
    except subprocess.TimeoutExpired:
        violations.append(
            f"{display}: Jazz fragment syntax check timed out after "
            f"{FRAGMENT_CHECK_TIMEOUT_SECONDS:g} seconds"
        )
    except OSError as exc:
        violations.append(
            f"{display}: could not start Jazz fragment syntax check: {exc}"
        )
    return None


def validate_fragment_syntax(
    root: Path,
    jazz_binary: Path,
    display: str,
    source: str,
    violations: list[str],
) -> None:
    result: subprocess.CompletedProcess[str] | None = None
    diagnostic_codes: set[str] = set()
    syntax_codes: set[str] = set()
    for program in fragment_programs(source):
        result = run_fragment_compiler(
            root, jazz_binary, display, program, violations
        )
        if result is None:
            return
        diagnostic_codes = set(DIAGNOSTIC_CODE_RE.findall(result.stderr))
        syntax_codes = diagnostic_codes.intersection(
            FRAGMENT_SYNTAX_ERROR_CODES
        )
        if not syntax_codes:
            break
    assert result is not None
    if not syntax_codes:
        if result.returncode != 0 and not any(
            code.startswith("E") for code in diagnostic_codes
        ):
            violations.append(
                f"{display}: Jazz fragment syntax check exited "
                f"{result.returncode} without a compiler diagnostic"
            )
        return

    syntax_diagnostic = next(
        (
            line
            for line in result.stderr.splitlines()
            if any(f": {code}" in line for code in syntax_codes)
        ),
        ", ".join(sorted(syntax_codes)),
    )
    violations.append(
        f"{display}: Jazz fragment has invalid syntax: {syntax_diagnostic}"
    )


def tracked_examples(root: Path, violations: list[str]) -> list[str]:
    try:
        result = subprocess.run(
            ["git", "-C", str(root), "ls-files", "--", "examples"],
            check=False,
            capture_output=True,
            text=True,
        )
    except OSError:
        violations.append(
            "repository: cannot enumerate tracked examples: git executable is unavailable"
        )
        return []
    if result.returncode != 0:
        violations.append(
            "repository: cannot enumerate tracked examples: "
            f"git ls-files exited {result.returncode}"
        )
        return []
    return sorted(
        line
        for line in result.stdout.splitlines()
        if line.startswith("examples/") and line.endswith(".jz")
    )


def valid_example_path(example_path: str) -> bool:
    pure_path = PurePosixPath(example_path)
    return (
        not pure_path.is_absolute()
        and len(pure_path.parts) >= 2
        and pure_path.parts[0] == "examples"
        and ".." not in pure_path.parts
        and pure_path.suffix == ".jz"
    )


def without_one_final_newline(text: str) -> str:
    if text.endswith("\r\n"):
        return text[:-2]
    if text.endswith("\n"):
        return text[:-1]
    return text


def marker_gap_is_container_whitespace(text: str) -> bool:
    for line in text.splitlines():
        remainder = line
        while True:
            blockquote = re.match(r"^[ \t]*>[ \t]?", remainder)
            if blockquote is None:
                break
            remainder = remainder[blockquote.end() :]
        if remainder.strip():
            return False
    return True


def has_top_level_heading(text: str) -> bool:
    return re.search(r"^#[ \t]+", visible_markdown(text), re.MULTILINE) is not None


def has_bound_executable_marker(text: str, example_path: str) -> bool:
    source_text = renderable_source_markdown(text)
    fences = markdown_fences(source_text)
    markers = [
        marker
        for marker in EXECUTABLE_MARKER_RE.finditer(source_text)
        if marker.group(1) == example_path
        and not any(fence.start <= marker.start() < fence.end for fence in fences)
    ]
    return any(
        fence.is_jazz
        and marker.end() <= fence.start
        and marker_gap_is_container_whitespace(
            source_text[marker.end() : fence.start]
        )
        for marker in markers
        for fence in fences
    )


def validate_example_cases(
    root: Path, tracked_example_paths: set[str], violations: list[str]
) -> dict[str, ExampleCase]:
    cases_path = root / EXAMPLE_CASES_PATH
    try:
        cases_text = cases_path.read_bytes().decode("utf-8")
    except (OSError, UnicodeError) as exc:
        violations.append(f"{EXAMPLE_CASES_PATH}: cannot read example cases: {exc}")
        cases_text = ""

    declared_sources: set[str] = set()
    case_names: set[str] = set()
    example_cases: dict[str, ExampleCase] = {}
    if cases_text and not cases_text.endswith("\n"):
        violations.append(
            f"{EXAMPLE_CASES_PATH}: file must end with a newline"
        )
    lines = cases_text.splitlines()
    if not lines or tuple(lines[0].split("\t")) != EXAMPLE_CASE_HEADER:
        violations.append(
            f"{EXAMPLE_CASES_PATH}: expected tab-separated header: "
            + "\\t".join(EXAMPLE_CASE_HEADER)
        )
    else:
        for line_number, line in enumerate(lines[1:], 2):
            fields = line.split("\t")
            if len(fields) != len(EXAMPLE_CASE_HEADER):
                violations.append(
                    f"{EXAMPLE_CASES_PATH}:{line_number}: expected exactly four "
                    "tab-separated fields"
                )
                continue

            case_name, raw_sources, expected, args = fields
            case_name_is_valid = False
            if case_name == EXAMPLE_CASE_HEADER[0]:
                violations.append(
                    f"{EXAMPLE_CASES_PATH}:{line_number}: case name is reserved "
                    f"for the header: {case_name}"
                )
            elif EXAMPLE_CASE_NAME_RE.fullmatch(case_name) is None:
                violations.append(
                    f"{EXAMPLE_CASES_PATH}:{line_number}: invalid case name: "
                    f"{case_name or '<empty>'}"
                )
            elif case_name in case_names:
                violations.append(
                    f"{EXAMPLE_CASES_PATH}:{line_number}: duplicate case name: "
                    f"{case_name}"
                )
            else:
                case_names.add(case_name)
                case_name_is_valid = True

            if not expected:
                violations.append(
                    f"{EXAMPLE_CASES_PATH}:{line_number}: expected output is empty"
                )
            if not args:
                violations.append(
                    f"{EXAMPLE_CASES_PATH}:{line_number}: arguments are empty"
                )

            sources = raw_sources.split(",") if raw_sources else []
            if not sources:
                violations.append(
                    f"{EXAMPLE_CASES_PATH}:{line_number}: sources are empty"
                )
            seen_case_sources: set[str] = set()
            for example_path in sources:
                if example_path in seen_case_sources:
                    violations.append(
                        f"{EXAMPLE_CASES_PATH}:{line_number}: duplicate case source: "
                        f"{example_path}"
                    )
                    continue
                seen_case_sources.add(example_path)
                declared_sources.add(example_path)
                if not valid_example_path(example_path):
                    violations.append(
                        f"{EXAMPLE_CASES_PATH}: invalid case source: {example_path}"
                    )
                elif example_path not in tracked_example_paths:
                    violations.append(
                        f"{EXAMPLE_CASES_PATH}: case source is not a tracked "
                        f"example: {example_path}"
                    )

            if sources and args:
                binding_violation = case_source_binding_violation(root, sources, args)
                if binding_violation is not None:
                    violations.append(
                        f"{EXAMPLE_CASES_PATH}:{line_number}: {binding_violation}"
                    )
            if case_name_is_valid and sources and expected:
                example_cases[case_name] = ExampleCase(
                    sources=frozenset(sources),
                    expected=expected,
                )

    for example_path in sorted(tracked_example_paths - declared_sources):
        violations.append(
            f"{example_path}: tracked example is missing from {EXAMPLE_CASES_PATH}"
        )

    return example_cases


def validate_jazz_fences(
    root: Path,
    jazz_binary: Path | None,
    display: str,
    text: str,
    tracked_example_paths: set[str],
    canonical_examples_root: Path,
    fragment_inventory: set[tuple[str, int, str]] | None,
    observed_fragment_inventory: set[tuple[str, int, str]],
    violations: list[str],
) -> set[str]:
    documented_examples: set[str] = set()
    source_text = renderable_source_markdown(text)
    all_fences = markdown_fences(source_text)
    fences = [fence for fence in all_fences if fence.is_jazz]
    markers = [
        marker
        for marker in JAZZ_EXAMPLE_MARKER_RE.finditer(source_text)
        if not any(
            fence.start <= marker.start() < fence.end for fence in all_fences
        )
    ]
    consumed_marker_starts: set[int] = set()
    fragment_ordinal = 0

    for fence in fences:
        preceding_marker = next(
            (
                marker
                for marker in reversed(markers)
                if marker.end() <= fence.start
            ),
            None,
        )
        if preceding_marker is None or not marker_gap_is_container_whitespace(
            source_text[preceding_marker.end() : fence.start]
        ):
            violations.append(
                f"{display}: Jazz fence must be immediately preceded by a "
                "jazz-example marker"
            )
            if not fence.closed:
                violations.append(f"{display}: unclosed Jazz fence")
            continue

        consumed_marker_starts.add(preceding_marker.start())
        if not fence.closed:
            violations.append(f"{display}: unclosed Jazz fence")
            continue

        marker_text = preceding_marker.group(0)
        if FRAGMENT_MARKER_RE.fullmatch(marker_text):
            fragment_ordinal += 1
            if jazz_binary is not None:
                validate_fragment_syntax(
                    root,
                    jazz_binary,
                    display,
                    fence.source,
                    violations,
                )
            elif fragment_inventory is not None:
                entry = (
                    display,
                    fragment_ordinal,
                    hashlib.sha256(fence.source.encode("utf-8")).hexdigest(),
                )
                if entry not in fragment_inventory:
                    violations.append(
                        f"{display}: Jazz fragment {fragment_ordinal} is missing "
                        f"from {FRAGMENT_INVENTORY_PATH}"
                    )
                else:
                    observed_fragment_inventory.add(entry)
            continue

        executable = EXECUTABLE_MARKER_RE.fullmatch(marker_text)
        if executable is None:
            violations.append(f"{display}: invalid jazz-example marker: {marker_text}")
            continue

        example_path = executable.group(1)
        if not valid_example_path(example_path):
            violations.append(
                f"{display}: invalid executable example path: {example_path}"
            )
            continue

        example_candidate = root / PurePosixPath(example_path)
        try:
            resolved_example = example_candidate.resolve(strict=True)
        except OSError:
            violations.append(
                f"{display}: executable example does not exist: {example_path}"
            )
            continue
        if not resolves_within(resolved_example, canonical_examples_root):
            violations.append(
                f"{display}: executable example resolves outside examples/: "
                f"{example_path}"
            )
            continue
        if example_path not in tracked_example_paths:
            violations.append(
                f"{display}: executable example is not tracked: {example_path}"
            )
            continue
        try:
            example_source = example_candidate.read_bytes().decode("utf-8")
        except (OSError, UnicodeError) as exc:
            violations.append(
                f"{display}: cannot read executable example {example_path}: {exc}"
            )
            continue
        if without_one_final_newline(fence.source) != (
            without_one_final_newline(example_source)
        ):
            violations.append(
                f"{display}: executable fence differs from {example_path}"
            )
            continue
        documented_examples.add(example_path)

    for marker in markers:
        if marker.start() not in consumed_marker_starts:
            violations.append(
                f"{display}: jazz-example marker is not immediately followed by a "
                "Jazz fence"
            )

    return documented_examples


def validate_example_outputs(
    display: str,
    text: str,
    example_cases: dict[str, ExampleCase],
    documented_examples: set[str],
    violations: list[str],
) -> set[str]:
    documented_cases: set[str] = set()
    source_text = renderable_source_markdown(text)
    fences = markdown_fences(source_text)
    comments = list(re.finditer(r"<!--.*?(?:-->|\Z)", source_text, re.DOTALL))
    markers = [
        comment
        for comment in comments
        if "jazz-example-output:" in comment.group(0)
        and not any(fence.start <= comment.start() < fence.end for fence in fences)
    ]

    for marker in markers:
        parsed_marker = EXAMPLE_OUTPUT_MARKER_RE.fullmatch(marker.group(0))
        if parsed_marker is None:
            violations.append(
                f"{display}: invalid jazz-example-output marker: {marker.group(0)}"
            )
            continue
        case_name = parsed_marker.group(1)
        example_case = example_cases.get(case_name)
        if example_case is None:
            violations.append(
                f"{display}: jazz-example-output names unknown case: {case_name}"
            )
            continue
        if not example_case.sources.issubset(documented_examples):
            violations.append(
                f"{display}: documented output for case {case_name} must be "
                "alongside all of its executable source fences"
            )
            continue
        output_fence = next(
            (fence for fence in fences if marker.end() <= fence.start),
            None,
        )
        if (
            output_fence is None
            or output_fence.info != "text"
            or not output_fence.closed
            or not marker_gap_is_container_whitespace(
                source_text[marker.end() : output_fence.start]
            )
        ):
            violations.append(
                f"{display}: jazz-example-output marker for case {case_name} "
                "must be immediately followed by a closed text fence"
            )
            continue
        if without_one_final_newline(output_fence.source) != example_case.expected:
            violations.append(
                f"{display}: documented output for case {case_name} differs from "
                f"{EXAMPLE_CASES_PATH}"
            )
            continue
        documented_cases.add(case_name)

    return documented_cases


def validate_readme(root: Path, text: str, violations: list[str]) -> None:
    exact_line_positions = exact_visible_line_positions(text)

    line_count = len(text.splitlines())
    if not 100 <= line_count <= 150:
        violations.append(
            "README.md: must contain between 100 and 150 lines "
            f"(found {line_count})"
        )

    rendered_contract_text = rendered_contract_text_with_code(text)
    if (
        README_TAGLINE not in exact_line_positions
        or README_TAGLINE not in rendered_contract_text
    ):
        violations.append("README.md: missing required tagline")
    if README_MATURITY_NOTICE not in rendered_contract_text:
        violations.append("README.md: missing required maturity notice")

    visible_text = without_indented_code_blocks(
        without_inert_html_subtrees(visible_markdown(text))
    )
    image_targets = [
        match.group(1) for match in MARKDOWN_IMAGE_RE.finditer(visible_text)
    ]
    image_targets.extend(
        html_image_targets(
            without_inert_html_subtrees(rendered_html_source_markdown(text))
        )
    )
    image_targets.extend(used_reference_image_targets(visible_text))
    local_logo_found = False
    for raw_target in image_targets:
        target = unquote(markdown_link_target(raw_target))
        parsed = urlsplit(target)
        if "?raw=true" in target.casefold():
            violations.append("README.md: image URLs must not use ?raw=true")
        if parsed.scheme or parsed.netloc or not parsed.path:
            violations.append(
                f"README.md: image must use a repository-local path: {target}"
            )
            continue
        candidate = (root / parsed.path).resolve()
        if not resolves_within(candidate, root.resolve()) or not candidate.is_file():
            violations.append(
                f"README.md: local image does not exist in repository: {target}"
            )
            continue
        if candidate.name == "jazz_logo.png":
            local_logo_found = True
    if not local_logo_found:
        violations.append("README.md: logo must use a repository-local path")

    if not has_bound_executable_marker(text, README_FACTORIAL_PATH):
        violations.append("README.md: missing executable factorial marker")

    for command in (
        "nix develop",
        "cabal build all",
        f"cabal run jazz -- --run {README_FACTORIAL_PATH}",
    ):
        if command not in rendered_contract_text:
            violations.append(f"README.md: missing quick-start command: {command}")

    raw_link_targets = [
        match.group(1)
        for match in LINK_RE.finditer(visible_text)
        if not match.group(0).startswith("!")
    ]
    raw_link_targets.extend(used_reference_targets(visible_text))
    raw_link_targets.extend(
        html_reference_targets(
            without_inert_html_subtrees(rendered_html_source_markdown(text))
        )
    )
    link_targets = {
        markdown_link_target(raw_link_target)
        for raw_link_target in raw_link_targets
    }
    for required_target in README_REQUIRED_LINKS:
        if required_target not in link_targets:
            violations.append(
                f"README.md: missing required navigation link: {required_target}"
            )
    for raw_link_target in raw_link_targets:
        link_violation = local_readme_link_violation(root, raw_link_target)
        if link_violation is not None:
            violations.append(f"README.md: {link_violation}")
    if (
        "[Website (publishing with Workstream 3)]"
        "(https://un3qual.github.io/jazz/)" not in text
    ):
        violations.append(
            "README.md: website must be labeled as publishing with Workstream 3"
        )
    if "[GPL-3.0-only](LICENSE)" not in text:
        violations.append("README.md: missing GPL-3.0-only license link")

    for section in README_REQUIRED_SECTIONS:
        if section not in exact_line_positions:
            violations.append(f"README.md: missing required section: {section}")

    decoded_text = decoded_public_policy_text(text)
    for banned in README_BANNED_TERMS:
        if banned.casefold() in decoded_text:
            violations.append(f"README.md: banned front-door term: {banned}")

    positions: list[int] = []
    for token in README_ORDERED_TOKENS:
        if token == README_TAGLINE or token in README_REQUIRED_SECTIONS:
            position = exact_line_positions.get(token, -1)
        else:
            position = text.find(token)
        if position < 0:
            break
        positions.append(position)
    if len(positions) == len(README_ORDERED_TOKENS) and positions != sorted(positions):
        violations.append("README.md: required content is not in the prescribed order")


def validate(root: Path, jazz_binary: Path | None) -> list[str]:
    violations: list[str] = []
    docs_root = root / "docs"
    if not docs_root.is_dir():
        return ["docs: missing public documentation directory"]
    canonical_docs_root = root.resolve() / "docs"
    canonical_examples_root = root.resolve() / "examples"
    tracked_example_paths = set(tracked_examples(root, violations))
    example_cases = validate_example_cases(root, tracked_example_paths, violations)
    fragment_inventory = (
        load_fragment_inventory(root, violations)
        if jazz_binary is None
        else None
    )
    observed_fragment_inventory: set[tuple[str, int, str]] = set()

    for example_path in sorted(tracked_example_paths):
        candidate = root / example_path
        try:
            resolved = candidate.resolve(strict=True)
        except OSError:
            violations.append(f"{example_path}: tracked example does not exist")
            continue
        if not resolves_within(resolved, canonical_examples_root):
            violations.append(
                f"{example_path}: tracked example resolves outside examples/"
            )

    unsafe_doc_paths: set[Path] = set()
    for path in sorted(docs_root.rglob("*")):
        if path.suffix.casefold() == ".mdx" and (
            path.is_file() or path.is_symlink()
        ):
            violations.append(
                f"{relative(root, path)}: MDX public pages are unsupported; "
                "use Markdown (.md)"
            )
        if not path.is_symlink():
            continue
        display = relative(root, path)
        try:
            resolved = path.resolve(strict=True)
        except OSError:
            violations.append(f"{display}: documentation symlink target does not exist")
            unsafe_doc_paths.add(path)
            continue
        if not resolves_within(resolved, canonical_docs_root):
            violations.append(
                f"{display}: documentation path resolves outside docs/"
            )
            unsafe_doc_paths.add(path)

    for entry in sorted(docs_root.iterdir(), key=lambda path: path.name):
        if entry.name not in ALLOWED_DOCS_ENTRIES:
            violations.append(f"docs/{entry.name}: disallowed top-level docs entry")

    doc_texts: dict[Path, str] = {}
    for path in sorted(docs_root.rglob("*.md")):
        try:
            resolved_path = path.resolve(strict=True)
        except OSError:
            if path not in unsafe_doc_paths:
                violations.append(
                    f"{relative(root, path)}: documentation path does not exist"
                )
            continue
        if not resolves_within(resolved_path, canonical_docs_root):
            if path not in unsafe_doc_paths:
                violations.append(
                    f"{relative(root, path)}: documentation path resolves outside docs/"
                )
            continue
        text = read_text(path, root, violations)
        if text is None:
            continue
        doc_texts[path] = text
        display = relative(root, path)
        parsed_front_matter = front_matter(text)
        markdown_body = text
        if parsed_front_matter is None:
            violations.append(f"{display}: missing valid YAML front matter")
        else:
            fields, markdown_body = parsed_front_matter
            for required_field in ("title", "description", "sidebar_position"):
                if required_field not in fields:
                    violations.append(
                        f"{display}: front matter is missing {required_field}"
                    )
            relative_doc = path.relative_to(docs_root).as_posix()
            if (
                relative_doc in REQUIRED_PAGES
                and not rendered_contract_text_with_code(markdown_body).strip()
            ):
                violations.append(
                    f"{display}: required public page has no rendered body"
                )

        if has_top_level_heading(markdown_body):
            violations.append(
                f"{display}: top-level heading duplicates front matter title"
            )

        decoded_text = decoded_public_policy_text(text)
        for banned in BANNED_REFERENCES:
            if banned.casefold() in decoded_text:
                violations.append(f"{display}: banned public reference: {banned}")

        visible_body = without_indented_code_blocks(
            without_inert_html_subtrees(visible_markdown(markdown_body))
        )
        raw_link_targets = [
            match.group(1) for match in LINK_RE.finditer(visible_body)
        ]
        raw_link_targets.extend(used_reference_targets(visible_body))
        raw_link_targets.extend(used_reference_image_targets(visible_body))
        raw_link_targets.extend(
            html_reference_targets(
                without_inert_html_subtrees(
                    rendered_html_source_markdown(markdown_body)
                )
            )
        )
        for raw_link_target in raw_link_targets:
            raw_target = markdown_link_target(raw_link_target)
            if not raw_target:
                violations.append(f"{display}: public link target is empty")
                continue
            label = internal_escape_label(path, raw_link_target, docs_root)
            if label is not None:
                violations.append(
                    f"{display}: public link escapes docs into {label}: {raw_target}"
                )
                continue
            link_violation = local_docs_link_violation(
                path, raw_link_target, docs_root
            )
            if link_violation is not None:
                violations.append(
                    f"{display}: {link_violation}"
                )

    for required_page in REQUIRED_PAGES:
        required_path = docs_root / required_page
        if not required_path.is_file():
            violations.append(f"docs/{required_page}: missing required public page")
            continue
        try:
            resolved_required = required_path.resolve(strict=True)
        except OSError:
            violations.append(f"docs/{required_page}: missing required public page")
            continue
        if not resolves_within(resolved_required, canonical_docs_root):
            violations.append(
                f"docs/{required_page}: required public page resolves outside docs/"
            )

    public_texts = {
        relative(root, path): text for path, text in doc_texts.items()
    }
    readme_path = root / "README.md"
    if readme_path.is_file():
        try:
            readme_text = readme_path.read_bytes().decode("utf-8")
            public_texts["README.md"] = readme_text
            validate_readme(root, readme_text, violations)
        except (OSError, UnicodeError) as exc:
            violations.append(f"README.md: cannot read UTF-8 Markdown: {exc}")
    else:
        violations.append("README.md: missing project front door")

    documented_examples: set[str] = set()
    documented_output_cases: set[str] = set()
    readme_output_cases: set[str] = set()
    for display, text in public_texts.items():
        document_examples = validate_jazz_fences(
            root,
            jazz_binary,
            display,
            text,
            tracked_example_paths,
            canonical_examples_root,
            fragment_inventory,
            observed_fragment_inventory,
            violations,
        )
        documented_examples.update(document_examples)
        output_cases = validate_example_outputs(
            display, text, example_cases, document_examples, violations
        )
        documented_output_cases.update(output_cases)
        if display == "README.md":
            readme_output_cases.update(output_cases)
    for example_path in sorted(tracked_example_paths - documented_examples):
        violations.append(
            f"{example_path}: tracked example has no executable public-docs fence"
        )
    if "factorial" not in readme_output_cases:
        violations.append("README.md: missing expected factorial output")
    for case_name in sorted(set(example_cases) - documented_output_cases):
        violations.append(
            f"{EXAMPLE_CASES_PATH}: case {case_name} has no documented expected output"
        )
    if fragment_inventory is not None:
        for display, ordinal, _digest in sorted(
            fragment_inventory - observed_fragment_inventory
        ):
            violations.append(
                f"{FRAGMENT_INVENTORY_PATH}: stale Jazz fragment entry: "
                f"{display}#{ordinal}"
            )

    return sorted(set(violations))


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "repository_root",
        nargs="?",
        type=Path,
        default=Path(__file__).resolve().parent.parent,
    )
    parser.add_argument("--jazz-bin")
    arguments = parser.parse_args(argv[1:])
    root = arguments.repository_root.resolve()
    jazz_binary, binary_error = resolve_jazz_binary(arguments.jazz_bin)
    if binary_error is not None:
        print(f"FAIL: repository: {binary_error}")
        return 1
    violations = validate(root, jazz_binary)
    if violations:
        for violation in violations:
            print(f"FAIL: {violation}")
        return 1
    print("Public documentation checks passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
