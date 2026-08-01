#!/usr/bin/env python3
"""Validate the boundary between public Jazz docs and internal project records."""

from __future__ import annotations

import re
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path, PurePosixPath
from urllib.parse import unquote, urlsplit


ALLOWED_DOCS_ENTRIES = {
    "getting-started",
    "language",
    "standard-library",
    "reference",
    "compiler",
    "project",
    "index.md",
}

BANNED_REFERENCES = (
    "docs/superpowers",
    "docs/execution",
    ".codex/",
    "jazz-next",
    "JazzNext",
    "jazz-hs",
    "jazz2",
)

README_TAGLINE = "A statically typed functional language with practical syntax"
README_MATURITY_NOTICE = "Experimental / pre-1.0"
README_FACTORIAL_PATH = "examples/functions/factorial.jz"
README_FACTORIAL_MARKER = (
    f"<!-- jazz-example: executable path={README_FACTORIAL_PATH} -->"
)
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
    "docs/superpowers",
    "docs/execution",
    ".codex/",
    "rfcs/",
    "docs/spec",
    "jazz-next",
    "JazzNext",
    "jazz-hs",
    "jazz2",
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
README_ORDERED_TOKENS = (
    "jazz_logo.png",
    README_TAGLINE,
    README_MATURITY_NOTICE,
    README_FACTORIAL_MARKER,
    "```text\n720\n```",
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
HTML_IMAGE_RE = re.compile(
    r"<img\b[^>]*\bsrc=[\"']([^\"']+)[\"'][^>]*>", re.IGNORECASE
)
REFERENCE_DEFINITION_RE = re.compile(
    r"^[ \t]{0,3}\[([^\]]+)\]:[ \t]*(?:<([^>]+)>|(\S+))",
    re.MULTILINE,
)
FULL_REFERENCE_RE = re.compile(r"\[([^\]]+)\]\[([^\]]*)\]")
SHORTCUT_REFERENCE_RE = re.compile(r"\[([^\]]+)\](?![\[(])")
JAZZ_EXAMPLE_MARKER_RE = re.compile(
    r"<!--\s*jazz-example:.*?-->", re.DOTALL
)
EXECUTABLE_MARKER_RE = re.compile(
    r"<!--\s*jazz-example:\s*executable\s+path=([^\s]+)\s*-->"
)
FRAGMENT_MARKER_RE = re.compile(
    r"<!--\s*jazz-example:\s*fragment\s*-->"
)
EXAMPLE_CASES_PATH = "scripts/example-cases.tsv"
EXAMPLE_CASE_HEADER = ("name", "sources", "expected", "args")
EXAMPLE_CASE_NAME_RE = re.compile(r"[A-Za-z0-9][A-Za-z0-9_-]*")
EXAMPLE_CASE_LOOP_RE = re.compile(
    r"^[ \t]*while[ \t]+IFS=\$'\\t'[ \t]+read[ \t]+-r[ \t]+"
    r"case_name[ \t]+case_sources[ \t]+case_expected[ \t]+case_args_text"
    r"[ \t]*;[ \t]*do[ \t]*\r?\n"
    r"(?P<body>.*?)"
    r"^[ \t]*done[ \t]*<[ \t]*scripts/example-cases\.tsv[ \t]*$",
    re.MULTILINE | re.DOTALL,
)
EXAMPLE_RUNNER_CALL_RE = re.compile(
    r'^[ \t]*run_example[ \t]+"\$case_name"[ \t]+'
    r'"\$case_expected"[ \t]+"\$\{case_args\[@\]\}"[ \t]*$',
    re.MULTILINE,
)


@dataclass(frozen=True)
class MarkdownFence:
    start: int
    end: int
    source: str
    is_jazz: bool
    closed: bool


def relative(root: Path, path: Path) -> str:
    return path.relative_to(root).as_posix()


def read_text(path: Path, root: Path, violations: list[str]) -> str | None:
    try:
        return path.read_bytes().decode("utf-8")
    except (OSError, UnicodeError) as exc:
        violations.append(f"{relative(root, path)}: cannot read UTF-8 Markdown: {exc}")
        return None


def front_matter_fields(text: str) -> set[str] | None:
    lines = text.splitlines()
    if not lines or lines[0].strip() != "---":
        return None
    try:
        end = next(index for index, line in enumerate(lines[1:], 1) if line.strip() == "---")
    except StopIteration:
        return None
    fields: set[str] = set()
    for line in lines[1:end]:
        match = re.match(r"^([A-Za-z][A-Za-z0-9_-]*):(?:\s*(.*))?$", line)
        if match and (match.group(2) or "").strip():
            fields.add(match.group(1))
    return fields


def markdown_link_target(raw_target: str) -> str:
    target = raw_target.strip()
    if target.startswith("<") and ">" in target:
        return target[1 : target.index(">")]
    # Markdown permits an optional title after a whitespace-delimited target.
    return target.split(maxsplit=1)[0]


def normalize_reference_label(label: str) -> str:
    return re.sub(r"\s+", " ", label.strip()).casefold()


def used_reference_targets(text: str) -> list[str]:
    definitions: dict[str, list[str]] = {}
    definition_spans: list[tuple[int, int]] = []
    for match in REFERENCE_DEFINITION_RE.finditer(text):
        label = normalize_reference_label(match.group(1))
        target = match.group(2) or match.group(3)
        definitions.setdefault(label, []).append(target)
        definition_spans.append(match.span())

    # Reference definitions are not shortcut usages. Blank them while
    # preserving offsets so usage parsing cannot reinterpret `[label]:`.
    usage_text = list(text)
    for start, end in definition_spans:
        usage_text[start:end] = " " * (end - start)
    usages = "".join(usage_text)

    used_labels: set[str] = set()
    full_reference_spans: list[tuple[int, int]] = []
    for match in FULL_REFERENCE_RE.finditer(usages):
        label = match.group(2) or match.group(1)
        used_labels.add(normalize_reference_label(label))
        full_reference_spans.append(match.span())

    shortcut_text = list(usages)
    for start, end in full_reference_spans:
        shortcut_text[start:end] = " " * (end - start)
    for match in SHORTCUT_REFERENCE_RE.finditer("".join(shortcut_text)):
        label = normalize_reference_label(match.group(1))
        if label in definitions:
            used_labels.add(label)

    return sorted(
        target
        for label in used_labels
        for target in definitions.get(label, [])
    )


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
    for internal_name in (".codex", "rfcs"):
        internal_root = docs_root.parent / internal_name
        if candidate == internal_root or internal_root in candidate.parents:
            return f"{internal_name}/"
    return None


def local_docs_link_violation(
    doc_path: Path, raw_target: str, docs_root: Path
) -> str | None:
    target = unquote(markdown_link_target(raw_target))
    parsed = urlsplit(target)
    if parsed.scheme or parsed.netloc or not parsed.path:
        return None
    candidate = (doc_path.parent / parsed.path).resolve()
    if not resolves_within(candidate, docs_root.resolve()):
        return f"public link leaves docs/: {markdown_link_target(raw_target)}"
    if not candidate.is_file():
        return f"public link target does not exist: {markdown_link_target(raw_target)}"
    return None


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


def without_line_ending(line: str) -> str:
    if line.endswith("\r\n"):
        return line[:-2]
    if line.endswith(("\n", "\r")):
        return line[:-1]
    return line


def leading_spaces(line: str) -> int:
    return len(line) - len(line.lstrip(" "))


def fence_opener(line: str) -> tuple[str, int, int, bool] | None:
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
    is_jazz = (
        first_info_token is not None
        and first_info_token.group(1) == "jazz"
    )
    return delimiter, length, indent, is_jazz


def is_fence_closer(line: str, delimiter: str, minimum_length: int) -> bool:
    content = without_line_ending(line)
    indent = leading_spaces(content)
    if indent > 3:
        return False
    candidate = content[indent:]
    length = len(candidate) - len(candidate.lstrip(delimiter))
    return (
        length >= minimum_length
        and candidate[length:].strip(" \t") == ""
    )


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

        delimiter, minimum_length, indent, is_jazz = opener
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
                is_jazz=is_jazz,
                closed=closed,
            )
        )

    return fences


def example_case_loop_executes_rows(script_text: str) -> bool:
    loops = list(EXAMPLE_CASE_LOOP_RE.finditer(script_text))
    if len(loops) != 1:
        return False
    body = loops[0].group("body")
    return (
        EXAMPLE_RUNNER_CALL_RE.search(body) is not None
        and '"$case_sources"' in body
        and '"$case_args_text"' in body
    )


def validate_example_cases(
    root: Path, tracked_example_paths: set[str], violations: list[str]
) -> set[str]:
    cases_path = root / EXAMPLE_CASES_PATH
    try:
        cases_text = cases_path.read_bytes().decode("utf-8")
    except (OSError, UnicodeError) as exc:
        violations.append(f"{EXAMPLE_CASES_PATH}: cannot read example cases: {exc}")
        cases_text = ""

    declared_sources: set[str] = set()
    case_names: set[str] = set()
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

    for example_path in sorted(tracked_example_paths - declared_sources):
        violations.append(
            f"{example_path}: tracked example is missing from {EXAMPLE_CASES_PATH}"
        )

    runner_path = root / "scripts/check-examples.sh"
    try:
        runner_text = runner_path.read_bytes().decode("utf-8")
    except (OSError, UnicodeError) as exc:
        violations.append(
            f"scripts/check-examples.sh: cannot read example runner: {exc}"
        )
    else:
        if not example_case_loop_executes_rows(runner_text):
            violations.append(
                "scripts/check-examples.sh: does not execute "
                f"{EXAMPLE_CASES_PATH}"
            )

    return declared_sources


def validate_jazz_fences(
    root: Path,
    display: str,
    text: str,
    tracked_example_paths: set[str],
    canonical_examples_root: Path,
    violations: list[str],
) -> set[str]:
    documented_examples: set[str] = set()
    all_fences = markdown_fences(text)
    fences = [fence for fence in all_fences if fence.is_jazz]
    markers = [
        marker
        for marker in JAZZ_EXAMPLE_MARKER_RE.finditer(text)
        if not any(
            fence.start <= marker.start() < fence.end for fence in all_fences
        )
    ]
    consumed_marker_starts: set[int] = set()

    for fence in fences:
        preceding_marker = next(
            (
                marker
                for marker in reversed(markers)
                if marker.end() <= fence.start
            ),
            None,
        )
        if preceding_marker is None or text[
            preceding_marker.end() : fence.start
        ].strip():
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


def validate_readme(root: Path, text: str, violations: list[str]) -> None:
    line_count = len(text.splitlines())
    if not 100 <= line_count <= 150:
        violations.append(
            "README.md: must contain between 100 and 150 lines "
            f"(found {line_count})"
        )

    if README_TAGLINE not in text:
        violations.append("README.md: missing required tagline")
    if README_MATURITY_NOTICE not in text:
        violations.append("README.md: missing required maturity notice")

    image_targets = [match.group(1) for match in MARKDOWN_IMAGE_RE.finditer(text)]
    image_targets.extend(match.group(1) for match in HTML_IMAGE_RE.finditer(text))
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

    if README_FACTORIAL_MARKER not in text:
        violations.append("README.md: missing executable factorial marker")
    if re.search(r"```text\r?\n720\r?\n```", text) is None:
        violations.append("README.md: missing expected factorial output")

    for command in (
        "nix develop",
        "cabal build all",
        f"cabal run jazz -- --run {README_FACTORIAL_PATH}",
    ):
        if command not in text:
            violations.append(f"README.md: missing quick-start command: {command}")

    link_targets = {
        markdown_link_target(match.group(1)) for match in LINK_RE.finditer(text)
    }
    for required_target in README_REQUIRED_LINKS:
        if required_target not in link_targets:
            violations.append(
                f"README.md: missing required navigation link: {required_target}"
            )
    if (
        "[Website (publishing with Workstream 3)]"
        "(https://un3qual.github.io/jazz/)" not in text
    ):
        violations.append(
            "README.md: website must be labeled as publishing with Workstream 3"
        )
    if "[GPL-3.0-only](LICENSE)" not in text:
        violations.append("README.md: missing GPL-3.0-only license link")

    for section in (
        "## Quick start",
        "## Available today",
        "## In development",
        "## Documentation",
        "## Contributing",
        "## License",
    ):
        if section not in text:
            violations.append(f"README.md: missing required section: {section}")

    for banned in README_BANNED_TERMS:
        if banned.casefold() in text.casefold():
            violations.append(f"README.md: banned front-door term: {banned}")

    positions: list[int] = []
    for token in README_ORDERED_TOKENS:
        position = text.find(token)
        if position < 0:
            break
        positions.append(position)
    if len(positions) == len(README_ORDERED_TOKENS) and positions != sorted(positions):
        violations.append("README.md: required content is not in the prescribed order")


def validate(root: Path) -> list[str]:
    violations: list[str] = []
    docs_root = root / "docs"
    if not docs_root.is_dir():
        return ["docs: missing public documentation directory"]
    canonical_docs_root = root.resolve() / "docs"
    canonical_examples_root = root.resolve() / "examples"
    tracked_example_paths = set(tracked_examples(root, violations))
    validate_example_cases(root, tracked_example_paths, violations)

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
        fields = front_matter_fields(text)
        if fields is None:
            violations.append(f"{display}: missing valid YAML front matter")
        else:
            for required_field in ("title", "description", "sidebar_position"):
                if required_field not in fields:
                    violations.append(
                        f"{display}: front matter is missing {required_field}"
                    )

        for banned in BANNED_REFERENCES:
            if banned in text:
                violations.append(f"{display}: banned public reference: {banned}")

        raw_link_targets = [match.group(1) for match in LINK_RE.finditer(text)]
        raw_link_targets.extend(used_reference_targets(text))
        for raw_link_target in raw_link_targets:
            raw_target = markdown_link_target(raw_link_target)
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
    for display, text in public_texts.items():
        documented_examples.update(
            validate_jazz_fences(
                root,
                display,
                text,
                tracked_example_paths,
                canonical_examples_root,
                violations,
            )
        )
    for example_path in sorted(tracked_example_paths - documented_examples):
        violations.append(
            f"{example_path}: tracked example has no executable public-docs fence"
        )

    return sorted(set(violations))


def main(argv: list[str]) -> int:
    if len(argv) > 2:
        print("usage: check-public-docs.py [repository-root]", file=sys.stderr)
        return 2
    root = Path(argv[1]).resolve() if len(argv) == 2 else Path(__file__).resolve().parent.parent
    violations = validate(root)
    if violations:
        for violation in violations:
            print(f"FAIL: {violation}")
        return 1
    print("Public documentation checks passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
