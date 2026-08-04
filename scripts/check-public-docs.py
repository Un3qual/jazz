#!/usr/bin/env python3
"""Validate the boundary between public Jazz docs and internal project records."""

from __future__ import annotations

import argparse
import json
import os
import re
import subprocess
import sys
from dataclasses import dataclass
from html.parser import HTMLParser
from pathlib import Path, PurePosixPath
from urllib.parse import unquote, urlsplit

from example_cases import case_source_binding_violation
from markdown_visibility import (
    html_source_markdown,
    markdown_fences,
    renderable_source_markdown,
    rendered_markdown,
    rendered_markdown_with_code,
    visible_markdown,
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
HTML_IMAGE_RE = re.compile(
    r"<img\b[^>]*\bsrc=[\"']([^\"']+)[\"'][^>]*>", re.IGNORECASE
)
REFERENCE_DEFINITION_RE = re.compile(
    r"^[ \t]{0,3}\[([^\]]+)\]:[ \t]*(?:<([^>]+)>|(\S+))",
    re.MULTILINE,
)
REFERENCE_DEFINITION_BLOCK_RE = re.compile(
    r"^[ \t]{0,3}\[[^\]\r\n]+\]:[^\r\n]*(?:\r?\n|$)"
    r"(?:[ \t]{1,3}(?:\"[^\"\r\n]*\"|'[^'\r\n]*'|\([^()\r\n]*\))"
    r"[ \t]*(?:\r?\n|$))?",
    re.MULTILINE,
)
FULL_REFERENCE_RE = re.compile(r"\[([^\]]+)\]\[([^\]]*)\]")
SHORTCUT_REFERENCE_RE = re.compile(r"\[([^\]]+)\](?![\[(])")
ATX_HEADING_RE = re.compile(
    r"^[ \t]{0,3}#{1,6}(?:[ \t]+|$)(.*?)[ \t]*$",
    re.MULTILINE,
)
EXPLICIT_HEADING_ID_RE = re.compile(
    r"[ \t]+\{#([A-Za-z][A-Za-z0-9_.:-]*)\}[ \t]*$"
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


class HtmlReferenceTargetParser(HTMLParser):
    def __init__(self) -> None:
        super().__init__(convert_charrefs=True)
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
        target_attribute = {"a": "href", "img": "src"}.get(folded_tag)
        if target_attribute is None:
            return
        for name, value in attributes:
            if name.casefold() == target_attribute:
                self.targets.append(value or "")

    def handle_endtag(self, tag: str) -> None:
        if tag.casefold() in INERT_HTML_CONTENT_TAGS and self.inert_depth:
            self.inert_depth -= 1


def html_reference_targets(text: str) -> list[str]:
    parser = HtmlReferenceTargetParser()
    parser.feed(text)
    parser.close()
    return parser.targets


class HtmlVisibleTextParser(HTMLParser):
    def __init__(self) -> None:
        super().__init__(convert_charrefs=True)
        self.parts: list[str] = []

    def handle_data(self, data: str) -> None:
        self.parts.append(data)


def html_visible_text(text: str) -> str:
    parser = HtmlVisibleTextParser()
    parser.feed(text)
    parser.close()
    return "".join(parser.parts)


class InertHtmlSubtreeMasker(HTMLParser):
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
            or any(name.casefold() == "hidden" for name, _value in attributes)
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
            or any(name.casefold() == "hidden" for name, _value in attributes)
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
    parser = InertHtmlSubtreeMasker()
    parser.feed(text)
    parser.close()
    return "".join(parser.parts)


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


def markdown_heading_text(markup: str) -> str:
    text = re.sub(r"!?\[([^\]]*)\]\([^)]+\)", r"\1", markup)
    text = re.sub(r"\[([^\]]+)\]\[[^\]]*\]", r"\1", text)
    text = re.sub(r"\[([^\]]+)\]", r"\1", text)
    text = text.replace("`", "")
    return html_visible_text(text)


def markdown_heading_slug(markup: str) -> str:
    heading = markdown_heading_text(markup).lower()
    heading = re.sub(r"[^\w _-]", "", heading)
    heading = re.sub(r"\s+", "-", heading)
    return heading.strip("-")


def rendered_heading_fragments(text: str) -> set[str]:
    rendered = without_inert_html_subtrees(rendered_markdown(text))
    fragments: set[str] = set()
    slug_counts: dict[str, int] = {}
    for match in ATX_HEADING_RE.finditer(rendered):
        heading = re.sub(r"[ \t]+#+[ \t]*$", "", match.group(1))
        explicit_id = EXPLICIT_HEADING_ID_RE.search(heading)
        if explicit_id is not None:
            fragments.add(explicit_id.group(1))
            continue
        slug = markdown_heading_slug(heading)
        if not slug:
            continue
        duplicate_index = slug_counts.get(slug, 0)
        slug_counts[slug] = duplicate_index + 1
        fragments.add(slug if duplicate_index == 0 else f"{slug}-{duplicate_index}")
    return fragments


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
    if parsed.fragment and candidate.suffix.casefold() in {".md", ".mdx"}:
        try:
            target_text = candidate.read_text(encoding="utf-8")
        except (OSError, UnicodeError):
            return None
        if parsed.fragment not in rendered_heading_fragments(target_text):
            return (
                "public link fragment does not exist: "
                f"{markdown_link_target(raw_target)}"
            )
    return None


def local_readme_link_violation(root: Path, raw_target: str) -> str | None:
    target = unquote(markdown_link_target(raw_target))
    parsed = urlsplit(target)
    if parsed.scheme or parsed.netloc or not parsed.path:
        return None
    canonical_root = root.resolve()
    candidate = (root / parsed.path).resolve()
    if not resolves_within(candidate, canonical_root):
        return f"local link leaves repository: {markdown_link_target(raw_target)}"
    if not candidate.is_file():
        return f"local link target does not exist: {markdown_link_target(raw_target)}"
    return None


def resolve_jazz_binary(
    root: Path, explicit: str | None
) -> tuple[Path | None, str | None]:
    if explicit is not None:
        binary = Path(explicit).resolve()
        if not binary.is_file():
            return None, f"Jazz executable does not exist: {binary}"
        return binary, None

    try:
        build = subprocess.run(["cabal", "build", "jazz"], cwd=root, check=False)
    except OSError as exc:
        return None, f"could not start cabal build jazz: {exc}"
    if build.returncode != 0:
        return None, "cabal build jazz failed"
    try:
        listed = subprocess.run(
            ["cabal", "list-bin", "jazz"],
            cwd=root,
            check=False,
            capture_output=True,
            text=True,
        )
    except OSError as exc:
        return None, f"could not start cabal list-bin jazz: {exc}"
    if listed.returncode != 0 or not listed.stdout.strip():
        return None, "cabal list-bin jazz failed"
    binary = Path(listed.stdout.strip()).resolve()
    if not binary.is_file():
        return None, f"cabal reported a missing Jazz executable: {binary}"
    return binary, None


def checked_jazz_environment() -> dict[str, str]:
    environment = os.environ.copy()
    for name in CLI_OVERRIDE_ENV:
        environment.pop(name, None)
    environment["JAZZ_WARNING_CONFIG"] = os.devnull
    return environment


def fragment_program(source: str) -> str:
    program = source.rstrip()
    if not program.endswith("."):
        program += "\n."
    return program + "\n"


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
    result = run_fragment_compiler(
        root, jazz_binary, display, fragment_program(source), violations
    )
    if result is None:
        return
    diagnostic_codes = set(DIAGNOSTIC_CODE_RE.findall(result.stderr))
    syntax_codes = diagnostic_codes.intersection(FRAGMENT_SYNTAX_ERROR_CODES)
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
    jazz_binary: Path,
    display: str,
    text: str,
    tracked_example_paths: set[str],
    canonical_examples_root: Path,
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
            validate_fragment_syntax(
                root,
                jazz_binary,
                display,
                fence.source,
                violations,
            )
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
    lines_with_endings = text.splitlines(keepends=True)
    exact_line_positions: dict[str, int] = {}
    offset = 0
    for line_with_ending in lines_with_endings:
        line = line_with_ending.rstrip("\r\n")
        exact_line_positions.setdefault(line, offset)
        offset += len(line_with_ending)

    line_count = len(text.splitlines())
    if not 100 <= line_count <= 150:
        violations.append(
            "README.md: must contain between 100 and 150 lines "
            f"(found {line_count})"
        )

    if README_TAGLINE not in exact_line_positions:
        violations.append("README.md: missing required tagline")
    rendered_contract_text = rendered_contract_text_with_code(text)
    if README_MATURITY_NOTICE not in rendered_contract_text:
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

    if not has_bound_executable_marker(text, README_FACTORIAL_PATH):
        violations.append("README.md: missing executable factorial marker")

    for command in (
        "nix develop",
        "cabal build all",
        f"cabal run jazz -- --run {README_FACTORIAL_PATH}",
    ):
        if command not in rendered_contract_text:
            violations.append(f"README.md: missing quick-start command: {command}")

    visible_text = without_inert_html_subtrees(visible_markdown(text))
    raw_link_targets = [
        match.group(1)
        for match in LINK_RE.finditer(visible_text)
        if not match.group(0).startswith("!")
    ]
    raw_link_targets.extend(used_reference_targets(visible_text))
    raw_link_targets.extend(
        html_reference_targets(
            without_inert_html_subtrees(html_source_markdown(text))
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

    for banned in README_BANNED_TERMS:
        if banned.casefold() in text.casefold():
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


def validate(root: Path, jazz_binary: Path) -> list[str]:
    violations: list[str] = []
    docs_root = root / "docs"
    if not docs_root.is_dir():
        return ["docs: missing public documentation directory"]
    canonical_docs_root = root.resolve() / "docs"
    canonical_examples_root = root.resolve() / "examples"
    tracked_example_paths = set(tracked_examples(root, violations))
    example_cases = validate_example_cases(root, tracked_example_paths, violations)

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

        if has_top_level_heading(markdown_body):
            violations.append(
                f"{display}: top-level heading duplicates front matter title"
            )

        for banned in BANNED_REFERENCES:
            if banned.casefold() in text.casefold():
                violations.append(f"{display}: banned public reference: {banned}")

        visible_body = without_inert_html_subtrees(
            visible_markdown(markdown_body)
        )
        raw_link_targets = [
            match.group(1) for match in LINK_RE.finditer(visible_body)
        ]
        raw_link_targets.extend(used_reference_targets(visible_body))
        raw_link_targets.extend(
            html_reference_targets(
                without_inert_html_subtrees(
                    html_source_markdown(markdown_body)
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
    jazz_binary, binary_error = resolve_jazz_binary(root, arguments.jazz_bin)
    if binary_error is not None or jazz_binary is None:
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
