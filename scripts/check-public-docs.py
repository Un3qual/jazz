#!/usr/bin/env python3
"""Validate Jazz's small, repository-specific public documentation contract.

Docusaurus owns Markdown, front matter, routes, and link resolution. This
checker owns only the public file inventory and explicit synchronization markers
that bind documentation to compiler-checked examples.
"""

from __future__ import annotations

import argparse
import hashlib
import os
import re
import subprocess
import sys
from pathlib import Path, PurePosixPath


ALLOWED_DOCS_ENTRIES = {
    "compiler",
    "getting-started",
    "index.md",
    "language",
    "project",
    "reference",
    "standard-library",
}
IGNORED_DOCS_ENTRIES = {".DS_Store"}
INTERNAL_TERMS = (
    ".codex/",
    "docs/execution",
    "docs/superpowers",
    "jazz-hs",
    "jazz-next",
    "jazz2",
    "jazznext",
    "rfcs/",
    "JavaScript output",
    "JavaScript artifact",
    "__kernel_",
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
    "standard-library/maybe.md",
    "standard-library/result.md",
    "standard-library/nonempty.md",
    "standard-library/dictionary.md",
    "standard-library/queue.md",
    "standard-library/map.md",
    "standard-library/set.md",
    "standard-library/char.md",
    "standard-library/text.md",
    "standard-library/io.md",
    "standard-library/io-error.md",
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
EXAMPLE_CASES = Path("scripts/example-cases.tsv")
FRAGMENT_RECEIPTS = Path("scripts/public-doc-fragments.tsv")
EXAMPLE_HEADER = ("name", "sources", "expected", "args")
FRAGMENT_HEADER = ("document", "ordinal", "sha256")
SHA256_RE = re.compile(r"[0-9a-f]{64}")
DRAFT_RE = re.compile(r"(?mi)^draft\s*:\s*true\s*(?:#.*)?$")
DIAGNOSTIC_RE = re.compile(r"(?m)\bE[0-9]{4}\b")
JAZZ_FENCE_RE = re.compile(
    r"(?m)^[ ]{0,3}(?:`{3,}|~{3,})jazz(?:[ \t].*)?\r?$", re.IGNORECASE
)
EXAMPLE_MARKER_RE = re.compile(r"<!--\s*jazz-example:.*?-->", re.DOTALL)
EXAMPLE_BINDING_RE = re.compile(
    r"<!--\s*jazz-example:\s*"
    r"(?:(?P<fragment>fragment)|executable\s+path=(?P<path>[^\s]+))\s*-->"
    r"\s*^```jazz(?:[ \t].*)?\r?\n(?P<source>.*?)\r?\n```[ \t]*\r?$",
    re.MULTILINE | re.DOTALL | re.IGNORECASE,
)
SIGNATURE_MARKER_RE = re.compile(r"<!--\s*jazz-signature\s*-->", re.IGNORECASE)
SIGNATURE_BINDING_RE = re.compile(
    r"<!--\s*jazz-signature\s*-->"
    r"\s*^```jazz(?:[ \t].*)?\r?\n.*?\r?\n```[ \t]*\r?$",
    re.MULTILINE | re.DOTALL | re.IGNORECASE,
)
OUTPUT_MARKER_RE = re.compile(r"<!--\s*jazz-example-output:.*?-->", re.DOTALL)
OUTPUT_BINDING_RE = re.compile(
    r"<!--\s*jazz-example-output:\s*case=(?P<case>[A-Za-z0-9][A-Za-z0-9_-]*)\s*-->"
    r"\s*^```text(?:[ \t].*)?\r?\n(?P<output>.*?)\r?\n```[ \t]*\r?$",
    re.MULTILINE | re.DOTALL | re.IGNORECASE,
)
CLI_OVERRIDE_ENV = (
    "JAZZ_PRELUDE",
    "JAZZ_WARNING_FLAGS",
    "JAZZ_WARNING_ERROR_FLAGS",
    "JAZZ_WARNING_CONFIG",
)


def display(root: Path, path: Path) -> str:
    return path.relative_to(root).as_posix()


def read_text(root: Path, path: Path, violations: list[str]) -> str | None:
    try:
        return path.read_text(encoding="utf-8")
    except (OSError, UnicodeError) as error:
        violations.append(f"{display(root, path)}: cannot read UTF-8 text: {error}")
        return None


def within(path: Path, directory: Path) -> bool:
    try:
        path.relative_to(directory)
    except ValueError:
        return False
    return True


def front_matter(text: str) -> str:
    lines = text.splitlines()
    if not lines or lines[0].strip() != "---":
        return ""
    for index, line in enumerate(lines[1:], 1):
        if line.strip() == "---":
            return "\n".join(lines[1:index])
    return ""


def normalized_fence(source: str) -> str:
    return source.replace("\r\n", "\n").removesuffix("\n")


def example_path(path: str) -> PurePosixPath | None:
    candidate = PurePosixPath(path)
    if (
        candidate.is_absolute()
        or len(candidate.parts) < 2
        or candidate.parts[0] != "examples"
        or ".." in candidate.parts
        or candidate.suffix != ".jz"
    ):
        return None
    return candidate


def load_example_cases(
    root: Path, violations: list[str]
) -> dict[str, tuple[frozenset[str], str]]:
    source = read_text(root, root / EXAMPLE_CASES, violations)
    if source is None:
        return {}
    rows = [tuple(line.split("\t")) for line in source.splitlines()]
    if not rows or rows[0] != EXAMPLE_HEADER:
        violations.append(f"{EXAMPLE_CASES}: invalid tab-separated header")
        return {}

    cases: dict[str, tuple[frozenset[str], str]] = {}
    for line_number, row in enumerate(rows[1:], 2):
        if len(row) != len(EXAMPLE_HEADER):
            violations.append(f"{EXAMPLE_CASES}:{line_number}: invalid row")
            continue
        name, raw_sources, expected, _arguments = row
        sources = frozenset(raw_sources.split(",")) if raw_sources else frozenset()
        if not name or not sources or not expected or name in cases:
            violations.append(f"{EXAMPLE_CASES}:{line_number}: invalid or duplicate case")
            continue
        for source_path in sorted(sources):
            pure_path = example_path(source_path)
            if pure_path is None or not (root / pure_path).is_file():
                violations.append(
                    f"{EXAMPLE_CASES}:{line_number}: invalid or missing source: {source_path}"
                )
        cases[name] = (sources, expected)
    return cases


def load_fragment_receipts(
    root: Path, violations: list[str]
) -> set[tuple[str, int, str]]:
    source = read_text(root, root / FRAGMENT_RECEIPTS, violations)
    if source is None:
        return set()
    rows = [tuple(line.split("\t")) for line in source.splitlines()]
    if not rows or rows[0] != FRAGMENT_HEADER:
        violations.append(f"{FRAGMENT_RECEIPTS}: invalid tab-separated header")
        return set()

    receipts: set[tuple[str, int, str]] = set()
    for line_number, row in enumerate(rows[1:], 2):
        try:
            document, raw_ordinal, digest = row
            ordinal = int(raw_ordinal)
        except ValueError:
            violations.append(f"{FRAGMENT_RECEIPTS}:{line_number}: invalid row")
            continue
        entry = (document, ordinal, digest)
        if ordinal < 1 or SHA256_RE.fullmatch(digest) is None or entry in receipts:
            violations.append(
                f"{FRAGMENT_RECEIPTS}:{line_number}: invalid or duplicate receipt"
            )
            continue
        receipts.add(entry)
    return receipts


def checked_environment() -> dict[str, str]:
    environment = os.environ.copy()
    for name in CLI_OVERRIDE_ENV:
        environment.pop(name, None)
    environment["JAZZ_WARNING_CONFIG"] = os.devnull
    return environment


def check_fragment_syntax(
    root: Path,
    jazz_binary: Path,
    document: str,
    source: str,
    violations: list[str],
) -> None:
    programs = [source.rstrip() + "\n"]
    if not source.rstrip().endswith("."):
        programs.append(source.rstrip() + "\n.\n")
    last_result: subprocess.CompletedProcess[str] | None = None
    try:
        for program in programs:
            last_result = subprocess.run(
                [str(jazz_binary), "--no-prelude"],
                cwd=root,
                env=checked_environment(),
                input=program,
                check=False,
                capture_output=True,
                text=True,
                timeout=30,
            )
            if last_result.returncode == 0:
                return
            if DIAGNOSTIC_RE.search(last_result.stderr) and "E0001" not in last_result.stderr:
                return
    except subprocess.TimeoutExpired:
        violations.append(f"{document}: Jazz fragment syntax check timed out")
        return
    except OSError as error:
        violations.append(f"{document}: could not run Jazz fragment check: {error}")
        return
    if last_result is not None:
        if DIAGNOSTIC_RE.search(last_result.stderr) is None:
            violations.append(
                f"{document}: Jazz fragment check failed without a compiler diagnostic"
            )
            return
        diagnostic = next(
            (line for line in last_result.stderr.splitlines() if "E0001" in line),
            "E0001",
        )
        violations.append(f"{document}: Jazz fragment has invalid syntax: {diagnostic}")


def check_inventory(root: Path, violations: list[str]) -> dict[str, str]:
    docs = root / "docs"
    if not docs.is_dir() or docs.is_symlink():
        violations.append("docs: missing public documentation directory")
        return {}
    canonical_docs = docs.resolve()

    for entry in sorted(docs.iterdir(), key=lambda candidate: candidate.name):
        if entry.name not in ALLOWED_DOCS_ENTRIES | IGNORED_DOCS_ENTRIES:
            violations.append(f"docs/{entry.name}: disallowed top-level docs entry")

    texts: dict[str, str] = {}
    for path in sorted(docs.rglob("*")):
        if path.name in IGNORED_DOCS_ENTRIES:
            continue
        label = display(root, path)
        if path.is_symlink():
            try:
                resolved = path.resolve(strict=True)
            except OSError:
                violations.append(f"{label}: documentation path does not exist")
                continue
            if not within(resolved, canonical_docs):
                violations.append(f"{label}: documentation path resolves outside docs/")
                continue
        if not path.is_file():
            continue
        if path.suffix.casefold() != ".md":
            violations.append(f"{label}: public docs files must use the .md extension")
            continue
        source = read_text(root, path, violations)
        if source is not None:
            texts[label] = source

    for relative in REQUIRED_PAGES:
        label = f"docs/{relative}"
        path = docs / relative
        if not path.is_file() or label not in texts:
            violations.append(f"{label}: missing required public page")
            continue
        if DRAFT_RE.search(front_matter(texts[label])):
            violations.append(f"{label}: required public page cannot be draft")

    readme = root / "README.md"
    if not readme.is_file():
        violations.append("README.md: missing project front door")
    else:
        source = read_text(root, readme, violations)
        if source is not None:
            texts["README.md"] = source

    for label, source in texts.items():
        folded = source.casefold()
        for term in INTERNAL_TERMS:
            if term.casefold() in folded:
                violations.append(f"{label}: banned public reference: {term}")
    return texts


def check_example_sync(
    root: Path,
    texts: dict[str, str],
    cases: dict[str, tuple[frozenset[str], str]],
    receipts: set[tuple[str, int, str]],
    jazz_binary: Path | None,
    violations: list[str],
) -> None:
    documented_sources: set[str] = set()
    documented_cases: set[str] = set()
    observed_receipts: set[tuple[str, int, str]] = set()

    for label, source in sorted(texts.items()):
        bindings = list(EXAMPLE_BINDING_RE.finditer(source))
        markers = list(EXAMPLE_MARKER_RE.finditer(source))
        signature_bindings = list(SIGNATURE_BINDING_RE.finditer(source))
        signature_markers = list(SIGNATURE_MARKER_RE.finditer(source))
        if (
            len(bindings) != len(markers)
            or len(signature_bindings) != len(signature_markers)
            or len(bindings) + len(signature_bindings)
            != len(JAZZ_FENCE_RE.findall(source))
        ):
            violations.append(
                f"{label}: Jazz fence must have an adjacent jazz-example marker or jazz-signature marker"
            )

        document_sources: set[str] = set()
        fragment_ordinal = 0
        for binding in bindings:
            fence_source = normalized_fence(binding.group("source"))
            source_path = binding.group("path")
            if source_path is not None:
                pure_path = example_path(source_path)
                candidate = root / pure_path if pure_path is not None else None
                if candidate is None or not candidate.is_file():
                    violations.append(f"{label}: invalid executable example: {source_path}")
                    continue
                candidate_source = read_text(root, candidate, violations)
                if candidate_source is None:
                    continue
                expected_source = normalized_fence(candidate_source)
                if fence_source != expected_source:
                    violations.append(
                        f"{label}: executable fence differs from {source_path}"
                    )
                    continue
                document_sources.add(source_path)
                documented_sources.add(source_path)
                continue

            fragment_ordinal += 1
            receipt = (
                label,
                fragment_ordinal,
                hashlib.sha256(
                    (binding.group("source") + "\n").encode("utf-8")
                ).hexdigest(),
            )
            observed_receipts.add(receipt)
            if jazz_binary is not None:
                check_fragment_syntax(
                    root, jazz_binary, label, binding.group("source"), violations
                )

        outputs = list(OUTPUT_BINDING_RE.finditer(source))
        output_markers = list(OUTPUT_MARKER_RE.finditer(source))
        if len(outputs) != len(output_markers):
            violations.append(
                f"{label}: jazz-example-output marker must have an adjacent text fence"
            )
        for output in outputs:
            case_name = output.group("case")
            case = cases.get(case_name)
            if case is None:
                violations.append(
                    f"{label}: documented output names unknown case: {case_name}"
                )
                continue
            expected_sources, expected_output = case
            if not expected_sources.issubset(document_sources):
                violations.append(
                    f"{label}: documented output for case {case_name} must be alongside its sources"
                )
                continue
            if normalized_fence(output.group("output")) != expected_output:
                violations.append(
                    f"{label}: documented output for case {case_name} differs from {EXAMPLE_CASES}"
                )
                continue
            documented_cases.add(case_name)

    required_sources = (
        set().union(*(sources for sources, _ in cases.values())) if cases else set()
    )
    for source_path in sorted(documented_sources - required_sources):
        violations.append(
            f"{source_path}: executable public fence is not compiler-backed by {EXAMPLE_CASES}"
        )
    for source_path in sorted(required_sources - documented_sources):
        violations.append(f"{source_path}: example has no synchronized public fence")
    for case_name in sorted(set(cases) - documented_cases):
        violations.append(f"{EXAMPLE_CASES}: case {case_name} has no synchronized output")
    if observed_receipts != receipts:
        violations.append(f"{FRAGMENT_RECEIPTS}: fragment receipt is missing or stale")


def validate(root: Path, jazz_binary: Path | None) -> list[str]:
    violations: list[str] = []
    texts = check_inventory(root, violations)
    cases = load_example_cases(root, violations)
    receipts = load_fragment_receipts(root, violations)
    check_example_sync(root, texts, cases, receipts, jazz_binary, violations)
    return sorted(set(violations))


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "repository_root",
        nargs="?",
        type=Path,
        default=Path(__file__).resolve().parent.parent,
    )
    parser.add_argument("--jazz-bin", type=Path)
    arguments = parser.parse_args(argv[1:])
    root = arguments.repository_root.resolve()
    jazz_binary = arguments.jazz_bin.resolve() if arguments.jazz_bin else None
    if jazz_binary is not None and not jazz_binary.is_file():
        print(f"FAIL: repository: Jazz executable does not exist: {jazz_binary}")
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
