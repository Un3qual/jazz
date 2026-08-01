#!/usr/bin/env python3
"""Validate the boundary between public Jazz docs and internal project records."""

from __future__ import annotations

import re
import subprocess
import sys
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
REFERENCE_DEFINITION_RE = re.compile(
    r"^[ \t]{0,3}\[([^\]]+)\]:[ \t]*(?:<([^>]+)>|(\S+))",
    re.MULTILINE,
)
FULL_REFERENCE_RE = re.compile(r"\[([^\]]+)\]\[([^\]]*)\]")
SHORTCUT_REFERENCE_RE = re.compile(r"\[([^\]]+)\](?![\[(])")
JAZZ_FENCE_RE = re.compile(
    r"^ {0,3}(?P<delimiter>`{3,}|~{3,})jazz[ \t]*\r?\n"
    r"(?P<source>.*?)^ {0,3}(?P=delimiter)[ \t]*(?:\r?\n|$)",
    re.MULTILINE | re.DOTALL,
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
EXAMPLE_MANIFEST_RE = re.compile(
    r"^JAZZ_EXAMPLE_MANIFEST=\([ \t]*\r?\n(?P<body>.*?)^\)[ \t]*$",
    re.MULTILINE | re.DOTALL,
)
EXAMPLE_MANIFEST_ENTRY_RE = re.compile(
    r'[ \t]*"(?P<path>[^"\r\n]+)"[ \t]*'
)


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


def validate_example_manifest(
    root: Path, tracked_example_paths: set[str], violations: list[str]
) -> set[str]:
    manifest_path = root / "scripts/check-examples.sh"
    try:
        manifest_text = manifest_path.read_text(encoding="utf-8")
    except (OSError, UnicodeError) as exc:
        violations.append(
            f"scripts/check-examples.sh: cannot read example manifest: {exc}"
        )
        return set()

    blocks = list(EXAMPLE_MANIFEST_RE.finditer(manifest_text))
    if len(blocks) != 1:
        violations.append(
            "scripts/check-examples.sh: expected exactly one "
            "JAZZ_EXAMPLE_MANIFEST array"
        )
        return set()

    manifest_paths: set[str] = set()
    for line in blocks[0].group("body").splitlines():
        if not line.strip():
            continue
        entry = EXAMPLE_MANIFEST_ENTRY_RE.fullmatch(line)
        if entry is None:
            violations.append(
                "scripts/check-examples.sh: invalid example manifest entry: "
                f"{line.strip()}"
            )
            continue
        example_path = entry.group("path")
        if example_path in manifest_paths:
            violations.append(
                "scripts/check-examples.sh: duplicate example manifest entry: "
                f"{example_path}"
            )
            continue
        manifest_paths.add(example_path)
        if not valid_example_path(example_path):
            violations.append(
                "scripts/check-examples.sh: invalid example manifest path: "
                f"{example_path}"
            )
        elif example_path not in tracked_example_paths:
            violations.append(
                "scripts/check-examples.sh: manifest entry is not a tracked example: "
                f"{example_path}"
            )

    for example_path in sorted(tracked_example_paths - manifest_paths):
        violations.append(
            f"{example_path}: tracked example is missing from "
            "scripts/check-examples.sh manifest"
        )
    return manifest_paths


def validate_jazz_fences(
    root: Path,
    display: str,
    text: str,
    tracked_example_paths: set[str],
    canonical_examples_root: Path,
    violations: list[str],
) -> set[str]:
    documented_examples: set[str] = set()
    fences = list(JAZZ_FENCE_RE.finditer(text))
    markers = [
        marker
        for marker in JAZZ_EXAMPLE_MARKER_RE.finditer(text)
        if not any(fence.start() <= marker.start() < fence.end() for fence in fences)
    ]
    consumed_marker_starts: set[int] = set()

    for fence in fences:
        preceding_marker = next(
            (
                marker
                for marker in reversed(markers)
                if marker.end() <= fence.start()
            ),
            None,
        )
        if preceding_marker is None or text[
            preceding_marker.end() : fence.start()
        ].strip():
            violations.append(
                f"{display}: Jazz fence must be immediately preceded by a "
                "jazz-example marker"
            )
            continue

        consumed_marker_starts.add(preceding_marker.start())
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
        if without_one_final_newline(fence.group("source")) != (
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


def validate(root: Path) -> list[str]:
    violations: list[str] = []
    docs_root = root / "docs"
    if not docs_root.is_dir():
        return ["docs: missing public documentation directory"]
    canonical_docs_root = root.resolve() / "docs"
    canonical_examples_root = root.resolve() / "examples"
    tracked_example_paths = set(tracked_examples(root, violations))
    validate_example_manifest(root, tracked_example_paths, violations)

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
            public_texts["README.md"] = readme_path.read_bytes().decode("utf-8")
        except (OSError, UnicodeError) as exc:
            violations.append(f"README.md: cannot read UTF-8 Markdown: {exc}")

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
