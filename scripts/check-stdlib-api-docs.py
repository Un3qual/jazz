#!/usr/bin/env python3
"""Check public standard-library exports against their API reference pages."""

from __future__ import annotations

import re
import sys
from collections import Counter
from pathlib import Path


MODULE_DOCUMENTS = {
    "Maybe.jz": "maybe.md",
    "Result.jz": "result.md",
    "NonEmpty.jz": "nonempty.md",
    "List.jz": "list.md",
    "Dictionary.jz": "dictionary.md",
    "Queue.jz": "queue.md",
    "Map.jz": "map.md",
    "Set.jz": "set.md",
    "Char.jz": "char.md",
    "Text.jz": "text.md",
    "IO.jz": "io.md",
    "IOError.jz": "io-error.md",
}
PRELUDE_HEADINGS = (
    "Ordering",
    "LT",
    "EQ",
    "GT",
    "Eq",
    "equals",
    "Ord",
    "compare",
    "Num",
    "Integral",
    "Fractional",
    "Showable",
    "show",
    "Default",
    "defaultValue",
)
PRELUDE_SIGNATURES = {
    "equals": "a -> a -> Bool",
    "compare": "a -> a -> Ordering",
    "show": "a -> Text",
    "defaultValue": "a",
    "map": "(a -> b) -> [a] -> [b]",
    "filter": "(a -> Bool) -> [a] -> [a]",
    "hd": "[a] -> a",
    "tl": "[a] -> [a]",
    "print!": "a -> a",
    "toInt8": "@{Num(a)}: a -> Int8",
    "toInt16": "@{Num(a)}: a -> Int16",
    "toInt32": "@{Num(a)}: a -> Int32",
    "toInt64": "@{Num(a)}: a -> Int64",
    "toUInt8": "@{Num(a)}: a -> UInt8",
    "toUInt16": "@{Num(a)}: a -> UInt16",
    "toUInt32": "@{Num(a)}: a -> UInt32",
    "toUInt64": "@{Num(a)}: a -> UInt64",
    "toFloat16": "@{Num(a)}: a -> Float16",
    "toFloat32": "@{Num(a)}: a -> Float32",
    "toFloat64": "@{Num(a)}: a -> Float64",
    "toInt": "@{Num(a)}: a -> Int64",
    "toFloat": "@{Num(a)}: a -> Float64",
}

MODULE_HEADER_RE = re.compile(r"\bmodule\s+\w+\s*\((.*?)\)\s*\{", re.DOTALL)
VALUE_EXPORT_RE = re.compile(r"\bvalue\s+([A-Za-z][A-Za-z0-9_!]*)")
TYPE_EXPORT_RE = re.compile(r"\btype\s+([A-Z][A-Za-z0-9_]*)(\(\.\.\))?")
CONSTRUCTOR_EXPORT_RE = re.compile(r"\bconstructor\s+([A-Z][A-Za-z0-9_]*)")
DATA_RE = re.compile(
    r"(?ms)^\s*data\s+([A-Z][A-Za-z0-9_]*)\b[^=]*=\s*(.*?)\.\s*$"
)
CONSTRUCTOR_RE = re.compile(r"(?:^|\|)\s*([A-Z][A-Za-z0-9_]*)\b")
SIGNATURE_RE = re.compile(
    r"(?m)^\s{2}([a-zA-Z][A-Za-z0-9_!]*)\s*::\s*(.*?)\.\s*$"
)


def display(root: Path, path: Path) -> str:
    try:
        return path.relative_to(root).as_posix()
    except ValueError:
        return path.as_posix()


def heading_count(document: str, name: str) -> int:
    pattern = re.compile(rf"(?m)^#{{2,3}}\s+`{re.escape(name)}`\s*$")
    return len(pattern.findall(document))


def exact_signature_present(document: str, name: str, signature: str) -> bool:
    expected = re.escape(f"{name} :: {signature}.")
    pattern = re.compile(
        rf"(?m)^(?P<fence>`{{3,}}|~{{3,}})jazz"
        rf"(?=[ \t])(?=[^\r\n]*(?<!\S)jazz-signature(?!\S))[^\r\n]*\r?\n"
        rf"{expected}\r?\n(?P=fence)[ \t]*$"
    )
    return pattern.search(document) is not None


def data_constructors(source: str) -> dict[str, tuple[str, ...]]:
    result: dict[str, tuple[str, ...]] = {}
    for match in DATA_RE.finditer(source):
        result[match.group(1)] = tuple(CONSTRUCTOR_RE.findall(match.group(2)))
    return result


def check_module(root: Path, source: Path, document: Path) -> list[str]:
    violations: list[str] = []
    source_text = source.read_text(encoding="utf-8")
    document_text = document.read_text(encoding="utf-8")
    header_match = MODULE_HEADER_RE.search(source_text)
    if header_match is None:
        return [f"{display(root, source)}: missing module export list"]

    header = header_match.group(1)
    values = VALUE_EXPORT_RE.findall(header)
    signatures = dict(SIGNATURE_RE.findall(source_text))
    required_headings: Counter[tuple[str, str]] = Counter()
    constructors_by_type = data_constructors(source_text)

    for type_name, exports_constructors in TYPE_EXPORT_RE.findall(header):
        required_headings[("type", type_name)] += 1
        if exports_constructors:
            for constructor in constructors_by_type.get(type_name, ()):
                required_headings[("constructor", constructor)] += 1
    for constructor in CONSTRUCTOR_EXPORT_RE.findall(header):
        required_headings[("constructor", constructor)] += 1

    seen_for_name: Counter[str] = Counter()
    for (kind, name), count in required_headings.items():
        seen_for_name[name] += count
        if heading_count(document_text, name) < seen_for_name[name]:
            violations.append(
                f"{display(root, document)}: missing heading for {kind} `{name}`"
            )

    for name in values:
        if heading_count(document_text, name) == 0:
            violations.append(
                f"{display(root, document)}: missing heading for value `{name}`"
            )
        signature = signatures.get(name)
        if signature is None:
            violations.append(
                f"{display(root, source)}: missing public signature for `{name}`"
            )
        elif not exact_signature_present(document_text, name, signature):
            violations.append(
                f"{display(root, document)}: missing exact signature for `{name}`"
            )
    return violations


def check_prelude(root: Path, document: Path) -> list[str]:
    violations: list[str] = []
    text = document.read_text(encoding="utf-8")
    for name in PRELUDE_HEADINGS:
        if heading_count(text, name) == 0:
            violations.append(
                f"{display(root, document)}: missing heading for Prelude `{name}`"
            )
    for name, signature in PRELUDE_SIGNATURES.items():
        if heading_count(text, name) == 0:
            violations.append(
                f"{display(root, document)}: missing heading for Prelude value `{name}`"
            )
        if not exact_signature_present(text, name, signature):
            violations.append(
                f"{display(root, document)}: missing exact signature for `{name}`"
            )
    return violations


def check_repository(root: Path) -> list[str]:
    violations: list[str] = []
    source_root = root / "jazz" / "stdlib"
    document_root = root / "docs" / "standard-library"
    for source_name, document_name in MODULE_DOCUMENTS.items():
        source = source_root / source_name
        document = document_root / document_name
        if not source.is_file():
            violations.append(f"{display(root, source)}: missing source module")
        elif not document.is_file():
            violations.append(f"{display(root, document)}: missing reference page")
        else:
            violations.extend(check_module(root, source, document))
    prelude = document_root / "prelude.md"
    if prelude.is_file():
        violations.extend(check_prelude(root, prelude))
    else:
        violations.append(f"{display(root, prelude)}: missing reference page")
    return violations


def main(argv: list[str]) -> int:
    root = Path(argv[1] if len(argv) > 1 else ".").resolve()
    violations = check_repository(root)
    if violations:
        for violation in violations:
            print(f"FAIL: {violation}", file=sys.stderr)
        return 1
    print("Standard-library API documentation checks passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
