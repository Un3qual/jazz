#!/usr/bin/env bash
set -euo pipefail

ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
cd "$ROOT"

if ! command -v python3 >/dev/null 2>&1; then
  printf 'FAIL: python3 is required for scripts/check-execution-queue.sh\n' >&2
  exit 1
fi

python3 <<'PY'
from pathlib import Path
import re
import sys
from typing import Dict, List, Optional, Tuple

ROOT = Path.cwd().resolve()
QUEUE_PATH = ROOT / "docs/execution/queue.md"
QUEUE_TEXT: Optional[str] = None

EXPECTED_READY_HEADERS = [
    "id",
    "title",
    "priority",
    "size",
    "kind",
    "autonomous_ready",
    "depends_on",
    "plan",
    "plan_section",
    "target_paths",
    "deliverable",
    "verification",
    "last_verified",
]

EXPECTED_BLOCKED_HEADERS = [
    "id",
    "title",
    "blocked_on",
    "reason",
    "plan",
    "last_verified",
]

FAILURES: List[str] = []
DOC_SUFFIXES = {".md", ".markdown", ".rst", ".txt"}


def fail(message: str) -> None:
    FAILURES.append(message)


if not QUEUE_PATH.is_file():
    fail(f"missing required file: {QUEUE_PATH}")
else:
    QUEUE_TEXT = QUEUE_PATH.read_text()


def normalize_text(value: str) -> str:
    value = value.strip()
    if value.startswith("`") and value.endswith("`"):
        value = value[1:-1]
    value = value.replace("`", "")
    return re.sub(r"\s+", " ", value).strip()


def split_inline_list(value: str, delimiter: str) -> List[str]:
    items = [normalize_text(part) for part in value.split(delimiter)]
    return [item for item in items if item]


def strip_yaml_inline_comment(value: str) -> str:
    in_single = False
    in_double = False
    idx = 0
    while idx < len(value):
        char = value[idx]
        if char == "'" and not in_double:
            if in_single and idx + 1 < len(value) and value[idx + 1] == "'":
                idx += 2
                continue
            in_single = not in_single
        elif char == '"' and not in_single:
            if idx == 0 or value[idx - 1] != "\\":
                in_double = not in_double
        elif char == "#" and not in_single and not in_double:
            if idx == 0 or value[idx - 1].isspace():
                return value[:idx].rstrip()
        idx += 1
    return value.rstrip()


def is_yaml_comment_line(line: str) -> bool:
    return line.lstrip().startswith("#")


def parse_yaml_scalar_value(value: str) -> str:
    value = strip_yaml_inline_comment(value).strip()
    return value.strip('"').strip("'")


def normalize_target_path(value: str) -> Path:
    path = Path(normalize_text(value))
    parts = [part for part in path.parts if part != "."]
    if not parts:
        return Path(".")
    return Path(*parts)


def is_doc_target_path(path: Path) -> bool:
    return any(part == "docs" for part in path.parts) or path.suffix.lower() in DOC_SUFFIXES


def extract_section_lines(text: str, section_name: str) -> List[str]:
    marker = f"## {section_name}"
    lines = text.splitlines()
    in_section = False
    collected: List[str] = []
    for line in lines:
        if line == marker:
            in_section = True
            continue
        if in_section and line.startswith("## "):
            break
        if in_section:
            collected.append(line)
    if not in_section:
        fail(f"{QUEUE_PATH} missing section: {section_name}")
    return collected


def is_separator_cell(cell: str) -> bool:
    return re.fullmatch(r":?-{3,}:?", cell) is not None


def is_markdown_table_line(line: str) -> bool:
    stripped = line.lstrip(" ")
    return len(line) - len(stripped) <= 3 and stripped.startswith("|")


def split_markdown_row(line: str) -> List[str]:
    row = line.strip()
    if row.startswith("|"):
        row = row[1:]
    if row.endswith("|"):
        row = row[:-1]

    cells: List[str] = []
    current: List[str] = []
    idx = 0
    while idx < len(row):
        char = row[idx]
        if char == "\\" and idx + 1 < len(row) and row[idx + 1] in {"\\", "|"}:
            current.append(row[idx + 1])
            idx += 2
            continue
        if char == "|":
            cells.append("".join(current).strip())
            current = []
            idx += 1
            continue
        current.append(char)
        idx += 1

    cells.append("".join(current).strip())
    return cells


def parse_markdown_table(section_name: str) -> Tuple[List[str], List[Dict[str, str]]]:
    if QUEUE_TEXT is None:
        return [], []
    section_lines = extract_section_lines(QUEUE_TEXT, section_name)
    table_lines = [line.lstrip(" ") for line in section_lines if is_markdown_table_line(line)]
    if len(table_lines) < 2:
        fail(f"{QUEUE_PATH} section '{section_name}' is missing a markdown table")
        return [], []

    headers = split_markdown_row(table_lines[0])
    separator_cells = split_markdown_row(table_lines[1])
    separator_valid = len(separator_cells) == len(headers) and all(
        is_separator_cell(cell) for cell in separator_cells
    )
    if not separator_valid:
        fail(
            f"{QUEUE_PATH} section '{section_name}' has a missing or malformed "
            f"markdown separator row: {table_lines[1]}"
        )

    data_start = 2 if separator_valid else 1
    rows: List[Dict[str, str]] = []
    for row_index, line in enumerate(table_lines[data_start:], start=data_start + 1):
        cells = split_markdown_row(line)
        if len(cells) != len(headers):
            fail(
                f"{QUEUE_PATH} section '{section_name}' row {row_index} has "
                f"{len(cells)} cells; expected {len(headers)}: {line}"
            )
            continue
        rows.append(dict(zip(headers, cells)))
    return headers, rows


def extract_plan_path(cell: str) -> Optional[Path]:
    match = re.search(r"\[[^\]]+\]\(([^)]+)\)", cell)
    if not match:
        fail(f"{QUEUE_PATH} plan cell is not a markdown link: {cell}")
        return None
    plan_path = (QUEUE_PATH.parent / match.group(1)).resolve()
    try:
        plan_path.relative_to(ROOT)
    except ValueError:
        fail(f"{QUEUE_PATH} plan link escapes repository root: {cell}")
        return None
    if plan_path.suffix.lower() not in DOC_SUFFIXES:
        fail(f"{QUEUE_PATH} plan link must point to a text plan file: {cell}")
        return None
    return plan_path


def parse_block_scalar(lines: List[str], start_idx: int, folded: bool) -> Tuple[str, int]:
    values: List[str] = []
    idx = start_idx + 1
    while idx < len(lines):
        line = lines[idx]
        if line == "---":
            break
        if not line.strip():
            values.append("")
            idx += 1
            continue
        if line.startswith((" ", "\t")):
            values.append(line.lstrip(" \t"))
            idx += 1
            continue
        break

    if folded:
        text = " ".join(value.strip() for value in values if value.strip())
    else:
        text = "\n".join(value.rstrip() for value in values).strip()
    return text, idx


def parse_frontmatter(path: Path) -> Optional[Dict[str, object]]:
    try:
        text = path.read_text(encoding="utf-8")
    except (OSError, UnicodeDecodeError) as exc:
        fail(f"{path} could not be read as UTF-8 text: {exc}")
        return None
    lines = text.splitlines()
    if not lines or lines[0] != "---":
        fail(f"{path} missing YAML frontmatter")
        return None

    data: Dict[str, object] = {}
    idx = 1
    while idx < len(lines):
        line = lines[idx]
        if line == "---":
            break
        if not line.strip() or is_yaml_comment_line(line):
            idx += 1
            continue

        list_key = re.match(r"^([A-Za-z_][A-Za-z0-9_]*):\s*$", line)
        if list_key:
            key = list_key.group(1)
            values: List[str] = []
            idx += 1
            while idx < len(lines):
                if not lines[idx].strip() or is_yaml_comment_line(lines[idx]):
                    idx += 1
                    continue
                list_item = re.match(r"^[ ]+-\s+(.*)$", lines[idx])
                if not list_item:
                    break
                values.append(parse_yaml_scalar_value(list_item.group(1)))
                idx += 1
            data[key] = values
            continue

        scalar = re.match(r"^([A-Za-z_][A-Za-z0-9_]*):\s*(.*)$", line)
        if not scalar:
            fail(f"{path} has unsupported frontmatter line: {line}")
            idx += 1
            continue

        key, raw_value = scalar.groups()
        raw_value = raw_value.strip()
        if raw_value.startswith((">", "|")):
            data[key], idx = parse_block_scalar(lines, idx, raw_value.startswith(">"))
            continue
        if raw_value.startswith("[") and raw_value.endswith("]"):
            inner = raw_value[1:-1].strip()
            if inner:
                values = [parse_yaml_scalar_value(item) for item in inner.split(",") if item.strip()]
            else:
                values = []
            data[key] = values
        else:
            data[key] = parse_yaml_scalar_value(raw_value)
        idx += 1

    if idx >= len(lines) or lines[idx] != "---":
        fail(f"{path} frontmatter is missing a closing --- delimiter")
        return None
    if not data:
        fail(f"{path} frontmatter is empty")
        return None
    return data


ready_headers, ready_rows = parse_markdown_table("Ready Now")
blocked_headers, blocked_rows = parse_markdown_table("Blocked")
done_headers, done_rows = parse_markdown_table("Done")

if ready_headers and ready_headers != EXPECTED_READY_HEADERS:
    fail(
        f"{QUEUE_PATH} Ready Now headers do not match expected columns: "
        f"{ready_headers!r}"
    )
    ready_rows = []

if blocked_headers and blocked_headers != EXPECTED_BLOCKED_HEADERS:
    fail(
        f"{QUEUE_PATH} Blocked headers do not match expected columns: "
        f"{blocked_headers!r}"
    )
    blocked_rows = []

if done_headers and "id" not in [normalize_text(header) for header in done_headers]:
    fail(f"{QUEUE_PATH} Done headers must include an 'id' column: {done_headers!r}")
    done_rows = []

all_ids = set()
seen_ids: Dict[str, str] = {}
for section_name, rows in (
    ("Ready Now", ready_rows),
    ("Blocked", blocked_rows),
    ("Done", done_rows),
):
    for row in rows:
        row_id = normalize_text(row.get("id", ""))
        if not row_id:
            fail(f"{QUEUE_PATH} {section_name} row is missing id")
            continue
        if row_id in seen_ids:
            fail(
                f"{QUEUE_PATH} duplicate id {row_id!r} appears in both "
                f"{seen_ids[row_id]} and {section_name}"
            )
            continue
        seen_ids[row_id] = section_name
        all_ids.add(row_id)

for row in ready_rows:
    row_id = normalize_text(row["id"])
    if not row_id:
        continue
    if not normalize_text(row.get("last_verified", "")):
        fail(f"{QUEUE_PATH} Ready Now row {row_id} is missing last_verified")

    plan_path = extract_plan_path(row["plan"])
    if not plan_path:
        continue
    if not plan_path.is_file():
        fail(
            f"{QUEUE_PATH} Ready Now row {row_id} links to missing or non-file plan: "
            f"{plan_path}"
        )
        continue

    dependencies = split_inline_list(row["depends_on"], ",")
    if dependencies == ["-"]:
        dependencies = []
    for dep in dependencies:
        if dep not in all_ids:
            fail(f"{QUEUE_PATH} Ready Now row {row_id} has unresolved dependency id: {dep}")

    target_paths = split_inline_list(row["target_paths"], ",")
    if normalize_text(row["kind"]) == "impl":
        real_target_paths: List[Tuple[str, Path]] = []
        for target_path in target_paths:
            if not target_path or target_path == "-":
                continue
            target_path_obj = normalize_target_path(target_path)
            if target_path_obj.is_absolute() or ".." in target_path_obj.parts:
                real_target_paths.append((target_path, target_path_obj))
                continue
            if target_path_obj == Path(".") or is_doc_target_path(target_path_obj):
                continue
            real_target_paths.append((target_path, target_path_obj))
        if not real_target_paths:
            fail(f"{QUEUE_PATH} Ready Now row {row_id} is impl but has no target_paths")
        for target_path, target_path_obj in real_target_paths:
            if target_path_obj.is_absolute() or ".." in target_path_obj.parts:
                fail(
                    f"{QUEUE_PATH} Ready Now row {row_id} names non-repo-relative "
                    f"target path: {target_path}"
                )
                continue
            if not (ROOT / target_path_obj).exists():
                fail(
                    f"{QUEUE_PATH} Ready Now row {row_id} names missing target path: "
                    f"{target_path}"
                )

    frontmatter = parse_frontmatter(plan_path)
    if frontmatter is None:
        continue

    expected_scalars = {
        "id": normalize_text(row["id"]),
        "status": "ready",
        "priority": normalize_text(row["priority"]),
        "size": normalize_text(row["size"]),
        "kind": normalize_text(row["kind"]),
        "autonomous_ready": normalize_text(row["autonomous_ready"]),
        "last_verified": normalize_text(row["last_verified"]),
        "plan_section": normalize_text(row["plan_section"]),
        "deliverable": normalize_text(row["deliverable"]),
    }

    for key, expected_value in expected_scalars.items():
        actual_value = normalize_text(str(frontmatter.get(key, "")))
        if actual_value != expected_value:
            fail(
                f"{plan_path} frontmatter field '{key}' does not match queue row "
                f"{row_id}: expected {expected_value!r}, got {actual_value!r}"
            )

    expected_lists = {
        "depends_on": dependencies,
        "target_paths": target_paths,
        "verification": split_inline_list(row["verification"], ";"),
    }

    for key, expected_values in expected_lists.items():
        raw_values = frontmatter.get(key, [])
        if isinstance(raw_values, str):
            fail(
                f"{plan_path} frontmatter field '{key}' should be a list, "
                f"not a scalar: {raw_values!r}"
            )
            continue
        actual_values = [normalize_text(str(item)) for item in raw_values]
        if actual_values != expected_values:
            fail(
                f"{plan_path} frontmatter list '{key}' does not match queue row "
                f"{row_id}: expected {expected_values!r}, got {actual_values!r}"
            )

for row in blocked_rows:
    row_id = normalize_text(row["id"])
    if not row_id:
        continue
    if not normalize_text(row.get("last_verified", "")):
        fail(f"{QUEUE_PATH} Blocked row {row_id} is missing last_verified")

    plan_path = extract_plan_path(row["plan"])
    if plan_path and not plan_path.is_file():
        fail(
            f"{QUEUE_PATH} Blocked row {row_id} links to missing or non-file plan: "
            f"{plan_path}"
        )

if FAILURES:
    for message in FAILURES:
        print(f"FAIL: {message}", file=sys.stderr)
    sys.exit(1)

print("Execution queue checks passed.")
PY
