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

ROOT = Path.cwd()
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


def parse_markdown_table(section_name: str) -> Tuple[List[str], List[Dict[str, str]]]:
    if QUEUE_TEXT is None:
        return [], []
    section_lines = extract_section_lines(QUEUE_TEXT, section_name)
    table_lines = [line for line in section_lines if line.startswith("|")]
    if len(table_lines) < 2:
        fail(f"{QUEUE_PATH} section '{section_name}' is missing a markdown table")
        return [], []

    def split_row(line: str) -> List[str]:
        return [cell.strip() for cell in line.strip().strip("|").split("|")]

    headers = split_row(table_lines[0])
    separator_cells = split_row(table_lines[1])
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
        cells = split_row(line)
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
    return (QUEUE_PATH.parent / match.group(1)).resolve()


def parse_frontmatter(path: Path) -> Dict[str, object]:
    text = path.read_text()
    lines = text.splitlines()
    if not lines or lines[0] != "---":
        fail(f"{path} missing YAML frontmatter")
        return {}

    data: Dict[str, object] = {}
    idx = 1
    while idx < len(lines):
        line = lines[idx]
        if line == "---":
            break
        if not line.strip():
            idx += 1
            continue

        list_key = re.match(r"^([A-Za-z_][A-Za-z0-9_]*):\s*$", line)
        if list_key:
            key = list_key.group(1)
            values: List[str] = []
            idx += 1
            while idx < len(lines) and lines[idx].startswith("  - "):
                values.append(lines[idx][4:].strip())
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
        if raw_value.startswith("[") and raw_value.endswith("]"):
            inner = raw_value[1:-1].strip()
            if inner:
                values = [
                    item.strip().strip('"').strip("'")
                    for item in inner.split(",")
                    if item.strip()
                ]
            else:
                values = []
            data[key] = values
        else:
            data[key] = raw_value.strip('"').strip("'")
        idx += 1

    if idx >= len(lines) or lines[idx] != "---":
        fail(f"{path} frontmatter is missing a closing --- delimiter")
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
    if not normalize_text(row.get("last_verified", "")):
        fail(f"{QUEUE_PATH} Ready Now row {row_id} is missing last_verified")

    plan_path = extract_plan_path(row["plan"])
    if not plan_path:
        continue
    if not plan_path.exists():
        fail(f"{QUEUE_PATH} Ready Now row {row_id} links to missing plan: {plan_path}")
        continue

    dependencies = split_inline_list(row["depends_on"], ",")
    if dependencies == ["-"]:
        dependencies = []
    for dep in dependencies:
        if dep not in all_ids:
            fail(f"{QUEUE_PATH} Ready Now row {row_id} has unresolved dependency id: {dep}")

    target_paths = split_inline_list(row["target_paths"], ",")
    if normalize_text(row["kind"]) == "impl":
        real_target_paths = [
            path
            for path in target_paths
            if path and path != "-" and not path.startswith("docs/")
        ]
        if not real_target_paths:
            fail(f"{QUEUE_PATH} Ready Now row {row_id} is impl but has no target_paths")
        for target_path in real_target_paths:
            if not (ROOT / target_path).exists():
                fail(
                    f"{QUEUE_PATH} Ready Now row {row_id} names missing target path: "
                    f"{target_path}"
                )

    frontmatter = parse_frontmatter(plan_path)
    if not frontmatter:
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
        actual_values = [
            normalize_text(str(item)) for item in frontmatter.get(key, [])
        ]
        if actual_values != expected_values:
            fail(
                f"{plan_path} frontmatter list '{key}' does not match queue row "
                f"{row_id}: expected {expected_values!r}, got {actual_values!r}"
            )

for row in blocked_rows:
    row_id = normalize_text(row["id"])
    if not normalize_text(row.get("last_verified", "")):
        fail(f"{QUEUE_PATH} Blocked row {row_id} is missing last_verified")

    plan_path = extract_plan_path(row["plan"])
    if plan_path and not plan_path.exists():
        fail(f"{QUEUE_PATH} Blocked row {row_id} links to missing plan: {plan_path}")

if FAILURES:
    for message in FAILURES:
        print(f"FAIL: {message}", file=sys.stderr)
    sys.exit(1)

print("Execution queue checks passed.")
PY
