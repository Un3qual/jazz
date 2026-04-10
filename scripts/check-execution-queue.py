from __future__ import annotations

import ast
from pathlib import Path
import re
import sys

ROOT = Path(__file__).resolve().parent.parent
QUEUE_PATH = ROOT / "docs/execution/queue.md"
QUEUE_TEXT: str | None = None

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

FAILURES: list[str] = []
DOC_SUFFIXES = {".md", ".markdown", ".rst", ".txt"}
ALLOWED_READY_KINDS = {"impl", "docs", "coordination"}
ALLOWED_PRIORITIES = {"P1", "P2", "P3"}
ALLOWED_SIZES = {"S", "M", "L"}
ALLOWED_AUTONOMOUS_READY = {"yes", "no"}


def fail(message: str) -> None:
    FAILURES.append(message)


if not QUEUE_PATH.is_file():
    fail(f"missing required file: {QUEUE_PATH}")
else:
    try:
        QUEUE_TEXT = QUEUE_PATH.read_text(encoding="utf-8")
    except (OSError, UnicodeDecodeError) as exc:
        fail(f"{QUEUE_PATH} could not be read as UTF-8 text: {exc}")


def normalize_text(value: str) -> str:
    value = value.strip()
    if value.startswith("`") and value.endswith("`"):
        value = value[1:-1]
    value = value.replace("`", "")
    return re.sub(r"\s+", " ", value).strip()


def normalize_list_item(value: str) -> str:
    """Strip queue-formatting backticks without altering item contents."""
    value = value.strip()
    if value.startswith("`") and value.endswith("`"):
        value = value[1:-1]
    return value.strip()


def split_inline_list(value: str, delimiter: str, normalizer=None) -> list[str]:
    if normalizer is None:
        normalizer = normalize_text
    items = [normalizer(part) for part in value.split(delimiter)]
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
            backslash_count = 0
            check_idx = idx - 1
            while check_idx >= 0 and value[check_idx] == "\\":
                backslash_count += 1
                check_idx -= 1
            if backslash_count % 2 == 0:
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
    if len(value) >= 2 and value[0] == value[-1] == "'":
        return value[1:-1].replace("''", "'")
    if len(value) >= 2 and value[0] == value[-1] == '"':
        try:
            parsed_value = ast.literal_eval(value)
        except (SyntaxError, ValueError):
            return value[1:-1]
        return parsed_value if isinstance(parsed_value, str) else str(parsed_value)
    return value


def split_yaml_flow_list(value: str) -> list[str]:
    inner = value[1:-1].strip()
    if not inner:
        return []

    items: list[str] = []
    current: list[str] = []
    in_single = False
    in_double = False
    idx = 0
    while idx < len(inner):
        char = inner[idx]
        if char == "'" and not in_double:
            if in_single and idx + 1 < len(inner) and inner[idx + 1] == "'":
                current.extend(("'", "'"))
                idx += 2
                continue
            in_single = not in_single
            current.append(char)
            idx += 1
            continue
        if char == '"' and not in_single:
            backslash_count = 0
            check_idx = idx - 1
            while check_idx >= 0 and inner[check_idx] == "\\":
                backslash_count += 1
                check_idx -= 1
            if backslash_count % 2 == 0:
                in_double = not in_double
            current.append(char)
            idx += 1
            continue
        if char == "," and not in_single and not in_double:
            item = "".join(current).strip()
            if item:
                items.append(parse_yaml_scalar_value(item))
            current = []
            idx += 1
            continue
        current.append(char)
        idx += 1

    item = "".join(current).strip()
    if item:
        items.append(parse_yaml_scalar_value(item))
    return items


def normalize_target_path(value: str) -> Path:
    path = Path(normalize_text(value))
    parts = [part for part in path.parts if part != "."]
    if not parts:
        return Path(".")
    return Path(*parts)


def is_doc_target_path(path: Path) -> bool:
    return any(part == "docs" for part in path.parts) or path.suffix.lower() in DOC_SUFFIXES


def extract_section_lines(text: str, section_name: str) -> list[str]:
    marker = f"## {section_name}"
    lines = text.splitlines()
    in_section = False
    collected: list[str] = []
    for line in lines:
        stripped = line.strip()
        if stripped == marker:
            in_section = True
            continue
        if in_section and stripped.startswith("## "):
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
    return (
        len(line) - len(stripped) <= 3
        and "|" in stripped
        and len(split_markdown_row(stripped)) >= 2
    )


def split_markdown_row(line: str) -> list[str]:
    row = line.strip()
    if row.startswith("|"):
        row = row[1:]
    if row.endswith("|"):
        row = row[:-1]

    cells: list[str] = []
    current: list[str] = []
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


def parse_markdown_table(section_name: str) -> tuple[list[str], list[dict[str, str]]]:
    if QUEUE_TEXT is None:
        return [], []
    section_lines = extract_section_lines(QUEUE_TEXT, section_name)
    table_lines: list[str] = []
    in_table = False
    for idx, line in enumerate(section_lines):
        stripped = line.lstrip(" ")
        if is_markdown_table_line(line):
            table_lines.append(stripped)
            in_table = True
            continue
        if in_table:
            if any(is_markdown_table_line(rest) for rest in section_lines[idx + 1 :]):
                fail(
                    f"{QUEUE_PATH} section '{section_name}' has non-table content "
                    "splitting its markdown table"
                )
            break
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
        return [], []

    data_start = 2
    rows: list[dict[str, str]] = []
    for row_index, line in enumerate(table_lines[data_start:], start=data_start + 1):
        cells = split_markdown_row(line)
        if len(cells) != len(headers):
            fail(
                f"{QUEUE_PATH} section '{section_name}' row {row_index} has "
                f"{len(cells)} cells; expected {len(headers)}: {line}"
            )
            continue
        rows.append(dict(zip(headers, cells, strict=True)))
    return headers, rows


def extract_plan_path(cell: str) -> Path | None:
    match = re.fullmatch(r"\[[^\]]+\]\(([^)]+)\)", cell.strip())
    if not match:
        fail(f"{QUEUE_PATH} plan cell is not a markdown link: {cell}")
        return None
    link_target = match.group(1).split("#", 1)[0]
    if not link_target:
        fail(f"{QUEUE_PATH} plan link is missing a file target: {cell}")
        return None
    plan_path = (QUEUE_PATH.parent / link_target).resolve()
    try:
        plan_path.relative_to(ROOT)
    except ValueError:
        fail(f"{QUEUE_PATH} plan link escapes repository root: {cell}")
        return None
    if plan_path.suffix.lower() not in DOC_SUFFIXES:
        fail(f"{QUEUE_PATH} plan link must point to a text plan file: {cell}")
        return None
    return plan_path


# parse_block_scalar handles only basic YAML block scalars (">" and "|").
# The folded parameter selects folded-vs-literal behavior; chomping indicators
# (-/+) and explicit indentation indicators are not supported by this parser.
# That matches the current frontmatter contract and should be revisited only if
# those modifiers become part of the queue format.
def parse_block_scalar(lines: list[str], start_idx: int, folded: bool) -> tuple[str, int]:
    values: list[str] = []
    idx = start_idx + 1
    while idx < len(lines):
        line = lines[idx]
        # An unindented delimiter closes frontmatter; indented `---` is content.
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


def parse_frontmatter(path: Path) -> dict[str, object] | None:
    try:
        text = path.read_text(encoding="utf-8")
    except (OSError, UnicodeDecodeError) as exc:
        fail(f"{path} could not be read as UTF-8 text: {exc}")
        return None
    lines = text.splitlines()
    if not lines or lines[0] != "---":
        fail(f"{path} missing YAML frontmatter")
        return None

    data: dict[str, object] = {}
    idx = 1
    while idx < len(lines):
        line = lines[idx]
        if line == "---":
            break
        if not line.strip() or is_yaml_comment_line(line):
            idx += 1
            continue

        list_key = re.match(r"^([A-Za-z_][A-Za-z0-9_]*):\s*(?:#.*)?$", line)
        if list_key:
            key = list_key.group(1)
            if key in data:
                fail(f"{path} has duplicate frontmatter key: {key}")
                return None
            values: list[str] = []
            idx += 1
            while idx < len(lines):
                if lines[idx] == "---":
                    break
                if not lines[idx].strip() or is_yaml_comment_line(lines[idx]):
                    idx += 1
                    continue
                list_item = re.match(r"^[ ]*-\s*(.*)$", lines[idx])
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
        if key in data:
            fail(f"{path} has duplicate frontmatter key: {key}")
            return None
        raw_value = raw_value.strip()
        parsed_value = strip_yaml_inline_comment(raw_value).strip()
        if parsed_value in {">", "|"}:
            data[key], idx = parse_block_scalar(lines, idx, parsed_value == ">")
            continue
        if parsed_value.startswith((">", "|")):
            fail(
                f"{path} frontmatter field '{key}' uses unsupported block scalar "
                f"modifier: {raw_value!r}"
            )
            # Consume the block scalar body to avoid repeated errors on indented lines
            header_indent = len(line) - len(line.lstrip())
            idx += 1
            while idx < len(lines):
                if lines[idx] == "---":
                    break
                if not lines[idx].strip():
                    idx += 1
                    continue
                line_indent = len(lines[idx]) - len(lines[idx].lstrip())
                if line_indent > header_indent:
                    idx += 1
                    continue
                break
            continue
        if parsed_value.startswith("[") and parsed_value.endswith("]"):
            data[key] = split_yaml_flow_list(parsed_value)
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
seen_ids: dict[str, str] = {}
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
    row_kind = normalize_text(row["kind"])
    row_priority = normalize_text(row["priority"])
    row_size = normalize_text(row["size"])
    row_autonomous_ready = normalize_text(row["autonomous_ready"])
    if row_kind not in ALLOWED_READY_KINDS:
        fail(
            f"{QUEUE_PATH} Ready Now row {row_id} has unsupported kind: "
            f"{row['kind']!r}"
        )
    if row_priority not in ALLOWED_PRIORITIES:
        fail(
            f"{QUEUE_PATH} Ready Now row {row_id} has unsupported priority: "
            f"{row['priority']!r}"
        )
    if row_size not in ALLOWED_SIZES:
        fail(
            f"{QUEUE_PATH} Ready Now row {row_id} has unsupported size: "
            f"{row['size']!r}"
        )
    if row_autonomous_ready not in ALLOWED_AUTONOMOUS_READY:
        fail(
            f"{QUEUE_PATH} Ready Now row {row_id} has unsupported autonomous_ready: "
            f"{row['autonomous_ready']!r}"
        )
    if not normalize_text(row["plan_section"]):
        fail(f"{QUEUE_PATH} Ready Now row {row_id} is missing plan_section")
    if not normalize_text(row["deliverable"]):
        fail(f"{QUEUE_PATH} Ready Now row {row_id} is missing deliverable")
    if normalize_list_item(row["verification"]) == "-":
        verification_commands = []
    else:
        verification_commands = split_inline_list(
            row["verification"], ";", normalize_list_item
        )
        if any(cmd == "-" for cmd in verification_commands):
            fail(
                f"{QUEUE_PATH} Ready Now row {row_id} has malformed "
                "verification sentinel"
            )
            verification_commands = []
    if not verification_commands:
        fail(f"{QUEUE_PATH} Ready Now row {row_id} is missing verification")

    plan_path = extract_plan_path(normalize_text(row["plan"]))
    if not plan_path:
        pass  # Continue with queue-only checks below
    elif not plan_path.is_file():
        fail(
            f"{QUEUE_PATH} Ready Now row {row_id} links to missing or non-file plan: "
            f"{plan_path}"
        )
        # Continue with queue-only checks below

    dependencies = split_inline_list(row["depends_on"], ",", normalize_list_item)
    if dependencies == ["-"]:
        dependencies = []
    for dep in dependencies:
        if dep == row_id:
            fail(f"{QUEUE_PATH} Ready Now row {row_id} cannot depend on itself")
            continue
        if dep not in all_ids:
            fail(f"{QUEUE_PATH} Ready Now row {row_id} has unresolved dependency id: {dep}")

    target_paths = split_inline_list(row["target_paths"], ",", normalize_list_item)
    if row_kind == "impl":
        real_target_paths: list[tuple[str, Path]] = []
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
            fail(
                f"{QUEUE_PATH} Ready Now row {row_id} is impl but has no "
                f"concrete non-doc target_paths"
            )
        for target_path, target_path_obj in real_target_paths:
            if target_path_obj.is_absolute() or ".." in target_path_obj.parts:
                fail(
                    f"{QUEUE_PATH} Ready Now row {row_id} names non-repo-relative "
                    f"target path: {target_path}"
                )
                continue
            if not (ROOT / target_path_obj).is_file():
                fail(
                    f"{QUEUE_PATH} Ready Now row {row_id} names missing or non-file "
                    f"target path: "
                    f"{target_path}"
                )

    if not plan_path or not plan_path.is_file():
        continue  # Skip frontmatter validation if plan is missing

    frontmatter = parse_frontmatter(plan_path)
    if frontmatter is None:
        continue

    # Check status separately: it is inferred from the entry living in Ready Now
    actual_status = normalize_text(str(frontmatter.get("status", "")))
    if actual_status != "ready":
        fail(
            f"{plan_path} frontmatter field 'status' must be 'ready' for queue row "
            f"{row_id} in Ready Now section: got {actual_status!r}"
        )

    expected_scalars = {
        "id": normalize_text(row["id"]),
        "priority": row_priority,
        "size": row_size,
        "kind": row_kind,
        "autonomous_ready": row_autonomous_ready,
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
        "verification": verification_commands,
    }

    for key, expected_values in expected_lists.items():
        raw_values = frontmatter.get(key, [])
        if isinstance(raw_values, str):
            fail(
                f"{plan_path} frontmatter field '{key}' should be a list, "
                f"not a scalar: {raw_values!r}"
            )
            continue
        if not isinstance(raw_values, list):
            fail(
                f"{plan_path} frontmatter field '{key}' should be a list, "
                f"got {type(raw_values).__name__}: {raw_values!r}"
            )
            continue
        actual_values = [normalize_list_item(str(item)) for item in raw_values]
        expected_compare = expected_values
        actual_compare = actual_values
        # depends_on is set-like; target_paths and verification stay ordered so
        # the queue and frontmatter describe the same current batch shape.
        if key == "depends_on":
            expected_compare = sorted(expected_values)
            actual_compare = sorted(actual_values)
        if actual_compare != expected_compare:
            fail(
                f"{plan_path} frontmatter list '{key}' does not match queue row "
                f"{row_id}: expected {expected_values!r}, got {actual_values!r}"
            )

for row in blocked_rows:
    row_id = normalize_text(row["id"])
    if not row_id:
        continue
    blocked_on = normalize_text(row.get("blocked_on", ""))
    if not blocked_on or blocked_on == "-":
        fail(f"{QUEUE_PATH} Blocked row {row_id} is missing blocked_on")
    if not normalize_text(row.get("last_verified", "")):
        fail(f"{QUEUE_PATH} Blocked row {row_id} is missing last_verified")

    plan_text = normalize_text(row.get("plan", ""))
    if plan_text and plan_text != "-":
        plan_path = extract_plan_path(plan_text)
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
