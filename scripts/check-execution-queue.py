from __future__ import annotations

import ast
from pathlib import Path
import re
import sys

ROOT = Path(__file__).resolve().parent.parent
QUEUE_PATH = ROOT / "docs/execution/queue.md"
BLOCKER_CONTRACTS_PATH = ROOT / "docs/execution/blocker-contracts.md"
DONE_ARCHIVE_PATH = ROOT / "docs/execution/done-archive.md"
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

EXPECTED_CURATION_HEADERS = [
    "blocked_id",
    "candidate_child_id",
    "kind",
    "source_contract",
    "why_next",
    "target_paths",
    "verification",
    "promotion_check",
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


def extract_section_lines(
    text: str, section_name: str, source_path: Path, required: bool = True
) -> list[str] | None:
    marker = f"## {section_name}"
    lines = text.splitlines()
    section_starts = [
        idx for idx, line in enumerate(lines) if line.strip() == marker
    ]
    if not section_starts:
        if required:
            fail(f"{source_path} missing section: {section_name}")
        return None
    if len(section_starts) > 1:
        fail(f"{source_path} section '{section_name}' appears multiple times")
        return None

    collected: list[str] = []
    for line in lines[section_starts[0] + 1 :]:
        stripped = line.strip()
        if stripped.startswith("## "):
            break
        collected.append(line)
    return collected


def is_separator_cell(cell: str) -> bool:
    return re.fullmatch(r":?-{3,}:?", cell) is not None


def is_markdown_table_line(line: str) -> bool:
    stripped = line.lstrip(" ")
    return (
        len(line) - len(stripped) <= 3
        and stripped.startswith("|")
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


def parse_markdown_table(
    section_name: str, required: bool = True
) -> tuple[list[str], list[dict[str, str]]]:
    if QUEUE_TEXT is None:
        return [], []
    section_lines = extract_section_lines(
        QUEUE_TEXT, section_name, QUEUE_PATH, required
    )
    if section_lines is None:
        return [], []
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
        rows.append(dict(zip(headers, cells)))
    return headers, rows


def extract_markdown_link(cell: str, label: str) -> tuple[Path, str | None] | None:
    match = re.fullmatch(r"\[[^\]]+\]\(([^)]+)\)", cell.strip())
    if not match:
        fail(f"{QUEUE_PATH} {label} cell is not a markdown link: {cell}")
        return None
    link_target = match.group(1)
    file_target, fragment = (
        link_target.split("#", 1) if "#" in link_target else (link_target, None)
    )
    if not file_target:
        fail(f"{QUEUE_PATH} {label} link is missing a file target: {cell}")
        return None
    link_path = (QUEUE_PATH.parent / file_target).resolve()
    try:
        link_path.relative_to(ROOT)
    except ValueError:
        fail(f"{QUEUE_PATH} {label} link escapes repository root: {cell}")
        return None
    if link_path.suffix.lower() not in DOC_SUFFIXES:
        fail(f"{QUEUE_PATH} {label} link must point to a text file: {cell}")
        return None
    return link_path, fragment


def extract_markdown_link_path(cell: str, label: str) -> Path | None:
    link = extract_markdown_link(cell, label)
    if link is None:
        return None
    link_path, _fragment = link
    return link_path


def extract_plan_path(cell: str) -> Path | None:
    return extract_markdown_link_path(cell, "plan")


def markdown_heading_anchor(heading: str) -> str:
    heading = re.sub(r"`([^`]*)`", r"\1", heading)
    heading = normalize_text(heading).lower()
    heading = re.sub(r"[^a-z0-9 _-]", "", heading)
    heading = re.sub(r"\s+", "-", heading)
    return heading.strip("-")


def read_text_file(path: Path, label: str) -> str | None:
    try:
        return path.read_text(encoding="utf-8")
    except FileNotFoundError:
        fail(f"missing required {label}: {path}")
    except (OSError, UnicodeDecodeError) as exc:
        fail(f"{path} could not be read as UTF-8 text: {exc}")
    return None


def extract_markdown_heading_index(path: Path) -> tuple[set[str], dict[str, str]]:
    text = read_text_file(path, "markdown file")
    if text is None:
        return set(), {}

    headings: set[str] = set()
    anchors: dict[str, str] = {}
    for line in text.splitlines():
        match = re.match(r"^(#{1,6})\s+(.+?)\s*#*\s*$", line)
        if not match:
            continue
        heading = normalize_text(match.group(2).strip())
        anchor = markdown_heading_anchor(heading)
        if not anchor:
            continue
        if anchor in anchors:
            fail(f"{path} has duplicate markdown anchor #{anchor}")
            continue
        anchors[anchor] = heading
        headings.add(heading)
    return headings, anchors


def extract_contract_candidate_child(value: str) -> str:
    code_match = re.search(r"`([^`]+)`", value)
    if code_match:
        return normalize_text(code_match.group(1))
    return normalize_text(value).rstrip(".")


def extract_blocker_contract_candidate_children(path: Path) -> dict[str, str]:
    text = read_text_file(path, "blocker contracts")
    if text is None:
        return {}

    candidate_children: dict[str, str] = {}
    current_heading: str | None = None
    for line in text.splitlines():
        heading_match = re.match(r"^(#{1,6})\s+(.+?)\s*#*\s*$", line)
        if heading_match:
            level = len(heading_match.group(1))
            if level == 3:
                current_heading = normalize_text(heading_match.group(2).strip())
            elif level < 3:
                current_heading = None
            continue

        if current_heading is None:
            continue

        candidate_match = re.match(
            r"^\s*-\s*Candidate child:\s*(.+?)\s*$", line, re.IGNORECASE
        )
        if not candidate_match:
            continue
        if current_heading in candidate_children:
            fail(f"{path} section {current_heading} has multiple Candidate child entries")
            continue
        candidate_child = extract_contract_candidate_child(candidate_match.group(1))
        if candidate_child:
            candidate_children[current_heading] = candidate_child

    return candidate_children


def extract_done_archive_ids() -> set[str]:
    text = read_text_file(DONE_ARCHIVE_PATH, "done archive")
    if text is None:
        return set()
    section_lines = extract_section_lines(text, "Done", DONE_ARCHIVE_PATH)
    if section_lines is None:
        return set()

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
                    f"{DONE_ARCHIVE_PATH} section 'Done' has non-table content "
                    "splitting its markdown table"
                )
            break

    if len(table_lines) < 2:
        fail(f"{DONE_ARCHIVE_PATH} section 'Done' is missing a markdown table")
        return set()

    headers = [
        normalize_text(header).lower()
        for header in split_markdown_row(table_lines[0])
    ]
    if "id" not in headers:
        fail(
            f"{DONE_ARCHIVE_PATH} Done headers must include an 'id' column: "
            f"{headers!r}"
        )
        return set()
    id_index = headers.index("id")

    separator_cells = split_markdown_row(table_lines[1])
    separator_valid = len(separator_cells) == len(headers) and all(
        is_separator_cell(cell) for cell in separator_cells
    )
    if not separator_valid:
        fail(
            f"{DONE_ARCHIVE_PATH} section 'Done' has a missing or malformed "
            f"markdown separator row: {table_lines[1]}"
        )
        return set()

    archive_ids: set[str] = set()
    seen_archive_ids: dict[str, int] = {}
    for row_index, line in enumerate(table_lines[2:], start=3):
        cells = split_markdown_row(line)
        if len(cells) != len(headers):
            fail(
                f"{DONE_ARCHIVE_PATH} section 'Done' row {row_index} has "
                f"{len(cells)} cells; expected {len(headers)}: {line}"
            )
            continue
        row_id = normalize_text(cells[id_index])
        if row_id:
            if row_id in seen_archive_ids:
                fail(
                    f"{DONE_ARCHIVE_PATH} section 'Done' row {row_index} "
                    f"duplicates archived id: {row_id}"
                )
                continue
            seen_archive_ids[row_id] = row_index
            archive_ids.add(row_id)
    return archive_ids


def validate_source_contract_link(
    row_context: str,
    cell: str,
    blocked_id: str,
    contract_anchors: dict[str, str],
) -> str | None:
    link = extract_markdown_link(cell, "source_contract")
    if link is None:
        return None
    source_contract_path, fragment = link
    if not source_contract_path.is_file():
        fail(f"{row_context} links to missing source_contract: {source_contract_path}")
        return None
    if source_contract_path != BLOCKER_CONTRACTS_PATH.resolve():
        fail(
            f"{row_context} source_contract must point to "
            f"{BLOCKER_CONTRACTS_PATH.relative_to(ROOT)}"
        )
        return None
    if not fragment:
        fail(f"{row_context} source_contract is missing a section anchor")
        return None
    contract_heading = contract_anchors.get(fragment)
    if contract_heading is None:
        fail(f"{row_context} source_contract anchor not found: #{fragment}")
        return None
    if blocked_id and contract_heading != blocked_id:
        fail(
            f"{row_context} source_contract points to {contract_heading}, "
            f"not blocked_id {blocked_id}"
        )
        return None
    return contract_heading


def validate_target_paths(
    row_context: str,
    target_paths: list[str],
    require_non_doc: bool,
    require_existing: bool,
) -> None:
    if any(target_path == "-" for target_path in target_paths):
        fail(f"{row_context} has malformed target_paths sentinel")
        return

    real_non_doc_paths: list[tuple[str, Path]] = []
    for target_path in target_paths:
        if not target_path:
            continue
        target_path_obj = normalize_target_path(target_path)
        if target_path_obj.is_absolute() or ".." in target_path_obj.parts:
            fail(f"{row_context} names non-repo-relative target path: {target_path}")
            continue
        if target_path_obj == Path("."):
            fail(f"{row_context} names non-concrete target path: {target_path}")
            continue
        resolved_target_path = (ROOT / target_path_obj).resolve()
        try:
            resolved_repo_relative = resolved_target_path.relative_to(ROOT)
        except ValueError:
            fail(f"{row_context} names non-repo-relative target path: {target_path}")
            continue
        if resolved_target_path.exists() and not resolved_target_path.is_file():
            fail(f"{row_context} names non-file target path: {target_path}")
            continue
        if require_existing and not resolved_target_path.exists():
            fail(f"{row_context} names missing or non-file target path: {target_path}")
            continue
        if resolved_target_path.is_file() and not is_doc_target_path(
            resolved_repo_relative
        ):
            real_non_doc_paths.append((target_path, target_path_obj))

    if require_non_doc and not real_non_doc_paths:
        fail(f"{row_context} is impl but has no concrete non-doc target_paths")


def parse_verification_commands(row_context: str, raw_verification: str) -> tuple[list[str], bool]:
    if normalize_list_item(raw_verification) == "-":
        return [], False

    verification_commands = split_inline_list(
        raw_verification, ";", normalize_list_item
    )
    verification_sentinel_malformed = any(
        command == "-" for command in verification_commands
    )
    if verification_sentinel_malformed:
        fail(f"{row_context} has malformed verification sentinel")
        return [], True
    return verification_commands, False


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
curation_headers, curation_rows = parse_markdown_table(
    "Next Curation Target", required=False
)
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

if curation_headers and curation_headers != EXPECTED_CURATION_HEADERS:
    fail(
        f"{QUEUE_PATH} Next Curation Target headers do not match expected columns: "
        f"{curation_headers!r}"
    )
    curation_rows = []

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

contract_headings, contract_anchors = (
    extract_markdown_heading_index(BLOCKER_CONTRACTS_PATH)
    if blocked_rows or curation_rows
    else (set(), {})
)
contract_candidate_children = (
    extract_blocker_contract_candidate_children(BLOCKER_CONTRACTS_PATH)
    if blocked_rows or curation_rows
    else {}
)
archived_ids = extract_done_archive_ids() if all_ids or curation_rows else set()
for row_id in sorted(all_ids & archived_ids):
    fail(
        f"{QUEUE_PATH} {seen_ids[row_id]} row {row_id} already exists in "
        f"{DONE_ARCHIVE_PATH.relative_to(ROOT)}"
    )

if len(curation_rows) > 3:
    fail(f"{QUEUE_PATH} Next Curation Target must contain at most 3 candidates")

if not ready_rows and not curation_rows:
    fail(
        f"{QUEUE_PATH} Ready Now is empty, so Next Curation Target must contain "
        "1-3 promotion candidates"
    )

blocked_ids = {normalize_text(row.get("id", "")) for row in blocked_rows}
seen_candidate_ids: set[str] = set()
for row in curation_rows:
    blocked_id = normalize_text(row.get("blocked_id", ""))
    candidate_child_id = normalize_text(row.get("candidate_child_id", ""))
    row_kind = normalize_text(row.get("kind", ""))
    row_context = f"{QUEUE_PATH} Next Curation Target row {candidate_child_id or blocked_id}"

    if not blocked_id:
        fail(f"{row_context} is missing blocked_id")
    elif blocked_id not in blocked_ids:
        fail(f"{row_context} references blocked_id that is not in Blocked: {blocked_id}")

    if not candidate_child_id:
        fail(f"{row_context} is missing candidate_child_id")
    elif candidate_child_id in all_ids:
        fail(
            f"{row_context} candidate_child_id already exists in queue sections: "
            f"{candidate_child_id}"
        )
    elif candidate_child_id in archived_ids:
        fail(
            f"{row_context} candidate_child_id already exists in done archive: "
            f"{candidate_child_id}"
        )
    elif candidate_child_id in seen_candidate_ids:
        fail(f"{row_context} duplicates candidate_child_id: {candidate_child_id}")
    else:
        seen_candidate_ids.add(candidate_child_id)

    if row_kind not in ALLOWED_READY_KINDS:
        fail(f"{row_context} has unsupported kind: {row.get('kind', '')!r}")

    for key in ("why_next", "promotion_check"):
        if not normalize_text(row.get(key, "")):
            fail(f"{row_context} is missing {key}")

    contract_heading = validate_source_contract_link(
        row_context,
        normalize_text(row.get("source_contract", "")),
        blocked_id,
        contract_anchors,
    )
    if contract_heading is not None:
        contract_candidate_child = contract_candidate_children.get(contract_heading)
        if not contract_candidate_child:
            fail(
                f"{row_context} source_contract section {contract_heading} "
                "is missing Candidate child"
            )
        elif candidate_child_id and contract_candidate_child != candidate_child_id:
            fail(
                f"{row_context} candidate_child_id does not match source_contract "
                f"Candidate child: expected {contract_candidate_child}, "
                f"got {candidate_child_id}"
            )

    target_paths = split_inline_list(
        row.get("target_paths", ""), ",", normalize_list_item
    )
    if not target_paths or target_paths == ["-"]:
        fail(f"{row_context} is missing target_paths")
    else:
        validate_target_paths(
            row_context,
            target_paths,
            row_kind == "impl",
            require_existing=False,
        )

    verification_commands, verification_sentinel_malformed = parse_verification_commands(
        row_context,
        row.get("verification", ""),
    )
    if not verification_commands and not verification_sentinel_malformed:
        fail(f"{row_context} is missing verification")

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
    verification_commands, verification_sentinel_malformed = parse_verification_commands(
        f"{QUEUE_PATH} Ready Now row {row_id}",
        row["verification"],
    )
    if not verification_commands and not verification_sentinel_malformed:
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
    if not target_paths or target_paths == ["-"]:
        fail(f"{QUEUE_PATH} Ready Now row {row_id} is missing target_paths")
    else:
        validate_target_paths(
            f"{QUEUE_PATH} Ready Now row {row_id}",
            target_paths,
            row_kind == "impl",
            require_existing=True,
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
    if row_id not in contract_headings:
        fail(
            f"{QUEUE_PATH} Blocked row {row_id} has no matching "
            "blocker-contracts.md section"
        )
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
