#!/usr/bin/env bash
set -euo pipefail

ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT"

fail_count=0

fail() {
  printf 'FAIL: %s\n' "$1" >&2
  fail_count=$((fail_count + 1))
}

rendered_markdown() {
  python3 "$SCRIPT_DIR/markdown_visibility.py" --preserve-inline-code "$1"
}

removed_identity_matches() {
  python3 -c '
import re
import sys
from pathlib import Path

sys.path.insert(0, sys.argv[1])
from markdown_targets import decode_markdown_escapes_and_html_entities

pattern = re.compile(r"(jazz-next|jazz-hs|jazz2|jazznext)", re.IGNORECASE)
paths = [Path("README.md"), *sorted(Path("docs").rglob("*.md"))]
for path in paths:
    with path.open(encoding="utf-8") as handle:
        for line_number, line in enumerate(handle, 1):
            decoded = decode_markdown_escapes_and_html_entities(line).rstrip("\r\n")
            if pattern.search(decoded):
                print(f"{path.as_posix()}:{line_number}:{decoded}")
' "$SCRIPT_DIR"
}

require_pattern() {
  local rendered_file="$1"
  local source_file="$2"
  local label="$3"
  local pattern="$4"
  if ! rg -n -i -e "$pattern" "$rendered_file" >/dev/null 2>&1; then
    fail "$source_file missing authority statement: $label"
  fi
}

require_block() {
  local rendered_file="$1"
  local source_file="$2"
  local label="$3"
  local pattern="$4"
  if ! rg -n -i -U -e "$pattern" "$rendered_file" >/dev/null 2>&1; then
    fail "$source_file missing authority statement: $label"
  fi
}

governance_file="docs/project/governance.md"
authority_rfc="rfcs/accepted/0001-language-authority-and-change-control.md"

for file in "$governance_file" "$authority_rfc"; do
  [[ -f "$file" ]] || fail "missing required authority file: $file"
done
if [[ "$fail_count" -ne 0 ]]; then
  exit 1
fi

rendered_root="$(mktemp -d "${TMPDIR:-/tmp}/jazz-authority.XXXXXX")"
trap 'rm -r -- "$rendered_root"' EXIT
governance_rendered="$rendered_root/governance.md"
authority_rfc_rendered="$rendered_root/authority-rfc.md"
rendered_markdown "$governance_file" > "$governance_rendered"
rendered_markdown "$authority_rfc" > "$authority_rfc_rendered"

require_pattern "$governance_rendered" "$governance_file" "public documentation authority" '^1\. curated public language and reference documentation;$'
require_block "$governance_rendered" "$governance_file" "implementation and tests evidence" '^2\. current compiler, standard-library, and test behavior as implementation\r?\n[ \t]+evidence;$'
require_pattern "$governance_rendered" "$governance_file" "accepted durable decisions" '^3\. accepted durable decision records; and$'
require_pattern "$governance_rendered" "$governance_file" "non-normative roadmap" '^4\. roadmap material, which is non-normative\.$'
require_block "$governance_rendered" "$governance_file" "semantic change control" '^Semantic language changes require a reviewed decision record before\r?\nimplementation\.'

require_block "$authority_rfc_rendered" "$authority_rfc" "public documentation authority" '^1\. Canonical public language contracts under `docs/language/` and\r?\n[ \t]+`docs/reference/`\.$'
require_block "$authority_rfc_rendered" "$authority_rfc" "implementation and tests evidence" '^2\. Behavior verified by the current implementation and tests under `src/`,\r?\n[ \t]+`jazz/`, and `test/` when the public contract does not yet cover a detail\.$'
require_pattern "$authority_rfc_rendered" "$authority_rfc" "accepted RFC authority" '^3\. Accepted durable decisions under `rfcs/accepted/`\.$'
require_pattern "$authority_rfc_rendered" "$authority_rfc" "non-normative roadmap" '^4\. Roadmap material, which is informative and non-normative\.$'
require_pattern "$authority_rfc_rendered" "$authority_rfc" "semantic change control" '^Every semantic language change requires an accepted RFC before implementation\.$'

for removed_path in docs/spec docs/feature-status.md docs/jazz-language-state.md docs/jazz-improvement-backlog.md; do
  if [[ -e "$removed_path" ]]; then
    fail "superseded authority path still exists: $removed_path"
  fi
done

identity_matches="$(removed_identity_matches)"
if [[ -n "$identity_matches" ]]; then
  printf '%s\n' "$identity_matches" >&2
  fail "removed implementation identity still appears in public documentation"
fi

if [[ "$fail_count" -ne 0 ]]; then
  exit 1
fi

echo "Documentation authority policy check passed."
