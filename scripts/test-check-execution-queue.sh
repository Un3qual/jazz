#!/usr/bin/env bash
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
VALIDATOR_SOURCE="$ROOT/scripts/check-execution-queue.py"

create_repo() {
  local repo_root="$1"

  mkdir -p \
    "$repo_root/scripts" \
    "$repo_root/docs/execution" \
    "$repo_root/docs/plans" \
    "$repo_root/src" \
    "$repo_root/test"

  cp "$VALIDATOR_SOURCE" "$repo_root/scripts/check-execution-queue.py"
  : > "$repo_root/src/Impl.hs"
  : > "$repo_root/test/ImplSpec.hs"
}

setup_inline_comment_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-INLINE-COMMENT-001` | `Inline comment list key` | `P1` | `S` | `impl` | `yes` | `-` | [Plan](../plans/case-inline-comment.md) | `Task 1` | `src/Impl.hs`, `test/ImplSpec.hs` | `Keep commented YAML list keys working.` | `bash verify.sh` | `2026-04-01` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-inline-comment.md"
---
id: CASE-INLINE-COMMENT-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-04-01
plan_section: "Task 1"
target_paths: # inline comment should still leave this as a YAML list
  - src/Impl.hs
  - test/ImplSpec.hs
verification:
  - bash verify.sh
deliverable: "Keep commented YAML list keys working."
supersedes: []
---

# Inline comment fixture
EOF
}

setup_dependency_order_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-DEP-ORDER-001` | `Dependency order` | `P1` | `S` | `impl` | `yes` | `DEP-ALPHA`, `DEP-BETA` | [Plan](../plans/case-dependency-order.md) | `Task 2` | `src/Impl.hs`, `test/ImplSpec.hs` | `Treat depends_on as order-insensitive.` | `bash verify.sh` | `2026-04-01` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |

## Done
| id | title |
| --- | --- |
| `DEP-ALPHA` | `Dependency alpha` |
| `DEP-BETA` | `Dependency beta` |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-dependency-order.md"
---
id: CASE-DEP-ORDER-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on:
  - DEP-BETA
  - DEP-ALPHA
last_verified: 2026-04-01
plan_section: "Task 2"
target_paths:
  - src/Impl.hs
  - test/ImplSpec.hs
verification:
  - bash verify.sh
deliverable: "Treat depends_on as order-insensitive."
supersedes: []
---

# Dependency order fixture
EOF
}

run_case() {
  local name="$1"
  local setup_fn="$2"
  local repo_root
  repo_root=$(mktemp -d)
  create_repo "$repo_root"
  "$setup_fn" "$repo_root"

  if ! (cd "$repo_root" && python3 scripts/check-execution-queue.py > "$repo_root/output.log" 2>&1); then
    printf '%s failed\n' "$name" >&2
    cat "$repo_root/output.log" >&2
    rm -rf "$repo_root"
    exit 1
  fi

  rm -rf "$repo_root"
}

main() {
  run_case "inline-comment regression" setup_inline_comment_case
  run_case "dependency-order regression" setup_dependency_order_case
  printf 'check-execution-queue regressions passed\n'
}

main "$@"
