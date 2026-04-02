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

setup_block_scalar_delimiter_content_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-BLOCK-SCALAR-001` | `Block scalar delimiter content` | `P1` | `S` | `docs` | `yes` | `-` | [Plan](../plans/case-block-scalar.md) | `Task 3` | `docs/plans/case-block-scalar.md` | Folded content keeps --- as data. | `bash verify.sh` | `2026-04-01` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-block-scalar.md"
---
id: CASE-BLOCK-SCALAR-001
status: ready
priority: P1
size: S
kind: docs
autonomous_ready: yes
depends_on: []
last_verified: 2026-04-01
plan_section: "Task 3"
target_paths:
  - docs/plans/case-block-scalar.md
verification:
  - bash verify.sh
deliverable: >
  Folded content keeps
  ---
  as data.
supersedes: []
---

# Block scalar fixture
EOF
}

setup_target_paths_order_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-TARGET-PATH-ORDER-001` | `Target path order` | `P1` | `S` | `impl` | `yes` | `-` | [Plan](../plans/case-target-path-order.md) | `Task 4` | `src/Impl.hs`, `test/ImplSpec.hs` | `Target path order stays exact.` | `bash verify.sh` | `2026-04-01` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-target-path-order.md"
---
id: CASE-TARGET-PATH-ORDER-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-04-01
plan_section: "Task 4"
target_paths:
  - test/ImplSpec.hs
  - src/Impl.hs
verification:
  - bash verify.sh
deliverable: "Target path order stays exact."
supersedes: []
---

# Target path order fixture
EOF
}

setup_trailing_list_delimiter_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-TRAILING-LIST-001` | `Trailing list delimiter` | `P1` | `S` | `impl` | `yes` | `-` | [Plan](../plans/case-trailing-list.md) | `Task 5` | `src/Impl.hs`, `test/ImplSpec.hs` | `Verification list may be the last frontmatter field.` | `bash verify.sh`; `bash verify-extra.sh` | `2026-04-01` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-trailing-list.md"
---
id: CASE-TRAILING-LIST-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-04-01
plan_section: "Task 5"
target_paths:
  - src/Impl.hs
  - test/ImplSpec.hs
deliverable: "Verification list may be the last frontmatter field."
verification:
  - bash verify.sh
  - bash verify-extra.sh
---

# Trailing list delimiter fixture
EOF
}

run_case() {
  local name="$1"
  local setup_fn="$2"
  local expectation="${3:-pass}"
  local expected_snippet="${4:-}"
  local repo_root
  local status
  repo_root=$(mktemp -d)
  create_repo "$repo_root"
  "$setup_fn" "$repo_root"

  if (cd "$repo_root" && python3 scripts/check-execution-queue.py > "$repo_root/output.log" 2>&1); then
    status=0
  else
    status=$?
  fi

  if [[ "$expectation" == "pass" && "$status" -ne 0 ]]; then
    printf '%s failed\n' "$name" >&2
    cat "$repo_root/output.log" >&2
    rm -rf "$repo_root"
    exit 1
  fi

  if [[ "$expectation" == "fail" && "$status" -eq 0 ]]; then
    printf '%s unexpectedly passed\n' "$name" >&2
    cat "$repo_root/output.log" >&2
    rm -rf "$repo_root"
    exit 1
  fi

  if [[ "$expectation" == "fail" && -n "$expected_snippet" ]]; then
    if ! grep -Fq "$expected_snippet" "$repo_root/output.log"; then
      printf '%s failed for the wrong reason\n' "$name" >&2
      cat "$repo_root/output.log" >&2
      rm -rf "$repo_root"
      exit 1
    fi
  fi

  rm -rf "$repo_root"
}

main() {
  run_case "inline-comment regression" setup_inline_comment_case
  run_case "dependency-order regression" setup_dependency_order_case
  run_case "block-scalar delimiter regression" setup_block_scalar_delimiter_content_case
  run_case "trailing list delimiter regression" setup_trailing_list_delimiter_case
  run_case \
    "target-path order regression" \
    setup_target_paths_order_case \
    fail \
    "frontmatter list 'target_paths' does not match queue row CASE-TARGET-PATH-ORDER-001"
  printf 'check-execution-queue regressions passed\n'
}

main "$@"
