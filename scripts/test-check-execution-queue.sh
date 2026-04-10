#!/usr/bin/env bash
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
VALIDATOR_SOURCE="$ROOT/scripts/check-execution-queue.py"
WRAPPER_SOURCE="$ROOT/scripts/check-execution-queue.sh"

create_repo() {
  local repo_root="$1"

  mkdir -p \
    "$repo_root/scripts" \
    "$repo_root/docs/execution" \
    "$repo_root/docs/plans" \
    "$repo_root/src" \
    "$repo_root/test"

  cp "$VALIDATOR_SOURCE" "$repo_root/scripts/check-execution-queue.py"
  cp "$WRAPPER_SOURCE" "$repo_root/scripts/check-execution-queue.sh"
  : > "$repo_root/src/Impl.hs"
  : > "$repo_root/test/ImplSpec.hs"
}

init_git_repo() {
  local repo_root="$1"

  (
    cd "$repo_root"
    git init -q
  )
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

setup_mixed_target_paths_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-MIXED-TARGET-001` | `Mixed target paths` | `P1` | `S` | `impl` | `yes` | `-` | [Plan](../plans/case-mixed-target.md) | `Task 6` | `src/Impl.hs`, `docs/plans/case-mixed-target.md`, `test/ImplSpec.hs` | `Accept mixed target_paths with concrete files and docs.` | `bash verify.sh` | `2026-04-01` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-mixed-target.md"
---
id: CASE-MIXED-TARGET-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-04-01
plan_section: "Task 6"
target_paths:
  - src/Impl.hs
  - docs/plans/case-mixed-target.md
  - test/ImplSpec.hs
deliverable: "Accept mixed target_paths with concrete files and docs."
verification:
  - bash verify.sh
supersedes: []
---

# Mixed target paths fixture
EOF
}

setup_non_contiguous_table_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-NONCONTIG-TABLE-001` | `Non-contiguous table` | `P1` | `S` | `impl` | `yes` | `-` | [Plan](../plans/case-noncontig-table.md) | `Task 8` | `src/Impl.hs`, `test/ImplSpec.hs` | `Ignore later table examples in the section.` | `bash verify.sh` | `2026-04-10` |

Notes below document a table shape example and must not be parsed as queue rows.

| example | value |
| --- | --- |
| `ignored` | `row` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-noncontig-table.md"
---
id: CASE-NONCONTIG-TABLE-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-04-10
plan_section: "Task 8"
target_paths:
  - src/Impl.hs
  - test/ImplSpec.hs
verification:
  - bash verify.sh
deliverable: "Ignore later table examples in the section."
supersedes: []
---

# Non-contiguous table fixture
EOF
}

setup_verification_order_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-VERIFY-ORDER-001` | `Verification order` | `P1` | `S` | `impl` | `yes` | `-` | [Plan](../plans/case-verify-order.md) | `Task 7` | `src/Impl.hs`, `test/ImplSpec.hs` | `Verification order must match exactly.` | `bash verify-first.sh`; `bash verify-second.sh` | `2026-04-01` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-verify-order.md"
---
id: CASE-VERIFY-ORDER-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-04-01
plan_section: "Task 7"
target_paths:
  - src/Impl.hs
  - test/ImplSpec.hs
deliverable: "Verification order must match exactly."
verification:
  - bash verify-second.sh
  - bash verify-first.sh
supersedes: []
---

# Verification order fixture
EOF
}

run_case_with_command() {
  local command="$1"
  local name="$2"
  local setup_fn="$3"
  local expectation="${4:-pass}"
  local expected_snippet="${5:-}"
  local run_dir="${6:-.}"
  local repo_mode="${7:-plain}"
  local repo_root
  local status
  repo_root=$(mktemp -d)
  create_repo "$repo_root"
  "$setup_fn" "$repo_root"
  if [[ "$repo_mode" == "git" ]]; then
    init_git_repo "$repo_root"
  fi

  if (cd "$repo_root/$run_dir" && $command > "$repo_root/output.log" 2>&1); then
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

run_case() {
  run_case_with_command "python3 scripts/check-execution-queue.py" "$@"
}

run_wrapper_case() {
  run_case_with_command "bash scripts/check-execution-queue.sh" "$@"
}

main() {
  run_case "inline-comment regression" setup_inline_comment_case
  run_case "dependency-order regression" setup_dependency_order_case
  run_case "block-scalar delimiter regression" setup_block_scalar_delimiter_content_case
  run_case "trailing list delimiter regression" setup_trailing_list_delimiter_case
  run_case "non-contiguous table regression" setup_non_contiguous_table_case
  run_case \
    "target-path order regression" \
    setup_target_paths_order_case \
    fail \
    "frontmatter list 'target_paths' does not match queue row CASE-TARGET-PATH-ORDER-001"
  run_case "mixed target_paths regression" setup_mixed_target_paths_case
  run_case \
    "verification order regression" \
    setup_verification_order_case \
    fail \
    "frontmatter list 'verification' does not match queue row CASE-VERIFY-ORDER-001"

  # Wrapper smoke tests exercising repo-root detection and python3 preflight
  run_wrapper_case "wrapper: inline-comment smoke test" setup_inline_comment_case
  run_case_with_command \
    "bash ../scripts/check-execution-queue.sh" \
    "wrapper: inline-comment child-dir smoke test" \
    setup_inline_comment_case \
    pass \
    "" \
    src
  run_wrapper_case \
    "wrapper: inline-comment git-repo smoke test" \
    setup_inline_comment_case \
    pass \
    "" \
    . \
    git
  run_wrapper_case "wrapper: dependency-order smoke test" setup_dependency_order_case
  run_wrapper_case \
    "wrapper: target-path order smoke test" \
    setup_target_paths_order_case \
    fail \
    "frontmatter list 'target_paths' does not match queue row CASE-TARGET-PATH-ORDER-001"

  printf 'check-execution-queue regressions passed\n'
}

main "$@"
