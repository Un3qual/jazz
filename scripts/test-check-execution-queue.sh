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

  cat <<'EOF' > "$repo_root/docs/execution/blocker-contracts.md"
# Blocker Unblocker Contracts

## Current Blockers
EOF

  cat <<'EOF' > "$repo_root/docs/execution/done-archive.md"
# Execution Queue Done Archive

## Done

| id | closure evidence | completed_on |
| --- | --- | --- |
EOF
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

setup_plan_link_fragment_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-PLAN-LINK-FRAGMENT-001` | `Plan link fragment anchor` | `P1` | `S` | `impl` | `yes` | `-` | [Plan](../plans/case-plan-link-fragment.md#task-1) | `Task 1` | `src/Impl.hs`, `test/ImplSpec.hs` | `Accept plan links with local fragment anchors.` | `bash verify.sh` | `2026-04-01` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-plan-link-fragment.md"
---
id: CASE-PLAN-LINK-FRAGMENT-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-04-01
plan_section: "Task 1"
target_paths:
  - src/Impl.hs
  - test/ImplSpec.hs
verification:
  - bash verify.sh
deliverable: "Accept plan links with local fragment anchors."
supersedes: []
---

# Plan link fragment fixture

## Task 1
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

setup_non_list_scalar_frontmatter_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-NON-LIST-SCALAR-001` | `Non-list scalar frontmatter` | `P1` | `S` | `impl` | `yes` | `-` | [Plan](../plans/case-non-list-scalar.md) | `Task 2` | `src/Impl.hs`, `test/ImplSpec.hs` | `Reject scalar values for list frontmatter fields.` | `bash verify.sh` | `2026-04-01` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-non-list-scalar.md"
---
id: CASE-NON-LIST-SCALAR-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: 123
last_verified: 2026-04-01
plan_section: "Task 2"
target_paths:
  - src/Impl.hs
  - test/ImplSpec.hs
verification:
  - bash verify.sh
deliverable: "Reject scalar values for list frontmatter fields."
supersedes: []
---

# Non-list scalar fixture
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

setup_curation_planned_target_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |

## Next Curation Target
| blocked_id | candidate_child_id | kind | source_contract | why_next | target_paths | verification | promotion_check |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-CURATION-BLOCKED-001` | `CASE-CURATION-CHILD-001` | `impl` | [blocker-contracts.md](blocker-contracts.md#case-curation-blocked-001) | `Promote one concrete child.` | `src/Impl.hs`, `test/NewSpec.hs` | `bash verify.sh` | `Create the child plan.` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |
| `CASE-CURATION-BLOCKED-001` | `Curation blocked` | `Concrete child split` | `Needs one child.` | [Plan](../plans/case-curation-blocked.md) | `2026-04-10` |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-curation-blocked.md"
# Curation blocked fixture
EOF

  cat <<'EOF' > "$repo_root/docs/execution/blocker-contracts.md"
# Blocker Unblocker Contracts

## Current Blockers

### CASE-CURATION-BLOCKED-001

- Smallest unblocker: promote one child.
EOF
}

setup_curation_dot_target_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |

## Next Curation Target
| blocked_id | candidate_child_id | kind | source_contract | why_next | target_paths | verification | promotion_check |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-CURATION-DOT-BLOCKED-001` | `CASE-CURATION-DOT-CHILD-001` | `coordination` | [blocker-contracts.md](blocker-contracts.md#case-curation-dot-blocked-001) | `Promote one concrete child.` | `.` | `bash verify.sh` | `Create the child plan.` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |
| `CASE-CURATION-DOT-BLOCKED-001` | `Curation dot blocked` | `Concrete child split` | `Needs one child.` | [Plan](../plans/case-curation-dot-blocked.md) | `2026-04-10` |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-curation-dot-blocked.md"
# Curation dot blocked fixture
EOF

  cat <<'EOF' > "$repo_root/docs/execution/blocker-contracts.md"
# Blocker Unblocker Contracts

## Current Blockers

### CASE-CURATION-DOT-BLOCKED-001

- Smallest unblocker: promote one child.
EOF
}

setup_curation_missing_anchor_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |

## Next Curation Target
| blocked_id | candidate_child_id | kind | source_contract | why_next | target_paths | verification | promotion_check |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-CURATION-ANCHOR-BLOCKED-001` | `CASE-CURATION-ANCHOR-CHILD-001` | `impl` | [blocker-contracts.md](blocker-contracts.md#case-curation-anchor-blocked-0001) | `Promote one concrete child.` | `src/Impl.hs` | `bash verify.sh` | `Create the child plan.` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |
| `CASE-CURATION-ANCHOR-BLOCKED-001` | `Curation anchor blocked` | `Concrete child split` | `Needs one child.` | [Plan](../plans/case-curation-anchor-blocked.md) | `2026-04-10` |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-curation-anchor-blocked.md"
# Curation anchor blocked fixture
EOF

  cat <<'EOF' > "$repo_root/docs/execution/blocker-contracts.md"
# Blocker Unblocker Contracts

## Current Blockers

### CASE-CURATION-ANCHOR-BLOCKED-001

- Smallest unblocker: promote one child.
EOF
}

setup_blocked_missing_contract_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-CONTRACT-READY-001` | `Ready row` | `P1` | `S` | `impl` | `yes` | `-` | [Plan](../plans/case-contract-ready.md) | `Task 1` | `src/Impl.hs` | `Keep Ready Now valid while blocked row is checked.` | `bash verify.sh` | `2026-04-10` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |
| `CASE-NO-CONTRACT-001` | `No contract` | `Missing contract` | `Needs a contract section.` | [Plan](../plans/case-no-contract.md) | `2026-04-10` |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-contract-ready.md"
---
id: CASE-CONTRACT-READY-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-04-10
plan_section: "Task 1"
target_paths:
  - src/Impl.hs
verification:
  - bash verify.sh
deliverable: "Keep Ready Now valid while blocked row is checked."
supersedes: []
---

# Contract ready fixture
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-no-contract.md"
# No contract fixture
EOF
}

setup_curation_archive_reuse_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |

## Next Curation Target
| blocked_id | candidate_child_id | kind | source_contract | why_next | target_paths | verification | promotion_check |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-ARCHIVE-BLOCKED-001` | `CASE-ARCHIVED-CHILD-001` | `impl` | [blocker-contracts.md](blocker-contracts.md#case-archive-blocked-001) | `Promote one concrete child.` | `src/Impl.hs` | `bash verify.sh` | `Create the child plan.` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |
| `CASE-ARCHIVE-BLOCKED-001` | `Archive blocked` | `Concrete child split` | `Needs one child.` | [Plan](../plans/case-archive-blocked.md) | `2026-04-10` |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-archive-blocked.md"
# Archive blocked fixture
EOF

  cat <<'EOF' > "$repo_root/docs/execution/blocker-contracts.md"
# Blocker Unblocker Contracts

## Current Blockers

### CASE-ARCHIVE-BLOCKED-001

- Smallest unblocker: promote one child.
EOF

  cat <<'EOF' > "$repo_root/docs/execution/done-archive.md"
# Execution Queue Done Archive

## Done

| id | closure evidence | completed_on |
| --- | --- | --- |
| `CASE-ARCHIVED-CHILD-001` | `Already completed.` | `2026-04-09` |
EOF
}

setup_active_archive_reuse_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-ACTIVE-ARCHIVED-001` | `Archived active reuse` | `P1` | `S` | `impl` | `yes` | `-` | [Plan](../plans/case-active-archive.md) | `Task 1` | `src/Impl.hs` | `Reject active queue ids already archived.` | `bash verify.sh` | `2026-04-10` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-active-archive.md"
---
id: CASE-ACTIVE-ARCHIVED-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-04-10
plan_section: "Task 1"
target_paths:
  - src/Impl.hs
verification:
  - bash verify.sh
deliverable: "Reject active queue ids already archived."
supersedes: []
---

# Active archive fixture
EOF

  cat <<'EOF' > "$repo_root/docs/execution/done-archive.md"
# Execution Queue Done Archive

## Done

| id | closure evidence | completed_on |
| --- | --- | --- |
| `CASE-ACTIVE-ARCHIVED-001` | `Already completed.` | `2026-04-09` |
EOF
}

setup_ready_docs_dot_target_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-READY-DOCS-DOT-001` | `Docs dot target` | `P1` | `S` | `docs` | `yes` | `-` | [Plan](../plans/case-ready-docs-dot.md) | `Task 1` | `.` | `Reject non-concrete docs target paths.` | `bash verify.sh` | `2026-04-10` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-ready-docs-dot.md"
---
id: CASE-READY-DOCS-DOT-001
status: ready
priority: P1
size: S
kind: docs
autonomous_ready: yes
depends_on: []
last_verified: 2026-04-10
plan_section: "Task 1"
target_paths:
  - .
verification:
  - bash verify.sh
deliverable: "Reject non-concrete docs target paths."
supersedes: []
---

# Ready docs dot target fixture
EOF
}

setup_source_contract_wrong_file_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |

## Next Curation Target
| blocked_id | candidate_child_id | kind | source_contract | why_next | target_paths | verification | promotion_check |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-WRONG-CONTRACT-BLOCKED-001` | `CASE-WRONG-CONTRACT-CHILD-001` | `impl` | [wrong-contract.md](../plans/wrong-contract.md#not-a-contract) | `Promote one concrete child.` | `src/Impl.hs` | `bash verify.sh` | `Create the child plan.` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |
| `CASE-WRONG-CONTRACT-BLOCKED-001` | `Wrong contract file` | `Concrete child split` | `Needs one child.` | [Plan](../plans/case-wrong-contract-blocked.md) | `2026-04-10` |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-wrong-contract-blocked.md"
# Wrong contract blocked fixture
EOF

  cat <<'EOF' > "$repo_root/docs/plans/wrong-contract.md"
# Wrong Contract File

## Not a Contract
EOF

  cat <<'EOF' > "$repo_root/docs/execution/blocker-contracts.md"
# Blocker Unblocker Contracts

## Current Blockers

### CASE-WRONG-CONTRACT-BLOCKED-001

- Smallest unblocker: promote one child.
EOF
}

setup_idle_queue_without_curation_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |
| `CASE-IDLE-BLOCKED-001` | `Idle blocked` | `Concrete child split` | `Needs one child.` | [Plan](../plans/case-idle-blocked.md) | `2026-04-10` |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-idle-blocked.md"
# Idle blocked fixture
EOF

  cat <<'EOF' > "$repo_root/docs/execution/blocker-contracts.md"
# Blocker Unblocker Contracts

## Current Blockers

### CASE-IDLE-BLOCKED-001

- Smallest unblocker: promote one child.
EOF
}

setup_symlink_target_escape_case() {
  local repo_root="$1"

  ln -s /etc "$repo_root/alias"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-SYMLINK-TARGET-001` | `Symlink target escape` | `P1` | `S` | `impl` | `yes` | `-` | [Plan](../plans/case-symlink-target.md) | `Task 7` | `alias/hosts` | `Reject impl target paths that escape the repo through symlinks.` | `bash verify.sh` | `2026-04-10` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-symlink-target.md"
---
id: CASE-SYMLINK-TARGET-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-04-10
plan_section: "Task 7"
target_paths:
  - alias/hosts
verification:
  - bash verify.sh
deliverable: "Reject impl target paths that escape the repo through symlinks."
supersedes: []
---

# Symlink target escape fixture
EOF
}

setup_symlink_doc_target_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/plans/doc-target.md"
# Doc target fixture
EOF
  ln -s docs/plans/doc-target.md "$repo_root/alias"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-SYMLINK-DOC-TARGET-001` | `Symlink doc target` | `P1` | `S` | `impl` | `yes` | `-` | [Plan](../plans/case-symlink-doc-target.md) | `Task 7b` | `alias` | `Reject impl target paths that resolve to docs-only files.` | `bash verify.sh` | `2026-04-11` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-symlink-doc-target.md"
---
id: CASE-SYMLINK-DOC-TARGET-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-04-11
plan_section: "Task 7b"
target_paths:
  - alias
verification:
  - bash verify.sh
deliverable: "Reject impl target paths that resolve to docs-only files."
supersedes: []
---

# Symlink doc target fixture
EOF
}

setup_non_contiguous_table_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-NONCONTIG-TABLE-001` | `Non-contiguous table` | `P1` | `S` | `impl` | `yes` | `-` | [Plan](../plans/case-noncontig-table.md) | `Task 8` | `src/Impl.hs`, `test/ImplSpec.hs` | `Reject non-table content that splits the queue table.` | `bash verify.sh` | `2026-04-10` |

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
deliverable: "Reject non-table content that splits the queue table."
supersedes: []
---

# Non-contiguous table fixture
EOF
}

setup_pipe_prose_before_table_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
This note mentions A | B before the table and should not be treated as a queue row.

| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-PIPE-PROSE-001` | `Pipe prose before table` | `P1` | `S` | `impl` | `yes` | `-` | [Plan](../plans/case-pipe-prose-before-table.md) | `Task 12` | `src/Impl.hs`, `test/ImplSpec.hs` | `Ignore prose lines with pipes before the real table.` | `bash verify.sh` | `2026-04-10` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-pipe-prose-before-table.md"
---
id: CASE-PIPE-PROSE-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-04-10
plan_section: "Task 12"
target_paths:
  - src/Impl.hs
  - test/ImplSpec.hs
verification:
  - bash verify.sh
deliverable: "Ignore prose lines with pipes before the real table."
supersedes: []
---

# Pipe prose before table fixture
EOF
}

setup_duplicate_section_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |

## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-DUPLICATE-SECTION-001` | `Duplicate section` | `P1` | `S` | `impl` | `yes` | `-` | [Plan](../plans/case-duplicate-section.md) | `Task 11` | `src/Impl.hs`, `test/ImplSpec.hs` | `Reject repeated queue sections.` | `bash verify.sh` | `2026-04-10` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-duplicate-section.md"
---
id: CASE-DUPLICATE-SECTION-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-04-10
plan_section: "Task 11"
target_paths:
  - src/Impl.hs
  - test/ImplSpec.hs
verification:
  - bash verify.sh
deliverable: "Reject repeated queue sections."
supersedes: []
---

# Duplicate section fixture
EOF
}

setup_blocked_on_placeholder_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-BLOCKED-PLACEHOLDER-READY-001` | `Ready row` | `P1` | `S` | `impl` | `yes` | `-` | [Plan](../plans/case-blocked-placeholder-ready.md) | `Task 9` | `src/Impl.hs`, `test/ImplSpec.hs` | `Keep Ready Now valid while Blocked row is checked.` | `bash verify.sh` | `2026-04-10` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |
| `CASE-BLOCKED-PLACEHOLDER-001` | `Blocked placeholder` | `-` | `Needs a concrete blocker.` | [Plan](../plans/case-blocked-placeholder.md) | `2026-04-10` |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-blocked-placeholder-ready.md"
---
id: CASE-BLOCKED-PLACEHOLDER-READY-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-04-10
plan_section: "Task 9"
target_paths:
  - src/Impl.hs
  - test/ImplSpec.hs
verification:
  - bash verify.sh
deliverable: "Keep Ready Now valid while Blocked row is checked."
supersedes: []
---

# Blocked placeholder ready fixture
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-blocked-placeholder.md"
# Blocked placeholder fixture
EOF
}

setup_verification_mixed_sentinel_case() {
  local repo_root="$1"

  cat <<'EOF' > "$repo_root/docs/execution/queue.md"
## Ready Now
| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `CASE-VERIFY-MIXED-SENTINEL-001` | `Verification mixed sentinel` | `P1` | `S` | `impl` | `yes` | `-` | [Plan](../plans/case-verify-mixed-sentinel.md) | `Task 10` | `src/Impl.hs`, `test/ImplSpec.hs` | `Reject mixed verification sentinels.` | `-`; `bash verify.sh` | `2026-04-10` |

## Blocked
| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |

## Done
| id | title |
| --- | --- |
EOF

  cat <<'EOF' > "$repo_root/docs/plans/case-verify-mixed-sentinel.md"
---
id: CASE-VERIFY-MIXED-SENTINEL-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-04-10
plan_section: "Task 10"
target_paths:
  - src/Impl.hs
  - test/ImplSpec.hs
verification:
  - bash verify.sh
deliverable: "Reject mixed verification sentinels."
supersedes: []
---

# Verification mixed sentinel fixture
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
  local -a args=("$@")
  local separator_index=-1
  local i
  for i in "${!args[@]}"; do
    if [[ "${args[$i]}" == "--" ]]; then
      separator_index=$i
      break
    fi
  done
  if (( separator_index < 0 )); then
    printf 'run_case_with_command missing command separator\n' >&2
    exit 1
  fi

  local -a command=("${args[@]:0:separator_index}")
  local name="${args[$((separator_index + 1))]:-}"
  local setup_fn="${args[$((separator_index + 2))]:-}"
  local expectation="${args[$((separator_index + 3))]:-pass}"
  local expected_snippet="${args[$((separator_index + 4))]:-}"
  local run_dir="${args[$((separator_index + 5))]:-.}"
  local repo_mode="${args[$((separator_index + 6))]:-plain}"
  local forbidden_snippet="${args[$((separator_index + 7))]:-}"
  local repo_root
  local status
  repo_root=$(mktemp -d)
  create_repo "$repo_root"
  "$setup_fn" "$repo_root"
  if [[ "$repo_mode" == "git" ]]; then
    init_git_repo "$repo_root"
  fi

  if (cd "$repo_root/$run_dir" && "${command[@]}" > "$repo_root/output.log" 2>&1); then
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

  if [[ -n "$forbidden_snippet" ]]; then
    if grep -Fq "$forbidden_snippet" "$repo_root/output.log"; then
      printf '%s emitted an unexpected failure snippet\n' "$name" >&2
      cat "$repo_root/output.log" >&2
      rm -rf "$repo_root"
      exit 1
    fi
  fi

  rm -rf "$repo_root"
}

run_case() {
  run_case_with_command python3 scripts/check-execution-queue.py -- "$@"
}

run_wrapper_case() {
  run_case_with_command bash scripts/check-execution-queue.sh -- "$@"
}

main() {
  run_case "inline-comment regression" setup_inline_comment_case
  run_case "dependency-order regression" setup_dependency_order_case
  run_case "plan-link fragment regression" setup_plan_link_fragment_case
  run_case "block-scalar delimiter regression" setup_block_scalar_delimiter_content_case
  run_case \
    "non-list scalar frontmatter regression" \
    setup_non_list_scalar_frontmatter_case \
    fail \
    "frontmatter field 'depends_on' should be a list, not a scalar"
  run_case "trailing list delimiter regression" setup_trailing_list_delimiter_case
  run_case \
    "non-contiguous table regression" \
    setup_non_contiguous_table_case \
    fail \
    "section 'Ready Now' has non-table content splitting its markdown table"
  run_case "pipe prose before table regression" setup_pipe_prose_before_table_case
  run_case \
    "duplicate section regression" \
    setup_duplicate_section_case \
    fail \
    "section 'Ready Now' appears multiple times"
  run_case \
    "blocked_on placeholder regression" \
    setup_blocked_on_placeholder_case \
    fail \
    "Blocked row CASE-BLOCKED-PLACEHOLDER-001 is missing blocked_on"
  run_case \
    "verification mixed sentinel regression" \
    setup_verification_mixed_sentinel_case \
    fail \
    "Ready Now row CASE-VERIFY-MIXED-SENTINEL-001 has malformed verification sentinel" \
    . \
    plain \
    "Ready Now row CASE-VERIFY-MIXED-SENTINEL-001 is missing verification"
  run_case \
    "target-path order regression" \
    setup_target_paths_order_case \
    fail \
    "frontmatter list 'target_paths' does not match queue row CASE-TARGET-PATH-ORDER-001"
  run_case "mixed target_paths regression" setup_mixed_target_paths_case
  run_case "curation planned target regression" setup_curation_planned_target_case
  run_case \
    "curation dot target regression" \
    setup_curation_dot_target_case \
    fail \
    "Next Curation Target row CASE-CURATION-DOT-CHILD-001 names non-concrete target path: ."
  run_case \
    "curation missing anchor regression" \
    setup_curation_missing_anchor_case \
    fail \
    "Next Curation Target row CASE-CURATION-ANCHOR-CHILD-001 source_contract anchor not found: #case-curation-anchor-blocked-0001"
  run_case \
    "source contract wrong file regression" \
    setup_source_contract_wrong_file_case \
    fail \
    "Next Curation Target row CASE-WRONG-CONTRACT-CHILD-001 source_contract must point to docs/execution/blocker-contracts.md" \
    . \
    plain \
    "source_contract anchor not found"
  run_case \
    "idle queue without curation regression" \
    setup_idle_queue_without_curation_case \
    fail \
    "Ready Now is empty, so Next Curation Target must contain 1-3 promotion candidates"
  run_case \
    "blocked missing contract regression" \
    setup_blocked_missing_contract_case \
    fail \
    "Blocked row CASE-NO-CONTRACT-001 has no matching blocker-contracts.md section"
  run_case \
    "curation archive id reuse regression" \
    setup_curation_archive_reuse_case \
    fail \
    "Next Curation Target row CASE-ARCHIVED-CHILD-001 candidate_child_id already exists in done archive: CASE-ARCHIVED-CHILD-001"
  run_case \
    "active archive id reuse regression" \
    setup_active_archive_reuse_case \
    fail \
    "Ready Now row CASE-ACTIVE-ARCHIVED-001 already exists in docs/execution/done-archive.md"
  run_case \
    "ready docs dot target regression" \
    setup_ready_docs_dot_target_case \
    fail \
    "Ready Now row CASE-READY-DOCS-DOT-001 names non-concrete target path: ."
  run_case \
    "symlink target escape regression" \
    setup_symlink_target_escape_case \
    fail \
    "Ready Now row CASE-SYMLINK-TARGET-001 names non-repo-relative target path: alias/hosts"
  run_case \
    "symlink doc target regression" \
    setup_symlink_doc_target_case \
    fail \
    "Ready Now row CASE-SYMLINK-DOC-TARGET-001 is impl but has no concrete non-doc target_paths"
  run_case \
    "verification order regression" \
    setup_verification_order_case \
    fail \
    "frontmatter list 'verification' does not match queue row CASE-VERIFY-ORDER-001"

  # Wrapper smoke tests exercising repo-root detection and python3 preflight
  run_wrapper_case "wrapper: inline-comment smoke test" setup_inline_comment_case
  run_case_with_command \
    bash \
    ../scripts/check-execution-queue.sh \
    -- \
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
  run_case_with_command \
    env \
    PATH=/nonexistent \
    /bin/bash \
    scripts/check-execution-queue.sh \
    -- \
    "wrapper: python3 preflight smoke test" \
    setup_inline_comment_case \
    fail \
    "FAIL: python3 is required for scripts/check-execution-queue.sh"

  printf 'check-execution-queue regressions passed\n'
}

main "$@"
