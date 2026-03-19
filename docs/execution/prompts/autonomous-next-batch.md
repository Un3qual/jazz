Start in autonomous execution mode.

1. Read `docs/execution/queue.md` first and treat it as the dispatch source of truth.
2. Select the highest-priority executable `Ready Now` entry with `kind: impl`, `autonomous_ready: yes`, and satisfied dependencies. Fall back to `kind: docs` or `kind: coordination` only when no executable implementation entry exists.
3. Verify status only for that entry and the exact linked plan section needed to confirm the next executable batch. Do not scan unrelated docs unless the queue entry is insufficient.
4. Execute one meaningful implementation batch. A valid batch is expected to modify at least one non-doc file when implementation-ready work exists. For compiler work, prefer `jazz-next/`.
5. Queue, plan, status, and spec updates are required follow-through, but they do not count as a successful standalone batch while implementation work exists.
6. Update the queue and linked plan metadata, run the listed verification, and make one commit at the verified milestone boundary.
7. If no executable `kind: impl` and `autonomous_ready: yes` entry exists, return to curated behavior: surface the top 2-3 candidate entries with their blockers and the smallest action needed to promote one of them.

Do not create a PR unless the queue entry explicitly requires it.
Do not end the session with a docs-only diff unless no executable implementation batch exists and the docs change is the smallest unblocker.
