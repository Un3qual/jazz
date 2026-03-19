Start in curated execution mode.

1. Read `docs/execution/queue.md` first and treat it as the dispatch source of truth.
2. Consider only the top 2-3 `Ready Now` entries whose dependencies are satisfied.
3. Prefer `kind: impl` entries. Do not choose `kind: docs` or `kind: coordination` while any executable `kind: impl` entry exists.
4. Verify status only for those entries and the exact linked plan sections needed to confirm the next executable batch. Do not scan the wider `docs/` tree unless the queue entry is insufficient.
5. Choose one meaningful implementation batch. A valid batch is expected to modify at least one non-doc file when implementation-ready work exists. For compiler work, prefer `jazz-next/`.
6. Execute the batch. Queue, plan, status, and spec updates are required follow-through, but they do not complete the batch by themselves while implementation work exists.
7. Update the queue and linked plan metadata, run the listed verification, and make one commit at the verified milestone boundary.
8. Use subagents only if the selected batch has 2+ disjoint implementation tracks.
9. If a candidate entry is docs-only, stale, or under-specified while linked implementation work remains, narrow or rewrite it to the next concrete implementation batch before executing.
10. If no executable implementation batch exists, move the blocked implementation item to `Blocked` with a concrete reason and do only the smallest docs or coordination change needed to restore flow.

Do not create a PR unless the queue entry explicitly requires it.
Do not end the session with a docs-only diff unless step 10 applies.
