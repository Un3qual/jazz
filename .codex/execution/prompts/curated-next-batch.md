Start in curated execution mode.

1. Read `.codex/execution/queue.md` first and treat it as the dispatch source of truth.
2. Consider only the top 2-3 `Ready Now` entries whose dependencies are satisfied.
3. Prefer `kind: impl` entries. Do not choose `kind: docs` or `kind: coordination` while any executable `kind: impl` entry exists.
4. Verify status only for those entries and the exact linked plan sections needed to confirm the next executable batch. Do not scan the wider `docs/` tree unless the queue entry is insufficient.
5. Choose one meaningful implementation batch. A valid batch is expected to modify at least one non-doc file when implementation-ready work exists. For compiler work, prefer the repository root.
6. Execute the batch. Queue, plan, status, and spec updates are required follow-through, but they do not complete the batch by themselves while implementation work exists.
7. Update the queue and linked plan metadata, run the listed verification, and make one commit at the verified milestone boundary.
8. Use subagents only if the selected batch has 2+ disjoint implementation tracks.
9. If a candidate entry is docs-only, stale, or under-specified while linked implementation work remains, narrow or rewrite it to the next concrete implementation batch before executing.
10. If `Ready Now` is empty and `Next Curation Target` has rows, use only `Next Curation Target` and the linked `.codex/execution/blocker-contracts.md` section. Promote exactly one candidate into `Ready Now` with matching plan frontmatter, or replace that candidate with a better bounded candidate.
11. If `Ready Now` and `Next Curation Target` are empty and the current executor status explicitly says there is no source-backed next curation target and no named candidate currently, stop after reporting that all source-backed candidates are exhausted. Do not scan broadly or invent work.
12. If no executable implementation batch exists after the curation target is checked, move or keep the item in `Blocked` with a concrete reason and do only the smallest docs or coordination change needed to restore flow.

Do not create a PR unless the queue entry explicitly requires it.
Do not end the session with a docs-only diff unless step 10, step 11, or step 12 applies.
