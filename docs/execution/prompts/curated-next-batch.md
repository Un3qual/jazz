Start in curated execution mode.

1. Read `docs/execution/queue.md` first and treat it as the dispatch source of truth.
2. Consider only the top 2-3 `Ready Now` entries whose dependencies are satisfied.
3. Verify implementation status only for those entries and their linked plans. Do not scan the wider `docs/` tree unless the queue entry is insufficient.
4. Choose one meaningful batch. Prefer one full task section or 3-5 related steps when safe.
5. Execute the batch, update the queue and linked plan metadata, run the listed verification, and make one commit at the verified milestone boundary.
6. Use subagents only if the selected batch has 2+ disjoint implementation tracks.
7. If no `Ready Now` entry is executable, move the item to `Blocked` with a concrete reason and propose the smallest action needed to restore flow.

Do not create a PR unless the queue entry explicitly requires it.
