Start in autonomous execution mode.

1. Read `docs/execution/queue.md` first and treat it as the dispatch source of truth.
2. Select the highest-priority `Ready Now` entry with `autonomous_ready: yes` and satisfied dependencies.
3. Verify implementation status only for that entry and its linked plans. Do not scan unrelated docs unless the queue entry is insufficient.
4. Execute one meaningful batch. Prefer one full task section or 3-5 related steps when safe.
5. Update the queue and linked plan metadata, run the listed verification, and make one commit at the verified milestone boundary.
6. If no executable `autonomous_ready: yes` entry exists, return to curated behavior: surface the top 2-3 candidate entries with their blockers and the smallest action needed to promote one of them.

Do not create a PR unless the queue entry explicitly requires it.
