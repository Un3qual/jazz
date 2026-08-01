#!/usr/bin/env python3

import unittest

from check_legacy_doc_claims import find_live_legacy_tree_claims


class LegacyDocClaimTests(unittest.TestCase):
    def test_rejects_wrapped_live_read_only_claim(self) -> None:
        source = """\
## Migration Safety

- Legacy directories remain
  read-only reference material.
"""

        self.assertEqual(
            [(3, "Legacy directories remain read-only reference material.")],
            find_live_legacy_tree_claims(source),
        )

    def test_allows_explicit_archive_history(self) -> None:
        source = """\
At archive tag `archive/pre-root-canonicalization-2026-07-31`, legacy
directories are preserved as read-only historical snapshots.
"""

        self.assertEqual([], find_live_legacy_tree_claims(source))

    def test_allows_truthful_absent_tree_statement(self) -> None:
        source = """\
Pre-root-canonicalization implementation trees are absent from the current
checkout; historical comparison is anchored to an archive tag.
"""

        self.assertEqual([], find_live_legacy_tree_claims(source))


if __name__ == "__main__":
    unittest.main()
