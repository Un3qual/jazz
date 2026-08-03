#!/usr/bin/env python3

from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

from check_legacy_doc_claims import find_live_legacy_tree_claims


CHECKER = Path(__file__).with_name("check_legacy_doc_claims.py")


def run_checker(*paths: Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [sys.executable, str(CHECKER), *(str(path) for path in paths)],
        capture_output=True,
        check=False,
        text=True,
    )


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

    def test_archive_mention_does_not_hide_live_checkout_claim(self) -> None:
        source = """\
Legacy directories remain read-only in the current checkout, not in the archive.
"""

        self.assertEqual(
            [
                (
                    1,
                    "Legacy directories remain read-only in the current checkout, not in the archive.",
                )
            ],
            find_live_legacy_tree_claims(source),
        )

    def test_allows_clearly_negated_live_claim(self) -> None:
        source = "It is false that legacy directories remain read-only.\n"

        self.assertEqual([], find_live_legacy_tree_claims(source))

    def test_negated_claim_does_not_hide_later_positive_claim(self) -> None:
        source = """\
It is false that legacy directories remain read-only. Legacy directories remain read-only in the current checkout.
"""

        self.assertEqual(
            [
                (
                    1,
                    "It is false that legacy directories remain read-only. Legacy directories remain read-only in the current checkout.",
                )
            ],
            find_live_legacy_tree_claims(source),
        )

    def test_unrelated_earlier_negation_does_not_hide_positive_claim(self) -> None:
        source = """\
It is false that the archive snapshot is writable. Legacy directories remain read-only in the current checkout.
"""

        self.assertEqual(
            [
                (
                    1,
                    "It is false that the archive snapshot is writable. Legacy directories remain read-only in the current checkout.",
                )
            ],
            find_live_legacy_tree_claims(source),
        )

    def test_repeated_obsolete_identity_claims_are_evaluated_independently(self) -> None:
        obsolete_identity = "jazz-" + "hs"
        source = (
            f"It is false that {obsolete_identity} remains read-only and "
            f"{obsolete_identity} remains read-only in the current checkout.\n"
        )

        self.assertEqual(
            [(1, source.strip())],
            find_live_legacy_tree_claims(source),
        )

    def test_unrelated_negation_does_not_hide_live_checkout_claim(self) -> None:
        source = """\
Legacy directories remain read-only in the current checkout, but are not
present in the archive snapshot.
"""

        self.assertEqual(
            [
                (
                    1,
                    "Legacy directories remain read-only in the current checkout, but are not present in the archive snapshot.",
                )
            ],
            find_live_legacy_tree_claims(source),
        )

    def test_allows_archive_snapshot_as_subject(self) -> None:
        source = "The archive-tag snapshot is read-only historical evidence.\n"

        self.assertEqual([], find_live_legacy_tree_claims(source))

    def test_allows_truthful_absent_tree_statement(self) -> None:
        source = """\
Pre-root-canonicalization implementation trees are absent from the current
checkout; historical comparison is anchored to an archive tag.
"""

        self.assertEqual([], find_live_legacy_tree_claims(source))

    def test_mismatched_fence_type_does_not_hide_following_claim(self) -> None:
        source = """\
```text
fenced example
~~~
```
Legacy directories remain
read-only in the current checkout.
"""

        self.assertEqual(
            [
                (
                    5,
                    "Legacy directories remain read-only in the current checkout.",
                )
            ],
            find_live_legacy_tree_claims(source),
        )

    def test_shorter_fence_does_not_close_longer_fence(self) -> None:
        source = """\
````text
```
````
Legacy directories remain read-only in the current checkout.
"""

        self.assertEqual(
            [
                (
                    4,
                    "Legacy directories remain read-only in the current checkout.",
                )
            ],
            find_live_legacy_tree_claims(source),
        )

    def test_four_space_indented_marker_does_not_open_fence(self) -> None:
        source = """\
    ```
Legacy directories remain read-only in the current checkout.
"""

        self.assertEqual(
            [
                (
                    1,
                    "``` Legacy directories remain read-only in the current checkout.",
                )
            ],
            find_live_legacy_tree_claims(source),
        )

    def test_tab_indented_marker_does_not_open_fence(self) -> None:
        source = """\
\t```
Legacy directories remain read-only in the current checkout.
"""

        self.assertEqual(
            [
                (
                    1,
                    "``` Legacy directories remain read-only in the current checkout.",
                )
            ],
            find_live_legacy_tree_claims(source),
        )

    def test_cli_fails_for_missing_path(self) -> None:
        with tempfile.TemporaryDirectory() as temporary_directory:
            result = run_checker(Path(temporary_directory) / "missing.md")

        self.assertNotEqual(0, result.returncode)

    def test_cli_fails_for_unsupported_explicit_file(self) -> None:
        with tempfile.TemporaryDirectory() as temporary_directory:
            unsupported = Path(temporary_directory) / "claims.txt"
            unsupported.write_text("no markdown here\n", encoding="utf-8")
            result = run_checker(unsupported)

        self.assertNotEqual(0, result.returncode)

    def test_cli_fails_for_empty_directory_selection(self) -> None:
        with tempfile.TemporaryDirectory() as temporary_directory:
            result = run_checker(Path(temporary_directory))

        self.assertNotEqual(0, result.returncode)

    def test_cli_fails_without_input_selection(self) -> None:
        self.assertNotEqual(0, run_checker().returncode)


if __name__ == "__main__":
    unittest.main()
