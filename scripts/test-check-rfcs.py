#!/usr/bin/env python3
"""Mutation tests for RFC structure validation."""

from __future__ import annotations

import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


CHECKER = Path(__file__).with_name("check-rfcs.py")


def rfc(status: str) -> str:
    return (
        "# RFC 0001: Fixture\n\n"
        f"Status: {status}\n"
        "Date: 2026-08-03\n"
        "Supersedes: None.\n\n"
        "## Decision\n\nDecision.\n\n"
        "## Context\n\nContext.\n\n"
        "## Consequences\n\nConsequences and explicit non-goals.\n"
    )


class RfcCheckerTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temp_dir = tempfile.TemporaryDirectory()
        self.root = Path(self.temp_dir.name)
        (self.root / "rfcs/accepted").mkdir(parents=True)
        (self.root / "rfcs/proposed").mkdir()
        (self.root / "rfcs/accepted/0001-fixture.md").write_text(
            rfc("Accepted"), encoding="utf-8"
        )
        (self.root / "rfcs/proposed/0002-fixture.md").write_text(
            rfc("Proposed").replace("RFC 0001", "RFC 0002"), encoding="utf-8"
        )
        (self.root / "rfcs/README.md").write_text(
            "[Fixture](accepted/0001-fixture.md)\n", encoding="utf-8"
        )
        (self.root / "rfcs/proposed/README.md").write_text(
            "# Proposed RFCs\n", encoding="utf-8"
        )

    def tearDown(self) -> None:
        self.temp_dir.cleanup()

    def run_checker(self) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [sys.executable, str(CHECKER), str(self.root)],
            check=False,
            capture_output=True,
            text=True,
        )

    def assert_violation(self, message: str) -> None:
        result = self.run_checker()
        self.assertNotEqual(0, result.returncode)
        self.assertIn(message, result.stdout)
        self.assertEqual("", result.stderr)

    def test_valid_fixture_passes(self) -> None:
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_metadata_inside_a_fence_cannot_satisfy_rfc_header(self) -> None:
        path = self.root / "rfcs/accepted/0001-fixture.md"
        path.write_text(
            rfc("Accepted").replace(
                "Status: Accepted\n",
                "```text\nStatus: Accepted\n```\n",
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "rfcs/accepted/0001-fixture.md: metadata must be a contiguous header"
        )

    def test_required_section_inside_a_fence_is_not_a_heading(self) -> None:
        path = self.root / "rfcs/accepted/0001-fixture.md"
        path.write_text(
            rfc("Accepted").replace(
                "## Context\n\nContext.",
                "```text\n## Context\n```\n\nContext.",
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "rfcs/accepted/0001-fixture.md: missing required heading: ## Context"
        )

    def test_required_section_inside_html_comment_is_not_a_heading(self) -> None:
        path = self.root / "rfcs/accepted/0001-fixture.md"
        path.write_text(
            rfc("Accepted").replace(
                "## Context\n\nContext.",
                "<!--\n## Context\n-->\n\nContext.",
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "rfcs/accepted/0001-fixture.md: missing required heading: ## Context"
        )

    def test_every_required_section_needs_visible_content(self) -> None:
        path = self.root / "rfcs/accepted/0001-fixture.md"
        original = rfc("Accepted")
        for heading, content in (
            ("## Decision", "Decision."),
            ("## Context", "Context."),
            ("## Consequences", "Consequences and explicit non-goals."),
        ):
            with self.subTest(heading=heading):
                try:
                    path.write_text(
                        original.replace(
                            f"{heading}\n\n{content}",
                            heading,
                        ),
                        encoding="utf-8",
                    )
                    self.assert_violation(
                        "rfcs/accepted/0001-fixture.md: "
                        f"required section is empty: {heading}"
                    )
                finally:
                    path.write_text(original, encoding="utf-8")

    def test_required_section_reference_definition_is_not_visible_content(self) -> None:
        path = self.root / "rfcs/accepted/0001-fixture.md"
        path.write_text(
            rfc("Accepted").replace(
                "Decision.",
                "[hidden]: https://example.com",
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "rfcs/accepted/0001-fixture.md: "
            "required section is empty: ## Decision"
        )

    def test_multiline_reference_definition_is_not_visible_content(self) -> None:
        path = self.root / "rfcs/accepted/0001-fixture.md"
        path.write_text(
            rfc("Accepted").replace(
                "Decision.",
                '[hidden]:\n  https://example.com\n  "Reference title"',
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "rfcs/accepted/0001-fixture.md: "
            "required section is empty: ## Decision"
        )

    def test_tab_indented_reference_shape_counts_as_visible_code(self) -> None:
        # A leading tab expands to a four-space CommonMark indented code block;
        # it does not satisfy the 0-3-space reference-definition prefix.
        path = self.root / "rfcs/accepted/0001-fixture.md"
        path.write_text(
            rfc("Accepted").replace(
                "Decision.",
                "\t[visible-code]: https://example.com",
            ),
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_tab_indented_reference_destination_is_not_visible_content(self) -> None:
        path = self.root / "rfcs/accepted/0001-fixture.md"
        path.write_text(
            rfc("Accepted").replace(
                "Decision.",
                "[hidden]:\n\thttps://example.com",
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "rfcs/accepted/0001-fixture.md: "
            "required section is empty: ## Decision"
        )

    def test_raw_html_block_cannot_supply_a_required_heading(self) -> None:
        path = self.root / "rfcs/accepted/0001-fixture.md"
        path.write_text(
            rfc("Accepted").replace(
                "## Decision\n\nDecision.",
                '<script type="text/plain">\n## Decision\n\nDecision.\n</script>',
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "rfcs/accepted/0001-fixture.md: missing required heading: ## Decision"
        )

    def test_visible_reference_link_counts_as_section_content(self) -> None:
        path = self.root / "rfcs/accepted/0001-fixture.md"
        path.write_text(
            rfc("Accepted").replace(
                "Decision.",
                "Adopt the [documented behavior][source].\n\n"
                "[source]: https://example.com",
            ),
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_backtick_in_fence_info_does_not_hide_following_headings(self) -> None:
        path = self.root / "rfcs/accepted/0001-fixture.md"
        path.write_text(
            rfc("Accepted").replace(
                "## Decision",
                "```text`invalid\n\n## Decision",
            ),
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_fence_closer_with_suffix_does_not_expose_hidden_heading(self) -> None:
        path = self.root / "rfcs/accepted/0001-fixture.md"
        path.write_text(
            rfc("Accepted").replace(
                "## Context\n\nContext.",
                "```text\n```invalid\n## Context\n```\n\nContext.",
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "rfcs/accepted/0001-fixture.md: missing required heading: ## Context"
        )

    def test_accepted_rfc_requires_an_exact_visible_index_link(self) -> None:
        (self.root / "rfcs/README.md").write_text(
            "The old artifact was accepted/0001-fixture.md.backup.\n"
            "Inline code is not an index entry: "
            "`[Decoy](accepted/0001-fixture.md)`.\n"
            "```text\n[Decoy](accepted/0001-fixture.md)\n```\n",
            encoding="utf-8",
        )
        self.assert_violation(
            "rfcs/README.md: missing accepted RFC index entry: "
            "accepted/0001-fixture.md"
        )

    def test_accepted_rfc_index_rejects_missing_targets(self) -> None:
        (self.root / "rfcs/README.md").write_text(
            "[Fixture](accepted/0001-fixture.md)\n"
            "[Missing](accepted/9999-missing.md)\n",
            encoding="utf-8",
        )
        self.assert_violation(
            "rfcs/README.md: stale accepted RFC index entry: "
            "accepted/9999-missing.md"
        )

    def test_rfc_numbers_are_unique_across_status_directories(self) -> None:
        (self.root / "rfcs/proposed/0001-copy.md").write_text(
            rfc("Proposed"), encoding="utf-8"
        )

        self.assert_violation(
            "rfcs/proposed/0001-copy.md: duplicate RFC number 0001; "
            "already used by rfcs/accepted/0001-fixture.md"
        )

    def test_rfc_symlink_is_rejected(self) -> None:
        path = self.root / "rfcs/accepted/0001-fixture.md"
        target = self.root / "outside.md"
        target.write_text(rfc("Accepted"), encoding="utf-8")
        path.unlink()
        path.symlink_to(target)
        self.assert_violation("rfcs/accepted/0001-fixture.md: RFC must be a regular file")


if __name__ == "__main__":
    unittest.main()
