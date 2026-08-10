#!/usr/bin/env python3
"""Mutation tests for the documentation authority checker."""

from __future__ import annotations

import subprocess
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
CHECKER = ROOT / "scripts/check-spec-authority.sh"


class AuthorityCheckerTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temp_dir = tempfile.TemporaryDirectory()
        self.root = Path(self.temp_dir.name)
        subprocess.run(["git", "init", "-q"], cwd=self.root, check=True)
        for relative in (
            "docs/project/governance.md",
            "rfcs/accepted/0001-language-authority-and-change-control.md",
        ):
            target = self.root / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text((ROOT / relative).read_text(encoding="utf-8"), encoding="utf-8")
        (self.root / "README.md").write_text("# Fixture\n", encoding="utf-8")
        baseline = self.run_checker()
        self.assertEqual(0, baseline.returncode, baseline.stdout + baseline.stderr)

    def tearDown(self) -> None:
        self.temp_dir.cleanup()

    def run_checker(self) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            ["bash", str(CHECKER)],
            cwd=self.root,
            check=False,
            capture_output=True,
            text=True,
        )

    def test_rejects_case_variant_of_removed_identity(self) -> None:
        (self.root / "docs/index.md").write_text("JAZZ2\n", encoding="utf-8")
        result = self.run_checker()
        self.assertNotEqual(0, result.returncode)
        self.assertIn("removed implementation identity", result.stderr)

    def test_rejects_html_entity_encoded_removed_identity(self) -> None:
        (self.root / "docs/index.md").write_text(
            "jazz&#45;next\n",
            encoding="utf-8",
        )

        result = self.run_checker()

        self.assertNotEqual(0, result.returncode)
        self.assertIn("docs/index.md:1:jazz-next", result.stderr)
        self.assertIn("removed implementation identity", result.stderr)

    def test_rejects_markdown_escape_encoded_removed_identity(self) -> None:
        (self.root / "docs/index.md").write_text(
            "jazz\\-next\n",
            encoding="utf-8",
        )

        result = self.run_checker()

        self.assertNotEqual(0, result.returncode)
        self.assertIn("docs/index.md:1:jazz-next", result.stderr)
        self.assertIn("removed implementation identity", result.stderr)

    def test_entity_encoded_backslash_does_not_create_a_markdown_escape(self) -> None:
        (self.root / "docs/index.md").write_text(
            "jazz&#92;-next\n",
            encoding="utf-8",
        )

        result = self.run_checker()

        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_rejects_every_superseded_authority_path(self) -> None:
        for relative, is_directory in (
            ("docs/spec", True),
            ("docs/feature-status.md", False),
            ("docs/jazz-language-state.md", False),
            ("docs/jazz-improvement-backlog.md", False),
        ):
            with self.subTest(relative=relative):
                target = self.root / relative
                try:
                    if is_directory:
                        target.mkdir()
                    else:
                        target.write_text("obsolete\n", encoding="utf-8")
                    result = self.run_checker()
                    self.assertNotEqual(0, result.returncode)
                    self.assertIn(
                        f"superseded authority path still exists: {relative}",
                        result.stderr,
                    )
                finally:
                    if target.exists():
                        if is_directory:
                            target.rmdir()
                        else:
                            target.unlink()

    def test_rejects_truncated_wrapped_authority_statements(self) -> None:
        mutations = (
            (
                "docs/project/governance.md",
                "   evidence;",
                "   claims;",
            ),
            (
                "docs/project/governance.md",
                "implementation. A proposal",
                "delivery. A proposal",
            ),
            (
                "rfcs/accepted/0001-language-authority-and-change-control.md",
                "   `docs/reference/`.",
                "   `docs/guide/`.",
            ),
            (
                "rfcs/accepted/0001-language-authority-and-change-control.md",
                "   `jazz/`, and `test/` when the public contract does not yet cover a detail.",
                "   `src/` alone when the public contract does not yet cover a detail.",
            ),
        )
        for relative, old, new in mutations:
            with self.subTest(relative=relative, removed=old):
                path = self.root / relative
                original = path.read_text(encoding="utf-8")
                self.assertIn(old, original)
                try:
                    path.write_text(original.replace(old, new), encoding="utf-8")
                    result = self.run_checker()
                    self.assertNotEqual(0, result.returncode)
                    self.assertIn("missing authority statement", result.stderr)
                finally:
                    path.write_text(original, encoding="utf-8")

    def test_incidental_phrases_cannot_replace_authority_decision_bullets(self) -> None:
        path = self.root / "rfcs/accepted/0001-language-authority-and-change-control.md"
        text = path.read_text(encoding="utf-8")
        start = text.index("Jazz uses this descending authority order:")
        end = text.index("For claims about behavior", start)
        decoy = (
            "Jazz uses this descending authority order:\n\n1. An unspecified source.\n\n"
            "```text\npublic documentation\nimplementation and tests\n"
            "accepted durable decisions\nroadmap is non-normative\n"
            "semantic language change RFC before implementation\n```\n\n"
        )
        path.write_text(text[:start] + decoy + text[end:], encoding="utf-8")
        result = self.run_checker()
        self.assertNotEqual(0, result.returncode)
        self.assertIn("missing authority statement", result.stderr)

    def test_hidden_markdown_cannot_satisfy_authority_statements(self) -> None:
        mutations = (
            (
                "docs/project/governance.md",
                "1. curated public language and reference documentation;",
                "<!--\n"
                "1. curated public language and reference documentation;\n"
                "-->\n"
                "1. private notes;",
            ),
            (
                "docs/project/governance.md",
                "Semantic language changes require a reviewed decision record before\n"
                "implementation.",
                "```text\n"
                "Semantic language changes require a reviewed decision record before\n"
                "implementation.\n"
                "```\n"
                "Semantic language changes require no review before implementation.",
            ),
            (
                "rfcs/accepted/0001-language-authority-and-change-control.md",
                "1. Canonical public language contracts under `docs/language/` and\n"
                "   `docs/reference/`.",
                "<!--\n"
                "1. Canonical public language contracts under `docs/language/` and\n"
                "   `docs/reference/`.\n"
                "-->\n"
                "1. Internal plans are canonical.",
            ),
            (
                "rfcs/accepted/0001-language-authority-and-change-control.md",
                "3. Accepted durable decisions under `rfcs/accepted/`.",
                "```text\n"
                "3. Accepted durable decisions under `rfcs/accepted/`.\n"
                "```\n"
                "3. Draft plans are durable decisions.",
            ),
        )
        for relative, old, replacement in mutations:
            with self.subTest(relative=relative, hidden=old):
                path = self.root / relative
                original = path.read_text(encoding="utf-8")
                self.assertIn(old, original)
                try:
                    path.write_text(
                        original.replace(old, replacement, 1), encoding="utf-8"
                    )

                    result = self.run_checker()

                    self.assertNotEqual(0, result.returncode)
                    self.assertIn("missing authority statement", result.stderr)
                finally:
                    path.write_text(original, encoding="utf-8")

    def test_large_documents_do_not_fail_after_an_early_match(self) -> None:
        path = self.root / "docs/project/governance.md"
        # The old rendered_markdown | rg pipeline failed under pipefail only when
        # an early match left enough producer output to exceed the OS pipe buffer.
        # Keep this payload multiple MiB so the regression is stable across hosts.
        path.write_text(
            path.read_text(encoding="utf-8") + ("\nordinary documentation" * 400_000),
            encoding="utf-8",
        )

        result = self.run_checker()

        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_raw_html_block_cannot_satisfy_an_authority_statement(self) -> None:
        path = self.root / "docs/project/governance.md"
        original = path.read_text(encoding="utf-8")
        required = "1. curated public language and reference documentation;"
        self.assertIn(required, original)
        path.write_text(
            original.replace(
                required,
                '<script type="text/plain">\n'
                f"{required}\n"
                "</script>\n"
                "1. private notes;",
                1,
            ),
            encoding="utf-8",
        )

        result = self.run_checker()

        self.assertNotEqual(0, result.returncode)
        self.assertIn("missing authority statement", result.stderr)


if __name__ == "__main__":
    unittest.main()
