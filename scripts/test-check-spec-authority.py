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

    def test_rejects_every_superseded_authority_path(self) -> None:
        (self.root / "docs/jazz-improvement-backlog.md").write_text(
            "obsolete\n", encoding="utf-8"
        )
        result = self.run_checker()
        self.assertNotEqual(0, result.returncode)
        self.assertIn(
            "superseded authority path still exists: docs/jazz-improvement-backlog.md",
            result.stderr,
        )

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


if __name__ == "__main__":
    unittest.main()
