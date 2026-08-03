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

    def test_rfc_symlink_is_rejected(self) -> None:
        path = self.root / "rfcs/accepted/0001-fixture.md"
        target = self.root / "outside.md"
        target.write_text(rfc("Accepted"), encoding="utf-8")
        path.unlink()
        path.symlink_to(target)
        self.assert_violation("rfcs/accepted/0001-fixture.md: RFC must be a regular file")


if __name__ == "__main__":
    unittest.main()
