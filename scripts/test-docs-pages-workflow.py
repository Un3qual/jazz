#!/usr/bin/env python3
"""Behavior tests for the Jazz Pages deployment safety contract."""

from __future__ import annotations

import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


REPOSITORY_ROOT = Path(__file__).resolve().parent.parent
CHECKER = REPOSITORY_ROOT / "scripts/check-docs-pages-workflow.py"
WORKFLOW = Path(".github/workflows/docs-pages.yml")
PNPM_ACTION = "pnpm/action-setup@b906affcce14559ad1aafd4ab0e942779e9f58b1"


class DocsPagesWorkflowTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        self.workflow = self.root / WORKFLOW
        self.workflow.parent.mkdir(parents=True)
        source = (REPOSITORY_ROOT / WORKFLOW).read_text(encoding="utf-8")
        self.workflow.write_text(source, encoding="utf-8")

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def run_checker(self) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [sys.executable, str(CHECKER), str(self.root)],
            check=False,
            capture_output=True,
            text=True,
        )

    def replace(self, old: str, new: str) -> None:
        source = self.workflow.read_text(encoding="utf-8")
        self.assertIn(old, source)
        self.workflow.write_text(source.replace(old, new, 1), encoding="utf-8")

    def assert_violation(self, message: str) -> None:
        result = self.run_checker()
        self.assertNotEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertIn(message, result.stdout)
        self.assertEqual("", result.stderr)

    def test_checked_in_contract_passes(self) -> None:
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertEqual("Documentation Pages workflow checks passed.\n", result.stdout)

    def test_editor_grammar_changes_trigger_pages(self) -> None:
        self.replace(
            '      - "editors/vscode-jazz/syntaxes/jazz.tmLanguage.json"',
            '      - "editors/vscode-jazz/README.md"',
        )
        self.assert_violation("required workflow setting is missing")

    def test_actions_must_use_immutable_commits(self) -> None:
        self.replace(PNPM_ACTION, "pnpm/action-setup@v4")
        self.assert_violation("action must use an immutable commit")

    def test_checkout_must_not_persist_credentials(self) -> None:
        self.replace("persist-credentials: false", "persist-credentials: true")
        self.assert_violation("checkout must disable credential persistence")

    def test_checkout_credentials_are_checked_in_the_checkout_step(self) -> None:
        self.replace("persist-credentials: false", "persist-credentials: true")
        self.replace(
            "          version: 11.18.0\n",
            "          version: 11.18.0\n          persist-credentials: false\n",
        )
        self.assert_violation("checkout must disable credential persistence")

    def test_checkout_cannot_override_repository_or_revision(self) -> None:
        self.replace(
            "          persist-credentials: false\n",
            "          persist-credentials: false\n"
            "          repository: un3qual/another-repository\n"
            "          ref: main\n",
        )
        self.assert_violation("checkout must use the triggering repository and revision")

    def test_permissions_remain_job_scoped_and_minimal(self) -> None:
        self.replace(
            "permissions: {}",
            "permissions:\n  contents: write\n  pages: write",
        )
        self.assert_violation("workflow permissions must be empty")

    def test_build_and_boundary_commands_are_required(self) -> None:
        self.replace(
            "      - name: Check generated publication boundary\n"
            "        run: python3 scripts/check-website-boundary.py --build-directory website/build\n",
            "",
        )
        self.assert_violation("required workflow step is missing")

    def test_step_names_must_match_the_contract_exactly(self) -> None:
        self.replace(
            "      - name: Check generated publication boundary\n",
            "      - name: Check generated publication boundary disabled\n",
        )
        self.assert_violation("required workflow step is missing")

    def test_critical_steps_cannot_continue_after_failure(self) -> None:
        self.replace(
            "      - name: Check public documentation\n",
            "      - name: Check public documentation\n"
            "        continue-on-error: true\n",
        )
        self.assert_violation("critical workflow step must fail the job")

    def test_critical_steps_cannot_be_conditional(self) -> None:
        self.replace(
            "      - name: Check generated publication boundary\n",
            "      - name: Check generated publication boundary\n"
            "        if: false\n",
        )
        self.assert_violation("critical workflow step must fail the job")

    def test_publication_boundary_must_run_before_upload(self) -> None:
        boundary = (
            "      - name: Check generated publication boundary\n"
            "        run: python3 scripts/check-website-boundary.py --build-directory website/build\n\n"
        )
        upload = (
            "      - name: Upload GitHub Pages artifact\n"
            "        uses: actions/upload-pages-artifact@56afc609e74202658d3ffba0e8f6dda462b719fa # v3\n"
            "        with:\n"
            "          path: website/build\n"
        )
        source = self.workflow.read_text(encoding="utf-8")
        self.assertIn(boundary, source)
        self.assertIn(upload, source)
        source = source.replace(boundary, "", 1).replace(upload, upload + "\n" + boundary, 1)
        self.workflow.write_text(source, encoding="utf-8")
        self.assert_violation("workflow steps are out of publication order")


if __name__ == "__main__":
    unittest.main()
