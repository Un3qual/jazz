#!/usr/bin/env python3
"""Fixture tests for the public documentation boundary checker."""

from __future__ import annotations

import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


CHECKER_PATH = Path(__file__).with_name("check-public-docs.py")

REQUIRED_PAGES = (
    "index.md",
    "getting-started/overview.md",
    "getting-started/installation.md",
    "getting-started/first-program.md",
    "getting-started/cli.md",
    "language/overview.md",
    "language/source-and-blocks.md",
    "language/bindings-and-functions.md",
    "language/types-and-signatures.md",
    "language/algebraic-data-types-and-patterns.md",
    "language/control-flow.md",
    "language/modules.md",
    "language/operators.md",
    "language/capabilities.md",
    "language/purity.md",
    "standard-library/overview.md",
    "standard-library/prelude.md",
    "standard-library/list.md",
    "standard-library/maybe-result-nonempty.md",
    "standard-library/dictionary.md",
    "standard-library/queue.md",
    "standard-library/map-and-set.md",
    "standard-library/char-and-text.md",
    "standard-library/io.md",
    "reference/lexical-grammar.md",
    "reference/expression-grammar.md",
    "reference/module-resolution.md",
    "reference/cli.md",
    "reference/diagnostics.md",
    "reference/runtime-values.md",
    "compiler/architecture.md",
    "compiler/pipeline.md",
    "compiler/bootstrapping.md",
    "project/status.md",
    "project/roadmap.md",
    "project/governance.md",
    "project/contributing.md",
)


def page(title: str = "Fixture", body: str = "Fixture body.\n") -> str:
    return (
        f"---\ntitle: {title}\ndescription: Test fixture.\n"
        f"sidebar_position: 1\n---\n\n{body}"
    )


class PublicDocsCheckerTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temp_dir = tempfile.TemporaryDirectory()
        self.root = Path(self.temp_dir.name)
        (self.root / "docs").mkdir()
        (self.root / "README.md").write_text("# Fixture\n", encoding="utf-8")
        for relative in REQUIRED_PAGES:
            target = self.root / "docs" / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(page(relative), encoding="utf-8")
        subprocess.run(["git", "init", "-q"], cwd=self.root, check=True)

    def tearDown(self) -> None:
        self.temp_dir.cleanup()

    def run_checker(self, root: Path | None = None) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [sys.executable, str(CHECKER_PATH), str(root or self.root)],
            check=False,
            capture_output=True,
            text=True,
        )

    def assert_violation(self, expected: str) -> None:
        result = self.run_checker()
        self.assertNotEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertIn(expected, result.stdout)
        self.assertEqual("", result.stderr)

    def test_valid_fixture_passes(self) -> None:
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertEqual("Public documentation checks passed.\n", result.stdout)

    def test_requires_title_and_description_front_matter(self) -> None:
        (self.root / "docs/index.md").write_text(
            "---\ntitle: Jazz\nsidebar_position: 1\n---\n\nMissing description.\n",
            encoding="utf-8",
        )
        self.assert_violation("docs/index.md: front matter is missing description")

    def test_requires_sidebar_position_front_matter(self) -> None:
        (self.root / "docs/index.md").write_text(
            "---\ntitle: Jazz\ndescription: Missing position.\n---\n",
            encoding="utf-8",
        )
        self.assert_violation("docs/index.md: front matter is missing sidebar_position")

    def test_rejects_missing_relative_link_targets(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(body="[Missing](language/not-there.md)\n"), encoding="utf-8"
        )
        self.assert_violation(
            "docs/index.md: public link target does not exist: language/not-there.md"
        )

    def test_rejects_links_that_leave_public_docs(self) -> None:
        (self.root / "outside.md").write_text("Outside.\n", encoding="utf-8")
        (self.root / "docs/index.md").write_text(
            page(body="[Outside](../outside.md)\n"), encoding="utf-8"
        )
        self.assert_violation(
            "docs/index.md: public link leaves docs/: ../outside.md"
        )

    def test_rejects_disallowed_top_level_docs_entries(self) -> None:
        target = self.root / "docs/superpowers/plan.md"
        target.parent.mkdir()
        target.write_text(page(), encoding="utf-8")
        self.assert_violation("docs/superpowers: disallowed top-level docs entry")

    def test_rejects_internal_and_legacy_references(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(body="See `.codex/plans/` and JazzNext.\n"), encoding="utf-8"
        )
        result = self.run_checker()
        self.assertIn("docs/index.md: banned public reference: .codex/", result.stdout)
        self.assertIn("docs/index.md: banned public reference: JazzNext", result.stdout)

    def test_rejects_links_that_escape_to_internal_trees(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(body="[Decision](../rfcs/accepted/0001.md)\n"), encoding="utf-8"
        )
        self.assert_violation(
            "docs/index.md: public link escapes docs into rfcs/: ../rfcs/accepted/0001.md"
        )

    def test_rejects_full_reference_links_that_escape_docs(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "Read the [decision][authority].\n\n"
                    "[authority]: ../rfcs/accepted/0001.md\n"
                )
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: public link escapes docs into rfcs/: ../rfcs/accepted/0001.md"
        )

    def test_rejects_collapsed_reference_links_that_escape_docs(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "Read [internal][].\n\n"
                    "[internal]: ../.codex/plans/private.md\n"
                )
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: public link escapes docs into .codex/: ../.codex/plans/private.md"
        )

    def test_rejects_shortcut_reference_links_that_escape_docs(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "Read [internal].\n\n"
                    "[internal]: ../.codex/execution/queue.md\n"
                )
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: public link escapes docs into .codex/: ../.codex/execution/queue.md"
        )

    def test_allows_reference_links_with_targets_inside_docs(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "Read the [language overview][guide].\n\n"
                    "[guide]: language/overview.md\n"
                )
            ),
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_executable_marker_must_name_existing_jazz_example(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(body="<!-- jazz-example: executable path=examples/missing.jz -->\n"),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: executable example does not exist: examples/missing.jz"
        )

    def test_executable_marker_cannot_escape_examples(self) -> None:
        (self.root / "outside.jz").write_text("0.\n", encoding="utf-8")
        (self.root / "docs/index.md").write_text(
            page(body="<!-- jazz-example: executable path=examples/../outside.jz -->\n"),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: invalid executable example path: examples/../outside.jz"
        )

    def test_executable_marker_must_name_a_tracked_example(self) -> None:
        example = self.root / "examples/untracked.jz"
        example.parent.mkdir()
        example.write_text("0.\n", encoding="utf-8")
        (self.root / "docs/index.md").write_text(
            page(
                body="<!-- jazz-example: executable path=examples/untracked.jz -->\n"
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: executable example is not tracked: examples/untracked.jz"
        )

    def test_rejects_tracked_executable_example_symlink_outside_examples(self) -> None:
        (self.root / "outside.jz").write_text("0.\n", encoding="utf-8")
        example = self.root / "examples/escape.jz"
        example.parent.mkdir()
        example.symlink_to("../outside.jz")
        subprocess.run(["git", "add", "examples/escape.jz"], cwd=self.root, check=True)
        (self.root / "docs/index.md").write_text(
            page(body="<!-- jazz-example: executable path=examples/escape.jz -->\n"),
            encoding="utf-8",
        )
        self.assert_violation(
            "examples/escape.jz: tracked example resolves outside examples/"
        )

    def test_every_tracked_example_is_referenced(self) -> None:
        example = self.root / "examples/hello.jz"
        example.parent.mkdir()
        example.write_text('"Hello".\n', encoding="utf-8")
        subprocess.run(["git", "add", "examples/hello.jz"], cwd=self.root, check=True)
        self.assert_violation(
            "examples/hello.jz: tracked example is not referenced by public docs or README.md"
        )

    def test_readme_can_reference_a_tracked_example(self) -> None:
        example = self.root / "examples/hello.jz"
        example.parent.mkdir()
        example.write_text('"Hello".\n', encoding="utf-8")
        (self.root / "README.md").write_text(
            "# Fixture\n\nSee `examples/hello.jz`.\n", encoding="utf-8"
        )
        subprocess.run(["git", "add", "examples/hello.jz"], cwd=self.root, check=True)
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_requires_every_task_five_page(self) -> None:
        (self.root / "docs/project/status.md").unlink()
        self.assert_violation("docs/project/status.md: missing required public page")

    def test_rejects_tracked_markdown_symlink_outside_docs(self) -> None:
        (self.root / "outside.md").write_text(page("Outside"), encoding="utf-8")
        link = self.root / "docs/language/escape.md"
        link.symlink_to("../../outside.md")
        subprocess.run(["git", "add", "docs/language/escape.md"], cwd=self.root, check=True)
        self.assert_violation(
            "docs/language/escape.md: documentation path resolves outside docs/"
        )

    def test_required_page_symlink_cannot_resolve_outside_docs(self) -> None:
        required = self.root / "docs/project/status.md"
        required.unlink()
        (self.root / "outside-status.md").write_text(
            page("Outside status"), encoding="utf-8"
        )
        required.symlink_to("../../outside-status.md")
        subprocess.run(["git", "add", "docs/project/status.md"], cwd=self.root, check=True)
        self.assert_violation(
            "docs/project/status.md: required public page resolves outside docs/"
        )

    def test_git_tracking_lookup_failure_is_actionable(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            non_repository = Path(temp_dir)
            (non_repository / "docs").mkdir()
            (non_repository / "docs/index.md").write_text(
                page("Index"), encoding="utf-8"
            )
            (non_repository / "README.md").write_text(
                "# Fixture\n", encoding="utf-8"
            )
            result = self.run_checker(non_repository)
            self.assertNotEqual(0, result.returncode)
            self.assertIn(
                "FAIL: repository: cannot enumerate tracked examples: git ls-files exited 128",
                result.stdout,
            )

    def test_violations_are_sorted_and_actionable(self) -> None:
        (self.root / "docs/index.md").write_text("No front matter.\n", encoding="utf-8")
        target = self.root / "docs/z-internal.md"
        target.write_text(page(body="See jazz2.\n"), encoding="utf-8")
        result = self.run_checker()
        lines = [line for line in result.stdout.splitlines() if line.startswith("FAIL:")]
        self.assertGreaterEqual(len(lines), 3)
        self.assertEqual(sorted(lines), lines)


if __name__ == "__main__":
    unittest.main()
