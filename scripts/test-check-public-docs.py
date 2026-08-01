#!/usr/bin/env python3
"""Fixture tests for the public documentation boundary checker."""

from __future__ import annotations

import importlib.util
import subprocess
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
    return f"---\ntitle: {title}\ndescription: Test fixture.\n---\n\n{body}"


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

    def tearDown(self) -> None:
        self.temp_dir.cleanup()

    def run_checker(self) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            ["python3", str(CHECKER_PATH), str(self.root)],
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
            "---\ntitle: Jazz\n---\n\nMissing description.\n", encoding="utf-8"
        )
        self.assert_violation("docs/index.md: front matter is missing description")

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
        subprocess.run(["git", "init", "-q"], cwd=self.root, check=True)
        self.assert_violation(
            "docs/index.md: executable example is not tracked: examples/untracked.jz"
        )

    def test_every_tracked_example_is_referenced(self) -> None:
        example = self.root / "examples/hello.jz"
        example.parent.mkdir()
        example.write_text('"Hello".\n', encoding="utf-8")
        subprocess.run(["git", "init", "-q"], cwd=self.root, check=True)
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
        subprocess.run(["git", "init", "-q"], cwd=self.root, check=True)
        subprocess.run(["git", "add", "examples/hello.jz"], cwd=self.root, check=True)
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_requires_every_task_five_page(self) -> None:
        (self.root / "docs/project/status.md").unlink()
        self.assert_violation("docs/project/status.md: missing required public page")

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
