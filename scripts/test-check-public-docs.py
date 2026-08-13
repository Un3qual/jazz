#!/usr/bin/env python3
"""Behavior tests for Jazz's public-documentation publication contract."""

from __future__ import annotations

import hashlib
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


REPOSITORY_ROOT = Path(__file__).resolve().parent.parent
CHECKER = REPOSITORY_ROOT / "scripts/check-public-docs.py"


class PublicDocsCheckerTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        shutil.copytree(REPOSITORY_ROOT / "docs", self.root / "docs")
        shutil.copytree(REPOSITORY_ROOT / "examples", self.root / "examples")
        shutil.copytree(
            REPOSITORY_ROOT / "website/static", self.root / "website/static"
        )
        shutil.copy2(REPOSITORY_ROOT / "README.md", self.root / "README.md")
        for name in ("CHANGELOG.md", "CONTRIBUTING.md", "LICENSE", "RELEASING.md", "SECURITY.md"):
            shutil.copy2(REPOSITORY_ROOT / name, self.root / name)
        (self.root / "scripts").mkdir()
        for name in ("example-cases.tsv", "public-doc-fragments.tsv"):
            shutil.copy2(REPOSITORY_ROOT / "scripts" / name, self.root / "scripts" / name)
        subprocess.run(["git", "init", "-q"], cwd=self.root, check=True)
        subprocess.run(["git", "add", "examples"], cwd=self.root, check=True)

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def run_checker(self, *arguments: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [sys.executable, str(CHECKER), str(self.root), *arguments],
            check=False,
            capture_output=True,
            text=True,
        )

    def assert_violation(self, expected: str, *arguments: str) -> None:
        result = self.run_checker(*arguments)
        self.assertNotEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertIn(expected, result.stdout)
        self.assertEqual("", result.stderr)

    def replace_once(self, relative: str, old: str, new: str) -> None:
        path = self.root / relative
        source = path.read_text(encoding="utf-8")
        self.assertIn(old, source)
        path.write_text(source.replace(old, new, 1), encoding="utf-8")

    def test_checked_in_public_docs_pass(self) -> None:
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertEqual("Public documentation checks passed.\n", result.stdout)

    def test_docusaurus_owns_valid_front_matter_yaml(self) -> None:
        self.replace_once(
            "docs/getting-started/overview.md",
            "description: Compile and run a first Jazz program.",
            "description: >-\n  Compile and run a first Jazz program.",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_required_page_cannot_be_missing_or_draft(self) -> None:
        (self.root / "docs/reference/diagnostics.md").unlink()
        self.replace_once(
            "docs/getting-started/overview.md",
            "sidebar_position: 1",
            "sidebar_position: 1\ndraft: true",
        )
        self.assert_violation("missing required public page")
        self.assert_violation("required public page cannot be draft")

    def test_required_page_can_explicitly_disable_draft_mode(self) -> None:
        self.replace_once(
            "docs/getting-started/overview.md",
            "sidebar_position: 1",
            "sidebar_position: 1\ndraft: false",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_docs_reject_non_markdown_files_and_escaping_symlinks(self) -> None:
        (self.root / "docs/internal.txt").write_text("not a page\n", encoding="utf-8")
        outside = self.root / "internal.md"
        outside.write_text("secret\n", encoding="utf-8")
        (self.root / "docs/escape.md").symlink_to(outside)
        self.assert_violation("public docs files must use the .md extension")
        self.assert_violation("documentation path resolves outside docs/")

    def test_internal_material_is_not_public(self) -> None:
        path = self.root / "docs/project/status.md"
        path.write_text(path.read_text(encoding="utf-8") + "\n.codex/execution/queue.md\n", encoding="utf-8")
        self.assert_violation("banned public reference: .codex/")

    def test_executable_source_and_output_stay_synchronized(self) -> None:
        self.replace_once("README.md", "factorial 6.", "factorial 7.")
        self.replace_once("docs/getting-started/overview.md", '"Hello, Jazz"\n```', '"Hello, Jazz!"\n```')
        self.assert_violation("executable fence differs from examples/functions/factorial.jz")
        self.assert_violation("documented output for case hello differs")

    def test_executable_fences_use_compiler_backed_case_sources(self) -> None:
        source_path = self.root / "examples/unlisted.jz"
        source_path.write_text("41 + 1.\n", encoding="utf-8")
        page = self.root / "docs/language/control-flow.md"
        page.write_text(
            page.read_text(encoding="utf-8")
            + "\n<!-- jazz-example: executable path=examples/unlisted.jz -->\n\n"
            + "```jazz\n41 + 1.\n```\n",
            encoding="utf-8",
        )
        self.assert_violation(
            "examples/unlisted.jz: executable public fence is not compiler-backed"
        )

    def test_every_jazz_fence_has_an_explicit_sync_marker(self) -> None:
        self.replace_once(
            "docs/language/control-flow.md",
            "<!-- jazz-example: fragment -->",
            "<!-- ordinary note -->",
        )
        self.assert_violation("Jazz fence must have an adjacent jazz-example marker")

    def test_signature_fences_are_documentation_contracts_not_examples(self) -> None:
        page = self.root / "docs/standard-library/maybe.md"
        page.write_text(
            page.read_text(encoding="utf-8")
            + '\n```jazz title="API \\"quoted jazz-signature\\" sample" jazz-signature\n'
            + "maybeMap :: (a -> b) -> Maybe(a) -> Maybe(b).\n```\n",
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_legacy_signature_comment_is_not_a_durable_contract(self) -> None:
        page = self.root / "docs/standard-library/maybe.md"
        page.write_text(
            page.read_text(encoding="utf-8")
            + "\n<!-- jazz-signature -->\n\n"
            + "```jazz\nmaybeMap :: (a -> b) -> Maybe(a) -> Maybe(b).\n```\n",
            encoding="utf-8",
        )
        self.assert_violation(
            "legacy jazz-signature comment is not allowed"
        )

    def test_signature_metadata_must_be_an_exact_fence_token(self) -> None:
        page = self.root / "docs/standard-library/maybe.md"
        page.write_text(
            page.read_text(encoding="utf-8")
            + "\n```jazz not-jazz-signature\n"
            + "maybeMap :: (a -> b) -> Maybe(a) -> Maybe(b).\n```\n",
            encoding="utf-8",
        )
        self.assert_violation(
            "Jazz fence must have an adjacent jazz-example marker or jazz-signature fence metadata"
        )

    def test_signature_metadata_ignores_marker_text_inside_quoted_values(self) -> None:
        page = self.root / "docs/standard-library/maybe.md"
        page.write_text(
            page.read_text(encoding="utf-8")
            + '\n```jazz title="ordinary jazz-signature sample"\n'
            + "maybeMap :: (a -> b) -> Maybe(a) -> Maybe(b).\n```\n",
            encoding="utf-8",
        )
        self.assert_violation(
            "Jazz fence must have an adjacent jazz-example marker or jazz-signature fence metadata"
        )

    def test_signature_metadata_requires_the_lowercase_marker_spelling(self) -> None:
        page = self.root / "docs/standard-library/maybe.md"
        page.write_text(
            page.read_text(encoding="utf-8")
            + "\n```jazz JAZZ-SIGNATURE\n"
            + "maybeMap :: (a -> b) -> Maybe(a) -> Maybe(b).\n```\n",
            encoding="utf-8",
        )
        self.assert_violation(
            "Jazz fence must have an adjacent jazz-example marker or jazz-signature fence metadata"
        )

    def test_signature_fence_requires_a_matching_delimiter(self) -> None:
        page = self.root / "docs/standard-library/maybe.md"
        page.write_text(
            page.read_text(encoding="utf-8")
            + "\n```jazz jazz-signature\n"
            + "maybeMap :: (a -> b) -> Maybe(a) -> Maybe(b).\n~~~\n",
            encoding="utf-8",
        )
        self.assert_violation(
            "Jazz fence must have an adjacent jazz-example marker or jazz-signature fence metadata"
        )

    def test_signature_fence_requires_a_closer_at_least_as_long_as_the_opener(self) -> None:
        page = self.root / "docs/standard-library/maybe.md"
        page.write_text(
            page.read_text(encoding="utf-8")
            + "\n````jazz jazz-signature\n"
            + "maybeMap :: (a -> b) -> Maybe(a) -> Maybe(b).\n```\n",
            encoding="utf-8",
        )
        self.assert_violation(
            "Jazz fence must have an adjacent jazz-example marker or jazz-signature fence metadata"
        )

    def test_every_docusaurus_jazz_fence_shape_is_synchronized(self) -> None:
        for opener, closer in (("~~~jazz", "~~~"), ("   ```jazz", "   ```")):
            with self.subTest(opener=opener):
                path = self.root / "docs/language/control-flow.md"
                path.write_text(
                    path.read_text(encoding="utf-8")
                    + f'\n{opener}\nif hidden then "bypass" else "checker"\n{closer}\n',
                    encoding="utf-8",
                )
                self.assert_violation("Jazz fence must have an adjacent jazz-example marker")
                shutil.copy2(
                    REPOSITORY_ROOT / "docs/language/control-flow.md",
                    self.root / "docs/language/control-flow.md",
                )

    def test_unreadable_executable_example_is_a_boundary_violation(self) -> None:
        (self.root / "examples/functions/factorial.jz").write_bytes(b"\xff")
        self.assert_violation("cannot read UTF-8 text")

    def test_fragment_receipts_detect_unchecked_changes(self) -> None:
        self.replace_once(
            "docs/language/control-flow.md",
            'if score >= 60 then "pass" else "retry"',
            'if score >= 70 then "pass" else "retry"',
        )
        self.assert_violation("fragment receipt is missing or stale")

    def test_compiler_rejects_fragment_syntax_regressions(self) -> None:
        self.replace_once(
            "docs/language/control-flow.md",
            'if score >= 60 then "pass" else "retry"',
            "if condition then else",
        )
        invalid_source = "if condition then else\n"
        inventory = self.root / "scripts/public-doc-fragments.tsv"
        rows = inventory.read_text(encoding="utf-8").splitlines()
        rows[3] = "docs/language/control-flow.md\t1\t" + hashlib.sha256(
            invalid_source.encode("utf-8")
        ).hexdigest()
        inventory.write_text("\n".join(rows) + "\n", encoding="utf-8")

        compiler = self.root / "fixture-jazz"
        compiler.write_text(
            "#!/usr/bin/env python3\n"
            "import sys\n"
            "source = sys.stdin.read()\n"
            "if 'if condition then else' in source:\n"
            "    print('error: E0001 invalid syntax', file=sys.stderr)\n"
            "    raise SystemExit(1)\n",
            encoding="utf-8",
        )
        compiler.chmod(0o755)
        self.assert_violation("Jazz fragment has invalid syntax", "--jazz-bin", str(compiler))

    def test_compiler_operational_failure_does_not_validate_fragments(self) -> None:
        compiler = self.root / "fixture-jazz"
        compiler.write_text(
            "#!/usr/bin/env python3\n"
            "import sys\n"
            "print('fixture compiler unavailable', file=sys.stderr)\n"
            "raise SystemExit(64)\n",
            encoding="utf-8",
        )
        compiler.chmod(0o755)
        self.assert_violation(
            "Jazz fragment check failed without a compiler diagnostic",
            "--jazz-bin",
            str(compiler),
        )


if __name__ == "__main__":
    unittest.main()
