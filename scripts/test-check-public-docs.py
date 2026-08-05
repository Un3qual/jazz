#!/usr/bin/env python3
"""Fixture tests for the public documentation boundary checker."""

from __future__ import annotations

import hashlib
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


CHECKER_PATH = Path(__file__).with_name("check-public-docs.py")

FACTORIAL_PATH = "examples/functions/factorial.jz"
FACTORIAL_SOURCE = (
    "factorial :: Int -> Int.\n"
    "factorial =\n"
    "  \\|(0) -> 1\n"
    "   |(n) -> n * factorial (n - 1).\n"
    "factorial 6.\n"
)

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


def valid_readme(*, extra: str = "") -> str:
    lines = [
        '<img src="./jazz_logo.png" alt="Jazz" width="120" />',
        "",
        "# Jazz",
        "",
        "A statically typed functional language with practical syntax",
        "",
        "> **Experimental / pre-1.0:** Jazz is under active development.",
        "",
        "## A first Jazz program",
        "",
        f"<!-- jazz-example: executable path={FACTORIAL_PATH} -->",
        "```jazz",
        *FACTORIAL_SOURCE.rstrip("\n").splitlines(),
        "```",
        "",
        "Expected output:",
        "",
        "<!-- jazz-example-output: case=factorial -->",
        "```text",
        "720",
        "```",
        "",
        "## Quick start",
        "",
        "```bash",
        "nix develop",
        "cabal build all",
        f"cabal run jazz -- --run {FACTORIAL_PATH}",
        "```",
        "",
        "## Available today",
        "",
        "- Static typing",
        "",
        "## In development",
        "",
        "- Stable releases",
        "",
        "## Documentation",
        "",
        "- [Getting started](docs/getting-started/overview.md)",
        "- [Language guide](docs/language/overview.md)",
        "- [Standard library](docs/standard-library/overview.md)",
        "- [Language reference](docs/reference/expression-grammar.md)",
        "- [Compiler](docs/compiler/architecture.md)",
        "- [Status](docs/project/status.md)",
        "- [Roadmap](docs/project/roadmap.md)",
        "- [Contribution guide](docs/project/contributing.md)",
        "- [Issue tracker](https://github.com/un3qual/jazz/issues)",
        "- [Website (publishing with Workstream 3)](https://un3qual.github.io/jazz/)",
        "",
        "## Contributing",
        "",
        "Contributions are welcome; read the [contribution guide](docs/project/contributing.md).",
        "",
        "## License",
        "",
        "Jazz is licensed under [GPL-3.0-only](LICENSE).",
    ]
    if extra:
        lines.extend(["", extra.rstrip("\n")])
    while len(lines) < 100:
        lines.extend(["", "<!-- fixture spacing -->"])
    return "\n".join(lines[:150]) + "\n"


class PublicDocsCheckerTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temp_dir = tempfile.TemporaryDirectory()
        self.root = Path(self.temp_dir.name) / "repo"
        self.root.mkdir()
        (self.root / "docs").mkdir()
        (self.root / "scripts").mkdir()
        (self.root / "scripts/public-doc-fragments.tsv").write_text(
            "document\tordinal\tsha256\n",
            encoding="utf-8",
        )
        subprocess.run(["git", "init", "-q"], cwd=self.root, check=True)
        self.jazz_binary = self.root / "fixture-jazz"
        self.jazz_binary.write_text(
            (
                "#!/usr/bin/env python3\n"
                "import sys\n"
                "source = sys.stdin.read()\n"
                "if 'if condition then else' in source:\n"
                "    print(\"error: E0001 1:15: expected expression after 'then'\", file=sys.stderr)\n"
                "    raise SystemExit(1)\n"
                "if 'unknownValue.' in source:\n"
                "    print(\"error: E1001 1:1: unknown name 'unknownValue'\", file=sys.stderr)\n"
                "    raise SystemExit(1)\n"
                "if source.lstrip().startswith('module ') and source.rstrip().endswith('}\\n.'):\n"
                "    print(\"error: E0001 4:1: unexpected statement terminator after module declaration\", file=sys.stderr)\n"
                "    raise SystemExit(1)\n"
            ),
            encoding="utf-8",
        )
        self.jazz_binary.chmod(0o755)
        self.example_cases: list[tuple[str, list[str], str, str]] = []
        self.write_example_cases()
        (self.root / "jazz_logo.png").write_bytes(b"fixture")
        (self.root / "LICENSE").write_text("Fixture license.\n", encoding="utf-8")
        factorial = self.root / FACTORIAL_PATH
        factorial.parent.mkdir(parents=True)
        factorial.write_text(FACTORIAL_SOURCE, encoding="utf-8")
        subprocess.run(["git", "add", FACTORIAL_PATH], cwd=self.root, check=True)
        self.example_cases.append(
            (
                "factorial",
                [FACTORIAL_PATH],
                "720",
                f"--run {FACTORIAL_PATH}",
            )
        )
        self.write_example_cases()
        (self.root / "README.md").write_text(valid_readme(), encoding="utf-8")
        for relative in REQUIRED_PAGES:
            target = self.root / "docs" / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(page(relative), encoding="utf-8")

    def tearDown(self) -> None:
        self.temp_dir.cleanup()

    def run_checker(self, root: Path | None = None) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [
                sys.executable,
                str(CHECKER_PATH),
                str(root or self.root),
                "--jazz-bin",
                str(self.jazz_binary),
            ],
            check=False,
            capture_output=True,
            text=True,
        )

    def run_checker_without_binary(self) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [sys.executable, str(CHECKER_PATH), str(self.root)],
            check=False,
            capture_output=True,
            text=True,
        )

    def assert_violation(self, expected: str) -> None:
        result = self.run_checker()
        self.assertNotEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertIn(expected, result.stdout)
        self.assertEqual("", result.stderr)

    def add_tracked_example(
        self, relative_path: str, source: str, *, add_to_cases: bool = True
    ) -> Path:
        target = self.root / relative_path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(source, encoding="utf-8")
        subprocess.run(["git", "add", relative_path], cwd=self.root, check=True)
        if add_to_cases:
            self.add_example_case([relative_path])
        return target

    def add_example_case(self, sources: list[str]) -> None:
        case_name = f"case-{len(self.example_cases) + 1}"
        self.example_cases.append(
            (case_name, sources, "0", f"--run {sources[0]}")
        )
        self.write_example_cases()

    def write_example_cases(self) -> None:
        rows = "".join(
            f"{name}\t{','.join(sources)}\t{expected}\t{args}\n"
            for name, sources, expected, args in self.example_cases
        )
        (self.root / "scripts/example-cases.tsv").write_text(
            "name\tsources\texpected\targs\n" + rows,
            encoding="utf-8",
        )

    @staticmethod
    def executable_example(relative_path: str, source: str) -> str:
        fenced_source = source[:-1] if source.endswith("\n") else source
        return (
            f"<!-- jazz-example: executable path={relative_path} -->\n"
            f"```jazz\n{fenced_source}\n```\n"
        )

    def test_valid_fixture_passes(self) -> None:
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertEqual("Public documentation checks passed.\n", result.stdout)

    def test_checker_does_not_require_a_jazz_binary_by_default(self) -> None:
        result = self.run_checker_without_binary()

        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertEqual("Public documentation checks passed.\n", result.stdout)

    def test_readme_must_be_between_100_and_150_lines(self) -> None:
        short_readme = "\n".join(valid_readme().splitlines()[:99]) + "\n"
        (self.root / "README.md").write_text(
            short_readme,
            encoding="utf-8",
        )
        self.assert_violation("README.md: must contain between 100 and 150 lines")
        (self.root / "README.md").write_text(
            valid_readme() + ("Overflow line.\n" * 51),
            encoding="utf-8",
        )
        self.assert_violation("README.md: must contain between 100 and 150 lines")

    def test_readme_requires_exact_tagline_and_maturity_notice(self) -> None:
        readme = valid_readme().replace(
            "A statically typed functional language with practical syntax",
            "A functional language",
        ).replace("Experimental / pre-1.0", "Experimental")
        (self.root / "README.md").write_text(readme, encoding="utf-8")
        result = self.run_checker()
        self.assertIn("README.md: missing required tagline", result.stdout)
        self.assertIn("README.md: missing required maturity notice", result.stdout)

    def test_readme_maturity_notice_must_be_rendered(self) -> None:
        readme = valid_readme().replace(
            "> **Experimental / pre-1.0:** Jazz is under active development.",
            (
                "<!-- Experimental / pre-1.0 -->\n"
                "> **Experimental beta:** Jazz is under active development."
            ),
            1,
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")

        self.assert_violation("README.md: missing required maturity notice")

    def test_readme_tagline_must_be_rendered(self) -> None:
        tagline = "A statically typed functional language with practical syntax"
        readme = valid_readme().replace(
            tagline,
            f"<!--\n{tagline}\n-->",
            1,
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")

        self.assert_violation("README.md: missing required tagline")

    def test_readme_rejects_embellished_tagline(self) -> None:
        readme = valid_readme().replace(
            "A statically typed functional language with practical syntax",
            "**A statically typed functional language with practical syntax**",
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")
        self.assert_violation("README.md: missing required tagline")

    def test_readme_requires_local_logo_without_raw_query(self) -> None:
        readme = valid_readme().replace(
            './jazz_logo.png',
            "https://github.com/un3qual/jazz/blob/main/jazz_logo.png?raw=true",
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")
        result = self.run_checker()
        self.assertIn("README.md: logo must use a repository-local path", result.stdout)
        self.assertIn("README.md: image must use a repository-local path", result.stdout)
        self.assertIn("README.md: image URLs must not use ?raw=true", result.stdout)

    def test_readme_requires_factorial_marker_and_expected_output(self) -> None:
        readme = valid_readme().replace(
            f"<!-- jazz-example: executable path={FACTORIAL_PATH} -->",
            "<!-- jazz-example: fragment -->",
        ).replace("```text\n720\n```", "```text\n721\n```")
        (self.root / "README.md").write_text(readme, encoding="utf-8")
        result = self.run_checker()
        self.assertIn("README.md: missing executable factorial marker", result.stdout)
        self.assertIn("README.md: missing expected factorial output", result.stdout)

    def test_readme_factorial_marker_must_be_visible_and_bound_to_its_fence(self) -> None:
        marker = f"<!-- jazz-example: executable path={FACTORIAL_PATH} -->"
        readme = valid_readme(
            extra=f"```text\n{marker}\n```"
        ).replace(
            marker,
            "<!-- jazz-example: fragment -->",
            1,
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")
        (self.root / "docs/index.md").write_text(
            page(body=self.executable_example(FACTORIAL_PATH, FACTORIAL_SOURCE)),
            encoding="utf-8",
        )
        self.assert_violation("README.md: missing executable factorial marker")

    def test_readme_requires_navigation_and_license_contract(self) -> None:
        readme = valid_readme().replace(
            "[Language guide](docs/language/overview.md)",
            "Language guide",
        ).replace("[GPL-3.0-only](LICENSE)", "GPL licensed")
        (self.root / "README.md").write_text(readme, encoding="utf-8")
        result = self.run_checker()
        self.assertIn(
            "README.md: missing required navigation link: docs/language/overview.md",
            result.stdout,
        )
        self.assertIn("README.md: missing GPL-3.0-only license link", result.stdout)

    def test_readme_navigation_links_must_be_rendered(self) -> None:
        target = "docs/getting-started/overview.md"
        readme = valid_readme(
            extra=f"<!-- [Hidden getting started]({target}) -->"
        ).replace(f"[Getting started]({target})", "Getting started")
        (self.root / "README.md").write_text(readme, encoding="utf-8")

        self.assert_violation(
            f"README.md: missing required navigation link: {target}"
        )

    def test_readme_rejects_missing_local_link_fragments(self) -> None:
        readme = valid_readme(
            extra=(
                "[Missing section]"
                "(docs/getting-started/overview.md#missing-section)"
            )
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")

        self.assert_violation(
            "README.md: local link fragment does not exist: "
            "docs/getting-started/overview.md#missing-section"
        )

    def test_readme_rejects_same_page_missing_link_fragments(self) -> None:
        (self.root / "README.md").write_text(
            valid_readme(extra="[Missing section](#missing-section)"),
            encoding="utf-8",
        )

        self.assert_violation(
            "README.md: local link fragment does not exist: #missing-section"
        )

    def test_readme_navigation_links_inside_html_templates_are_inert(self) -> None:
        target = "docs/getting-started/overview.md"
        hidden_link = (
            "<template>\n"
            f"  <a href=\"{target}\">Hidden getting started</a>\n"
            "</template>"
        )
        readme = valid_readme().replace(
            f"[Getting started]({target})", hidden_link
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")

        self.assert_violation(
            f"README.md: missing required navigation link: {target}"
        )

    def test_readme_navigation_markdown_links_inside_html_templates_are_inert(
        self,
    ) -> None:
        target = "docs/getting-started/overview.md"
        hidden_link = (
            "<template>\n"
            f"  [Hidden getting started]({target})\n"
            "</template>"
        )
        readme = valid_readme().replace(
            f"[Getting started]({target})", hidden_link
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")

        self.assert_violation(
            f"README.md: missing required navigation link: {target}"
        )

    def test_readme_navigation_links_inside_hidden_html_are_inert(self) -> None:
        target = "docs/getting-started/overview.md"
        hidden_link = f"<span hidden>[Hidden getting started]({target})</span>"
        readme = valid_readme().replace(
            f"[Getting started]({target})", hidden_link
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")

        self.assert_violation(
            f"README.md: missing required navigation link: {target}"
        )

    def test_readme_navigation_links_inside_css_hidden_html_are_inert(self) -> None:
        target = "docs/getting-started/overview.md"
        hidden_link = (
            f'<a style="display: none" href="{target}">'
            "Hidden getting started</a>"
        )
        readme = valid_readme().replace(
            f"[Getting started]({target})", hidden_link
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")

        self.assert_violation(
            f"README.md: missing required navigation link: {target}"
        )

    def test_readme_navigation_links_inside_visibility_hidden_html_are_inert(
        self,
    ) -> None:
        target = "docs/getting-started/overview.md"
        hidden_link = (
            f'<a style="visibility: hidden !important" href="{target}">'
            "Hidden getting started</a>"
        )
        readme = valid_readme().replace(
            f"[Getting started]({target})", hidden_link
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")

        self.assert_violation(
            f"README.md: missing required navigation link: {target}"
        )

    def test_readme_navigation_links_inside_css_escaped_hidden_html_are_inert(
        self,
    ) -> None:
        target = "docs/getting-started/overview.md"
        hidden_link = (
            f'<a style="display: \\6e one" href="{target}">'
            "Hidden getting started</a>"
        )
        readme = valid_readme().replace(
            f"[Getting started]({target})", hidden_link
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")

        self.assert_violation(
            f"README.md: missing required navigation link: {target}"
        )

    def test_readme_navigation_links_with_visible_inline_styles_are_rendered(
        self,
    ) -> None:
        target = "docs/getting-started/overview.md"
        styled_link = (
            f'<a style="color: #b58900" href="{target}">Getting started</a>'
        )
        readme = valid_readme().replace(
            f"[Getting started]({target})", styled_link
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")

        result = self.run_checker()

        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_indented_code_cannot_supply_readme_navigation(self) -> None:
        target = "docs/getting-started/overview.md"
        readme = valid_readme().replace(
            f"- [Getting started]({target})",
            f"Getting started\n\n    [Code-only link]({target})",
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")

        self.assert_violation(
            f"README.md: missing required navigation link: {target}"
        )

    def test_indented_raw_html_cannot_supply_readme_navigation(self) -> None:
        target = "docs/getting-started/overview.md"
        readme = valid_readme().replace(
            f"- [Getting started]({target})",
            f"Getting started\n\n    <a href=\"{target}\">Code-only link</a>",
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")

        self.assert_violation(
            f"README.md: missing required navigation link: {target}"
        )

    def test_indented_raw_html_does_not_escape_the_docs_boundary(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(body='    <a href="../rfcs/accepted/0001.md">Code only</a>\n'),
            encoding="utf-8",
        )

        result = self.run_checker()

        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_readme_quick_start_commands_must_be_rendered(self) -> None:
        commands = (
            "nix develop\n"
            "cabal build all\n"
            f"cabal run jazz -- --run {FACTORIAL_PATH}\n"
        )
        hidden_commands = f"<template>\n{commands}</template>"
        readme = valid_readme().replace(
            f"```bash\n{commands}```", hidden_commands
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")

        result = self.run_checker()

        self.assertNotEqual(0, result.returncode, result.stdout + result.stderr)
        for command in commands.splitlines():
            self.assertIn(
                f"README.md: missing quick-start command: {command}",
                result.stdout,
            )
        self.assertEqual("", result.stderr)

    def test_readme_quick_start_commands_ignore_markdown_metadata(self) -> None:
        for decoy in (
            "[quick-start]: nix develop",
            (
                "[Setup][nix develop]\n\n"
                "[nix develop]: https://example.com"
            ),
            (
                "[quick-start]: https://example.com\n"
                '  "nix develop"'
            ),
            'Setup metadata <span title="nix develop">is ignored</span>.',
            "```nix develop\nnot a command\n```",
        ):
            with self.subTest(decoy=decoy):
                readme = valid_readme(extra=decoy).replace(
                    "\nnix develop\n",
                    "\necho enter the development environment\n",
                    1,
                )
                (self.root / "README.md").write_text(readme, encoding="utf-8")

                self.assert_violation(
                    "README.md: missing quick-start command: nix develop"
                )

    def test_readme_rejects_invalid_local_link_targets(self) -> None:
        outside = self.root.parent / "outside.md"
        outside.write_text("Outside.\n", encoding="utf-8")
        for markup, expected in (
            (
                "[Broken](docs/does-not-exist.md)",
                "README.md: local link target does not exist: "
                "docs/does-not-exist.md",
            ),
            (
                "[Broken](../outside.md)",
                "README.md: local link leaves repository: ../outside.md",
            ),
            (
                "[Broken][missing]\n\n[missing]: docs/does-not-exist.md",
                "README.md: local link target does not exist: "
                "docs/does-not-exist.md",
            ),
            (
                '<a href="docs/does-not-exist.md">\nBroken\n</a>',
                "README.md: local link target does not exist: "
                "docs/does-not-exist.md",
            ),
        ):
            with self.subTest(markup=markup):
                (self.root / "README.md").write_text(
                    valid_readme(extra=markup), encoding="utf-8"
                )
                self.assert_violation(expected)

    def test_readme_rejects_legacy_and_internal_terms(self) -> None:
        (self.root / "README.md").write_text(
            valid_readme(extra="See jazz2 and .codex/plans for Spec Authority."),
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertIn("README.md: banned front-door term: jazz2", result.stdout)
        self.assertIn("README.md: banned front-door term: .codex/", result.stdout)
        self.assertIn("README.md: banned front-door term: Spec Authority", result.stdout)

    def test_readme_rejects_private_kernel_intrinsics(self) -> None:
        (self.root / "README.md").write_text(
            valid_readme(extra="Call __kernel_writeTextRaw! directly."),
            encoding="utf-8",
        )

        self.assert_violation("README.md: banned front-door term: __kernel_")

    def test_readme_rejects_percent_encoded_internal_links(self) -> None:
        internal = self.root / ".codex/execution/queue.md"
        internal.parent.mkdir(parents=True)
        internal.write_text("Internal queue.\n", encoding="utf-8")
        target = ".%63odex/execution/queue.md"
        (self.root / "README.md").write_text(
            valid_readme(extra=f"[Internal queue]({target})"),
            encoding="utf-8",
        )

        self.assert_violation(
            "README.md: local link targets internal tree .codex/: " + target
        )

    def test_readme_requires_prescribed_content_order(self) -> None:
        readme = valid_readme().replace(
            "## Available today", "## Temporary heading", 1
        ).replace("## In development", "## Available today", 1).replace(
            "## Temporary heading", "## In development", 1
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")
        self.assert_violation(
            "README.md: required content is not in the prescribed order"
        )

    def test_readme_requires_each_named_section(self) -> None:
        (self.root / "README.md").write_text(
            valid_readme().replace("## Available today", "Available now"),
            encoding="utf-8",
        )
        self.assert_violation(
            "README.md: missing required section: ## Available today"
        )

    def test_readme_required_sections_must_be_rendered(self) -> None:
        section = "## Documentation"
        (self.root / "README.md").write_text(
            valid_readme().replace(section, f"<!--\n{section}\n-->", 1),
            encoding="utf-8",
        )

        self.assert_violation(
            "README.md: missing required section: ## Documentation"
        )

    def test_readme_rejects_section_heading_mentioned_inline(self) -> None:
        readme = valid_readme().replace(
            "## Available today",
            "The required heading would be `## Available today`.",
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")
        self.assert_violation(
            "README.md: missing required section: ## Available today"
        )

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

    def test_rejects_malformed_front_matter_even_with_required_fields(self) -> None:
        (self.root / "docs/index.md").write_text(
            (
                "---\n"
                "title: Jazz\n"
                "description: Test fixture.\n"
                "sidebar_position: 1\n"
                ": malformed\n"
                "---\n\n"
                "Fixture body.\n"
            ),
            encoding="utf-8",
        )
        self.assert_violation("docs/index.md: missing valid YAML front matter")

    def test_rejects_unterminated_yaml_flow_value(self) -> None:
        (self.root / "docs/index.md").write_text(
            page().replace("title: Fixture", "title: [broken"),
            encoding="utf-8",
        )

        self.assert_violation("docs/index.md: missing valid YAML front matter")

    def test_rejects_top_level_heading_that_duplicates_front_matter_title(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(body="# Fixture\n\nBody.\n"), encoding="utf-8"
        )
        self.assert_violation(
            "docs/index.md: top-level heading duplicates front matter title"
        )

    def test_front_matter_comment_is_not_a_rendered_heading(self) -> None:
        (self.root / "docs/index.md").write_text(
            (
                "---\n"
                "title: Fixture\n"
                "# This comment is valid YAML front matter.\n"
                "description: Test fixture.\n"
                "sidebar_position: 1\n"
                "---\n\n"
                "Fixture body.\n"
            ),
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_rejects_missing_relative_link_targets(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(body="[Missing](language/not-there.md)\n"), encoding="utf-8"
        )
        self.assert_violation(
            "docs/index.md: public link target does not exist: language/not-there.md"
        )

    def test_rejects_missing_local_link_fragments(self) -> None:
        (self.root / "docs/language/overview.md").write_text(
            page(body="## Existing section\n\nBody.\n"),
            encoding="utf-8",
        )
        (self.root / "docs/index.md").write_text(
            page(body="[Missing](language/overview.md#missing-section)\n"),
            encoding="utf-8",
        )

        self.assert_violation(
            "docs/index.md: public link fragment does not exist: "
            "language/overview.md#missing-section"
        )

    def test_rejects_same_page_missing_link_fragments(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "## Existing section\n\n"
                    "[Missing](#missing-section)\n"
                )
            ),
            encoding="utf-8",
        )

        self.assert_violation(
            "docs/index.md: public link fragment does not exist: #missing-section"
        )

    def test_hidden_headings_do_not_supply_local_link_fragments(self) -> None:
        (self.root / "docs/language/overview.md").write_text(
            page(body="<!--\n## Hidden section\n-->\nBody.\n"),
            encoding="utf-8",
        )
        (self.root / "docs/index.md").write_text(
            page(body="[Hidden](language/overview.md#hidden-section)\n"),
            encoding="utf-8",
        )

        self.assert_violation(
            "docs/index.md: public link fragment does not exist: "
            "language/overview.md#hidden-section"
        )

    def test_accepts_rendered_local_link_fragments(self) -> None:
        (self.root / "docs/language/overview.md").write_text(
            page(
                body=(
                    "## Repeated section\n\n"
                    "First.\n\n"
                    "## Repeated section\n\n"
                    "Second.\n\n"
                    "## Named section {#custom-anchor}\n\n"
                    "Named.\n"
                )
            ),
            encoding="utf-8",
        )
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "[First](language/overview.md#repeated-section)\n"
                    "[Second](language/overview.md#repeated-section-1)\n"
                    "[Named](language/overview.md#custom-anchor)\n"
                )
            ),
            encoding="utf-8",
        )

        result = self.run_checker()

        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_accepts_local_link_fragments_for_underscore_emphasis_headings(
        self,
    ) -> None:
        (self.root / "docs/language/overview.md").write_text(
            page(body="## _Emphasized_ heading\n\nBody.\n"),
            encoding="utf-8",
        )
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "[Emphasized heading]"
                    "(language/overview.md#emphasized-heading)\n"
                )
            ),
            encoding="utf-8",
        )

        result = self.run_checker()

        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_underscore_emphasis_slugging_preserves_literal_underscores(
        self,
    ) -> None:
        (self.root / "docs/language/overview.md").write_text(
            page(
                body=(
                    "## snake_case\n\n"
                    "## `_code_`\n\n"
                    "## \\_literal\\_\n"
                )
            ),
            encoding="utf-8",
        )
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "[Identifier](language/overview.md#snake_case)\n"
                    "[Code](language/overview.md#_code_)\n"
                    "[Escaped](language/overview.md#_literal_)\n"
                )
            ),
            encoding="utf-8",
        )

        result = self.run_checker()

        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_accepts_local_link_fragments_for_container_headings(self) -> None:
        (self.root / "docs/language/overview.md").write_text(
            page(
                body=(
                    "> ## Quoted heading\n>\n> Body.\n\n"
                    "1. ### Listed heading\n\n"
                    "   Body.\n\n"
                    "- > #### Nested heading\n"
                )
            ),
            encoding="utf-8",
        )
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "[Quoted](language/overview.md#quoted-heading)\n"
                    "[Listed](language/overview.md#listed-heading)\n"
                    "[Nested](language/overview.md#nested-heading)\n"
                )
            ),
            encoding="utf-8",
        )

        result = self.run_checker()

        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_accepts_local_link_fragments_for_autolink_headings(self) -> None:
        (self.root / "docs/language/overview.md").write_text(
            page(
                body=(
                    "## <https://example.com>\n\n"
                    "URI.\n\n"
                    "## <person@example.com>\n\n"
                    "Email.\n"
                )
            ),
            encoding="utf-8",
        )
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "[URI](language/overview.md#httpsexamplecom)\n"
                    "[Email](language/overview.md#personexamplecom)\n"
                )
            ),
            encoding="utf-8",
        )

        result = self.run_checker()

        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

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

    def test_rejects_mdx_pages_in_the_public_documentation_tree(self) -> None:
        (self.root / "docs/language/unvalidated.mdx").write_text(
            "---\ntitle: Unvalidated\n---\n\n"
            "[Internal](../../.codex/execution/queue.md)\n",
            encoding="utf-8",
        )

        self.assert_violation(
            "docs/language/unvalidated.mdx: MDX public pages are unsupported; "
            "use Markdown (.md)"
        )

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

    def test_rejects_html_links_that_escape_to_internal_trees(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(body='<a href="../rfcs/accepted/0001.md">Decision</a>\n'),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: public link escapes docs into rfcs/: ../rfcs/accepted/0001.md"
        )

    def test_rejects_unquoted_wrapped_html_links_to_internal_trees(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(body="<a\n  href=../rfcs/accepted/0001.md>Decision</a>\n"),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: public link escapes docs into rfcs/: ../rfcs/accepted/0001.md"
        )

    def test_rejects_multiline_html_block_links_to_internal_trees(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(body='<a href="../rfcs/accepted/0001.md">\nDecision\n</a>\n'),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: public link escapes docs into rfcs/: ../rfcs/accepted/0001.md"
        )

    def test_rejects_missing_raw_html_image_targets(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(body='<img src="missing.png" alt="Missing fixture">\n'),
            encoding="utf-8",
        )

        self.assert_violation(
            "docs/index.md: public link target does not exist: missing.png"
        )

    def test_rejects_missing_reference_style_image_targets(self) -> None:
        (self.root / "docs/language/operators.md").write_text(
            page(body="![Missing][fixture-image]\n\n[fixture-image]: missing.png\n"),
            encoding="utf-8",
        )

        self.assert_violation(
            "docs/language/operators.md: public link target does not exist: "
            "missing.png"
        )

    def test_inline_link_brackets_do_not_create_reference_usages(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    '[Visible](https://example.com/path/[internal] "Title [internal]")\n\n'
                    "[internal]: ../rfcs/accepted/0001-hidden.md\n"
                )
            ),
            encoding="utf-8",
        )

        result = self.run_checker()

        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_reference_definition_titles_do_not_create_reference_usages(
        self,
    ) -> None:
        definitions = {
            "same line": '[external]: https://example.com "Title [internal]"\n',
            "continued": (
                "[external]: https://example.com\n"
                '  "Title [internal]"\n'
            ),
        }
        for label, definition in definitions.items():
            with self.subTest(label=label):
                (self.root / "docs/index.md").write_text(
                    page(
                        body=(
                            "[Visible][external]\n\n"
                            f"{definition}"
                            "[internal]: ../rfcs/accepted/0001-hidden.md\n"
                        )
                    ),
                    encoding="utf-8",
                )

                result = self.run_checker()

                self.assertEqual(
                    0, result.returncode, result.stdout + result.stderr
                )

    def test_rejects_banned_public_references_case_insensitively(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(body="The old identity was JAZZ2.\n"), encoding="utf-8"
        )
        self.assert_violation("docs/index.md: banned public reference: jazz2")

    def test_rejects_html_entity_encoded_banned_public_references(self) -> None:
        (self.root / "docs/language/operators.md").write_text(
            page(body="The retired identity is jazz&#45;next.\n"),
            encoding="utf-8",
        )

        self.assert_violation(
            "docs/language/operators.md: banned public reference: jazz-next"
        )

    def test_rejects_markdown_escape_encoded_banned_public_references(self) -> None:
        (self.root / "docs/language/operators.md").write_text(
            page(body="The retired identity is jazz\\-next.\n"),
            encoding="utf-8",
        )

        self.assert_violation(
            "docs/language/operators.md: banned public reference: jazz-next"
        )

    def test_rejects_private_kernel_intrinsics_in_public_docs(self) -> None:
        (self.root / "docs/language/operators.md").write_text(
            page(body="Call __kernel_writeTextRaw! directly.\n"),
            encoding="utf-8",
        )

        self.assert_violation(
            "docs/language/operators.md: banned public reference: __kernel_"
        )

    def test_rejects_obsolete_generated_output_claims(self) -> None:
        (self.root / "docs/language/operators.md").write_text(
            page(body="Jazz produces JavaScript output.\n"),
            encoding="utf-8",
        )

        self.assert_violation(
            "docs/language/operators.md: banned public reference: "
            "JavaScript output"
        )

    def test_whitespace_only_link_target_is_an_actionable_violation(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(body="[Broken](   )\n"), encoding="utf-8"
        )
        result = self.run_checker()
        self.assertNotEqual(0, result.returncode)
        self.assertIn("docs/index.md: public link target is empty", result.stdout)
        self.assertEqual("", result.stderr)

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

    def test_rejects_escaped_reference_labels_that_escape_docs(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "Read the [decision][authority\\]].\n\n"
                    "[authority\\]]: ../rfcs/accepted/0001.md\n"
                )
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: public link escapes docs into rfcs/: ../rfcs/accepted/0001.md"
        )

    def test_rejects_blockquoted_reference_links_that_escape_docs(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "> Read the [decision][authority].\n>\n"
                    "> [authority]: ../rfcs/accepted/0001.md\n"
                )
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: public link escapes docs into rfcs/: ../rfcs/accepted/0001.md"
        )

    def test_tabbed_list_continuation_link_remains_rendered(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "- Durable decisions\n\n"
                    "\t[Decision](../rfcs/accepted/0001.md)\n"
                )
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: public link escapes docs into rfcs/: ../rfcs/accepted/0001.md"
        )

    def test_duplicate_reference_labels_use_the_first_definition(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "Read the [language guide][guide].\n\n"
                    "[guide]: language/overview.md\n"
                    "[GUIDE]: ../rfcs/accepted/0001.md\n"
                )
            ),
            encoding="utf-8",
        )

        result = self.run_checker()

        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

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

    def test_allows_reference_destinations_with_escaped_punctuation(self) -> None:
        (self.root / "docs/language/overview(old).md").write_text(
            page(), encoding="utf-8"
        )
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "Read the [language overview][guide].\n\n"
                    "[guide]: language/overview\\(old\\).md\n"
                )
            ),
            encoding="utf-8",
        )

        result = self.run_checker()

        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_executable_marker_must_name_existing_jazz_example(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "<!-- jazz-example: executable path=examples/missing.jz -->\n"
                    "```jazz\n0.\n```\n"
                )
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: executable example does not exist: examples/missing.jz"
        )

    def test_executable_marker_cannot_escape_examples(self) -> None:
        (self.root / "outside.jz").write_text("0.\n", encoding="utf-8")
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "<!-- jazz-example: executable path=examples/../outside.jz -->\n"
                    "```jazz\n0.\n```\n"
                )
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: invalid executable example path: examples/../outside.jz"
        )

    def test_executable_marker_must_name_a_tracked_example(self) -> None:
        example = self.root / "examples/untracked.jz"
        example.parent.mkdir(exist_ok=True)
        example.write_text("0.\n", encoding="utf-8")
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "<!-- jazz-example: executable path=examples/untracked.jz -->\n"
                    "```jazz\n0.\n```\n"
                )
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: executable example is not tracked: examples/untracked.jz"
        )

    def test_rejects_tracked_executable_example_symlink_outside_examples(self) -> None:
        (self.root / "outside.jz").write_text("0.\n", encoding="utf-8")
        example = self.root / "examples/escape.jz"
        example.parent.mkdir(exist_ok=True)
        example.symlink_to("../outside.jz")
        subprocess.run(["git", "add", "examples/escape.jz"], cwd=self.root, check=True)
        (self.root / "docs/index.md").write_text(
            page(body="<!-- jazz-example: executable path=examples/escape.jz -->\n"),
            encoding="utf-8",
        )
        self.assert_violation(
            "examples/escape.jz: tracked example resolves outside examples/"
        )

    def test_every_tracked_example_has_an_executable_fence(self) -> None:
        self.add_tracked_example("examples/hello.jz", '"Hello".\n')
        self.assert_violation(
            "examples/hello.jz: tracked example has no executable public-docs fence"
        )

    def test_readme_path_mention_does_not_cover_a_tracked_example(self) -> None:
        self.add_tracked_example("examples/hello.jz", '"Hello".\n')
        (self.root / "README.md").write_text(
            "# Fixture\n\nSee `examples/hello.jz`.\n", encoding="utf-8"
        )
        self.assert_violation(
            "examples/hello.jz: tracked example has no executable public-docs fence"
        )

    def test_accepts_matching_executable_fence_for_tracked_example(self) -> None:
        source = '"Hello".\n'
        self.add_tracked_example("examples/hello.jz", source)
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    self.executable_example("examples/hello.jz", source)
                    + "\n<!-- jazz-example-output: case=case-2 -->\n"
                    + "```text\n0\n```\n"
                )
            ),
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_raw_html_block_cannot_document_an_example_or_its_output(self) -> None:
        source = '"Hello".\n'
        self.add_tracked_example("examples/hello.jz", source)
        hidden_contract = (
            '<script type="text/plain">\n'
            + self.executable_example("examples/hello.jz", source)
            + "\n<!-- jazz-example-output: case=case-2 -->\n"
            + "```text\n0\n```\n"
            + "</script>\n"
        )
        (self.root / "docs/index.md").write_text(
            page(body=hidden_contract), encoding="utf-8"
        )

        result = self.run_checker()

        self.assertNotEqual(0, result.returncode)
        self.assertIn(
            "examples/hello.jz: tracked example has no executable public-docs fence",
            result.stdout,
        )
        self.assertIn(
            "scripts/example-cases.tsv: case case-2 has no documented expected output",
            result.stdout,
        )

    def test_larger_html_comment_cannot_document_an_example_or_output(
        self,
    ) -> None:
        source = '"Hello".\n'
        self.add_tracked_example("examples/hello.jz", source)
        hidden_contract = (
            "<!--\n"
            + self.executable_example("examples/hello.jz", source)
            + "\n<!-- jazz-example-output: case=case-2 -->\n"
            + "```text\n0\n```\n"
            + "-->\n"
        )
        (self.root / "docs/index.md").write_text(
            page(body=hidden_contract), encoding="utf-8"
        )

        result = self.run_checker()

        self.assertNotEqual(0, result.returncode)
        self.assertIn(
            "examples/hello.jz: tracked example has no executable public-docs fence",
            result.stdout,
        )
        self.assertIn(
            "scripts/example-cases.tsv: case case-2 has no documented expected output",
            result.stdout,
        )
        self.assertEqual("", result.stderr)

    def test_blockquoted_raw_html_cannot_document_an_example_or_output(
        self,
    ) -> None:
        source = '"Hello".\n'
        self.add_tracked_example("examples/hello.jz", source)
        hidden_contract = (
            '<script type="text/plain">\n'
            + self.executable_example("examples/hello.jz", source)
            + "\n<!-- jazz-example-output: case=case-2 -->\n"
            + "```text\n0\n```\n"
            + "</script>\n"
        )
        blockquoted_contract = "".join(
            f"> {line}" for line in hidden_contract.splitlines(keepends=True)
        )
        (self.root / "docs/index.md").write_text(
            page(body=blockquoted_contract), encoding="utf-8"
        )

        result = self.run_checker()

        self.assertNotEqual(0, result.returncode)
        self.assertIn(
            "examples/hello.jz: tracked example has no executable public-docs fence",
            result.stdout,
        )
        self.assertIn(
            "scripts/example-cases.tsv: case case-2 has no documented expected output",
            result.stdout,
        )
        self.assertEqual("", result.stderr)

    def test_rejects_documented_output_that_differs_from_manifest(self) -> None:
        (self.root / "README.md").write_text(
            valid_readme().replace(
                "<!-- jazz-example-output: case=factorial -->\n```text\n720",
                "<!-- jazz-example-output: case=factorial -->\n```text\n721",
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "README.md: documented output for case factorial differs from "
            "scripts/example-cases.tsv"
        )

    def test_every_example_case_has_documented_expected_output(self) -> None:
        source = '"Extra".\n'
        path = "examples/extra.jz"
        self.add_tracked_example(path, source)
        (self.root / "docs/index.md").write_text(
            page(body=self.executable_example(path, source)),
            encoding="utf-8",
        )
        status_path = self.root / "docs/project/status.md"
        status_path.write_text(
            status_path.read_text(encoding="utf-8")
            + "\n<!-- jazz-example-output: case=case-2 -->\n"
            + "```text\n0\n```\n",
            encoding="utf-8",
        )
        self.assert_violation(
            "scripts/example-cases.tsv: case case-2 has no documented expected output"
        )

    def test_readme_executable_fence_can_cover_a_tracked_example(self) -> None:
        source = '"Hello".\n'
        self.add_tracked_example("examples/hello.jz", source)
        (self.root / "README.md").write_text(
            valid_readme(
                extra=(
                    self.executable_example("examples/hello.jz", source)
                    + "\n<!-- jazz-example-output: case=case-2 -->\n"
                    + "```text\n0\n```\n"
                )
            ),
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_rejects_unclassified_jazz_fence(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(body="```jazz\n0.\n```\n"), encoding="utf-8"
        )
        self.assert_violation(
            "docs/index.md: Jazz fence must be immediately preceded by a jazz-example marker"
        )

    def test_rejects_unclassified_indented_jazz_fence(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(body="  ```jazz\n0.\n  ```\n"), encoding="utf-8"
        )
        self.assert_violation(
            "docs/index.md: Jazz fence must be immediately preceded by a jazz-example marker"
        )

    def test_rejects_unclassified_tilde_jazz_fence(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(body="~~~jazz\n0.\n~~~\n"), encoding="utf-8"
        )
        self.assert_violation(
            "docs/index.md: Jazz fence must be immediately preceded by a jazz-example marker"
        )

    def test_accepts_space_before_jazz_info_token(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "<!-- jazz-example: fragment -->\n"
                    "``` jazz\n0.\n```\n"
                )
            ),
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_accepts_longer_matching_fence_closer(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "<!-- jazz-example: fragment -->\n"
                    "``` jazz\n0.\n````\n"
                )
            ),
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_accepts_both_commonmark_fence_delimiters(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "<!-- jazz-example: fragment -->\n"
                    "``` jazz\n0.\n```\n\n"
                    "<!-- jazz-example: fragment -->\n"
                    "~~~ jazz\n1.\n~~~\n"
                )
            ),
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_invalid_closer_stays_inside_jazz_fence(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "<!-- jazz-example: fragment -->\n"
                    "```` jazz\n"
                    "0.\n"
                    "```\n"
                    "~~~\n"
                    "still code\n"
                    "`````\n"
                )
            ),
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_parser_resumes_after_a_valid_jazz_fence_closer(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "<!-- jazz-example: fragment -->\n"
                    "``` jazz\n0.\n````\n"
                    "Prose between fences.\n"
                    "<!-- jazz-example: fragment -->\n"
                    "~~~ jazz\n1.\n~~~~\n"
                )
            ),
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_rejects_unclosed_jazz_fence(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "<!-- jazz-example: fragment -->\n"
                    "``` jazz\n"
                    "0.\n"
                )
            ),
            encoding="utf-8",
        )
        self.assert_violation("docs/index.md: unclosed Jazz fence")

    def test_four_space_and_tab_pseudo_fences_are_not_fences(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "    ``` jazz\n"
                    "    0.\n"
                    "    ```\n"
                    "\t~~~ jazz\n"
                    "\t1.\n"
                    "\t~~~\n"
                )
            ),
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_rejects_unclassified_readme_jazz_fence(self) -> None:
        (self.root / "README.md").write_text(
            "# Fixture\n\n```jazz\n0.\n```\n", encoding="utf-8"
        )
        self.assert_violation(
            "README.md: Jazz fence must be immediately preceded by a jazz-example marker"
        )

    def test_accepts_fragment_marker_before_jazz_fence(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(body="<!-- jazz-example: fragment -->\n```jazz\n0.\n```\n"),
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_accepts_complete_module_declaration_fragment(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "<!-- jazz-example: fragment -->\n"
                    "```jazz\n"
                    "module Example::Fixture {\n"
                    "  answer = 1.\n"
                    "}\n"
                    "```\n"
                )
            ),
            encoding="utf-8",
        )

        result = self.run_checker()

        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_default_checker_requires_fragments_in_compiler_inventory(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(body="<!-- jazz-example: fragment -->\n```jazz\n0.\n```\n"),
            encoding="utf-8",
        )

        result = self.run_checker_without_binary()

        self.assertNotEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertIn(
            "docs/index.md: Jazz fragment 1 is missing from "
            "scripts/public-doc-fragments.tsv",
            result.stdout,
        )

    def test_default_checker_accepts_compiler_inventory_fragment(self) -> None:
        source = "0.\n"
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "<!-- jazz-example: fragment -->\n"
                    f"```jazz\n{source}```\n"
                )
            ),
            encoding="utf-8",
        )
        digest = hashlib.sha256(source.encode("utf-8")).hexdigest()
        (self.root / "scripts/public-doc-fragments.tsv").write_text(
            (
                "document\tordinal\tsha256\n"
                f"docs/index.md\t1\t{digest}\n"
            ),
            encoding="utf-8",
        )

        result = self.run_checker_without_binary()

        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_default_checker_requires_fragment_inventory_file(self) -> None:
        (self.root / "scripts/public-doc-fragments.tsv").unlink()

        result = self.run_checker_without_binary()

        self.assertNotEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertIn(
            "scripts/public-doc-fragments.tsv: cannot read inventory:",
            result.stdout,
        )

    def test_default_checker_rejects_malformed_fragment_inventory(self) -> None:
        digest = "0" * 64
        cases = (
            (
                "missing final newline",
                "document\tordinal\tsha256",
                "scripts/public-doc-fragments.tsv: file must end with a newline",
            ),
            (
                "invalid header",
                "path\tordinal\tsha256\n",
                "scripts/public-doc-fragments.tsv: invalid header",
            ),
            (
                "wrong field count",
                "document\tordinal\tsha256\ndocs/index.md\t1\n",
                "scripts/public-doc-fragments.tsv:2: expected three tab-separated fields",
            ),
            (
                "unsafe path",
                f"document\tordinal\tsha256\n../README.md\t1\t{digest}\n",
                "scripts/public-doc-fragments.tsv:2: invalid document path",
            ),
            (
                "invalid ordinal",
                f"document\tordinal\tsha256\ndocs/index.md\t0\t{digest}\n",
                "scripts/public-doc-fragments.tsv:2: invalid fragment ordinal",
            ),
            (
                "invalid digest",
                "document\tordinal\tsha256\ndocs/index.md\t1\tnot-a-digest\n",
                "scripts/public-doc-fragments.tsv:2: invalid SHA-256 digest",
            ),
            (
                "duplicate entry",
                (
                    "document\tordinal\tsha256\n"
                    f"docs/index.md\t1\t{digest}\n"
                    f"docs/index.md\t1\t{digest}\n"
                ),
                "scripts/public-doc-fragments.tsv:3: duplicate fragment entry",
            ),
            (
                "stale entry",
                (
                    "document\tordinal\tsha256\n"
                    f"docs/index.md\t1\t{digest}\n"
                ),
                "scripts/public-doc-fragments.tsv: stale Jazz fragment entry: "
                "docs/index.md#1",
            ),
        )
        for label, inventory, expected in cases:
            with self.subTest(label=label):
                (self.root / "scripts/public-doc-fragments.tsv").write_text(
                    inventory,
                    encoding="utf-8",
                )

                result = self.run_checker_without_binary()

                self.assertNotEqual(
                    0, result.returncode, result.stdout + result.stderr
                )
                self.assertIn(expected, result.stdout)

    def test_required_public_pages_need_rendered_body_content(self) -> None:
        (self.root / "docs/reference/cli.md").write_text(
            page(body=""),
            encoding="utf-8",
        )

        self.assert_violation(
            "docs/reference/cli.md: required public page has no rendered body"
        )

    def test_rejects_fragment_with_invalid_jazz_syntax(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "<!-- jazz-example: fragment -->\n"
                    "```jazz\n"
                    "if condition then else.\n"
                    "```\n"
                )
            ),
            encoding="utf-8",
        )

        self.assert_violation("docs/index.md: Jazz fragment has invalid syntax")

    def test_rejects_invalid_jazz_fragments_inside_markdown_containers(
        self,
    ) -> None:
        container_bodies = {
            "ordered list": (
                "1. Example\n\n"
                "    <!-- jazz-example: fragment -->\n"
                "    ```jazz\n"
                "    if condition then else.\n"
                "    ```\n"
            ),
            "blockquote": (
                "> <!-- jazz-example: fragment -->\n"
                "> ```jazz\n"
                "> if condition then else.\n"
                "> ```\n"
            ),
        }
        for container, body in container_bodies.items():
            with self.subTest(container=container):
                (self.root / "docs/index.md").write_text(
                    page(body=body), encoding="utf-8"
                )

                self.assert_violation(
                    "docs/index.md: Jazz fragment has invalid syntax"
                )

    def test_fragment_syntax_check_ignores_contextual_semantic_errors(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "<!-- jazz-example: fragment -->\n"
                    "```jazz\n"
                    "unknownValue.\n"
                    "```\n"
                )
            ),
            encoding="utf-8",
        )

        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertEqual("Public documentation checks passed.\n", result.stdout)

    def test_rejects_orphan_jazz_example_marker(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(body="<!-- jazz-example: fragment -->\nNo fence follows.\n"),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: jazz-example marker is not immediately followed by a Jazz fence"
        )

    def test_requires_marker_immediately_before_jazz_fence(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "<!-- jazz-example: fragment -->\n"
                    "This prose breaks the association.\n\n"
                    "```jazz\n0.\n```\n"
                )
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: Jazz fence must be immediately preceded by a jazz-example marker"
        )

    def test_rejects_executable_fence_content_drift(self) -> None:
        self.add_tracked_example("examples/hello.jz", '"Hello".\n')
        (self.root / "docs/index.md").write_text(
            page(
                body=self.executable_example(
                    "examples/hello.jz", '"Different".\n'
                )
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: executable fence differs from examples/hello.jz"
        )

    def test_executable_fence_comparison_preserves_extra_final_newlines(self) -> None:
        self.add_tracked_example("examples/hello.jz", '"Hello".\n\n')
        (self.root / "docs/index.md").write_text(
            page(
                body=self.executable_example("examples/hello.jz", '"Hello".\n')
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: executable fence differs from examples/hello.jz"
        )

    def test_executable_fence_comparison_preserves_internal_line_endings(self) -> None:
        self.add_tracked_example("examples/two-lines.jz", "1.\r\n2.\r\n")
        (self.root / "docs/index.md").write_text(
            page(
                body=self.executable_example(
                    "examples/two-lines.jz", "1.\n2.\n"
                )
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: executable fence differs from examples/two-lines.jz"
        )

    def test_requires_tracked_examples_in_operational_example_cases(self) -> None:
        source = '"Hello".\n'
        self.add_tracked_example(
            "examples/hello.jz", source, add_to_cases=False
        )
        (self.root / "docs/index.md").write_text(
            page(body=self.executable_example("examples/hello.jz", source)),
            encoding="utf-8",
        )
        self.assert_violation(
            "examples/hello.jz: tracked example is missing from scripts/example-cases.tsv"
        )

    def test_run_case_cannot_claim_an_uninvoked_source(self) -> None:
        extra_path = "examples/extra.jz"
        extra_source = "0.\n"
        self.add_tracked_example(extra_path, extra_source, add_to_cases=False)
        self.example_cases[0] = (
            "factorial",
            [FACTORIAL_PATH, extra_path],
            "720",
            f"--run {FACTORIAL_PATH}",
        )
        self.write_example_cases()
        (self.root / "docs/index.md").write_text(
            page(body=self.executable_example(extra_path, extra_source)),
            encoding="utf-8",
        )
        self.assert_violation(
            "scripts/example-cases.tsv:2: --run source does not match declared sources"
        )

    def test_rejects_untracked_operational_case_source(self) -> None:
        self.add_example_case(["examples/ghost.jz"])
        self.assert_violation(
            "scripts/example-cases.tsv: case source is not a tracked example: examples/ghost.jz"
        )

    def test_header_name_cannot_be_reused_as_an_example_case(self) -> None:
        source = '"Hello".\n'
        example_path = "examples/hello.jz"
        self.add_tracked_example(example_path, source, add_to_cases=False)
        self.example_cases.append(
            ("name", [example_path], "0", f"--run {example_path}")
        )
        self.write_example_cases()
        (self.root / "docs/index.md").write_text(
            page(body=self.executable_example(example_path, source)),
            encoding="utf-8",
        )
        self.assert_violation(
            "scripts/example-cases.tsv:3: case name is reserved for the header: name"
        )

    def test_example_case_table_requires_a_final_newline(self) -> None:
        (self.root / "scripts/example-cases.tsv").write_text(
            "name\tsources\texpected\targs",
            encoding="utf-8",
        )
        self.assert_violation(
            "scripts/example-cases.tsv: file must end with a newline"
        )

    def test_module_dependency_sources_require_real_case_and_doc_coverage(self) -> None:
        greeting = "module Example::Greeting {\n  greeting = \"Hello\".\n}\n"
        main = (
            "module Example::Main {\n"
            "  import Example::Greeting.\n"
            "  greeting.\n"
            "}\n"
        )
        greeting_path = "examples/modules/src/Example/Greeting.jz"
        main_path = "examples/modules/src/Example/Main.jz"
        self.add_tracked_example(greeting_path, greeting, add_to_cases=False)
        self.add_tracked_example(main_path, main, add_to_cases=False)
        self.example_cases.append(
            (
                "module",
                [main_path, greeting_path],
                '"Hello"',
                "--run --entry-module Example::Main "
                "--module-root examples/modules/src",
            )
        )
        self.write_example_cases()
        body = self.executable_example(
            greeting_path, greeting
        ) + self.executable_example(
            main_path, main
        ) + (
            "\n<!-- jazz-example-output: case=module -->\n"
            "```text\n\"Hello\"\n```\n"
        )
        (self.root / "docs/index.md").write_text(page(body=body), encoding="utf-8")
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
