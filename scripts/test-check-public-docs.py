#!/usr/bin/env python3
"""Fixture tests for the public documentation boundary checker."""

from __future__ import annotations

import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


CHECKER_PATH = Path(__file__).with_name("check-public-docs.py")

FACTORIAL_PATH = "examples/functions/factorial.jz"
WORDMARK_PATH = "website/static/img/jazz-wordmark.svg"
DARK_WORDMARK_PATH = "website/static/img/jazz-wordmark-dark.svg"
PUBLIC_WEBSITE_URL = "https://un3qual.github.io/jazz/"
PROSPECTIVE_WEBSITE_LABEL = "available after merge and Pages enablement"
README_WEBSITE_LINK = f"[Website ({PROSPECTIVE_WEBSITE_LABEL})]({PUBLIC_WEBSITE_URL})"
GETTING_STARTED_WEBSITE_LINK = (
    f"[Jazz documentation website ({PROSPECTIVE_WEBSITE_LABEL})]"
    f"({PUBLIC_WEBSITE_URL})"
)
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
        "<picture>",
        (
            f'  <source srcset="./{DARK_WORDMARK_PATH}" '
            'media="(prefers-color-scheme: dark)" />'
        ),
        f'  <img src="./{WORDMARK_PATH}" alt="Jazz" width="240" />',
        "</picture>",
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
        f"- {README_WEBSITE_LINK}",
        "  — enabling GitHub Pages for GitHub Actions is a post-merge follow-up.",
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
        self.root = Path(self.temp_dir.name)
        (self.root / "docs").mkdir()
        (self.root / "scripts").mkdir()
        subprocess.run(["git", "init", "-q"], cwd=self.root, check=True)
        self.example_cases: list[tuple[str, list[str], str, str]] = []
        self.write_example_cases()
        self.write_example_runner()
        wordmark = self.root / WORDMARK_PATH
        wordmark.parent.mkdir(parents=True)
        wordmark.write_text("<svg></svg>\n", encoding="utf-8")
        (self.root / DARK_WORDMARK_PATH).write_text("<svg></svg>\n", encoding="utf-8")
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
        (self.root / "docs/getting-started/overview.md").write_text(
            page(
                "Getting started",
                f"The {GETTING_STARTED_WEBSITE_LINK} will publish these guides.\n\n"
                "Enabling GitHub Pages for GitHub Actions is a post-merge follow-up.\n",
            ),
            encoding="utf-8",
        )

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

    def write_example_runner(self, *, consume_cases: bool = True) -> None:
        consumer = ""
        if consume_cases:
            consumer = (
                "while IFS=$'\\t' read -r case_name case_sources "
                "case_expected case_args_text; do\n"
                "  [[ \"$case_name\" == \"name\" ]] && continue\n"
                "  IFS=',' read -r -a case_source_paths <<< \"$case_sources\"\n"
                "  IFS=' ' read -r -a case_args <<< \"$case_args_text\"\n"
                "  run_example \"$case_name\" \"$case_expected\" "
                "\"${case_args[@]}\"\n"
                "done < scripts/example-cases.tsv\n"
            )
        (self.root / "scripts/check-examples.sh").write_text(
            "#!/usr/bin/env bash\n\n" + consumer,
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

    def test_readme_rejects_embellished_tagline(self) -> None:
        readme = valid_readme().replace(
            "A statically typed functional language with practical syntax",
            "**A statically typed functional language with practical syntax**",
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")
        self.assert_violation("README.md: missing required tagline")

    def test_readme_requires_local_logo_without_raw_query(self) -> None:
        readme = valid_readme().replace(
            f'./{WORDMARK_PATH}',
            "https://github.com/un3qual/jazz/blob/main/website/static/img/jazz-wordmark.svg?raw=true",
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")
        result = self.run_checker()
        self.assertIn("README.md: logo must use a repository-local path", result.stdout)
        self.assertIn("README.md: image must use a repository-local path", result.stdout)
        self.assertIn("README.md: image URLs must not use ?raw=true", result.stdout)

    def test_readme_requires_canonical_wordmark_path(self) -> None:
        alternate = self.root / "website/static/img/alternate.svg"
        alternate.write_text("<svg></svg>\n", encoding="utf-8")
        readme = valid_readme().replace(WORDMARK_PATH, "website/static/img/alternate.svg")
        (self.root / "README.md").write_text(readme, encoding="utf-8")
        self.assert_violation("README.md: logo must use a repository-local path")

    def test_readme_requires_canonical_dark_mode_wordmark_path(self) -> None:
        readme = valid_readme().replace(
            f'  <source srcset="./{DARK_WORDMARK_PATH}" '
            'media="(prefers-color-scheme: dark)" />\n',
            "",
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")
        self.assert_violation(
            "README.md: dark-mode logo must use the canonical repository-local path"
        )

    def test_readme_requires_factorial_marker_and_expected_output(self) -> None:
        readme = valid_readme().replace(
            f"<!-- jazz-example: executable path={FACTORIAL_PATH} -->",
            "<!-- jazz-example: fragment -->",
        ).replace("```text\n720\n```", "```text\n721\n```")
        (self.root / "README.md").write_text(readme, encoding="utf-8")
        result = self.run_checker()
        self.assertIn("README.md: missing executable factorial marker", result.stdout)
        self.assertIn("README.md: missing expected factorial output", result.stdout)

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

    def test_readme_comment_decoy_cannot_supply_license_link(self) -> None:
        readme = valid_readme().replace(
            "[GPL-3.0-only](LICENSE)",
            "GPL-3.0-only\n\n<!-- [GPL-3.0-only](LICENSE) -->",
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")
        self.assert_violation("README.md: missing GPL-3.0-only license link")

    def test_readme_requires_honest_prospective_website_label(self) -> None:
        readme = valid_readme().replace(
            README_WEBSITE_LINK,
            f"[Website]({PUBLIC_WEBSITE_URL})",
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")
        self.assert_violation(
            "README.md: website must use the prospective canonical Website label"
        )

    def test_readme_comment_decoy_cannot_hide_stale_visible_wording(self) -> None:
        readme = valid_readme().replace(
            README_WEBSITE_LINK,
            f"<!-- {README_WEBSITE_LINK} -->",
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")
        result = self.run_checker()
        self.assertIn(
            "README.md: website must use the prospective canonical Website label",
            result.stdout,
        )
        self.assertIn(
            f"README.md: missing required navigation link: {PUBLIC_WEBSITE_URL}",
            result.stdout,
        )

    def test_readme_inline_code_decoy_cannot_hide_stale_visible_wording(self) -> None:
        readme = valid_readme().replace(
            README_WEBSITE_LINK,
            f"``{README_WEBSITE_LINK}``",
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")
        result = self.run_checker()
        self.assertIn(
            "README.md: website must use the prospective canonical Website label",
            result.stdout,
        )
        self.assertIn(
            f"README.md: missing required navigation link: {PUBLIC_WEBSITE_URL}",
            result.stdout,
        )

    def test_readme_escaped_link_decoy_is_not_a_visible_link(self) -> None:
        readme = valid_readme().replace(
            README_WEBSITE_LINK,
            f"\\{README_WEBSITE_LINK}",
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")
        result = self.run_checker()
        self.assertIn(
            "README.md: website must use the prospective canonical Website label",
            result.stdout,
        )
        self.assertIn(
            f"README.md: missing required navigation link: {PUBLIC_WEBSITE_URL}",
            result.stdout,
        )

    def test_readme_requires_post_merge_pages_follow_up(self) -> None:
        readme = valid_readme().replace(
            "enabling GitHub Pages for GitHub Actions is a post-merge follow-up",
            "the documentation is published",
        )
        (self.root / "README.md").write_text(readme, encoding="utf-8")
        self.assert_violation(
            "README.md: missing post-merge GitHub Pages activation follow-up"
        )

    def test_getting_started_requires_canonical_website_link(self) -> None:
        overview = self.root / "docs/getting-started/overview.md"
        overview.write_text(
            page("Getting started", "Read the local language guide.\n"),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/getting-started/overview.md: missing visible prospective website link"
        )

    def test_getting_started_fence_only_link_is_not_visible(self) -> None:
        overview = self.root / "docs/getting-started/overview.md"
        overview.write_text(
            page(
                "Getting started",
                f"```text\n{GETTING_STARTED_WEBSITE_LINK}\n```\n\n"
                "Enabling GitHub Pages for GitHub Actions is a post-merge follow-up.\n",
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/getting-started/overview.md: missing visible prospective website link"
        )

    def test_getting_started_inline_code_decoy_is_not_visible(self) -> None:
        overview = self.root / "docs/getting-started/overview.md"
        overview.write_text(
            page(
                "Getting started",
                f"The future address is ```{GETTING_STARTED_WEBSITE_LINK}```.\n\n"
                "Enabling GitHub Pages for GitHub Actions is a post-merge follow-up.\n",
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/getting-started/overview.md: missing visible prospective website link"
        )

    def test_getting_started_escaped_link_decoy_is_not_visible(self) -> None:
        overview = self.root / "docs/getting-started/overview.md"
        overview.write_text(
            page(
                "Getting started",
                f"\\{GETTING_STARTED_WEBSITE_LINK}\n\n"
                "Enabling GitHub Pages for GitHub Actions is a post-merge follow-up.\n",
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/getting-started/overview.md: missing visible prospective website link"
        )

    def test_visible_links_remain_valid_beside_inline_code_decoys(self) -> None:
        readme = valid_readme(extra=f"`[Not the website]({PUBLIC_WEBSITE_URL})`")
        (self.root / "README.md").write_text(readme, encoding="utf-8")
        overview = self.root / "docs/getting-started/overview.md"
        overview.write_text(
            page(
                "Getting started",
                f"The {GETTING_STARTED_WEBSITE_LINK} will publish these guides.\n\n"
                f"Ignore ``[Example]({PUBLIC_WEBSITE_URL})``.\n\n"
                "Enabling GitHub Pages for GitHub Actions is a post-merge follow-up.\n",
            ),
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_getting_started_requires_post_merge_pages_follow_up(self) -> None:
        overview = self.root / "docs/getting-started/overview.md"
        overview.write_text(
            page(
                "Getting started",
                f"The {GETTING_STARTED_WEBSITE_LINK} will publish these guides.\n",
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/getting-started/overview.md: missing post-merge GitHub Pages activation follow-up"
        )

    def test_getting_started_accepts_wrapped_post_merge_pages_follow_up(self) -> None:
        overview = self.root / "docs/getting-started/overview.md"
        overview.write_text(
            page(
                "Getting started",
                f"The {GETTING_STARTED_WEBSITE_LINK} will publish these guides.\n\n"
                "Enabling GitHub\nPages for GitHub Actions is a post-merge follow-up.\n",
            ),
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_readme_rejects_legacy_and_internal_terms(self) -> None:
        (self.root / "README.md").write_text(
            valid_readme(extra="See jazz2 and .codex/plans for Spec Authority."),
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertIn("README.md: banned front-door term: jazz2", result.stdout)
        self.assertIn("README.md: banned front-door term: .codex/", result.stdout)
        self.assertIn("README.md: banned front-door term: Spec Authority", result.stdout)

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

    def test_rejects_angle_bracket_links_with_parentheses_that_escape_docs(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(body="[Decision](<../rfcs/accepted/draft(1.md>)\n"),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: public link escapes docs into rfcs/: "
            "../rfcs/accepted/draft(1.md"
        )

    def test_comment_literal_inside_fence_cannot_hide_later_links(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "```text\n"
                    "<!-- an example literal\n"
                    "```\n\n"
                    "[Decision](../rfcs/accepted/0001.md)\n"
                )
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: public link escapes docs into rfcs/: "
            "../rfcs/accepted/0001.md"
        )

    def test_rejects_escaping_images_nested_inside_link_labels(self) -> None:
        (self.root / "docs/index.md").write_text(
            page(
                body=(
                    "[![Private diagram](../rfcs/private.png)]"
                    "(language/overview.md)\n"
                )
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "docs/index.md: public link escapes docs into rfcs/: ../rfcs/private.png"
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
            page(body=self.executable_example("examples/hello.jz", source)),
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_readme_executable_fence_can_cover_a_tracked_example(self) -> None:
        source = '"Hello".\n'
        self.add_tracked_example("examples/hello.jz", source)
        (self.root / "README.md").write_text(
            valid_readme(extra=self.executable_example("examples/hello.jz", source)),
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

    def test_dead_case_table_cannot_satisfy_execution_coverage(self) -> None:
        source = '"Hello".\n'
        self.add_tracked_example("examples/hello.jz", source)
        (self.root / "docs/index.md").write_text(
            page(body=self.executable_example("examples/hello.jz", source)),
            encoding="utf-8",
        )
        self.write_example_runner(consume_cases=False)
        self.assert_violation(
            "scripts/check-examples.sh: does not execute scripts/example-cases.tsv"
        )

    def test_case_table_reader_without_runner_call_is_not_execution_coverage(self) -> None:
        source = '"Hello".\n'
        self.add_tracked_example("examples/hello.jz", source)
        (self.root / "docs/index.md").write_text(
            page(body=self.executable_example("examples/hello.jz", source)),
            encoding="utf-8",
        )
        (self.root / "scripts/check-examples.sh").write_text(
            (
                "#!/usr/bin/env bash\n\n"
                "while IFS=$'\\t' read -r case_name case_sources "
                "case_expected case_args_text; do\n"
                "  [[ \"$case_name\" == \"name\" ]] && continue\n"
                "  printf '%s\\n' \"$case_name\"\n"
                "done < scripts/example-cases.tsv\n"
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "scripts/check-examples.sh: does not execute scripts/example-cases.tsv"
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
        self.add_example_case([main_path, greeting_path])
        body = self.executable_example(
            greeting_path, greeting
        ) + self.executable_example(
            main_path, main
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
