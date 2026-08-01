#!/usr/bin/env python3
"""Fixture tests for the Jazz website publication boundary."""

from __future__ import annotations

import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


CHECKER_PATH = Path(__file__).with_name("check-website-boundary.py")

VALID_CONFIG = """\
import type {Config} from '@docusaurus/types';

const config: Config = {
  title: 'Jazz',
  url: 'https://un3qual.github.io',
  baseUrl: '/jazz/',
  onBrokenLinks: 'throw',
  markdown: {
    hooks: {
      onBrokenMarkdownLinks: 'throw',
    },
  },
  presets: [
    [
      'classic',
      {
        docs: {
          path: '../docs',
          routeBasePath: 'docs',
          sidebarPath: './sidebars.ts',
        },
        blog: false,
      },
    ],
  ],
};

export default config;
"""


class WebsiteBoundaryCheckerTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temp_dir = tempfile.TemporaryDirectory()
        self.root = Path(self.temp_dir.name)
        (self.root / "docs").mkdir()
        (self.root / "docs/index.md").write_text("# Public docs\n", encoding="utf-8")
        (self.root / "website/src/pages").mkdir(parents=True)
        (self.root / "website/src/css").mkdir(parents=True)
        (self.root / "website/static/img").mkdir(parents=True)
        (self.root / "website/docusaurus.config.ts").write_text(
            VALID_CONFIG,
            encoding="utf-8",
        )
        (self.root / "website/src/pages/index.tsx").write_text(
            """\
import Link from '@docusaurus/Link';
export default function Home() {
  return <Link to=\"https://github.com/un3qual/jazz\">GitHub</Link>;
}
""",
            encoding="utf-8",
        )
        (self.root / "website/src/css/custom.css").write_text(
            ".mark { background-image: url('/img/mark.svg'); }\n",
            encoding="utf-8",
        )
        (self.root / "website/static/img/mark.svg").write_text(
            '<svg xmlns="http://www.w3.org/2000/svg"></svg>\n',
            encoding="utf-8",
        )

    def tearDown(self) -> None:
        self.temp_dir.cleanup()

    def run_checker(self) -> subprocess.CompletedProcess[str]:
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

    def replace_config(self, old: str, new: str) -> None:
        path = self.root / "website/docusaurus.config.ts"
        path.write_text(path.read_text(encoding="utf-8").replace(old, new), encoding="utf-8")

    def test_valid_fixture_passes(self) -> None:
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertEqual("Website boundary checks passed.\n", result.stdout)
        self.assertEqual("", result.stderr)

    def test_docs_source_must_be_exactly_public_root_docs(self) -> None:
        self.replace_config("path: '../docs'", "path: '../rfcs'")
        self.assert_violation(
            "website/docusaurus.config.ts: classic preset docs path must be exactly ../docs"
        )

    def test_blog_must_be_disabled(self) -> None:
        self.replace_config("blog: false", "blog: {}")
        self.assert_violation(
            "website/docusaurus.config.ts: classic preset blog must be disabled"
        )

    def test_decoy_options_cannot_mask_the_real_classic_preset(self) -> None:
        config_path = self.root / "website/docusaurus.config.ts"
        config = config_path.read_text(encoding="utf-8")
        config = config.replace(
            "const config: Config = {",
            "const decoy = {path: '../docs', blog: false};\n\nconst config: Config = {",
        ).replace(
            """\
        docs: {
          path: '../docs',
          routeBasePath: 'docs',
          sidebarPath: './sidebars.ts',
        },
        blog: false,
""",
            "        docs: false,\n        blog: true,\n",
        )
        config_path.write_text(config, encoding="utf-8")

        result = self.run_checker()
        self.assertNotEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertIn(
            "website/docusaurus.config.ts: classic preset docs path must be exactly ../docs",
            result.stdout,
        )
        self.assertIn(
            "website/docusaurus.config.ts: classic preset blog must be disabled",
            result.stdout,
        )

    def test_checked_config_must_be_the_default_export(self) -> None:
        config_path = self.root / "website/docusaurus.config.ts"
        config = config_path.read_text(encoding="utf-8").replace(
            "export default config;",
            """\
const actual: Config = {
  title: 'Decoy target',
  url: 'https://example.invalid',
  baseUrl: '/',
  onBrokenLinks: 'warn',
  markdown: {hooks: {onBrokenMarkdownLinks: 'warn'}},
  presets: [['classic', {docs: false, blog: true}]],
};

export default actual;
""",
        )
        config_path.write_text(config, encoding="utf-8")

        result = self.run_checker()
        self.assertNotEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertIn(
            "website/docusaurus.config.ts: production URL must be https://un3qual.github.io",
            result.stdout,
        )
        self.assertIn(
            "website/docusaurus.config.ts: classic preset docs path must be exactly ../docs",
            result.stdout,
        )
        self.assertIn(
            "website/docusaurus.config.ts: classic preset blog must be disabled",
            result.stdout,
        )

    def test_boundary_objects_reject_spreads_and_computed_overrides(self) -> None:
        mutations = (
            (
                "config spread",
                "const config: Config = {",
                "const config: Config = {\n  ...override,",
                "config object must not contain spreads or computed properties",
            ),
            (
                "config computed",
                "const config: Config = {",
                "const config: Config = {\n  [overrideKey]: override,",
                "config object must not contain spreads or computed properties",
            ),
            (
                "presets spread",
                "  presets: [\n    [",
                "  presets: [\n    ...override,\n    [",
                "presets array must not contain spreads",
            ),
            (
                "classic spread",
                "      {\n        docs:",
                "      {\n        ...override,\n        docs:",
                "classic preset options must not contain spreads or computed properties",
            ),
            (
                "classic computed",
                "      {\n        docs:",
                "      {\n        [overrideKey]: override,\n        docs:",
                "classic preset options must not contain spreads or computed properties",
            ),
            (
                "docs spread",
                "        docs: {\n          path:",
                "        docs: {\n          ...override,\n          path:",
                "classic preset docs options must not contain spreads or computed properties",
            ),
            (
                "docs computed",
                "        docs: {\n          path:",
                "        docs: {\n          [overrideKey]: override,\n          path:",
                "classic preset docs options must not contain spreads or computed properties",
            ),
            (
                "markdown spread",
                "  markdown: {\n    hooks:",
                "  markdown: {\n    ...override,\n    hooks:",
                "markdown options must not contain spreads or computed properties",
            ),
            (
                "markdown computed",
                "  markdown: {\n    hooks:",
                "  markdown: {\n    [overrideKey]: override,\n    hooks:",
                "markdown options must not contain spreads or computed properties",
            ),
            (
                "hooks spread",
                "    hooks: {\n      onBrokenMarkdownLinks:",
                "    hooks: {\n      ...override,\n      onBrokenMarkdownLinks:",
                "markdown hooks must not contain spreads or computed properties",
            ),
            (
                "hooks computed",
                "    hooks: {\n      onBrokenMarkdownLinks:",
                "    hooks: {\n      [overrideKey]: override,\n      onBrokenMarkdownLinks:",
                "markdown hooks must not contain spreads or computed properties",
            ),
        )
        config_path = self.root / "website/docusaurus.config.ts"
        for label, old, new, expected in mutations:
            with self.subTest(label=label):
                config_path.write_text(VALID_CONFIG.replace(old, new), encoding="utf-8")
                self.assert_violation(f"website/docusaurus.config.ts: {expected}")

    def test_broken_links_must_fail_the_build(self) -> None:
        self.replace_config("onBrokenLinks: 'throw'", "onBrokenLinks: 'warn'")
        self.replace_config(
            "onBrokenMarkdownLinks: 'throw'",
            "onBrokenMarkdownLinks: 'warn'",
        )
        result = self.run_checker()
        self.assertIn(
            "website/docusaurus.config.ts: broken links must throw",
            result.stdout,
        )
        self.assertIn(
            "website/docusaurus.config.ts: broken Markdown links must throw through markdown hooks",
            result.stdout,
        )

    def test_deprecated_top_level_markdown_link_policy_is_rejected(self) -> None:
        self.replace_config(
            """\
  markdown: {
    hooks: {
      onBrokenMarkdownLinks: 'throw',
    },
  },
""",
            "  onBrokenMarkdownLinks: 'throw',\n",
        )
        self.assert_violation(
            "website/docusaurus.config.ts: broken Markdown links must throw through markdown hooks"
        )

    def test_production_origin_and_base_path_are_fixed(self) -> None:
        self.replace_config(
            "url: 'https://un3qual.github.io'",
            "url: 'https://example.invalid'",
        )
        self.replace_config("baseUrl: '/jazz/'", "baseUrl: '/'")
        result = self.run_checker()
        self.assertIn(
            "website/docusaurus.config.ts: production URL must be https://un3qual.github.io",
            result.stdout,
        )
        self.assertIn(
            "website/docusaurus.config.ts: base URL must be /jazz/",
            result.stdout,
        )

    def test_authored_sources_reject_internal_references(self) -> None:
        (self.root / "website/src/pages/index.tsx").write_text(
            "import queue from '../../../.codex/execution/queue.md';\n",
            encoding="utf-8",
        )
        self.assert_violation(
            "website/src/pages/index.tsx: forbidden publication source reference: .codex"
        )

    def test_static_tree_rejects_internal_copy_sources(self) -> None:
        (self.root / "website/static/internal.txt").write_text(
            "Copied from docs/execution/queue.md.\n",
            encoding="utf-8",
        )
        self.assert_violation(
            "website/static/internal.txt: forbidden publication source reference: docs/execution"
        )

    def test_symlinks_cannot_escape_the_public_website_boundary(self) -> None:
        internal = self.root / ".codex/execution"
        internal.mkdir(parents=True)
        (internal / "queue.md").write_text("internal\n", encoding="utf-8")
        os.symlink(
            internal / "queue.md",
            self.root / "website/static/internal.md",
        )
        self.assert_violation(
            "website/static/internal.md: symlink is not allowed in website sources"
        )

    def test_generated_output_rejects_internal_and_legacy_identity_strings(self) -> None:
        build = self.root / "website/build"
        build.mkdir()
        (build / "index.html").write_text(
            "Published from rfcs with jazz-next.\n",
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertIn(
            "website/build/index.html: generated output contains forbidden string: rfcs",
            result.stdout,
        )
        self.assertIn(
            "website/build/index.html: generated output contains forbidden string: jazz-next",
            result.stdout,
        )

    def test_authored_assets_must_be_local(self) -> None:
        (self.root / "website/src/css/custom.css").write_text(
            "@import url('https://fonts.example.invalid/font.css');\n",
            encoding="utf-8",
        )
        (self.root / "website/src/pages/index.tsx").write_text(
            '<img src="https://images.example.invalid/mark.svg" alt="" />;\n',
            encoding="utf-8",
        )
        (self.root / "website/src/remote.md").write_text(
            "![Remote](https://images.example.invalid/photo.png)\n",
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertIn(
            "website/src/css/custom.css: remote authored URL is not allowed",
            result.stdout,
        )
        self.assertIn(
            "website/src/pages/index.tsx: remote authored URL is not allowed",
            result.stdout,
        )
        self.assertIn(
            "website/src/remote.md: remote authored URL is not allowed",
            result.stdout,
        )

    def test_github_navigation_requires_the_exact_repository_boundary(self) -> None:
        for url in (
            "https://github.com/un3qual/jazz-lookalike",
            "https://github.com.evil.example/un3qual/jazz",
            "https://github.com/un3qual/jazz/%2e%2e/private",
        ):
            with self.subTest(url=url):
                (self.root / "website/src/pages/index.tsx").write_text(
                    f"""\
import Link from '@docusaurus/Link';
export default function Home() {{
  return <Link to=\"{url}\">GitHub</Link>;
}}
""",
                    encoding="utf-8",
                )
                self.assert_violation(
                    "website/src/pages/index.tsx: remote authored URL is not allowed"
                )

    def test_github_repository_urls_are_rejected_in_asset_and_import_contexts(self) -> None:
        (self.root / "website/src/css/custom.css").write_text(
            "background: url('https://github.com/un3qual/jazz/blob/main/mark.svg');\n",
            encoding="utf-8",
        )
        (self.root / "website/src/pages/index.tsx").write_text(
            """\
import mark from 'https://github.com/un3qual/jazz/blob/main/mark.svg';
export default function Home() {
  return <img src=\"https://github.com/un3qual/jazz/blob/main/mark.svg\" alt=\"\" />;
}
""",
            encoding="utf-8",
        )
        (self.root / "website/src/remote.md").write_text(
            "![Remote](https://github.com/un3qual/jazz/blob/main/photo.png)\n",
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertIn(
            "website/src/css/custom.css: remote authored URL is not allowed",
            result.stdout,
        )
        self.assertIn(
            "website/src/pages/index.tsx: remote authored URL is not allowed",
            result.stdout,
        )
        self.assertIn(
            "website/src/remote.md: remote authored URL is not allowed",
            result.stdout,
        )

    def test_exact_github_repository_navigation_is_allowed(self) -> None:
        (self.root / "website/src/pages/index.tsx").write_text(
            """\
import Link from '@docusaurus/Link';
export default function Home() {
  return (
    <>
      <Link to=\"https://github.com/un3qual/jazz\">Repository</Link>
      <a href=\"https://github.com/un3qual/jazz/issues\">Issues</a>
    </>
  );
}
""",
            encoding="utf-8",
        )
        (self.root / "website/src/repository.md").write_text(
            "[Repository](https://github.com/un3qual/jazz)\n",
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_lowercase_link_resource_is_not_github_navigation(self) -> None:
        (self.root / "website/src/pages/index.tsx").write_text(
            """\
export default function Resource() {
  return (
    <link
      rel=\"stylesheet\"
      href=\"https://github.com/un3qual/jazz/blob/main/remote.css\"
    />
  );
}
""",
            encoding="utf-8",
        )
        self.assert_violation(
            "website/src/pages/index.tsx: remote authored URL is not allowed"
        )

    def test_protocol_relative_assets_are_rejected(self) -> None:
        (self.root / "website/src/css/custom.css").write_text(
            "background-image: url('//cdn.example.invalid/mark.svg');\n",
            encoding="utf-8",
        )
        (self.root / "website/src/pages/index.tsx").write_text(
            '<img src="//cdn.example.invalid/mark.svg" alt="" />;\n',
            encoding="utf-8",
        )
        (self.root / "website/src/remote.md").write_text(
            "![Remote](//cdn.example.invalid/photo.png)\n",
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertIn(
            "website/src/css/custom.css: remote authored URL is not allowed",
            result.stdout,
        )
        self.assertIn(
            "website/src/pages/index.tsx: remote authored URL is not allowed",
            result.stdout,
        )
        self.assertIn(
            "website/src/remote.md: remote authored URL is not allowed",
            result.stdout,
        )

    def test_violations_are_sorted_and_reported_on_stdout(self) -> None:
        self.replace_config("blog: false", "blog: true")
        self.replace_config("baseUrl: '/jazz/'", "baseUrl: '/wrong/'")
        result = self.run_checker()
        lines = result.stdout.splitlines()
        self.assertGreaterEqual(len(lines), 3)
        self.assertEqual(sorted(lines[1:]), lines[1:])
        self.assertEqual("", result.stderr)


if __name__ == "__main__":
    unittest.main()
