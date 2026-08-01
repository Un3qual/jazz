#!/usr/bin/env python3
"""Focused contract tests for the Jazz website publication boundary."""

from __future__ import annotations

import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


CHECKER = Path(__file__).with_name("check-website-boundary.py")

VALID_CONFIG = """\
import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

const config: Config = {
  title: 'Jazz',
  url: 'https://un3qual.github.io',
  baseUrl: '/jazz/',
  onBrokenLinks: 'throw',
  markdown: {
    format: 'md',
    hooks: {onBrokenMarkdownLinks: 'throw'},
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
      } satisfies Preset.Options,
    ],
  ],
  themeConfig: {
    navbar: {
      items: [{href: 'https://github.com/un3qual/jazz', label: 'GitHub'}],
    },
  } satisfies Preset.ThemeConfig,
};

export default config;
"""


class WebsiteBoundaryTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        (self.root / "docs/guide").mkdir(parents=True)
        (self.root / "docs/index.md").write_text("# Public docs\n", encoding="utf-8")
        (self.root / "docs/guide/page.md").write_text("# Guide\n", encoding="utf-8")
        (self.root / "website/src/pages").mkdir(parents=True)
        (self.root / "website/src/css").mkdir(parents=True)
        (self.root / "website/scripts").mkdir(parents=True)
        (self.root / "website/static/img").mkdir(parents=True)
        (self.root / "website/static/img/mark.svg").write_text(
            '<svg xmlns="http://www.w3.org/2000/svg"></svg>\n', encoding="utf-8"
        )
        self.config = self.root / "website/docusaurus.config.ts"
        self.config.write_text(VALID_CONFIG, encoding="utf-8")
        (self.root / "website/sidebars.ts").write_text(
            "export default {jazzSidebar: ['index']};\n", encoding="utf-8"
        )
        (self.root / "website/src/pages/index.tsx").write_text(
            """\
import mark from '@site/static/img/mark.svg';
export default function Home(props: object) {
  return <img {...props} src={mark} alt="" />;
}
""",
            encoding="utf-8",
        )
        (self.root / "website/src/css/custom.css").write_text(
            ".mark { background: url('/img/mark.svg'); }\n", encoding="utf-8"
        )

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def run_checker(self) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [sys.executable, str(CHECKER), str(self.root)],
            check=False,
            capture_output=True,
            text=True,
        )

    def replace_config(self, old: str, new: str) -> None:
        self.config.write_text(
            self.config.read_text(encoding="utf-8").replace(old, new),
            encoding="utf-8",
        )

    def assert_violation(self, message: str) -> None:
        result = self.run_checker()
        self.assertNotEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertIn(message, result.stdout)
        self.assertEqual("", result.stderr)

    def test_valid_trusted_source_fixture_passes(self) -> None:
        """Local expressions and spreads are ordinary trusted application code."""
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertEqual("Website boundary checks passed.\n", result.stdout)
        self.assertEqual("", result.stderr)

    def test_config_uses_canonical_static_export_shape(self) -> None:
        for old, new in (
            ("const config: Config = {", "const config = {"),
            ("export default config;", "export default {...config};"),
        ):
            with self.subTest(new=new):
                self.config.write_text(VALID_CONFIG.replace(old, new), encoding="utf-8")
                self.assert_violation(
                    "website/docusaurus.config.ts: use canonical `const config: Config` and `export default config` shape"
                )

    def test_config_fixes_production_origin_and_base_route(self) -> None:
        for old, new, message in (
            (
                "url: 'https://un3qual.github.io'",
                "url: 'https://example.invalid'",
                "production url must be exactly https://un3qual.github.io",
            ),
            (
                "baseUrl: '/jazz/'",
                "baseUrl: '/'",
                "production baseUrl must be exactly /jazz/",
            ),
        ):
            with self.subTest(new=new):
                self.config.write_text(VALID_CONFIG.replace(old, new), encoding="utf-8")
                self.assert_violation(f"website/docusaurus.config.ts: {message}")

    def test_config_publishes_only_plain_root_docs_without_a_blog(self) -> None:
        cases = (
            (
                "path: '../docs'",
                "path: '../rfcs'",
                "docs path must be exactly ../docs",
            ),
            (
                "routeBasePath: 'docs'",
                "routeBasePath: 'guide'",
                "docs routeBasePath must be exactly docs",
            ),
            ("blog: false", "blog: {}", "blog must be disabled"),
            ("format: 'md'", "format: 'mdx'", "markdown format must be exactly md"),
        )
        for old, new, message in cases:
            with self.subTest(new=new):
                self.config.write_text(VALID_CONFIG.replace(old, new), encoding="utf-8")
                self.assert_violation(f"website/docusaurus.config.ts: {message}")

    def test_config_turns_broken_links_into_build_errors(self) -> None:
        cases = (
            ("onBrokenLinks: 'throw'", "onBrokenLinks: 'warn'", "broken links must throw"),
            (
                "onBrokenMarkdownLinks: 'throw'",
                "onBrokenMarkdownLinks: 'warn'",
                "broken Markdown links must throw",
            ),
        )
        for old, new, message in cases:
            with self.subTest(new=new):
                self.config.write_text(VALID_CONFIG.replace(old, new), encoding="utf-8")
                self.assert_violation(f"website/docusaurus.config.ts: {message}")

    def test_public_docs_reject_every_non_markdown_regular_file(self) -> None:
        for name in (
            "interactive.mdx",
            "redirect.html",
            "metadata.json",
            "notes.txt",
            "diagram.svg",
            "photo.png",
            "README",
        ):
            with self.subTest(name=name):
                path = self.root / "docs/guide" / name
                path.write_bytes(b"fixture\n")
                try:
                    self.assert_violation(
                        f"docs/guide/{name}: public docs regular files must use the .md "
                        "extension; move site assets to website/static"
                    )
                finally:
                    path.unlink()

    def test_public_doc_symlinks_must_stay_within_docs(self) -> None:
        outside = self.root / "private.md"
        outside.write_text("secret\n", encoding="utf-8")
        (self.root / "docs/escape.md").symlink_to(outside)
        self.assert_violation("docs/escape.md: public documentation symlink escapes docs")

    def test_public_docs_allow_directories_markdown_and_contained_symlinks(self) -> None:
        (self.root / "docs/nested/deeper").mkdir(parents=True)
        (self.root / "docs/nested/deeper/page.md").write_text(
            "# Nested page\n", encoding="utf-8"
        )
        (self.root / "docs/guide-link.md").symlink_to(self.root / "docs/guide/page.md")
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_authored_sources_forbid_internal_and_legacy_references(self) -> None:
        page = self.root / "website/src/pages/index.tsx"
        for reference in (".codex", "rfcs", "docs/execution", "docs/superpowers", "jazz-hs", "jazz-next", "jazz2", "JazzNext"):
            with self.subTest(reference=reference):
                page.write_text(f"export const value = '{reference}';\n", encoding="utf-8")
                self.assert_violation(
                    f"website/src/pages/index.tsx: forbidden publication reference: {reference.casefold()}"
                )

    def test_authored_sources_reject_obvious_remote_references(self) -> None:
        files = {
            "website/src/pages/index.tsx": "const image = 'https://images.example/mark.svg';\n",
            "website/src/css/custom.css": "@import url('//fonts.example/jazz.css');\n",
            "website/scripts/load.mjs": "fetch('http://api.example/data');\n",
        }
        for name, source in files.items():
            (self.root / name).write_text(source, encoding="utf-8")
        result = self.run_checker()
        for name in files:
            self.assertIn(f"{name}: remote authored reference is not allowlisted", result.stdout)

    def test_authored_sources_allow_only_named_navigation_urls(self) -> None:
        allowed = (
            "https://un3qual.github.io",
            "https://un3qual.github.io/jazz/",
            "https://github.com/un3qual/jazz",
            "https://github.com/un3qual/jazz/issues",
            "https://github.com/un3qual/jazz/security/policy",
            "https://github.com/un3qual/jazz/blob/main/LICENSE",
        )
        (self.root / "website/src/pages/index.tsx").write_text(
            "\n".join(f"export const link{index} = '{url}';" for index, url in enumerate(allowed)),
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_near_allowlist_remote_url_is_rejected(self) -> None:
        for url in (
            "https://github.com/un3qual/jazzish",
            "https://github.com/un3qual/jazz.",
        ):
            with self.subTest(url=url):
                (self.root / "website/src/pages/index.tsx").write_text(
                    f"export const link = '{url}';\n", encoding="utf-8"
                )
                self.assert_violation(
                    "website/src/pages/index.tsx: remote authored reference is not allowlisted"
                )

    def test_generated_output_forbids_internal_and_legacy_strings(self) -> None:
        build = self.root / "website/build"
        build.mkdir()
        (build / "index.html").write_text("<p>.codex jazz-next</p>\n", encoding="utf-8")
        result = self.run_checker()
        self.assertIn(
            "website/build/index.html: generated output contains forbidden string: .codex",
            result.stdout,
        )
        self.assertIn(
            "website/build/index.html: generated output contains forbidden string: jazz-next",
            result.stdout,
        )

    def test_generated_html_rejects_remote_resource_attributes(self) -> None:
        build = self.root / "website/build"
        build.mkdir()
        (build / "index.html").write_text(
            '<img src="https://images.example/mark.svg"><link rel="stylesheet" href="//cdn.example/site.css">',
            encoding="utf-8",
        )
        self.assert_violation(
            "website/build/index.html: generated output loads a remote resource"
        )

    def test_generated_html_rejects_remote_srcset_candidates(self) -> None:
        build = self.root / "website/build"
        build.mkdir()
        (build / "index.html").write_text(
            '<img src="/jazz/img/mark.svg" srcset="/jazz/img/mark.svg 1x, https://images.example/mark@2x.svg 2x">',
            encoding="utf-8",
        )
        self.assert_violation(
            "website/build/index.html: generated output loads a remote resource"
        )

    def test_generated_html_allows_docusaurus_metadata_navigation_and_local_assets(self) -> None:
        build = self.root / "website/build"
        build.mkdir()
        (build / "index.html").write_text(
            """\
<link rel="canonical" href="https://un3qual.github.io/jazz/">
<a href="https://github.com/un3qual/jazz">GitHub</a>
<link rel="stylesheet" href="/jazz/assets/styles.css">
<img src="data:image/svg+xml;base64,PHN2Zy8+" alt="">
""",
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_generated_css_rejects_remote_url_and_import(self) -> None:
        build = self.root / "website/build"
        build.mkdir()
        (build / "styles.css").write_text(
            "@import 'https://fonts.example/jazz.css';\n.a{background:url(//cdn.example/a.png)}\n",
            encoding="utf-8",
        )
        self.assert_violation(
            "website/build/styles.css: generated output loads a remote resource"
        )

    def test_generated_css_allows_local_and_embedded_resources(self) -> None:
        build = self.root / "website/build"
        build.mkdir()
        (build / "styles.css").write_text(
            ".a{background:url('/jazz/img/a.svg')} .b{src:url(data:font/woff2;base64,AA==)}\n",
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_violations_are_deduplicated_sorted_and_printed_on_stdout(self) -> None:
        self.replace_config("blog: false", "blog: true")
        self.replace_config("baseUrl: '/jazz/'", "baseUrl: '/wrong/'")
        result = self.run_checker()
        lines = result.stdout.splitlines()
        self.assertGreaterEqual(len(lines), 3)
        self.assertEqual(sorted(set(lines[1:])), lines[1:])
        self.assertEqual("", result.stderr)


if __name__ == "__main__":
    unittest.main()
