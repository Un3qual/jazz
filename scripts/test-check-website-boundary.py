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
    format: 'md',
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
            "const config: Config = {\n  decoy: {path: '../docs', blog: false},",
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
            "website/docusaurus.config.ts: default export must resolve unambiguously to a literal Config object",
            result.stdout,
        )

    def test_exported_config_rejects_executable_mutation_statements(self) -> None:
        mutations = (
            (
                "assignment before export",
                "config.url = 'https://example.invalid';\nexport default config;",
            ),
            (
                "object mutation before export",
                "Object.assign(config, {baseUrl: '/'});\nexport default config;",
            ),
            (
                "executable statement before export",
                "console.log(config);\nexport default config;",
            ),
            (
                "assignment after export",
                "export default config;\nconfig.url = 'https://example.invalid';",
            ),
        )
        config_path = self.root / "website/docusaurus.config.ts"
        for label, replacement in mutations:
            with self.subTest(label=label):
                config_path.write_text(
                    VALID_CONFIG.replace("export default config;", replacement),
                    encoding="utf-8",
                )
                self.assert_violation(
                    "website/docusaurus.config.ts: default export must resolve unambiguously to a literal Config object"
                )

    def test_dynamic_import_is_not_a_canonical_static_import(self) -> None:
        config_path = self.root / "website/docusaurus.config.ts"
        config_path.write_text(
            VALID_CONFIG.replace(
                "const config: Config = {",
                "import('https://example.invalid/mutate.js');\n\nconst config: Config = {",
            ),
            encoding="utf-8",
        )
        self.assert_violation(
            "website/docusaurus.config.ts: default export must resolve unambiguously to a literal Config object"
        )

    def test_config_static_imports_share_the_authored_scheme_policy(self) -> None:
        config_path = self.root / "website/docusaurus.config.ts"
        config_path.write_text(
            "import remote from 'd&#97;ta:text/javascript,remote';\n" + VALID_CONFIG,
            encoding="utf-8",
        )
        self.assert_violation(
            "website/docusaurus.config.ts: remote authored URL is not allowed"
        )

    def test_config_non_navigation_targets_share_the_authored_scheme_policy(self) -> None:
        self.replace_config(
            "  title: 'Jazz',",
            "  title: 'Jazz',\n  favicon: `d&#97;ta:image/svg+xml,remote`,",
        )
        self.assert_violation(
            "website/docusaurus.config.ts: remote authored URL is not allowed"
        )

    def test_config_site_alias_authorization_is_bound_to_the_import_occurrence(self) -> None:
        config_path = self.root / "website/docusaurus.config.ts"
        config = VALID_CONFIG.replace(
            "import type {Config} from '@docusaurus/types';",
            """\
import type {Config} from '@docusaurus/types';
import mark from '@site/static/img/mark.svg';
""",
        ).replace(
            "  title: 'Jazz',",
            "  title: 'Jazz',\n  favicon: '@site/static/img/mark.svg',",
        )
        config_path.write_text(config, encoding="utf-8")
        self.assert_violation(
            "website/docusaurus.config.ts: @site is authorized only for site source imports"
        )

    def test_config_resource_properties_require_direct_static_literals(self) -> None:
        replacements = (
            "favicon: icon",
            "favicon: 'img/mark.svg' + suffix",
            "image: `img/${theme}.svg`",
            "href: repositoryUrl",
            "content: socialCard",
        )
        for replacement in replacements:
            with self.subTest(replacement=replacement):
                config = VALID_CONFIG.replace(
                    "  title: 'Jazz',",
                    f"  title: 'Jazz',\n  {replacement},",
                )
                (self.root / "website/docusaurus.config.ts").write_text(
                    config,
                    encoding="utf-8",
                )
                self.assert_violation(
                    "website/docusaurus.config.ts: config resource properties must use direct static literals"
                )

    def test_config_resource_spreads_are_forbidden(self) -> None:
        config = VALID_CONFIG.replace(
            "  title: 'Jazz',",
            "  title: 'Jazz',\n  metadata: {...remoteMetadata},",
        )
        (self.root / "website/docusaurus.config.ts").write_text(config, encoding="utf-8")
        self.assert_violation(
            "website/docusaurus.config.ts: config resource spreads are forbidden"
        )

    def test_static_local_config_resources_and_navigation_remain_allowed(self) -> None:
        config = VALID_CONFIG.replace(
            "  title: 'Jazz',",
            """\
  title: 'Jazz',
  favicon: 'img/mark.svg',
  image: "img/mark.svg",
  content: 'img/mark.svg',
  href: 'https://github.com/un3qual/jazz',
""",
        )
        (self.root / "website/docusaurus.config.ts").write_text(config, encoding="utf-8")
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

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
                "  markdown: {\n    format:",
                "  markdown: {\n    ...override,\n    format:",
                "markdown options must not contain spreads or computed properties",
            ),
            (
                "markdown computed",
                "  markdown: {\n    format:",
                "  markdown: {\n    [overrideKey]: override,\n    format:",
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
    format: 'md',
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

    def test_markdown_format_must_be_exactly_plain_md(self) -> None:
        for replacement in ("format: 'mdx'", "format: 'detect'", ""):
            with self.subTest(replacement=replacement):
                config = VALID_CONFIG.replace("    format: 'md',", f"    {replacement}")
                (self.root / "website/docusaurus.config.ts").write_text(
                    config,
                    encoding="utf-8",
                )
                self.assert_violation(
                    "website/docusaurus.config.ts: markdown format must be exactly md"
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

    def test_published_docs_apply_remote_and_local_target_boundaries(self) -> None:
        docs_index = self.root / "docs/index.md"
        docs_index.write_text(
            """\
![Remote](https://github.com/un3qual/jazz/blob/main/mark.svg)
[Elsewhere](https://example.invalid/docs)
[Internal](../.codex/execution/queue.md)
![Missing](missing.svg)
""",
            encoding="utf-8",
        )
        internal = self.root / ".codex/execution"
        internal.mkdir(parents=True)
        (internal / "queue.md").write_text("Internal.\n", encoding="utf-8")

        result = self.run_checker()
        self.assertIn(
            "docs/index.md: remote authored URL is not allowed",
            result.stdout,
        )
        self.assertIn(
            "docs/index.md: local Markdown target escapes published docs: ../.codex/execution/queue.md",
            result.stdout,
        )
        self.assertIn(
            "docs/index.md: local Markdown asset does not exist: missing.svg",
            result.stdout,
        )

    def test_published_docs_allow_local_content_and_exact_repository_navigation(self) -> None:
        (self.root / "docs/guide.md").write_text("# Guide\n", encoding="utf-8")
        image = self.root / "docs/img/mark.svg"
        image.parent.mkdir()
        image.write_text('<svg xmlns="http://www.w3.org/2000/svg" />\n', encoding="utf-8")
        (self.root / "docs/index.md").write_text(
            """\
[Guide](guide.md)
![Local mark](img/mark.svg)
[Repository][repo]

[repo]: https://github.com/un3qual/jazz/issues
""",
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_published_docs_reject_mdx_even_when_it_is_benign(self) -> None:
        (self.root / "docs/index.mdx").write_text("# Public docs\n", encoding="utf-8")
        self.assert_violation(
            "docs/index.mdx: published documentation must use plain .md files; .mdx is forbidden"
        )

    def test_published_docs_reject_front_matter_format_overrides(self) -> None:
        overrides = (
            "format: mdx",
            "format: 'detect'",
            "markdown.format: mdx",
            "mdx: true",
            "format: {parser: mdx}",
        )
        for override in overrides:
            with self.subTest(override=override):
                (self.root / "docs/index.md").write_text(
                    f"---\ntitle: Public docs\n{override}\n---\n\n# Public docs\n",
                    encoding="utf-8",
                )
                self.assert_violation(
                    "docs/index.md: front matter must not enable or detect MDX"
                )

    def test_plain_markdown_front_matter_remains_allowed(self) -> None:
        (self.root / "docs/index.md").write_text(
            "---\ntitle: Public docs\nformat: md\n---\n\n# Public docs\n",
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_published_markdown_checks_all_html_resource_attributes(self) -> None:
        (self.root / "docs/index.md").write_text(
            """\
<a href="missing.md">Missing page</a>
<img src="missing.svg" alt="Missing asset" />
<Link to="missing-link.md">Missing Link target</Link>
<video poster="missing-poster.svg"></video>
<source srcSet="missing-one.svg 1x, missing-two.svg 2x" />
""",
            encoding="utf-8",
        )
        result = self.run_checker()
        for target in (
            "missing.md",
            "missing.svg",
            "missing-link.md",
            "missing-poster.svg",
            "missing-one.svg",
            "missing-two.svg",
        ):
            with self.subTest(target=target):
                self.assertIn(target, result.stdout)

    def test_html_targets_decode_entities_before_scheme_classification(self) -> None:
        (self.root / "docs/index.md").write_text(
            """\
<img src="jav&#x61;script:alert(1)" alt="Remote" />
<video poster="d&#97;ta:text/plain,remote"></video>
<source srcSet="ftp&colon;//assets.example.invalid/one.svg 1x" />
""",
            encoding="utf-8",
        )
        self.assert_violation("docs/index.md: remote authored URL is not allowed")

    def test_srcset_decodes_entities_before_candidate_splitting(self) -> None:
        (self.root / "website/src/pages/media.tsx").write_text(
            '<source srcSet="/img/mark.svg 1x&#44; d&#97;ta:image/svg+xml,x 2x" />\n',
            encoding="utf-8",
        )
        self.assert_violation(
            "website/src/pages/media.tsx: remote authored URL is not allowed"
        )

    def test_local_targets_reject_raw_html_and_percent_decoded_backslashes(self) -> None:
        (self.root / "docs/index.md").write_text(
            r"""\
![Raw](raw\mark.svg)
<img src="html&#92;mark.svg" alt="HTML encoded" />
<img src="percent%5Cmark.svg" alt="Percent encoded" />
""",
            encoding="utf-8",
        )
        result = self.run_checker()
        for target in (r"raw\mark.svg", "html&#92;mark.svg", "percent%5Cmark.svg"):
            with self.subTest(target=target):
                self.assertIn(
                    f"docs/index.md: local target contains a backslash: {target}",
                    result.stdout,
                )

    def test_published_docs_preserve_anchors_routes_and_existing_html_targets(self) -> None:
        (self.root / "docs/guide.md").write_text("# Guide\n", encoding="utf-8")
        image = self.root / "docs/img/mark.svg"
        image.parent.mkdir()
        image.write_text('<svg xmlns="http://www.w3.org/2000/svg" />\n', encoding="utf-8")
        (self.root / "docs/index.md").write_text(
            """\
<a href="#local-heading">Anchor</a>
<a href="guide.md?mode=short#guide">Existing guide</a>
<a href="/docs/language/overview?mode=full#syntax">Published route</a>
<img src="img/mark.svg?raw=1#mark" alt="Local mark" />
<video poster="img/mark.svg"></video>
<source srcSet="img/mark.svg 1x, img/mark.svg 2x" />
""",
            encoding="utf-8",
        )

        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_internal_trees_are_not_scanned_as_published_docs(self) -> None:
        for relative_path in (".codex/private.md", "rfcs/accepted/private.md"):
            path = self.root / relative_path
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(
                "![Internal](https://example.invalid/internal.png)\n",
                encoding="utf-8",
            )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

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

    def test_generated_html_and_css_reject_remote_resource_loads(self) -> None:
        build = self.root / "website/build"
        build.mkdir()
        (build / "index.html").write_text(
            '<img src="https://assets.example.invalid/mark.svg">\n',
            encoding="utf-8",
        )
        (build / "styles.css").write_text(
            '.remote { background: url("https://assets.example.invalid/mark.svg"); }\n',
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertIn(
            "website/build/index.html: generated output loads a remote resource",
            result.stdout,
        )
        self.assertIn(
            "website/build/styles.css: generated output loads a remote resource",
            result.stdout,
        )

    def test_generated_stylesheet_links_reject_remote_fetches(self) -> None:
        build = self.root / "website/build"
        build.mkdir()
        (build / "index.html").write_text(
            '<link rel="stylesheet" href="https://assets.example.invalid/site.css">\n',
            encoding="utf-8",
        )
        self.assert_violation(
            "website/build/index.html: generated output loads a remote resource"
        )

    def test_generated_output_allows_local_resources_and_embedded_css_data(self) -> None:
        build = self.root / "website/build"
        build.mkdir()
        (build / "index.html").write_text(
            '<link rel="canonical" href="https://un3qual.github.io/jazz/">'
            '<link rel="alternate" href="https://un3qual.github.io/jazz/">'
            '<img src="/jazz/img/mark.svg">'
            '<a href="https://example.invalid">Framework link</a>\n',
            encoding="utf-8",
        )
        (build / "styles.css").write_text(
            '.embedded { background: url("data:image/svg+xml,x"); }\n',
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

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

    def test_all_authored_site_extensions_apply_the_url_boundary(self) -> None:
        cases = {
            ".css": '.remote { background: url("d&#97;ta:image/svg+xml,x"); }\n',
            ".ts": "import 'd&#97;ta:text/javascript,x';\n",
            ".tsx": '<img src="d&#97;ta:image/svg+xml,x" />;\n',
            ".js": "import('d&#97;ta:text/javascript,x');\n",
            ".jsx": '<img src="d&#97;ta:image/svg+xml,x" />;\n',
            ".mjs": "import 'd&#97;ta:text/javascript,x';\n",
            ".cjs": "import('d&#97;ta:text/javascript,x');\n",
            ".md": "![Remote](d&#97;ta:image/svg+xml,x)\n",
            ".mdx": '<img src="d&#97;ta:image/svg+xml,x" />\n',
        }
        for suffix, source in cases.items():
            path = self.root / f"website/src/remote{suffix}"
            path.write_text(source, encoding="utf-8")
        result = self.run_checker()
        for suffix in cases:
            with self.subTest(suffix=suffix):
                self.assertIn(
                    f"website/src/remote{suffix}: remote authored URL is not allowed",
                    result.stdout,
                )

    def test_unsupported_stylesheet_dialects_are_explicitly_rejected(self) -> None:
        for suffix in (".scss", ".sass", ".less"):
            (self.root / f"website/src/unsupported{suffix}").write_text(
                ".local { color: black; }\n",
                encoding="utf-8",
            )
        result = self.run_checker()
        for suffix in (".scss", ".sass", ".less"):
            with self.subTest(suffix=suffix):
                self.assertIn(
                    f"website/src/unsupported{suffix}: unsupported stylesheet dialect; author plain .css",
                    result.stdout,
                )

    def test_resource_attributes_cover_poster_and_srcset_with_quote_aware_values(self) -> None:
        (self.root / "website/src/pages/media.tsx").write_text(
            """\
export default function Media() {
  return <>
    <video poster="d&#97;ta:image/svg+xml,<svg id='poster' />" />
    <source srcSet='local.svg 1x, d&#97;ta:image/svg+xml,<svg id="remote" /> 2x' />
  </>;
}
""",
            encoding="utf-8",
        )
        self.assert_violation(
            "website/src/pages/media.tsx: remote authored URL is not allowed"
        )

    def test_resource_attributes_require_direct_static_literals(self) -> None:
        cases = {
            "dynamic.tsx": "<img src={icon} />\n",
            "dynamic.jsx": "<Link to={route}>Route</Link>\n",
            "dynamic.js": "<video poster={poster} />\n",
            "dynamic.html": "<source srcSet={sources}>\n",
            "dynamic.md": "<img src={icon} />\n",
        }
        for filename, source in cases.items():
            (self.root / "website/src" / filename).write_text(source, encoding="utf-8")
        result = self.run_checker()
        for filename in cases:
            with self.subTest(filename=filename):
                self.assertIn(
                    f"website/src/{filename}: resource attributes must use direct static literals",
                    result.stdout,
                )

    def test_jsx_spreads_are_forbidden_at_resource_boundaries(self) -> None:
        (self.root / "website/src/pages/spread.tsx").write_text(
            "export default () => <img {...props} />;\n",
            encoding="utf-8",
        )
        self.assert_violation(
            "website/src/pages/spread.tsx: JSX/HTML spreads are forbidden by the static resource profile"
        )

    def test_direct_static_resource_literals_remain_allowed(self) -> None:
        (self.root / "website/src/static.html").write_text(
            '<img src=\"/img/mark.svg\"><video poster=/img/mark.svg></video>\n',
            encoding="utf-8",
        )
        (self.root / "website/src/static.tsx").write_text(
            '<img src="/img/mark.svg" srcSet="/img/mark.svg 1x" />;\n',
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_css_scanner_handles_opposite_quotes_escapes_and_imports(self) -> None:
        (self.root / "website/src/css/custom.css").write_text(
            r'''\
.opposite { background: url("d&#97;ta:image/svg+xml,<svg id='remote' />"); }
.escaped { background: url('local\'escaped.svg'); }
@import 'd&#97;ta:text/css,"remote"';
''',
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertIn(
            "website/src/css/custom.css: remote authored URL is not allowed",
            result.stdout,
        )
        self.assertIn(
            r"website/src/css/custom.css: local target contains a backslash: local\'escaped.svg",
            result.stdout,
        )

    def test_static_and_dynamic_imports_share_scheme_and_alias_rules(self) -> None:
        (self.root / "website/src/imports.ts").write_text(
            """\
import remote from 'f&#105;le:///tmp/private.svg';
const dynamic = import('jav&#x61;script:alert(1)');
import mark from '@site/static/img/mark.svg';
import privateSource from '@site/src/private.svg';
""",
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertIn(
            "website/src/imports.ts: remote authored URL is not allowed",
            result.stdout,
        )
        self.assertIn(
            "website/src/imports.ts: site import uses an unauthorized @site root: @site/src/private.svg",
            result.stdout,
        )

    def test_site_imports_allow_packages_relative_paths_and_static_alias(self) -> None:
        (self.root / "website/src/imports.ts").write_text(
            """\
import type {Config} from '@docusaurus/types';
import './css/custom.css';
import mark from '@site/static/img/mark.svg';
const page = import('./pages/index');
""",
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_navigation_entities_are_decoded_only_in_navigation_contexts(self) -> None:
        (self.root / "website/src/pages/index.tsx").write_text(
            """\
import Link from '@docusaurus/Link';
export default function Home() {
  return <>
    <a href="https&colon;//github.com/un3qual/jazz/issues">Issues</a>
    <Link to="https&colon;//un3qual.github.io/jazz/docs/">Docs</Link>
    <img src="https&colon;//github.com/un3qual/jazz/blob/main/mark.svg" />
  </>;
}
""",
            encoding="utf-8",
        )
        self.assert_violation(
            "website/src/pages/index.tsx: remote authored URL is not allowed"
        )

    def test_site_targets_reject_raw_and_decoded_backslashes(self) -> None:
        (self.root / "website/src/pages/paths.tsx").write_text(
            r'''\
import raw from '.\private.svg';
const encoded = import('./percent%5Cprivate.svg');
export default function Paths() {
  return <img src="html&#92;private.svg" />;
}
''',
            encoding="utf-8",
        )
        result = self.run_checker()
        for target in (r".\private.svg", "./percent%5Cprivate.svg", "html&#92;private.svg"):
            with self.subTest(target=target):
                self.assertIn(
                    f"website/src/pages/paths.tsx: local target contains a backslash: {target}",
                    result.stdout,
                )

    def test_non_local_schemes_are_rejected_in_asset_and_import_contexts(self) -> None:
        (self.root / "website/src/css/custom.css").write_text(
            "background-image: url('data:image/svg+xml,remote');\n",
            encoding="utf-8",
        )
        (self.root / "website/src/pages/index.tsx").write_text(
            """\
import icon from 'file:///tmp/icon.svg';
export default function Home() {
  return <img src="javascript:alert('remote')" alt="" />;
}
""",
            encoding="utf-8",
        )
        (self.root / "website/src/remote.md").write_text(
            "![Remote](ftp://assets.example.invalid/photo.png)\n",
            encoding="utf-8",
        )
        (self.root / "docs/index.md").write_text(
            "![Remote](file:///tmp/private.svg)\n",
            encoding="utf-8",
        )

        result = self.run_checker()
        for path in (
            "website/src/css/custom.css",
            "website/src/pages/index.tsx",
            "website/src/remote.md",
            "docs/index.md",
        ):
            with self.subTest(path=path):
                self.assertIn(
                    f"{path}: remote authored URL is not allowed",
                    result.stdout,
                )

    def test_non_local_schemes_are_rejected_in_dynamic_imports(self) -> None:
        (self.root / "website/src/pages/dynamic.tsx").write_text(
            "const icon = import('data:image/svg+xml,remote');\n",
            encoding="utf-8",
        )

        self.assert_violation(
            "website/src/pages/dynamic.tsx: remote authored URL is not allowed"
        )

    def test_non_local_scheme_attributes_may_contain_the_opposite_quote(self) -> None:
        (self.root / "website/src/pages/index.tsx").write_text(
            """\
export default function Home() {
  return <img src="javascript:alert('remote')" alt="" />;
}
""",
            encoding="utf-8",
        )

        self.assert_violation(
            "website/src/pages/index.tsx: remote authored URL is not allowed"
        )

    def test_non_local_schemes_are_rejected_in_markdown_autolinks(self) -> None:
        (self.root / "website/src/remote.md").write_text(
            "<ftp://assets.example.invalid/photo.png>\n",
            encoding="utf-8",
        )

        self.assert_violation(
            "website/src/remote.md: remote authored URL is not allowed"
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

    def test_exact_production_site_navigation_is_allowed(self) -> None:
        (self.root / "website/src/pages/index.tsx").write_text(
            """\
import Link from '@docusaurus/Link';
export default function Home() {
  return (
    <>
      <Link to="https://un3qual.github.io/jazz/">Production site</Link>
      <a href="https://un3qual.github.io/jazz/docs/language/overview?mode=full#syntax">
        Language guide
      </a>
    </>
  );
}
""",
            encoding="utf-8",
        )
        (self.root / "docs/index.md").write_text(
            """\
[Production](https://un3qual.github.io/jazz/)
<a href="https://un3qual.github.io/jazz/docs/project/status?view=current#status">Status</a>
""",
            encoding="utf-8",
        )

        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_production_site_navigation_requires_the_exact_base_route(self) -> None:
        for url in (
            "https://un3qual.github.io/jazz",
            "https://un3qual.github.io/jazz-lookalike",
            "https://un3qual.github.io.evil.example/jazz/",
            "https://un3qual.github.io/jazz/%2e%2e/private",
            r"https://un3qual.github.io/jazz/\..\private",
            "https://un3qual.github.io/private",
        ):
            with self.subTest(url=url):
                (self.root / "website/src/pages/index.tsx").write_text(
                    f'<a href="{url}">Production site</a>\n',
                    encoding="utf-8",
                )
                self.assert_violation(
                    "website/src/pages/index.tsx: remote authored URL is not allowed"
                )

    def test_production_site_urls_are_rejected_in_asset_and_import_contexts(self) -> None:
        (self.root / "website/src/css/custom.css").write_text(
            "background: url('https://un3qual.github.io/jazz/img/mark.svg');\n",
            encoding="utf-8",
        )
        (self.root / "website/src/pages/index.tsx").write_text(
            """\
import mark from 'https://un3qual.github.io/jazz/img/mark.svg';
export default function Home() {
  return <img src="https://un3qual.github.io/jazz/img/mark.svg" alt="" />;
}
""",
            encoding="utf-8",
        )
        (self.root / "docs/index.md").write_text(
            "![Remote](https://un3qual.github.io/jazz/img/mark.svg)\n",
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
            "docs/index.md: remote authored URL is not allowed",
            result.stdout,
        )

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

    def test_protocol_relative_css_in_js_and_reference_definitions_are_rejected(self) -> None:
        (self.root / "website/src/pages/styles.tsx").write_text(
            "const style = {backgroundImage: 'url(//cdn.example.invalid/tsx.svg)'};\n",
            encoding="utf-8",
        )
        (self.root / "website/src/pages/styles.js").write_text(
            "const style = {backgroundImage: 'url(//cdn.example.invalid/js.svg)'};\n",
            encoding="utf-8",
        )
        (self.root / "docs/index.md").write_text(
            """\
![Remote][asset]

[asset]: //cdn.example.invalid/reference.svg
""",
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertIn(
            "website/src/pages/styles.tsx: remote authored URL is not allowed",
            result.stdout,
        )
        self.assertIn(
            "website/src/pages/styles.js: remote authored URL is not allowed",
            result.stdout,
        )
        self.assertIn(
            "docs/index.md: remote authored URL is not allowed",
            result.stdout,
        )

    def test_nonlocal_uri_tokens_in_site_comments_are_forbidden(self) -> None:
        (self.root / "website/src/pages/index.tsx").write_text(
            "// Remote example: https://example.invalid/mark.svg\nexport default 1;\n",
            encoding="utf-8",
        )
        self.assert_violation(
            "website/src/pages/index.tsx: nonlocal URI scheme tokens are forbidden"
        )

    def test_jsx_double_slash_text_cannot_hide_later_resources(self) -> None:
        (self.root / "website/src/pages/index.tsx").write_text(
            "export default () => <>// text <img src={icon} /><img src=\"data:image/svg+xml,x\" /></>;\n",
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertIn(
            "website/src/pages/index.tsx: nonlocal URI scheme tokens are forbidden",
            result.stdout,
        )
        self.assertIn(
            "website/src/pages/index.tsx: resource attributes must use direct static literals",
            result.stdout,
        )

    def test_plain_double_slash_text_and_markdown_code_fences_remain_allowed(self) -> None:
        (self.root / "website/src/pages/index.tsx").write_text(
            "export default () => <p>10 // 2 is Jazz source text</p>;\n",
            encoding="utf-8",
        )
        (self.root / "docs/index.md").write_text(
            """\
Normal prose can contain // local Jazz comment text.

```css
.example { background: url(//not-a-resource.example/example.svg); }
```
""",
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

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
