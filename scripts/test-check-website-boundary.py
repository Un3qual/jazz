#!/usr/bin/env python3
"""Behavior tests for Jazz's narrow website publication boundary."""

from __future__ import annotations

import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


REPOSITORY_ROOT = Path(__file__).resolve().parent.parent
CHECKER = REPOSITORY_ROOT / "scripts/check-website-boundary.py"


class WebsiteBoundaryTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        shutil.copytree(REPOSITORY_ROOT / "docs", self.root / "docs")
        shutil.copytree(
            REPOSITORY_ROOT / "website",
            self.root / "website",
            ignore=shutil.ignore_patterns("node_modules", "build"),
        )
        shutil.copytree(REPOSITORY_ROOT / "examples", self.root / "examples")
        (self.root / "scripts").mkdir()
        shutil.copy2(
            REPOSITORY_ROOT / "scripts/example-cases.tsv",
            self.root / "scripts/example-cases.tsv",
        )
        self.config = self.root / "website/docusaurus.config.ts"

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
        source = self.config.read_text(encoding="utf-8")
        self.assertIn(old, source)
        self.config.write_text(source.replace(old, new, 1), encoding="utf-8")

    def assert_violation(self, message: str) -> None:
        result = self.run_checker()
        self.assertNotEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertIn(message, result.stdout)
        self.assertEqual("", result.stderr)

    def test_checked_in_source_boundary_passes(self) -> None:
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertEqual("Website boundary checks passed.\n", result.stdout)

    def test_equivalent_config_source_shape_is_not_policy(self) -> None:
        self.replace_config(
            "const config: Config = {",
            "const config = {",
        )
        self.replace_config(
            "};\n\nexport default config;",
            "} satisfies Config;\n\nexport default config;",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_broken_links_must_fail_the_production_build(self) -> None:
        self.replace_config("onBrokenLinks: 'throw'", "onBrokenLinks: 'warn'")
        self.assert_violation("Docusaurus must fail broken links")

    def test_docs_root_and_route_are_fixed(self) -> None:
        self.replace_config("path: '../docs'", "path: '../rfcs'")
        self.assert_violation("Docusaurus must publish only ../docs")

    def test_built_output_rejects_internal_material(self) -> None:
        build = self.root / "website/build"
        build.mkdir()
        (build / "index.html").write_text("<p>.codex/execution/queue.md</p>", encoding="utf-8")
        self.assert_violation("generated output contains internal-only material")

    def test_built_output_rejects_ordinary_remote_resources(self) -> None:
        build = self.root / "website/build"
        build.mkdir()
        (build / "styles.css").write_text(
            "@font-face{src:url(https://fonts.example/jazz.woff2)}",
            encoding="utf-8",
        )
        self.assert_violation("generated output contains a non-allowlisted remote URL")

    def test_built_output_allows_navigation_data_urls_and_local_assets(self) -> None:
        build = self.root / "website/build"
        build.mkdir()
        (build / "index.html").write_text(
            '<link rel="canonical" href="https://un3qual.github.io/jazz/">\n'
            '<a href="https://github.com/un3qual/jazz">GitHub</a>\n'
            '<a href="https://docusaurus.io/docs">Docusaurus docs</a>\n'
            '<img src="/jazz/img/jazz-mark.svg">\n',
            encoding="utf-8",
        )
        (build / "styles.css").write_text(
            "@font-face{src:url(data:font/woff2;base64,aHR0cHM6Ly8=)}",
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)


if __name__ == "__main__":
    unittest.main()
