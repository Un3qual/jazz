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
        self.build = self.root / "website/build"
        self.build.mkdir()

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def run_checker(self) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [sys.executable, str(CHECKER), "--build-directory", str(self.build)],
            check=False,
            capture_output=True,
            text=True,
        )

    def assert_violation(self, message: str) -> None:
        result = self.run_checker()
        self.assertNotEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertIn(message, result.stdout)
        self.assertEqual("", result.stderr)

    def test_empty_built_output_boundary_passes(self) -> None:
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertEqual("Website boundary checks passed.\n", result.stdout)

    def test_explicit_build_directory_must_exist(self) -> None:
        shutil.rmtree(self.build)
        self.assert_violation("build directory is missing")

    def test_built_output_rejects_internal_material(self) -> None:
        (self.build / "index.html").write_text("<p>.codex/execution/queue.md</p>", encoding="utf-8")
        self.assert_violation("generated output contains internal-only material")

    def test_built_output_rejects_ordinary_remote_resources(self) -> None:
        (self.build / "styles.css").write_text(
            "@font-face{src:url(https://fonts.example/jazz.woff2)}",
            encoding="utf-8",
        )
        self.assert_violation("generated output contains a non-allowlisted remote URL")

    def test_built_output_rejects_object_data_remote_resources(self) -> None:
        (self.build / "index.html").write_text(
            '<object data="https://cdn.example/manual.pdf"></object>',
            encoding="utf-8",
        )
        self.assert_violation("generated output contains a non-allowlisted remote URL")

    def test_built_output_rejects_javascript_remote_resources(self) -> None:
        (self.build / "main.js").write_text(
            'fetch("https://cdn.example/index.json");\n'
            'import("https://cdn.example/search.js");\n',
            encoding="utf-8",
        )
        self.assert_violation("generated output contains a non-allowlisted remote URL")

    def test_built_output_allows_javascript_url_parsing_bases(self) -> None:
        (self.build / "main.js").write_text(
            'new URL(path, "https://jazz.invalid");\n'
            'new URL(`https://example.com${normalized}`);\n',
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertEqual("Website boundary checks passed.\n", result.stdout)

    def test_built_output_rejects_inline_html_css_resources(self) -> None:
        cases = {
            "style element": (
                "<style>"
                "@font-face{src:url(https://fonts.example/jazz.woff2)}"
                "</style>"
            ),
            "double-quoted style attribute": (
                '<div style="background:url(https://cdn.example/double.png)"></div>'
            ),
            "single-quoted style attribute": (
                "<div style='background:url(https://cdn.example/single.png)'></div>"
            ),
            "unquoted style attribute": (
                "<div style=background:url(https://cdn.example/unquoted.png)></div>"
            ),
        }
        for label, source in cases.items():
            with self.subTest(label):
                (self.build / "index.html").write_text(source, encoding="utf-8")
                self.assert_violation(
                    "generated output contains a non-allowlisted remote URL"
                )

    def test_built_output_rejects_inline_html_javascript_resources(self) -> None:
        (self.build / "index.html").write_text(
            '<script>fetch("https://cdn.example/index.json");'
            'import("https://cdn.example/search.js");</script>',
            encoding="utf-8",
        )
        self.assert_violation("generated output contains a non-allowlisted remote URL")

    def test_built_output_allows_inline_html_url_parsing_bases(self) -> None:
        (self.build / "index.html").write_text(
            '<script>new URL(path, "https://jazz.invalid");'
            'new URL(`https://example.com${normalized}`);</script>',
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertEqual("Website boundary checks passed.\n", result.stdout)

    def test_built_output_allows_navigation_data_urls_and_local_assets(self) -> None:
        (self.build / "index.html").write_text(
            '<link rel="canonical" href="https://un3qual.github.io/jazz/">\n'
            '<a href="https://github.com/un3qual/jazz">GitHub</a>\n'
            '<a href="https://docusaurus.io/docs">Docusaurus docs</a>\n'
            '<img src="/jazz/img/jazz-mark.svg">\n',
            encoding="utf-8",
        )
        (self.build / "styles.css").write_text(
            "@font-face{src:url(data:font/woff2;base64,aHR0cHM6Ly8=)}",
            encoding="utf-8",
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)


if __name__ == "__main__":
    unittest.main()
