#!/usr/bin/env python3
"""Focused fixture tests for the Jazz GitHub Pages workflow contract."""

from __future__ import annotations

import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


CHECKER = Path(__file__).with_name("check-docs-pages-workflow.py")

VALID_WORKFLOW = """\
name: Deploy documentation

on:
  push:
    branches:
      - main
    paths:
      - "docs/**"
      - "website/**"
      - "README.md"
      - ".github/workflows/docs-pages.yml"
  workflow_dispatch:

permissions:
  contents: read
  pages: write
  id-token: write

concurrency:
  group: pages
  cancel-in-progress: true

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - name: Check out repository
        uses: actions/checkout@v4
      - name: Set up Node.js
        uses: actions/setup-node@v4
        with:
          node-version: 22
          cache: npm
          cache-dependency-path: website/package-lock.json
      - name: Install website dependencies
        run: npm ci
        working-directory: website
      - name: Check brand assets
        run: npm run test:brand
        working-directory: website
      - name: Check website experience
        run: npm run test:experience
        working-directory: website
      - name: Type-check website
        run: npm run typecheck
        working-directory: website
      - name: Build website
        run: npm run build
        working-directory: website
      - name: Check generated publication boundary
        run: python3 scripts/check-website-boundary.py
      - name: Configure GitHub Pages
        uses: actions/configure-pages@v5
      - name: Upload GitHub Pages artifact
        uses: actions/upload-pages-artifact@v3
        with:
          path: website/build

  deploy:
    needs: build
    runs-on: ubuntu-latest
    environment:
      name: github-pages
      url: ${{ steps.deployment.outputs.page_url }}
    steps:
      - name: Deploy GitHub Pages
        id: deployment
        uses: actions/deploy-pages@v4
"""


class DocsPagesWorkflowTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temp_dir = tempfile.TemporaryDirectory()
        self.root = Path(self.temp_dir.name)
        self.workflow = self.root / ".github/workflows/docs-pages.yml"
        self.workflow.parent.mkdir(parents=True)
        self.workflow.write_text(VALID_WORKFLOW, encoding="utf-8")

    def tearDown(self) -> None:
        self.temp_dir.cleanup()

    def run_checker(self) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [sys.executable, str(CHECKER), str(self.root)],
            check=False,
            capture_output=True,
            text=True,
        )

    def assert_violation(self, expected: str) -> None:
        result = self.run_checker()
        self.assertNotEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertIn(expected, result.stdout)
        self.assertEqual("", result.stderr)

    def replace(self, old: str, new: str) -> None:
        text = self.workflow.read_text(encoding="utf-8")
        self.assertIn(old, text)
        self.workflow.write_text(text.replace(old, new), encoding="utf-8")

    def test_valid_workflow_passes(self) -> None:
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertEqual("Documentation Pages workflow checks passed.\n", result.stdout)

    def test_requires_only_main_pushes_and_manual_dispatch(self) -> None:
        self.replace("      - main", "      - release")
        self.replace("  workflow_dispatch:\n", "  pull_request:\n")
        result = self.run_checker()
        self.assertIn("push branch must be exactly main", result.stdout)
        self.assertIn("workflow_dispatch trigger is required", result.stdout)
        self.assertIn("pull_request triggers are forbidden", result.stdout)

    def test_requires_exact_path_filters(self) -> None:
        self.replace('      - "README.md"\n', "")
        self.assert_violation("push paths must be exactly")

    def test_requires_exact_least_privilege_permissions(self) -> None:
        self.replace("  contents: read", "  contents: write")
        self.assert_violation("permissions must be exactly")

    def test_rejects_build_job_permission_override(self) -> None:
        self.replace(
            "  build:\n    runs-on: ubuntu-latest",
            "  build:\n"
            "    permissions:\n"
            "      contents: write\n"
            "    runs-on: ubuntu-latest",
        )
        self.assert_violation("job-level permissions are forbidden: build")

    def test_rejects_deploy_job_permission_override(self) -> None:
        self.replace(
            "  deploy:\n    needs: build",
            "  deploy:\n    permissions: write-all\n    needs: build",
        )
        self.assert_violation("job-level permissions are forbidden: deploy")

    def test_requires_pages_concurrency(self) -> None:
        self.replace("  group: pages", "  group: documentation")
        self.assert_violation("concurrency must use group pages")

    def test_requires_pinned_action_major_versions(self) -> None:
        replacements = {
            "actions/checkout@v4": "actions/checkout@main",
            "actions/setup-node@v4": "actions/setup-node@v3",
            "actions/configure-pages@v5": "actions/configure-pages@v4",
            "actions/upload-pages-artifact@v3": "actions/upload-pages-artifact@v2",
            "actions/deploy-pages@v4": "actions/deploy-pages@v3",
        }
        for expected, replacement in replacements.items():
            with self.subTest(action=expected):
                self.workflow.write_text(VALID_WORKFLOW, encoding="utf-8")
                self.replace(expected, replacement)
                self.assert_violation(f"required action is missing: {expected}")

    def test_requires_node_22_and_npm_lockfile_cache(self) -> None:
        self.replace("          node-version: 22", "          node-version: 20")
        self.replace(
            "          cache-dependency-path: website/package-lock.json\n", ""
        )
        result = self.run_checker()
        self.assertIn("setup-node must use Node.js 22", result.stdout)
        self.assertIn("npm cache must use website/package-lock.json", result.stdout)

    def test_requires_clean_install_and_site_checks_in_order(self) -> None:
        self.replace("        run: npm ci", "        run: npm install")
        self.replace("        run: npm run test:brand", "        run: npm run build")
        result = self.run_checker()
        self.assertIn("required command is missing: npm ci", result.stdout)
        self.assertIn("required command is missing: npm run test:brand", result.stdout)
        self.assertIn("required command appears more than once: npm run build", result.stdout)

    def test_requires_post_build_publication_boundary(self) -> None:
        self.replace(
            "      - name: Check generated publication boundary\n"
            "        run: python3 scripts/check-website-boundary.py\n",
            "",
        )
        self.assert_violation("generated publication boundary check is required after build")

    def test_requires_build_artifact_and_deploy_dependency(self) -> None:
        self.replace("          path: website/build", "          path: website")
        self.replace("    needs: build", "    needs: lint")
        result = self.run_checker()
        self.assertIn("Pages artifact path must be website/build", result.stdout)
        self.assertIn("deploy job must depend on build", result.stdout)

    def test_requires_pages_environment_url_from_deployment(self) -> None:
        self.replace("      name: github-pages", "      name: production")
        self.replace(
            "      url: ${{ steps.deployment.outputs.page_url }}",
            "      url: https://example.invalid",
        )
        result = self.run_checker()
        self.assertIn("deploy environment must be github-pages", result.stdout)
        self.assertIn("deploy environment URL must use the deployment page_url", result.stdout)

    def test_forbids_compiler_and_performance_work(self) -> None:
        self.replace(
            "      - name: Configure GitHub Pages",
            "      - name: Cabal benchmark\n"
            "        run: cabal bench --benchmark-options full-parser-scale\n"
            "      - name: Configure GitHub Pages",
        )
        self.assert_violation("compiler or performance work is forbidden")


if __name__ == "__main__":
    unittest.main()
