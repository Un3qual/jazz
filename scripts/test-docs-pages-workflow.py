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
      - "examples/functions/factorial.jz"
      - "scripts/example-cases.tsv"
      - "scripts/check-website.sh"
      - "scripts/check-website-boundary.py"
      - "scripts/check-public-docs.py"
      - "scripts/example_cases.py"
      - "scripts/markdown_targets.py"
      - "scripts/markdown_visibility.py"
      - "scripts/public-doc-fragments.tsv"
      - "README.md"
      - ".github/workflows/docs-pages.yml"
  workflow_dispatch:

permissions: {}

concurrency:
  group: pages
  cancel-in-progress: true

jobs:
  build:
    permissions:
      contents: read # Checkout reads the source used to build the site.
    runs-on: ubuntu-latest
    steps:
      - name: Check out repository
        uses: actions/checkout@11d5960a326750d5838078e36cf38b85af677262 # v4
        with:
          persist-credentials: false
      - name: Set up Node.js
        uses: actions/setup-node@49933ea5288caeca8642d1e84afbd3f7d6820020 # v4
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
      - name: Check public documentation
        run: python3 scripts/check-public-docs.py
      - name: Check source publication boundary
        run: python3 scripts/check-website-boundary.py
      - name: Build website
        run: npm run build
        working-directory: website
      - name: Check generated publication boundary
        run: python3 scripts/check-website-boundary.py
      - name: Configure GitHub Pages
        uses: actions/configure-pages@983d7736d9b0ae728b81ab479565c72886d7745b # v5
      - name: Upload GitHub Pages artifact
        uses: actions/upload-pages-artifact@56afc609e74202658d3ffba0e8f6dda462b719fa # v3
        with:
          path: website/build

  deploy:
    permissions:
      pages: write # Publish the checked Pages artifact.
      id-token: write # Authenticate the deployment through OIDC.
    needs: build
    runs-on: ubuntu-latest
    environment:
      name: github-pages
      url: ${{ steps.deployment.outputs.page_url }}
    steps:
      - name: Deploy GitHub Pages
        id: deployment
        uses: actions/deploy-pages@d6db90164ac5ed86f2b6aed7e0febac5b3c0c03e # v4
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

    def test_requires_website_validation_inputs_in_path_filters(self) -> None:
        for path in (
            "scripts/check-website.sh",
            "scripts/check-website-boundary.py",
            "scripts/check-public-docs.py",
            "scripts/example_cases.py",
            "scripts/markdown_targets.py",
            "scripts/markdown_visibility.py",
            "scripts/public-doc-fragments.tsv",
        ):
            with self.subTest(path=path):
                self.workflow.write_text(VALID_WORKFLOW, encoding="utf-8")
                self.replace(f'      - "{path}"\n', "")
                self.assert_violation("push paths must be exactly")

    def test_requires_empty_workflow_permissions(self) -> None:
        self.replace(
            "permissions: {}",
            "permissions:\n  contents: read\n  pages: write\n  id-token: write",
        )
        self.assert_violation("workflow permissions must be empty")

    def test_requires_exact_least_privilege_job_permissions(self) -> None:
        self.replace("      contents: read", "      contents: write")
        self.replace("      id-token: write", "      contents: read")
        result = self.run_checker()
        self.assertIn(
            "build permissions must be exactly contents:read", result.stdout
        )
        self.assertIn(
            "deploy permissions must be exactly pages:write, id-token:write",
            result.stdout,
        )

    def test_requires_pages_concurrency(self) -> None:
        self.replace("  group: pages", "  group: documentation")
        self.assert_violation("concurrency must use group pages")

    def test_requires_immutable_action_commits(self) -> None:
        replacements = {
            "actions/checkout@11d5960a326750d5838078e36cf38b85af677262": "actions/checkout@v4",
            "actions/setup-node@49933ea5288caeca8642d1e84afbd3f7d6820020": "actions/setup-node@v4",
            "actions/configure-pages@983d7736d9b0ae728b81ab479565c72886d7745b": "actions/configure-pages@v5",
            "actions/upload-pages-artifact@56afc609e74202658d3ffba0e8f6dda462b719fa": "actions/upload-pages-artifact@v3",
            "actions/deploy-pages@d6db90164ac5ed86f2b6aed7e0febac5b3c0c03e": "actions/deploy-pages@v4",
        }
        for expected, replacement in replacements.items():
            with self.subTest(action=expected):
                self.workflow.write_text(VALID_WORKFLOW, encoding="utf-8")
                self.replace(expected, replacement)
                self.assert_violation("required action is missing from")

    def test_checkout_must_not_persist_credentials(self) -> None:
        self.replace("          persist-credentials: false", "          persist-credentials: true")
        self.assert_violation("checkout must disable credential persistence")

    def test_block_scalar_text_cannot_impersonate_actions_or_commands(self) -> None:
        self.replace(
            "      - name: Install website dependencies\n"
            "        run: npm ci\n"
            "        working-directory: website\n",
            "      - name: Spoof required structure\n"
            "        run: |\n"
            "          run: npm ci\n"
            "          uses: actions/configure-pages@983d7736d9b0ae728b81ab479565c72886d7745b\n"
            "          uses: actions/upload-pages-artifact@56afc609e74202658d3ffba0e8f6dda462b719fa\n"
            "          uses: actions/deploy-pages@d6db90164ac5ed86f2b6aed7e0febac5b3c0c03e\n",
        )
        self.replace(
            "      - name: Configure GitHub Pages\n"
            "        uses: actions/configure-pages@983d7736d9b0ae728b81ab479565c72886d7745b # v5\n"
            "      - name: Upload GitHub Pages artifact\n"
            "        uses: actions/upload-pages-artifact@56afc609e74202658d3ffba0e8f6dda462b719fa # v3\n"
            "        with:\n"
            "          path: website/build\n",
            "",
        )
        self.replace(
            "      - name: Deploy GitHub Pages\n"
            "        id: deployment\n"
            "        uses: actions/deploy-pages@d6db90164ac5ed86f2b6aed7e0febac5b3c0c03e # v4\n",
            "",
        )
        result = self.run_checker()
        self.assertIn("required command is missing: npm ci", result.stdout)
        self.assertIn("required action is missing from build job", result.stdout)
        self.assertIn("required action is missing from deploy job", result.stdout)

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

    def test_rejects_commands_prefixed_with_echo(self) -> None:
        self.replace("        run: npm ci", "        run: echo npm ci")
        self.assert_violation("required command is missing: npm ci")

    def test_requires_public_docs_validation_before_build(self) -> None:
        self.replace(
            "      - name: Check public documentation\n"
            "        run: python3 scripts/check-public-docs.py\n",
            "",
        )
        self.assert_violation("public documentation check is required before build")

    def test_requires_source_boundary_before_build(self) -> None:
        self.replace(
            "      - name: Check source publication boundary\n"
            "        run: python3 scripts/check-website-boundary.py\n",
            "",
        )
        self.assert_violation("source publication boundary check is required before build")

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
