#!/usr/bin/env python3
"""Behavior tests for the executable-example runner."""

from __future__ import annotations

import os
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path


CHECKER = Path(__file__).with_name("check-examples.py")


class ExampleRunnerTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temp_dir = tempfile.TemporaryDirectory()
        self.root = Path(self.temp_dir.name)
        (self.root / "examples").mkdir()
        (self.root / "scripts").mkdir()
        (self.root / "examples/hello.jz").write_text('"Hello".\n', encoding="utf-8")
        self.write_cases(
            "hello\texamples/hello.jz\t\"Hello\"\t--run examples/hello.jz\n"
        )
        self.jazz_bin = self.root / "fake-jazz"
        self.write_fake_jazz("print('\"Hello\"')")

    def tearDown(self) -> None:
        self.temp_dir.cleanup()

    def write_cases(self, rows: str) -> None:
        (self.root / "scripts/example-cases.tsv").write_text(
            "name\tsources\texpected\targs\n" + rows,
            encoding="utf-8",
        )

    def write_fake_jazz(self, body: str) -> None:
        self.jazz_bin.write_text(
            "#!/usr/bin/env python3\nimport os\nimport sys\nimport time\n" + body + "\n",
            encoding="utf-8",
        )
        self.jazz_bin.chmod(0o755)

    def run_checker(
        self,
        *,
        env: dict[str, str] | None = None,
        timeout_seconds: str = "1",
        cwd: Path | None = None,
        explicit_binary: bool = True,
    ) -> subprocess.CompletedProcess[str]:
        command = [sys.executable, str(CHECKER), str(self.root)]
        if explicit_binary:
            command.extend(["--jazz-bin", str(self.jazz_bin)])
        command.extend(["--timeout-seconds", timeout_seconds])
        return subprocess.run(
            command,
            cwd=cwd,
            env=env,
            check=False,
            capture_output=True,
            text=True,
        )

    def test_runs_manifest_case_from_outside_the_repository(self) -> None:
        with tempfile.TemporaryDirectory() as outside:
            result = self.run_checker(cwd=Path(outside))
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertIn("PASS: hello", result.stdout)

    def test_rejects_source_that_is_not_selected_by_run_arguments(self) -> None:
        (self.root / "examples/extra.jz").write_text("0.\n", encoding="utf-8")
        self.write_cases(
            "hello\texamples/hello.jz,examples/extra.jz\t\"Hello\"\t"
            "--run examples/hello.jz\n"
        )
        result = self.run_checker()
        self.assertNotEqual(0, result.returncode)
        self.assertIn("--run source does not match declared sources", result.stderr)

    def test_accepts_order_independent_standalone_source_selection(self) -> None:
        for arguments in (
            "examples/hello.jz --run --runtime-stats=json",
            "--run --runtime-stats=json examples/hello.jz",
        ):
            with self.subTest(arguments=arguments):
                self.write_cases(
                    "hello\texamples/hello.jz\t\"Hello\"\t" + arguments + "\n"
                )
                result = self.run_checker()
                self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_checked_examples_cannot_override_the_bundled_prelude(self) -> None:
        (self.root / "examples/custom-prelude.jz").write_text(
            "customValue = 1.\n", encoding="utf-8"
        )
        for prelude_arguments in (
            "--prelude examples/custom-prelude.jz",
            "--no-prelude",
        ):
            with self.subTest(prelude_arguments=prelude_arguments):
                self.write_cases(
                    "hello\texamples/hello.jz\t\"Hello\"\t"
                    f"--run {prelude_arguments} examples/hello.jz\n"
                )

                result = self.run_checker()

                self.assertNotEqual(0, result.returncode)
                self.assertIn(
                    "checked examples must use the bundled Prelude",
                    result.stderr,
                )

    def test_option_values_named_like_prelude_flags_are_not_overrides(self) -> None:
        self.write_cases(
            "hello\texamples/hello.jz\t\"Hello\"\t"
            "--run --warnings-config --prelude examples/hello.jz\n"
        )

        result = self.run_checker()

        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_clears_cli_environment_overrides(self) -> None:
        self.write_fake_jazz(
            "names = ('JAZZ_PRELUDE', 'JAZZ_WARNING_FLAGS', "
            "'JAZZ_WARNING_ERROR_FLAGS')\n"
            "present = [name for name in names if name in os.environ]\n"
            "if present:\n"
            "    print(','.join(present), file=sys.stderr)\n"
            "    raise SystemExit(9)\n"
            "if os.environ.get('JAZZ_WARNING_CONFIG') != os.devnull:\n"
            "    print('warning config was not isolated', file=sys.stderr)\n"
            "    raise SystemExit(9)\n"
            "print('\"Hello\"')"
        )
        env = os.environ.copy()
        env.update(
            {
                "JAZZ_PRELUDE": "other.jz",
                "JAZZ_WARNING_FLAGS": "-unused-binding",
                "JAZZ_WARNING_ERROR_FLAGS": "-unused-binding",
                "JAZZ_WARNING_CONFIG": "warnings.txt",
            }
        )
        result = self.run_checker(env=env)
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_invokes_jazz_with_exactly_the_manifest_arguments(self) -> None:
        self.write_fake_jazz(
            "expected = ['--run', 'examples/hello.jz']\n"
            "if sys.argv[1:] != expected:\n"
            "    print(f'unexpected argv: {sys.argv[1:]!r}', file=sys.stderr)\n"
            "    raise SystemExit(9)\n"
            "print('\"Hello\"')"
        )

        result = self.run_checker()

        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_uses_an_explicit_empty_warning_config_from_the_environment(self) -> None:
        (self.root / ".jazz-warnings").write_text(
            "invalid-ambient-warning\n", encoding="utf-8"
        )
        self.write_fake_jazz(
            "selected = os.environ.get('JAZZ_WARNING_CONFIG', '.jazz-warnings')\n"
            "if open(selected, encoding='utf-8').read():\n"
            "    print(f'non-empty warning config: {selected}', file=sys.stderr)\n"
            "    raise SystemExit(9)\n"
            "print('\"Hello\"')"
        )

        result = self.run_checker()

        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_times_out_non_terminating_example(self) -> None:
        self.write_fake_jazz("time.sleep(5)\nprint('\"Hello\"')")
        started = time.monotonic()
        result = self.run_checker(timeout_seconds="0.05")
        elapsed = time.monotonic() - started
        self.assertNotEqual(0, result.returncode)
        self.assertLess(elapsed, 2)
        self.assertIn("FAIL: hello timed out after 0.05 seconds", result.stderr)

    def test_rejects_non_finite_timeout_without_traceback(self) -> None:
        for timeout in ("nan", "inf"):
            with self.subTest(timeout=timeout):
                result = self.run_checker(timeout_seconds=timeout)
                self.assertNotEqual(0, result.returncode)
                self.assertIn("timeout must be finite and greater than zero", result.stderr)
                self.assertNotIn("Traceback", result.stderr)

    def test_header_only_manifest_is_not_a_successful_check(self) -> None:
        self.write_cases("")
        result = self.run_checker()
        self.assertNotEqual(0, result.returncode)
        self.assertIn("scripts/example-cases.tsv has no example cases", result.stderr)

    def test_missing_cabal_is_reported_without_traceback(self) -> None:
        with tempfile.TemporaryDirectory() as empty_path:
            environment = os.environ.copy()
            environment["PATH"] = empty_path
            result = self.run_checker(env=environment, explicit_binary=False)
        self.assertNotEqual(0, result.returncode)
        self.assertIn("could not start cabal", result.stderr)
        self.assertNotIn("Traceback", result.stderr)

    def test_module_sources_must_be_reachable_from_entry_module(self) -> None:
        module_root = self.root / "examples/modules"
        (module_root / "Example").mkdir(parents=True)
        (module_root / "Example/Main.jz").write_text(
            "module Example::Main {\n  0.\n}\n", encoding="utf-8"
        )
        (module_root / "Example/Unused.jz").write_text(
            "module Example::Unused {\n  1.\n}\n", encoding="utf-8"
        )
        self.write_cases(
            "module\texamples/modules/Example/Main.jz,"
            "examples/modules/Example/Unused.jz\t0\t"
            "--run --entry-module Example::Main --module-root examples/modules\n"
        )
        result = self.run_checker()
        self.assertNotEqual(0, result.returncode)
        self.assertIn(
            "declared module sources are not reachable from --entry-module",
            result.stderr,
        )

    def test_module_sources_cannot_repeat_the_same_declaration(self) -> None:
        module_root = self.root / "examples/modules"
        (module_root / "Example").mkdir(parents=True)
        source = "examples/modules/Example/Main.jz"
        (self.root / source).write_text(
            "module Example::Main {\n  0.\n}\n", encoding="utf-8"
        )
        self.write_cases(
            f"module\t{source},{source}\t0\t"
            "--run --entry-module Example::Main --module-root examples/modules\n"
        )
        self.write_fake_jazz("print('0')")

        result = self.run_checker()

        self.assertNotEqual(0, result.returncode)
        self.assertIn("module case contains duplicate declared sources", result.stderr)

    def test_module_imports_must_be_declared_by_the_same_case(self) -> None:
        module_root = self.root / "examples/modules"
        (module_root / "Example").mkdir(parents=True)
        (module_root / "Example/Main.jz").write_text(
            "module Example::Main {\n  import Example::Greeting.\n  greeting.\n}\n",
            encoding="utf-8",
        )
        (module_root / "Example/Greeting.jz").write_text(
            "module Example::Greeting {\n  greeting = \"Hello\".\n}\n",
            encoding="utf-8",
        )
        self.write_cases(
            "module\texamples/modules/Example/Main.jz\t\"Hello\"\t"
            "--run --entry-module Example::Main --module-root examples/modules\n"
        )
        result = self.run_checker()
        self.assertNotEqual(0, result.returncode)
        self.assertIn(
            "imported module source is not declared: "
            "examples/modules/Example/Greeting.jz",
            result.stderr,
        )

    def test_module_import_scanning_uses_jazz_token_boundaries(self) -> None:
        module_root = self.root / "examples/modules"
        (module_root / "Example").mkdir(parents=True)
        source = "examples/modules/Example/Main.jz"
        for layout in (
            "module Example::Main { import Example::Greeting. greeting. }\n",
            "module Example::Main {\n\u2003import Example::Greeting.\n  greeting.\n}\n",
        ):
            with self.subTest(layout=layout):
                (self.root / source).write_text(layout, encoding="utf-8")
                self.write_cases(
                    f'module\t{source}\t"Hello"\t'
                    "--run --entry-module Example::Main "
                    "--module-root examples/modules\n"
                )
                result = self.run_checker()
                self.assertNotEqual(0, result.returncode)
                self.assertIn(
                    "module does not resolve under --module-root: "
                    "Example::Greeting",
                    result.stderr,
                )

    def test_import_text_in_literals_and_comments_is_not_a_dependency(self) -> None:
        module_root = self.root / "examples/modules"
        (module_root / "Example").mkdir(parents=True)
        source = "examples/modules/Example/Main.jz"
        (self.root / source).write_text(
            'module Example::Main {\n  text = "import Example::Missing.".\n'
            "  # import Example::AlsoMissing.\n  text.\n}\n",
            encoding="utf-8",
        )
        self.write_cases(
            f'module\t{source}\t"import Example::Missing."\t'
            "--run --entry-module Example::Main --module-root examples/modules\n"
        )
        self.write_fake_jazz("print('\"import Example::Missing.\"')")

        result = self.run_checker()

        self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_module_sources_follow_ordered_module_roots(self) -> None:
        first_root = self.root / "examples/modules/first"
        second_root = self.root / "examples/modules/second"
        (first_root / "Example").mkdir(parents=True)
        (second_root / "Example").mkdir(parents=True)
        (first_root / "Example/Main.jz").write_text(
            "module Example::Main {\n  import Example::Greeting.\n  greeting.\n}\n",
            encoding="utf-8",
        )
        (second_root / "Example/Greeting.jz").write_text(
            "module Example::Greeting {\n  greeting = \"Hello\".\n}\n",
            encoding="utf-8",
        )
        self.write_cases(
            "module\texamples/modules/first/Example/Main.jz,"
            "examples/modules/second/Example/Greeting.jz\t\"Hello\"\t"
            "--run --module-root examples/modules/first "
            "--entry-module Example::Main "
            "--module-root examples/modules/second\n"
        )
        result = self.run_checker()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)


if __name__ == "__main__":
    unittest.main()
