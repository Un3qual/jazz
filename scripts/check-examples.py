#!/usr/bin/env python3
"""Build Jazz and execute every checked public example deterministically."""

from __future__ import annotations

import argparse
import csv
import math
import os
import shlex
import subprocess
import sys
from pathlib import Path, PurePosixPath

from example_cases import case_source_binding_violation


HEADER = ("name", "sources", "expected", "args")
CLI_OVERRIDE_ENV = (
    "JAZZ_PRELUDE",
    "JAZZ_WARNING_FLAGS",
    "JAZZ_WARNING_ERROR_FLAGS",
    "JAZZ_WARNING_CONFIG",
)


def fail(message: str) -> int:
    print(f"FAIL: {message}", file=sys.stderr)
    return 1


def valid_source(root: Path, source: str) -> bool:
    pure = PurePosixPath(source)
    if (
        pure.is_absolute()
        or len(pure.parts) < 2
        or pure.parts[0] != "examples"
        or ".." in pure.parts
        or pure.suffix != ".jz"
    ):
        return False
    candidate = (root / pure).resolve()
    examples_root = (root / "examples").resolve()
    try:
        candidate.relative_to(examples_root)
    except ValueError:
        return False
    return candidate.is_file()


def resolve_jazz_binary(root: Path, explicit: str | None) -> tuple[Path | None, int]:
    if explicit is not None:
        binary = Path(explicit).resolve()
        if not binary.is_file():
            return None, fail(f"Jazz executable does not exist: {binary}")
        return binary, 0

    try:
        build = subprocess.run(["cabal", "build", "jazz"], cwd=root, check=False)
    except OSError as exc:
        return None, fail(f"could not start cabal build jazz: {exc}")
    if build.returncode != 0:
        return None, fail("cabal build jazz failed")
    try:
        listed = subprocess.run(
            ["cabal", "list-bin", "jazz"],
            cwd=root,
            check=False,
            capture_output=True,
            text=True,
        )
    except OSError as exc:
        return None, fail(f"could not start cabal list-bin jazz: {exc}")
    if listed.returncode != 0 or not listed.stdout.strip():
        return None, fail("cabal list-bin jazz failed")
    binary = Path(listed.stdout.strip()).resolve()
    if not binary.is_file():
        return None, fail(f"cabal reported a missing Jazz executable: {binary}")
    return binary, 0


def checked_environment() -> dict[str, str]:
    environment = os.environ.copy()
    for name in CLI_OVERRIDE_ENV:
        environment.pop(name, None)
    return environment


def run_case(
    root: Path,
    jazz_binary: Path,
    name: str,
    expected: str,
    arguments: list[str],
    timeout_seconds: float,
) -> int:
    try:
        result = subprocess.run(
            [str(jazz_binary), *arguments, "--warnings-config", os.devnull],
            cwd=root,
            env=checked_environment(),
            check=False,
            capture_output=True,
            text=True,
            timeout=timeout_seconds,
        )
    except subprocess.TimeoutExpired:
        return fail(f"{name} timed out after {timeout_seconds:g} seconds")
    except OSError as exc:
        return fail(f"{name} could not start Jazz: {exc}")

    if result.returncode != 0:
        print(f"FAIL: {name} exited nonzero", file=sys.stderr)
        if result.stderr:
            for line in result.stderr.splitlines():
                print(f"  stderr: {line}", file=sys.stderr)
        return 1
    if result.stderr:
        print(f"FAIL: {name} wrote unexpected stderr", file=sys.stderr)
        for line in result.stderr.splitlines():
            print(f"  {line}", file=sys.stderr)
        return 1

    wanted = expected + "\n"
    if result.stdout != wanted:
        print(f"FAIL: {name} stdout did not match the checked result", file=sys.stderr)
        print(f"  expected: {wanted!r}", file=sys.stderr)
        print(f"  actual:   {result.stdout!r}", file=sys.stderr)
        return 1
    print(f"PASS: {name}")
    return 0


def run(root: Path, jazz_binary: Path, timeout_seconds: float) -> int:
    manifest = root / "scripts/example-cases.tsv"
    try:
        rows = list(
            csv.reader(
                manifest.read_text(encoding="utf-8").splitlines(),
                delimiter="\t",
                quoting=csv.QUOTE_NONE,
            )
        )
    except (OSError, UnicodeError) as exc:
        return fail(f"cannot read scripts/example-cases.tsv: {exc}")
    if not rows or tuple(rows[0]) != HEADER:
        return fail("scripts/example-cases.tsv has an invalid header")
    if len(rows) == 1:
        return fail("scripts/example-cases.tsv has no example cases")

    for line_number, fields in enumerate(rows[1:], 2):
        if len(fields) != len(HEADER):
            return fail(
                f"scripts/example-cases.tsv:{line_number}: "
                "expected four tab-separated fields"
            )
        name, raw_sources, expected, args_text = fields
        if not name or not raw_sources or not expected or not args_text:
            return fail(f"scripts/example-cases.tsv:{line_number}: malformed example case")
        sources = raw_sources.split(",")
        for source in sources:
            if not valid_source(root, source):
                return fail(
                    f"scripts/example-cases.tsv:{line_number}: "
                    f"invalid or missing source: {source}"
                )
        binding_violation = case_source_binding_violation(root, sources, args_text)
        if binding_violation is not None:
            return fail(
                f"scripts/example-cases.tsv:{line_number}: {binding_violation}"
            )
        try:
            arguments = shlex.split(args_text)
        except ValueError as exc:
            return fail(f"scripts/example-cases.tsv:{line_number}: invalid arguments: {exc}")
        status = run_case(
            root, jazz_binary, name, expected, arguments, timeout_seconds
        )
        if status != 0:
            return status

    print("Checked Jazz examples passed.")
    return 0


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("repository_root", type=Path)
    parser.add_argument("--jazz-bin")
    parser.add_argument("--timeout-seconds", type=float, default=30.0)
    arguments = parser.parse_args(argv[1:])
    if not math.isfinite(arguments.timeout_seconds) or arguments.timeout_seconds <= 0:
        return fail("timeout must be finite and greater than zero")
    root = arguments.repository_root.resolve()
    jazz_binary, status = resolve_jazz_binary(root, arguments.jazz_bin)
    if status != 0 or jazz_binary is None:
        return status
    return run(root, jazz_binary, arguments.timeout_seconds)


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
