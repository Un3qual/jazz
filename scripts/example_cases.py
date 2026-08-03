#!/usr/bin/env python3
"""Parse and validate executable-example manifest bindings."""

from __future__ import annotations

import re
import shlex
from pathlib import Path, PurePosixPath


IMPORT_RE = re.compile(
    r"^[ \t]*import[ \t]+([A-Z][A-Za-z0-9_]*(?:::[A-Z][A-Za-z0-9_]*)*)",
    re.MULTILINE,
)


def option_value(arguments: list[str], option: str) -> str | None:
    if arguments.count(option) != 1:
        return None
    index = arguments.index(option)
    if index + 1 >= len(arguments) or arguments[index + 1].startswith("--"):
        return None
    return arguments[index + 1]


def module_name(module_root: PurePosixPath, source: PurePosixPath) -> str:
    return "::".join(source.relative_to(module_root).with_suffix("").parts)


def module_sources_are_reachable(
    root: Path,
    module_root: PurePosixPath,
    entry_module: str,
    sources: list[str],
) -> bool:
    module_paths = {
        module_name(module_root, PurePosixPath(source)): source
        for source in sources
    }
    reachable: set[str] = set()
    pending = [entry_module]
    while pending:
        current = pending.pop()
        if current in reachable:
            continue
        source = module_paths.get(current)
        if source is None:
            continue
        reachable.add(current)
        try:
            text = (root / source).read_text(encoding="utf-8")
        except (OSError, UnicodeError):
            return False
        pending.extend(
            imported
            for imported in IMPORT_RE.findall(text)
            if imported in module_paths and imported not in reachable
        )
    return reachable == set(module_paths)


def case_source_binding_violation(
    root: Path, sources: list[str], args_text: str
) -> str | None:
    """Return why manifest sources are not the programs selected by the CLI."""
    try:
        arguments = shlex.split(args_text)
    except ValueError as exc:
        return f"arguments are not valid shell words: {exc}"

    if arguments.count("--run") != 1:
        return "arguments must contain exactly one --run selector"

    entry_module = option_value(arguments, "--entry-module")
    module_root_text = option_value(arguments, "--module-root")
    has_module_option = "--entry-module" in arguments or "--module-root" in arguments
    if has_module_option:
        if entry_module is None or module_root_text is None:
            return "module case requires one --entry-module and one --module-root value"
        module_root = PurePosixPath(module_root_text)
        source_paths = [PurePosixPath(source) for source in sources]
        if any(
            source == module_root or module_root not in source.parents
            for source in source_paths
        ):
            return "module case source is outside --module-root"
        entry_source = module_root / PurePosixPath(*entry_module.split("::")).with_suffix(
            ".jz"
        )
        if entry_source.as_posix() not in sources:
            return "--entry-module source is not declared"
        if not module_sources_are_reachable(root, module_root, entry_module, sources):
            return "declared module sources are not reachable from --entry-module"
        return None

    run_index = arguments.index("--run")
    if run_index + 1 >= len(arguments) or arguments[run_index + 1].startswith("--"):
        return "--run requires a standalone source path or module selectors"
    run_source = arguments[run_index + 1]
    if sources != [run_source]:
        return "--run source does not match declared sources"
    return None
