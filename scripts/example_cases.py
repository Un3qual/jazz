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


OPTIONS_WITH_VALUES = {
    "--warnings-config",
    "--prelude",
    "--runtime-profile",
    "--entry-module",
    "--module-root",
}


def parsed_source_selection(
    arguments: list[str],
) -> tuple[str | None, str | None, list[str], str | None]:
    """Mirror the CLI's order-independent source and module selectors."""
    source_paths: list[str] = []
    entry_module: str | None = None
    module_roots: list[str] = []
    index = 0
    while index < len(arguments):
        argument = arguments[index]
        if argument in OPTIONS_WITH_VALUES:
            if index + 1 >= len(arguments):
                return None, None, [], f"missing value after {argument}"
            value = arguments[index + 1]
            if argument == "--entry-module":
                entry_module = value
            elif argument == "--module-root":
                module_roots.append(value)
            index += 2
            continue
        if argument == "-" or not argument.startswith("-"):
            source_paths.append(argument)
        index += 1

    if len(source_paths) > 1:
        return None, entry_module, module_roots, "multiple source files are not supported"
    source_path = source_paths[0] if source_paths else None
    return source_path, entry_module, module_roots, None


def resolved_module_source(
    root: Path,
    module_roots: list[PurePosixPath],
    module: str,
) -> tuple[str | None, str | None]:
    relative_module = PurePosixPath(*module.split("::")).with_suffix(".jz")
    canonical_root = root.resolve()
    for module_root in module_roots:
        source = module_root / relative_module
        candidate = root / source
        if not candidate.is_file():
            continue
        try:
            candidate.resolve().relative_to(canonical_root)
        except ValueError:
            return None, f"module source resolves outside the repository: {source}"
        return source.as_posix(), None
    return None, None


def module_source_binding_violation(
    root: Path,
    module_roots: list[PurePosixPath],
    entry_module: str,
    sources: list[str],
) -> str | None:
    declared_sources = set(sources)
    reachable_sources: set[str] = set()
    visited_modules: set[str] = set()
    pending = [entry_module]
    while pending:
        current = pending.pop()
        if current in visited_modules:
            continue
        visited_modules.add(current)
        source, resolution_violation = resolved_module_source(
            root, module_roots, current
        )
        if resolution_violation is not None:
            return resolution_violation
        if source is None:
            return f"module does not resolve under --module-root: {current}"
        if source not in declared_sources:
            if current == entry_module:
                return "--entry-module source is not declared"
            return f"imported module source is not declared: {source}"
        reachable_sources.add(source)
        try:
            text = (root / source).read_text(encoding="utf-8")
        except (OSError, UnicodeError):
            return f"cannot read declared module source: {source}"
        pending.extend(IMPORT_RE.findall(text))
    if reachable_sources != declared_sources:
        return "declared module sources are not reachable from --entry-module"
    return None


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

    source_path, entry_module, module_root_texts, selection_violation = (
        parsed_source_selection(arguments)
    )
    if selection_violation is not None:
        return selection_violation

    if entry_module is not None:
        if source_path is not None:
            return "cannot combine source file with --entry-module"
        module_root_texts = module_root_texts or ["."]
        module_roots = [PurePosixPath(value) for value in module_root_texts]
        if any(
            module_root.is_absolute() or ".." in module_root.parts
            for module_root in module_roots
        ):
            return "module roots must be repository-relative paths"
        source_paths = [PurePosixPath(source) for source in sources]
        if any(
            not any(
                source != module_root and module_root in source.parents
                for module_root in module_roots
            )
            for source in source_paths
        ):
            return "module case source is outside --module-root"
        return module_source_binding_violation(root, module_roots, entry_module, sources)

    if module_root_texts:
        return "cannot use --module-root without --entry-module"

    if source_path is None:
        return "--run requires a standalone source path or module selectors"
    if sources != [source_path]:
        return "--run source does not match declared sources"
    return None
