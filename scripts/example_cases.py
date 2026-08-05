#!/usr/bin/env python3
"""Parse and validate executable-example manifest bindings."""

from __future__ import annotations

import shlex
from dataclasses import dataclass
from pathlib import Path, PurePosixPath


RESERVED_WORDS = {
    "module",
    "import",
    "as",
    "data",
    "value",
    "if",
    "then",
    "else",
    "case",
}


OPTIONS_WITH_VALUES = {
    "--warnings-config",
    "--prelude",
    "--runtime-profile",
    "--entry-module",
    "--module-root",
}
OPTIONS_WITHOUT_VALUES = {"--run"}
RUNTIME_STATISTICS_OPTIONS = {
    "--runtime-stats",
    "--runtime-stats=human",
    "--runtime-stats=json",
}
WARNING_CATEGORY_TOKENS = frozenset(
    {
        "same-scope-rebinding",
        "shadowing-outer-scope",
        "unused-binding",
        "deprecated-syntax",
    }
)


@dataclass(frozen=True)
class ParsedSourceSelection:
    source_path: str | None = None
    entry_module: str | None = None
    module_roots: tuple[str, ...] = ()
    prelude_override: bool = False
    warning_config_override: bool = False
    runtime_statistics_requested: bool = False
    runtime_profile_requested: bool = False
    violation: str | None = None


def invalid_source_selection(violation: str) -> ParsedSourceSelection:
    return ParsedSourceSelection(violation=violation)


def module_path_violation(module_path: str) -> str | None:
    """Mirror ModuleResolver.parseModulePathText before filesystem lookup."""
    if not module_path:
        return "entry module path cannot be empty"
    segments = module_path.split("::")
    if any(not segment for segment in segments):
        return f"invalid entry module path '{module_path}': empty path segment"
    if not all(
        (segment[0].isalpha() or segment[0] == "_")
        and all(
            character.isalnum() or character in {"_", "'", "!"}
            for character in segment[1:]
        )
        for segment in segments
    ):
        return (
            f"invalid entry module path '{module_path}': "
            "segments must be identifiers"
        )
    return None


def warning_flag_violation(argument: str) -> str | None:
    """Mirror WarningConfig.parseCliWarningDirective for manifest validation."""
    token = argument[2:].strip()
    if not token:
        return "empty warning token"
    if token in {"error", "none"}:
        return None
    if token.startswith("error="):
        category = token.removeprefix("error=")
    elif token.startswith("no-"):
        category = token.removeprefix("no-")
    else:
        category = token
    normalized_category = category.strip().lower()
    if normalized_category not in WARNING_CATEGORY_TOKENS:
        return f"unknown warning category: {normalized_category}"
    return None


def jazz_source_tokens(source: str) -> list[tuple[str, str]]:
    """Tokenize the source shapes needed to identify real import declarations."""
    tokens: list[tuple[str, str]] = []
    index = 0
    while index < len(source):
        character = source[index]
        if character.isspace():
            index += 1
            continue
        if character == "#":
            newline = source.find("\n", index)
            index = len(source) if newline == -1 else newline + 1
            continue
        if character in {"'", '"'}:
            delimiter = character
            index += 1
            while index < len(source):
                if source[index] == "\\":
                    index += 2
                elif source[index] == delimiter:
                    index += 1
                    break
                else:
                    index += 1
            continue
        if character.isalpha() or character == "_":
            end = index + 1
            while end < len(source) and (
                source[end].isalnum() or source[end] in {"_", "'", "!"}
            ):
                end += 1
            identifier = source[index:end]
            kind = "keyword" if identifier in RESERVED_WORDS else "identifier"
            tokens.append((kind, identifier))
            index = end
            continue
        if source.startswith("::", index):
            tokens.append(("symbol", "::"))
            index += 2
            continue
        tokens.append(("symbol", character))
        index += 1
    return tokens


def imported_module_paths(source: str) -> list[str]:
    """Return import paths using the Jazz lexer token boundaries."""
    tokens = jazz_source_tokens(source)
    imports: list[str] = []
    for index, token in enumerate(tokens):
        if token != ("keyword", "import"):
            continue
        cursor = index + 1
        if cursor >= len(tokens) or tokens[cursor][0] != "identifier":
            continue
        segments = [tokens[cursor][1]]
        cursor += 1
        while (
            cursor + 1 < len(tokens)
            and tokens[cursor] == ("symbol", "::")
            and tokens[cursor + 1][0] == "identifier"
        ):
            segments.append(tokens[cursor + 1][1])
            cursor += 2
        if cursor < len(tokens) and tokens[cursor] in {
            ("symbol", "."),
            ("symbol", "("),
            ("keyword", "as"),
        }:
            imports.append("::".join(segments))
    return imports


def parsed_source_selection(
    arguments: list[str],
) -> ParsedSourceSelection:
    """Mirror the CLI's order-independent source and module selectors."""
    source_paths: list[str] = []
    entry_module: str | None = None
    module_roots: list[str] = []
    prelude_override = False
    warning_config_override = False
    runtime_statistics_requested = False
    runtime_profile_requested = False
    index = 0
    while index < len(arguments):
        argument = arguments[index]
        if argument in OPTIONS_WITH_VALUES:
            if index + 1 >= len(arguments):
                return invalid_source_selection(f"missing value after {argument}")
            value = arguments[index + 1]
            if argument == "--entry-module":
                violation = module_path_violation(value)
                if violation is not None:
                    return invalid_source_selection(violation)
                entry_module = value
            elif argument == "--module-root":
                module_roots.append(value)
            elif argument == "--prelude":
                prelude_override = True
            elif argument == "--warnings-config":
                warning_config_override = True
            elif argument == "--runtime-profile":
                runtime_profile_requested = True
            index += 2
            continue
        if argument.startswith("--runtime-profile="):
            runtime_profile_requested = True
            index += 1
            continue
        if argument == "--no-prelude":
            prelude_override = True
            index += 1
            continue
        if argument in RUNTIME_STATISTICS_OPTIONS:
            runtime_statistics_requested = True
            index += 1
            continue
        if argument.startswith("-W"):
            violation = warning_flag_violation(argument)
            if violation is not None:
                return invalid_source_selection(violation)
            index += 1
            continue
        if argument in OPTIONS_WITHOUT_VALUES:
            index += 1
            continue
        if argument == "-" or not argument.startswith("-"):
            source_paths.append(argument)
            index += 1
            continue
        return invalid_source_selection(f"unknown argument: {argument}")

    if len(source_paths) > 1:
        return invalid_source_selection("multiple source files are not supported")
    return ParsedSourceSelection(
        source_path=source_paths[0] if source_paths else None,
        entry_module=entry_module,
        module_roots=tuple(module_roots),
        prelude_override=prelude_override,
        warning_config_override=warning_config_override,
        runtime_statistics_requested=runtime_statistics_requested,
        runtime_profile_requested=runtime_profile_requested,
    )


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
    if len(set(sources)) != len(sources):
        return "module case contains duplicate declared sources"
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
        pending.extend(imported_module_paths(text))
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

    if any(argument in {"--help", "-h"} for argument in arguments):
        return "checked examples cannot use help mode"
    if arguments.count("--run") != 1:
        return "arguments must contain exactly one --run selector"

    selection = parsed_source_selection(arguments)
    if selection.violation is not None:
        return selection.violation
    if selection.prelude_override:
        return "checked examples must use the bundled Prelude"
    if selection.warning_config_override:
        return "checked examples cannot override the warning config"
    if selection.runtime_statistics_requested:
        return "checked examples cannot request runtime statistics"
    if selection.runtime_profile_requested:
        return "checked examples cannot write runtime profiles"

    if selection.entry_module is not None:
        if selection.source_path is not None:
            return "cannot combine source file with --entry-module"
        module_root_texts = list(selection.module_roots) or ["."]
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
        return module_source_binding_violation(
            root, module_roots, selection.entry_module, sources
        )

    if selection.module_roots:
        return "cannot use --module-root without --entry-module"

    if selection.source_path is None:
        return "--run requires a standalone source path or module selectors"
    if sources != [selection.source_path]:
        return "--run source does not match declared sources"
    return None
