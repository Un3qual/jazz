"""Lexical helpers for Docusaurus fenced-code metadata."""

from __future__ import annotations


def metadata_tokens(metastring: str) -> tuple[str, ...]:
    """Split on unquoted whitespace while preserving quotes and escapes.

    Single- and double-quoted regions remain part of their surrounding raw
    token. Within a quoted region, a backslash escapes the next character.
    Keeping the raw spelling ensures quoted values never become standalone
    metadata tokens.
    """

    tokens: list[str] = []
    start: int | None = None
    quote: str | None = None
    escaped = False

    for index, character in enumerate(metastring):
        if start is None:
            if character.isspace():
                continue
            start = index

        if quote is not None:
            if escaped:
                escaped = False
            elif character == "\\":
                escaped = True
            elif character == quote:
                quote = None
        elif character in ('"', "'"):
            quote = character
        elif character.isspace():
            tokens.append(metastring[start:index])
            start = None

    if start is not None:
        tokens.append(metastring[start:])

    return tuple(tokens)


def has_metadata_token(metastring: str, expected: str) -> bool:
    """Return whether an exact, unquoted metadata token is present."""

    return expected in metadata_tokens(metastring)
