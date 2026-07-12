# Jazz Required `then` Keyword Design

**Status:** Approved  
**Date:** 2026-07-12  
**Scope:** `jazz-next/` conditional surface syntax and canonical lexer parity

## Decision

Jazz requires `then` between an `if` condition and its true branch:

```jz
if condition then trueValue else falseValue
```

The previous form without `then` is rejected immediately. Jazz does not accept
both forms, emit a deprecation warning, or provide a compatibility parser.
`then` is a reserved keyword and cannot be used as an identifier.

## Motivation

The previous grammar used adjacency to separate the condition from the true
branch:

```jz
if condition trueValue else falseValue
```

That form is harder to scan and requires the parser to give an `if` condition a
special no-application grammar so it can infer where the true branch begins.
The explicit `then` keyword makes the two branch boundaries symmetric and lets
the condition use the ordinary full-expression grammar up to a concrete token.

## Surface Grammar

The canonical form is:

```text
if-expression := "if" expression "then" expression "else" expression
```

The parser consumes the condition as a full expression stopping at `then`, then
consumes the true branch as a full expression stopping at `else`. Nested
conditionals retain nearest-`else` association:

```jz
if outer then if inner then a else b else c
```

Both `then` and `else` are mandatory. A missing `then` diagnostic identifies
the `if` expression and reports the token encountered where `then` was expected.
A missing `else` retains the existing focused diagnostic.

## Compiler Representation and Semantics

This is only a surface-syntax change. Parsing still produces
`SEIf condition trueBranch falseBranch`, lowering still produces
`EIf condition trueBranch falseBranch`, and all existing typing and lazy branch
evaluation rules remain unchanged.

Future LLVM code generation will consume the lowered conditional form, not the
surface tokens. Requiring `then` therefore creates no intermediate compiler
architecture that would be removed for native code generation.

## Lexer and Bootstrap Contract

The Haskell lexer and Jazz-authored lexer both recognize `then` as a keyword.
Their canonical comparison representation adds `ThenKeyword`, preserving exact
token-kind, lexeme, and span parity. `LexerTypes.jz` exports the new constructor
so bootstrap consumers can pattern-match the complete canonical keyword set.

## Migration

All active `.jz` sources and embedded Jazz programs under `jazz-next/` migrate
in the same change. Net-new compiler work remains under `jazz-next/`; the legacy
`jazz-hs/` and `jazz2/` trees remain untouched.

Jazz files retain two-space indentation. The migration inserts only the
required keyword and does not reformat unrelated source.

## Verification

Tests prove that:

- the Haskell lexer reserves and emits `then`;
- the Jazz-authored lexer emits the matching `ThenKeyword` value and span;
- canonical lexer comparison includes `then` in the keyword corpus;
- canonical `if condition then trueBranch else falseBranch` syntax parses;
- the old syntax is rejected with an expected-`then` diagnostic;
- nested `if` expressions retain nearest-`else` association;
- active Jazz sources compile after migration; and
- the full `jazz-next` warning/test gate remains green.
