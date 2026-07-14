# Text and Character Semantics

Status: implemented bootstrap contract

## Purpose

Define backend-independent `Char` and `Text` semantics required by the
Jazz-authored bootstrap compiler while preserving the Haskell interpreter as
stage 0 and LLVM-generated native binaries as the long-term artifact target.

## Values

`Char` is one Unicode scalar value. Surrogate code points are not scalar values
and cannot inhabit `Char`.

`Text` is an immutable sequence of Unicode scalar values. The Haskell stage-0
interpreter may store it as `Data.Text.Text`, but that representation is
non-normative. The future native runtime must implement the same Jazz-visible
semantics through its versioned ABI.

## Literal Syntax

Character literals use single quotes. Text literals use double quotes.

The accepted escapes are:

- `\\` for backslash;
- `\'` for single quote;
- `\"` for double quote;
- `\n`, `\r`, `\t`, and `\0`;
- `\u{HEX}`, where `HEX` contains 1-6 hexadecimal digits and denotes a scalar
  no greater than `0x10FFFF` and outside `0xD800..0xDFFF`.

Raw newline and carriage-return characters are not accepted inside either
quoted form. Invalid escapes, invalid scalar escapes, empty or multi-scalar
character literals, and unterminated literals report `E0001` at the opening
quote. Tokens preserve the raw quoted lexeme while AST/runtime values preserve
the decoded scalar or text.

## Types and Patterns

`Char` and `Text` are distinct closed primitive types. Neither converts
implicitly to the other. Both are accepted in adjacent monomorphic signatures,
lists, tuples, function signatures, literal expressions, and literal patterns.

## Equality and Rendering

`==` and `!=` require two operands of the same type. `Char` compares scalar
values. `Text` compares exact scalar sequences without normalization or
locale-sensitive behavior.

Runtime rendering uses valid Jazz source spelling with deterministic escaping.
The first child does not add interpolation or multiline text.

## Explicit-Import Traversal

The ordinary [`Text`](../../../jazz-next/jazz/stdlib/Text.jz) module exports exactly:

- `textEmpty :: Text`;
- `textLength :: Text -> Int`;
- `textIsEmpty :: Text -> Bool`;
- `textUncons :: Text -> Maybe((Char, Text))`;
- `textAppend :: Text -> Text -> Text`;
- `textAppendChar :: Text -> Char -> Text`; and
- `textFromChars :: [Char] -> Text`.

Programs must import `Text` explicitly. The module imports the ordinary
`Maybe` module and does not re-export `Maybe`, `Nothing`, or `Just`.
`textLength` counts Unicode scalar values. `textUncons` returns `Nothing` for
empty text and `Just (first, rest)` otherwise, where `first` is the first scalar
and `rest` is the exact remaining scalar sequence. `textAppend` concatenates
two scalar sequences, `textAppendChar` appends one scalar, and `textFromChars`
constructs text from a list of scalars in list order. None of these operations
performs normalization, byte conversion, locale-sensitive processing, or
external I/O.

The stage-0 interpreter implements these semantics through private
`__kernel_textLength :: Text -> Int`,
`__kernel_textUnconsRaw :: Text -> [(Char, Text)]`,
`__kernel_textAppend :: Text -> Text -> Text`,
`__kernel_textAppendChar :: Text -> Char -> Text`, and
`__kernel_textFromChars :: [Char] -> Text` adapters. The raw uncons adapter
returns only `[]` or one tuple; none of the adapters is a public API. All five
are backend-neutral semantic hooks: future LLVM lowering and the native
runtime must preserve the Jazz-visible signatures and behavior without
exposing the Haskell representation.

## Staged Follow-Ups

The following remain separate bootstrap children:

- total ordering and scalar classification;
- checked indexing, slicing, richer builders, prefix/suffix checks, and
  substring search;
- host text I/O and process arguments;
- stack-safe interpreter evaluation;
- immutable bytes and UTF-8 conversion;
- the Jazz-authored lexer;
- backend-neutral lowered IR, LLVM IR generation, native linking, and the
  native runtime.

No bytecode format or bytecode VM is planned between canonical core and LLVM.
