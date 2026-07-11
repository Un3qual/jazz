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

## Staged Follow-Ups

The following remain separate bootstrap children:

- total ordering and scalar classification;
- text traversal, length, indexing, slicing, concatenation, and builders;
- generic named-type applications needed by `Maybe` and `Result` APIs;
- host text I/O and process arguments;
- stack-safe interpreter evaluation;
- immutable bytes and UTF-8 conversion;
- the Jazz-authored lexer;
- backend-neutral lowered IR, LLVM IR generation, native linking, and the
  native runtime.

No bytecode format or bytecode VM is planned between canonical core and LLVM.
