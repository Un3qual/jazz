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

## Explicit-Import Character and Text APIs

Programs import [`Char`](../../../jazz-next/jazz/stdlib/Char.jz) and
[`Text`](../../../jazz-next/jazz/stdlib/Text.jz) explicitly. Imports are not
re-exported. The complete public operation inventory, edge behavior, and
complexity promises are maintained in the
[standard-library reference](../../../jazz-next/jazz/stdlib/README.md).

`Char` exposes scalar conversion, Unicode classification, newline detection,
and simple one-scalar case mapping. Checked `charFromUInt32` rejects values
outside Unicode and surrogate code points with `Nothing`. Classification and
case mapping are locale-independent; case mapping never expands one scalar
into multiple characters.

`Text` exposes:

- shape and scalar traversal through `textEmpty`, `textLength`, `textIsEmpty`,
  `textUncons`, `textAt`, `textTake`, `textDrop`, `textSlice`, `textToChars`,
  and `textReverse`;
- construction through `textAppend`, `textAppendChar`, `textFromChars`,
  `textRepeat`, `textConcat`, and `textJoin`;
- exact prefix, suffix, containment, and first-index search;
- delimiter, newline, and Unicode-whitespace splitting;
- left-to-right non-overlapping replacement; and
- Unicode-whitespace trimming plus scalar-width padding.

All indices, lengths, and widths count scalars. Negative indices return
`Nothing`; negative counts clamp to zero. An empty search needle matches at
zero, splitting on an empty delimiter returns one text value per scalar, and
replacing an empty needle leaves the input unchanged. Search and replacement
compare exact scalar sequences and perform no normalization.

The stage-0 interpreter implements irreducible scalar/text behavior through
private `__kernel_char*` and `__kernel_text*` adapters. The public library
functions compose those hooks in Jazz wherever practical. Kernel names are not
public API. Future LLVM lowering and the native runtime must preserve the same
Jazz-visible signatures and behavior without exposing Haskell representation
details.

## Staged Follow-Ups

The following remain separate bootstrap children:

- immutable bytes and explicit UTF-8 conversion;
- Unicode normalization and locale-sensitive or multi-scalar case conversion;
- advanced search algorithms where corpus/benchmark evidence justifies them;
- binary I/O, handles, directories, environment variables, and async I/O; and
- backend-neutral lowered IR, LLVM IR generation, native linking, and the
  native runtime.

No bytecode format or bytecode VM is planned between canonical core and LLVM.
