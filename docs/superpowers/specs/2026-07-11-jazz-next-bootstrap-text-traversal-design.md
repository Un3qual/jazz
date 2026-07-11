# Jazz-Next Bootstrap Text Traversal Design

## Status

Approved in discussion on `2026-07-11` as the next child of the hosted
bootstrap interpreter profile. This document locks the API and architecture;
implementation remains owned by `JN-BOOTSTRAP-TEXT-TRAVERSAL-001`.

## Goal

Add the smallest backend-independent text traversal surface needed for
Jazz-authored compiler code to consume immutable Unicode text safely. The
surface must work in the Haskell stage-0 interpreter and remain implementable
through the future native runtime ABI without changing Jazz source APIs.

## Public Module Boundary

Text traversal is an ordinary importable module, not a bundled-prelude API.
Consumers use `import Text.` and receive exactly these value exports:

```jazz
module Text (
  value textEmpty,
  value textLength,
  value textIsEmpty,
  value textUncons
) {
  import Maybe.

  textEmpty :: Text.
  textLength :: Text -> Int.
  textIsEmpty :: Text -> Bool.
  textUncons :: Text -> Maybe((Char, Text)).
}
```

`Text.jz` imports the ordinary `Maybe` module so its public type references the
same nominal `Maybe` identity available to consumers. Importing `Text` does not
re-export `Maybe`, `Nothing`, or `Just`.

The module exports no kernel bridge, raw adapter, helper binding, type, class,
or constructor. `Maybe`, `Result`, and `Text` module bodies use two spaces of
indentation.

## Public Semantics

`textEmpty` is the empty immutable text value `""`.

`textLength` returns the number of Unicode scalar values in its argument. It
does not count UTF-8 bytes or expose the stage-0 runtime representation. The
result uses the existing cross-platform `Int` semantics.

`textIsEmpty` returns `True` exactly when its argument contains no scalar
values. It can be implemented in Jazz through exact `Text` equality.

`textUncons` is total:

- `textUncons ""` returns `Nothing`;
- non-empty input returns `Just (first, remainder)`;
- `first` is the first Unicode scalar value as `Char`;
- `remainder` contains every later scalar value in the original order; and
- no normalization, transcoding, file access, or other host effect occurs.

Repeated `textUncons` calls can traverse a `Text` value entirely in process.
This child does not specify asymptotic representation guarantees for the
remainder, but implementations must not perform external I/O per scalar.

## Stage-0 Kernel Adapter

Two compiler-owned kernel intrinsics support the Jazz module:

- `__kernel_textLength :: Text -> Int`; and
- `__kernel_textUnconsRaw :: Text -> [(Char, Text)]`.

The raw decomposition result contains zero elements for empty input and exactly
one `(Char, Text)` tuple for non-empty input. This structural adapter avoids
teaching the builtin catalog, type inference, or runtime evaluator about the
nominal identity of the ordinary `Maybe` module. `Text.jz` converts the raw
zero-or-one list to `Nothing` or `Just`.

Both symbols have `KernelIntrinsic` ownership. The bundled prelude includes the
kernel self-bridges required by normal compilation, but does not generate
public aliases for them. The existing `PreludeTarget` symbols retain their
current public aliases. Low-level runtime application with a non-`Text`
argument reports a deterministic runtime diagnostic; normal source programs
are rejected earlier by type inference.

The raw adapter is private compiler/runtime protocol. Jazz code outside
`Text.jz` must not depend on its representation. A future native runtime may
implement the same operation with a more efficient internal calling convention
as long as the public `Text` module semantics remain identical.

## Data Flow

```text
Jazz consumer
  -> import Text
  -> textLength / textUncons
  -> Jazz-authored Text.jz wrapper
  -> private kernel intrinsic
  -> stage-0 immutable Text runtime value

Future native compiler/runtime
  -> the same Jazz Text.jz public API
  -> versioned native runtime service
```

The canonical frontend and module interface see only ordinary Jazz signatures
and values. No bytecode node, LLVM instruction object, or backend-specific type
enters the parser, analyzer, or public module interface.

## Verification Contract

Tests must prove:

- the builtin catalog records both new symbols as arity-one
  `KernelIntrinsic` values;
- the bundled prelude emits their self-bridges and no public aliases;
- type inference accepts only `Text` arguments and infers the structural raw
  adapter types;
- runtime length counts empty, ASCII, and multibyte Unicode text by scalar;
- runtime raw decomposition returns zero or one tuple with the exact remainder;
- low-level invalid arguments produce deterministic diagnostics;
- the checked-in `Text.jz` module exports only the four public functions;
- the real module loader executes empty checks, scalar length, empty uncons,
  non-empty uncons, and repeated traversal through the public `Maybe` result;
- the four public names remain unavailable from the bundled prelude without an
  explicit `Text` import; and
- `Maybe.jz`, `Result.jz`, and `Text.jz` use two-space module-body indentation.

Focused verification covers the builtin catalog, primitive semantics, runtime
semantics, prelude loading, and module loader. The repository warning matrix,
queue validator, documentation validator, and whitespace check remain final
gates.

## Scope Boundaries

This child does not add checked indexing, slicing, concatenation, builders,
ordering, prefix/suffix/substring search, character classification, bytes,
UTF-8 conversion, host text I/O, process arguments, stack-safe evaluation, the
Jazz-authored lexer, backend-neutral lowered IR, LLVM lowering, object
generation, linking, or a native runtime.

No bytecode format or VM is introduced. The permanent direction remains
canonical typed core, backend-neutral lowered IR, LLVM IR, native linking, and
the native runtime ABI.
