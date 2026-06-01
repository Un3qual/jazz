# Trait Vocabulary

Status: active vocabulary contract
Primary plan: `docs/plans/spec-clarification/2026-03-02/abstractions/08-trait-vocabulary-and-capability-model.md`

## Canonical Vocabulary

Jazz uses Haskell-like names for the core capability vocabulary:

- `Eq`
- `Ord`
- `Num`
- `Integral`
- `Fractional`
- `Showable`
- `Default`

`Showable` is the intentional compatibility exception to exact Haskell naming. Haskell calls this capability `Show`, but the active Jazz constrained-signature surface and legacy vocabulary already use `Showable`; any future `Show` alias or migration must be staged explicitly instead of silently renaming the accepted compiler vocabulary.

The canonical declaration keyword is `class`. Implementations use `impl`.

## Non-Canonical Vocabulary

- `trait` is non-canonical declaration syntax.
- `Collection`, `Orderable`, and `Numeric` are not canonical core capability names.
- Non-canonical names may be used in prose only when marked as examples, aliases, historical notes, or future library names.

## Implementation Status

`jazz-next` parses and lowers canonical empty `class` and `impl` declarations,
validates duplicate class declarations and duplicate concrete impl facts, and
uses visible concrete class/impl facts for constrained-signature validation.
Non-empty class/impl bodies reject at parse time until method syntax and
dispatch have a concrete contract. The default bundled prelude now declares the
canonical class vocabulary listed above and includes the first inert default
concrete impl facts for `Int`, `Float`, and `Bool` capability constraints.

Those declarations still add no method lookup, dispatch, defaulting, or runtime
values. `jazz-next` rejects non-canonical `trait` declarations. Ordinary
bindings, signatures, and qualified import aliases may still use these words
where the parser permits them.
