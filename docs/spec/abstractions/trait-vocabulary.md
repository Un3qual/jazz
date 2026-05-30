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

`jazz-next` parses and lowers canonical `class` and `impl` declaration-shaped syntax as inert declaration nodes. Those declarations intentionally add no class environments, solver obligations, method lookup, dispatch, defaulting, or runtime values yet. It still rejects non-canonical `trait` declarations. Ordinary bindings, signatures, and qualified import aliases may still use these words where the parser permits them.
