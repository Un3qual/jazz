# RFC 0014: Typed-core managed Text

Status: Accepted
Date: 2026-08-15
Supersedes: None.

## Decision

Jazz will extend the opt-in Typed Core and backend-neutral Lowered IR profile
with managed `Text` construction and transport, strict equality, length,
append, and append-char.

Inference remains the sole semantic owner. The producer reuses
`TypedTextType`, `TypedManagedTextRecipe`, `TypedTextLiteral`,
`TypedBuiltinName`, and the existing expression nodes. It retains Text through
bindings, parameters, results, captures, direct and closure calls,
conditionals, scalar-pattern-case results, returns, and tail calls. Pattern
scrutinees remain inside the existing immediate-scalar lowering profile.

The approved Text kernel applications are resolved through `BuiltinCatalog`
and must be exactly saturated. A bare kernel builtin, partial application, or
oversaturation is outside this child. Other managed values and non-local calls
retain their existing profile failures.

Lowered IR uses one canonical Text layout:

```text
LoweredLayoutId "jazz.layout.text.v1"
LoweredLayout "jazz.layout.text.v1" LoweredTextLayout
LoweredManagedReferenceRepresentation "jazz.layout.text.v1"
```

Text literals use `LoweredConstructText`. Existing operands, parameters,
temporaries, closure environments, calls, block edges, returns, and tail calls
transport the managed reference.

The selected pure operations use exact semantic runtime-service IDs and
signatures:

```text
jazz.runtime.text.equal.v1       (Text, Text) -> Bool
jazz.runtime.text.length.v1      (Text) -> Int64
jazz.runtime.text.append.v1      (Text, Text) -> Text
jazz.runtime.text.append-char.v1 (Text, Char) -> Text
```

Text `!=` uses the equality service followed by the existing Boolean-not
primitive. Runtime calls remain instructions followed by ordinary return or
continuation behavior; this RFC adds no runtime-call tail terminator.

The lowerer structurally collects Text layout and service requirements from
validated Typed Core. It emits the layout once when any Text recipe is used
and emits only referenced services, deduplicated in equality, length, append,
append-char order. Requirement collection may not infer, resolve, or select
evidence again.

The services are pure backend dependencies, not `RuntimeHost` effects,
Haskell callbacks, public source names, or native symbols. A later native ABI
must implement their existing Jazz-visible semantics.

Ordinary `compile` and `run` remain on canonical core and the reference
interpreter. Public Text behavior, the Typed Core and Lowered IR schemas,
mirrored validators, and Lowered IR version 1 remain unchanged. This decision
authorizes one managed-Text child only.

## Context

The current opt-in Typed Core profile rejects `TTextType` and Text literals
even though the typed schema already defines managed Text recipes and Lowered
IR version 1 already defines managed references, Text layouts, Text
construction, runtime-service declarations, and runtime calls. The Haskell
reference interpreter already owns the public Text semantics.

Transport alone would establish representation plumbing without allowing the
backend-neutral path to inspect or transform Text. The selected scalar-result
kernel adds useful behavior without importing the independent managed-list,
product, variant, module, runtime-host, or native-ABI contracts required by the
rest of the public Text module.

## Consequences

- Text becomes the first non-closure managed source value in the opt-in
  backend profile.
- Text may cross the established callable and CFG boundaries without changing
  their identity, ordering, or failure contracts.
- Exact runtime-service IDs and signatures become durable semantic backend
  dependencies.
- Text inequality requires no extra service.
- Pure Text services do not broaden `RuntimeHost` or claim native execution.
- Pointer representation, allocation, ownership, collection, and destruction
  remain runtime decisions.
- Text literal patterns, managed scrutinees, lists, tuples, ADTs, uncons,
  from-chars, concat, the public imported `Text` module, Text I/O, multi-module
  lowering, LLVM, linking, and native execution require separate contracts.

## Implementation status

Accepted for implementation planning on 2026-08-15. No implementation or
ordinary compile/run behavior is claimed until the queue child is completed
and verified.
