# Bindings and Signatures Semantics

Status: active (`jazz-next` structured-signature rebase closed for the supported monomorphic subset; adjacent monomorphic signatures, width-specific numeric signature names, `Int`/`Float` aliases, empty `@{}:` constrained signatures, concrete unary non-empty constraints validated against class/impl facts, generalized variable constrained-signature schemes with per-use evidence checks, deterministic unsupported-variable diagnostics, unsupported constrained-signature primary spans, structural list/tuple/ADT equality over equality-supported element and constructor payload types, and ordinary binding type schemes with fresh per-use instantiation are implemented; inferred class constraints, broad defaulting, runtime evidence, and explicit type application remain future verifier-backed child rows)
Locked decisions: 2026-03-03
Primary plan: `docs/plans/2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md`

## Purpose

Define one canonical contract for declaration order, type-signature attachment, shadowing/rebinding, and recursion so parser/analyzer behavior can converge without order-dependent ambiguity.

## Scope

In scope:
- top-level and nested `let`-style binding behavior,
- type-signature placement and attachment rules,
- same-scope rebinding behavior,
- recursion and forward-reference policy.

Out of scope:
- higher-rank polymorphism,
- module import/name-resolution behavior,
- effect typing semantics.

Adjacent future generic ADT work:

- Generic ADT constructor schemes are separate from ordinary binding
  polymorphism. The landed `data Maybe a = Nothing | Just a.` slice gives
  constructors fresh per-use schemes, while the ordinary binding child now
  generalizes eligible user bindings after binding or recursive-group
  inference.
- The dedicated polymorphism/defaulting/typeclass-solver contract is accepted as
  `JN-TYPE-SOLVER-CONTRACT-001`, but implementation is split into child rows.
  The first executable child, ordinary binding type schemes and per-use
  instantiation, has landed.

Adjacent numeric-width work:

- `Int` remains accepted as the default integer signature spelling and maps to `Int64`.
- `Float` maps to `Float64`.
- Explicit width names (`Int8`, `Int16`, `Int32`, `Int64`, `UInt8`, `UInt16`, `UInt32`, `UInt64`, `Float16`, `Float32`, and `Float64`) are ordinary monomorphic signature types before broader polymorphic/defaulting work.
- Integer literals can satisfy explicit integral-width annotations.
- Decimal fractional literals can satisfy explicit `Float`, `Float16`, `Float32`, and `Float64` binding signatures when the literal is the direct binding value; same concrete `Float`/`Float16`/`Float32`/`Float64` arithmetic and comparison/equality are supported with width-preserving runtime float results; structural list/tuple equality is supported only when every nested element type is equality-supported, and structural ADT equality is supported only when every declared constructor payload type is equality-supported.

## Canonical Contract

The numbered items below describe the active implemented baseline unless the
accepted type-solver contract section explicitly supersedes them in a later
implementation child.

1. Type signatures are optional when a binding can be inferred.
2. A type signature, when present, must appear immediately above the binding it annotates.
3. A signature does not float across unrelated declarations or expressions.
4. An empty constrained-signature prefix (`@{}:`) has no semantic obligations and normalizes to the same monomorphic type subset as an ordinary adjacent signature.
5. Non-empty concrete constrained signatures are accepted when the unary constraint name has a visible explicit-parameter `class` declaration, such as `class Eq(a) { }.`, and the single concrete argument has a visible matching concrete `impl` fact. Concrete arguments are limited to `Int`, `Float`, width-specific numeric types, `Bool`, nested lists, or tuple compositions of those concrete types. Accepted concrete constraints are annotation-only obligations and normalize to the same monomorphic signature body as an ordinary adjacent signature. The default bundled prelude provides canonical unary vocabulary class declarations plus the first default concrete impl facts for `Eq`, `Ord`, `Num`, `Integral`, `Fractional`, `Default`, and `Showable` over `Int`, `Float`, and `Bool` where currently scoped; explicit-prelude and no-prelude entry points do not inherit those bundled facts.
6. Non-empty constrained signatures are also accepted for known unary constraint names whose single argument is a lower-case type variable, when every lower-case type variable in the signature body appears in at least one supported unary constraint and every constrained variable appears in the body. Repeated source variable names in one signature map to the same fresh internal inference variable for that binding. These accepted variable constraints are monomorphic and annotation-only: they do not generalize at later use sites, do not introduce defaulting, do not call a typeclass solver, and do not add runtime dispatch.
7. Non-empty constrained signatures with duplicate constraint names, missing class/impl facts for concrete constraints, arguments that do not match the declared class arity, unconstrained body variables, unused constrained variables, type applications, or function-type constraint arguments must fail deterministically with `E2009`; duplicate constraint names must name the duplicate, arity errors must name the expected and actual argument counts, unsupported variable-bearing constrained signatures must name the supported unary-constraint requirement, and the diagnostic primary span must remain attached to the signature statement.
8. Same-scope rebinding is allowed and deterministic: last declaration in the same scope wins.
9. Nested scopes may shadow outer bindings.
10. Non-recursive use-before-definition is invalid and must produce a compile-time error.
11. Recursion is allowed, including both self-recursion and mutual recursion, using fixpoint treatment for recursive groups.
12. Binding references are value snapshots, not live references. Rebinding a name later does not retroactively change previously evaluated values.
13. Rebinding diagnostics are silent by default in this phase; warning emission is available through compiler warning flags.

## Accepted Type Solver Contract

`JN-TYPE-SOLVER-CONTRACT-001` was accepted on `2026-06-04`. It locks one broad
solver semantics agreement while requiring implementation to land through
verifier-backed child rows.

1. Ordinary user bindings generalize after their binding or recursive binding
   group has been inferred and solved. Generalization quantifies type variables
   that are not fixed by the surrounding environment, an adjacent concrete
   monomorphic signature, or unresolved ambiguity.
2. Recursive groups are solved with shared monomorphic placeholders, then
   generalized as one unit after the group constraints are solved. This does
   not introduce polymorphic recursion.
3. Each use site instantiates a generalized scheme with fresh type variables
   before unification, so independent uses can refine to different concrete
   types.
4. Inferred class constraints are represented in the scheme context. Concrete
   constraints solve against visible `class` declarations and concrete `impl`
   facts using the declared class arity. Constraints over generalized variables
   remain on the scheme until a use site supplies concrete evidence or reaches
   the final defaulting phase.
5. Defaulting runs after unification and visible class/impl solving, before
   ambiguity diagnostics. Existing numeric literal defaults are preserved:
   ambiguous integer literals default through `Int`/`Int64`, and ambiguous
   fractional literals default through `Float`/`Float64`. This phase must not
   reopen primitive implicit integer-to-float promotion or implicit mixed-width
   behavior.
6. Variable constrained signatures such as `@{Eq(a)}: a -> a` graduate from the
   current monomorphic annotation-only behavior to generalized constrained
   schemes when the solver-backed constrained-signature child lands. Concrete
   and currently monomorphic constrained signatures are not reworked by the
   first ordinary-binding schemes child.
7. Diagnostics must remain deterministic: unsolved constraints name missing
   class/impl evidence, duplicate constraints report the duplicate in source
   order, arity errors name expected and actual argument counts, unsupported
   type applications remain explicit unsupported-type-application errors until
   that syntax has its own contract, and un-defaulted variables report an
   ambiguity/defaulting failure with the relevant binding or signature span.

Out of scope for this accepted contract:

- runtime dictionary representation or runtime evidence,
- abstraction method dispatch,
- explicit type application,
- higher-rank polymorphism,
- generic constructor pattern typing,
- module/import behavior,
- effect typing,
- primitive mixed-width behavior or implicit numeric promotion,
- reworking the completed parser/type AST rebase,
- arrow associativity,
- concrete or monomorphic constrained-signature behavior,
- generic ADT constructor schemes,
- any `jazz-hs/` or `jazz2/` work.

## Decision Matrix: Baseline vs Canonical

| Scenario | Current behavior (observed) | Canonical contract |
| --- | --- | --- |
| Declaration with no signature | Inferred via existing analyzer constraints. | Valid; infer type when possible. |
| Signature directly above declaration | Partially modeled and order-sensitive today. | Valid; signature attaches to next declaration only. |
| Declaration before signature (`x = 1.` then `x : Int.`) | Parsed as separate expressions; linkage ambiguous. | Invalid ordering; emit compile-time diagnostic. |
| Signature separated by non-signature expression | Parsed but grouping is ambiguous. | Invalid; emit compile-time diagnostic. |
| Duplicate declaration in same scope | Behavior varies by pass ordering. | Valid; deterministic `last wins`. |
| Non-recursive forward reference (`x = y.` then `y = 1.`) | Currently under-specified and order-sensitive. | Invalid; emit unbound/use-before-definition diagnostic. |
| Self recursion (`fact = ... fact ...`) | Partially supported by fixpoint-style code paths. | Valid by contract. |
| Mutual recursion (`even` <-> `odd`) | Not explicitly locked in docs. | Valid by contract as a recursive group. |

## Candidate Option Matrix (Decision Record)

### Gate A: Signature attachment model

- A1 selected: immediate adjacency (`sig` must be directly above binding).
- A2 rejected: name-based attachment before or after declaration in same scope.
- A3 rejected: remove standalone signature syntax.

Expected behavior under A1:
- fast/clear diagnostics for misplaced signatures,
- no cross-statement signature search,
- parser/analyzer contract remains local and deterministic.

### Gate B: Redeclaration policy

- B3 selected: allow same-scope rebinding with deterministic `last wins`.
- B1 rejected: hard error on same-scope redeclaration.
- B2 rejected: nested-only shadowing with same-scope rejection.

Expected behavior under B3:
- deterministic reading order in a scope,
- compatibility with existing rebinding-heavy examples,
- optional warnings handled by tooling flags instead of hard errors.

### Gate C: Recursion policy

- C2 selected: unrestricted recursion with fixpoint treatment.
- C1 rejected: recursion allowed only with explicit signature.
- C3 rejected: recursion fully disallowed for this phase.

Expected behavior under C2:
- self recursion and mutual recursion are both valid,
- recursive groups may rely on shared constraint solving.

### Gate D: Forward-reference policy

- D1 selected: non-recursive use-before-definition is invalid.
- D2 rejected: allow all forward references in same scope.

Expected behavior under D1:
- `x = y.` before `y` definition is rejected unless part of a valid recursive group,
- diagnostics should name both use site and missing/not-yet-bound symbol.

### Gate E: Rebinding warning policy

- E2 selected: warnings are optional and controlled by compiler warning flags.
- E1 rejected: always warn on same-scope rebinding.
- E3 rejected: permanently silent with no warning pathway.

Expected behavior under E2:
- current default remains silent,
- warning pathway is available via CLI/config flags without changing semantic validity.

## Canonical Examples

Valid:

```jz
sum :: Int -> Int -> Int.
sum = a -> b -> a + b.
```

```jz
applyToOne :: @{}: (Int -> Int) -> Int.
applyToOne = \(f) -> f 1.
```

```jz
x :: @{Eq(Int)}: Int.
x = 1.
```

```jz
id :: @{Eq(a)}: a -> a.
id = \(x) -> x.
```

```jz
counter = 0.
counter = counter + 1.
```

```jz
even = n -> case n {
  | 0 -> True
  | _ -> odd (n - 1)
}.
odd = n -> case n {
  | 0 -> False
  | _ -> even (n - 1)
}.
```

Invalid:

```jz
x = 1.
x : Int.
```

```jz
x : Int.
y = 1.
x = 2.
```

```jz
x = y.
y = 1.
```

```jz
bad :: @{Eq(a)}: a -> b.
bad = \(x) -> x.
```

## Deferred Work

- Implement the remaining accepted type-solver contract through verifier-backed
  child rows. Ordinary binding type schemes, per-use instantiation, and
  solver-backed variable constrained-signature schemes are landed; inferred
  class constraints, broad defaulting, runtime evidence, and explicit type
  application remain later children.
