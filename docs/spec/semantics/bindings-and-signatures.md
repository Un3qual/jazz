# Bindings and Signatures Semantics

Status: active (phase 1 partial implementation in `jazz-next`; adjacent monomorphic signatures, empty `@{}:` constrained signatures, and concrete unary non-empty constraints are implemented)
Locked decisions: 2026-03-03
Primary plan: `docs/plans/spec-clarification/2026-03-03/semantics/13-binding-and-signature-coherence.md`

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

## Canonical Contract

1. Type signatures are optional when a binding can be inferred.
2. A type signature, when present, must appear immediately above the binding it annotates.
3. A signature does not float across unrelated declarations or expressions.
4. An empty constrained-signature prefix (`@{}:`) has no semantic obligations and normalizes to the same monomorphic type subset as an ordinary adjacent signature.
5. Non-empty constrained signatures are accepted only for known unary constraint names (`Default`, `Eq`, `Fractional`, `Integral`, `Num`, `Ord`, `Showable`) whose single argument is a concrete `Int`, `Bool`, or nested list of those concrete types. Accepted concrete constraints are annotation-only obligations and normalize to the same monomorphic signature body as an ordinary adjacent signature.
6. Non-empty constrained signatures with duplicate constraint names, unknown constraint names, wrong arity, type-variable arguments, type applications, or function-type constraint arguments must fail deterministically with `E2009`; duplicate constraint names must name the duplicate.
7. Same-scope rebinding is allowed and deterministic: last declaration in the same scope wins.
8. Nested scopes may shadow outer bindings.
9. Non-recursive use-before-definition is invalid and must produce a compile-time error.
10. Recursion is allowed, including both self-recursion and mutual recursion, using fixpoint treatment for recursive groups.
11. Binding references are value snapshots, not live references. Rebinding a name later does not retroactively change previously evaluated values.
12. Rebinding diagnostics are silent by default in this phase; warning emission is available through compiler warning flags.

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
id :: @{Eq(a)}: a -> a.
id = \(x) -> x.
```

## Deferred Work

- Complete recursion-group semantics in `jazz-next` (self + mutual recursion) so implementation fully matches locked policy.
- Add parser-surface tests once parser work lands in `jazz-next`.
- Define type-variable constrained-signature binding/defaulting and inference interaction before any variable-bearing non-empty constrained-signature implementation batch moves into `Ready Now`.
