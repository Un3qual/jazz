# Authoritative Syntax

Status: active (module/import and lambda slices are implemented in `jazz-next`; `class`/`impl` abstraction declaration forms are reserved by parser diagnostics, non-canonical `trait` declarations reject explicitly, and abstraction semantics remain pending)
Locked decisions: 2026-03-02
Primary plan: `docs/plans/spec-cleanup/2026-03-02/decisions/01-authoritative-syntax.md`

Implementation note (2026-05-25): `jazz-next` accepts canonical brace-bodied module declarations (`module A::B { ... }`), canonical lambdas (`\(x) -> expr`) including pattern-shaped parameters such as `_`, `(left, right)`, `[head | tail]`, and `Just item`, list literals/types, tuple literals, and concrete tuple signature types in the active parser/type/runtime/CLI path. The active parser also reserves top-level and module-body `class`/`impl` declaration-shaped forms such as `class Eq { ... }` with deterministic unsupported-syntax diagnostics, and rejects non-canonical `trait ... { ... }` declaration-shaped forms with diagnostics that point future abstraction syntax back to `class`/`impl`; ordinary uses of those identifiers as binding names, signature names, or qualified aliases remain valid, and abstraction semantics remain future work.

## Purpose

Define one canonical surface syntax for functions, modules/imports, abstractions, and collections so specs, examples, and future compiler work converge on a single contract.

## Implementation Target

- All new parser/analyzer/codegen work for this decision must land in `jazz-next/`.
- `jazz-hs/` and `jazz2/` are read-only legacy references and may only be used as historical evidence.

## Canonical Syntax Contract

1. **Bindings and function definitions**
   - Canonical form: `name = expr.`
   - Optional type signature form: `name :: Type.` directly above its binding.
   - Lambda form: `\(args) -> expr`; arguments may be identifiers or active
     pattern forms.

2. **Modules and imports**
   - Module declaration: `module A::B { ... }`
   - Import declaration: `import A::B`
   - Qualified forms remain canonical where needed: `import A::B as B`, `import Std::List (map, filter)`.
   - In the active `jazz-next` resolver, an explicit symbol list is also a visibility boundary: `import Std::List (map, filter)` exposes only those selected names from that import, and references to other exported names from the same dependency report `E4011`.
   - Alias imports are parsed, collision-validated, and do not expose dependency exports as unqualified names; bare references to bindings available only through an alias import report `E4012`.
   - Qualified alias lookup uses `Alias::symbol`; unknown aliases report `E4013`, and aliases that do not export the requested symbol report `E4014`.
   - Import aliases live in a module-alias namespace for qualified lookup, so value binders do not shadow `Alias` in `Alias::symbol`.

3. **Abstractions**
   - Canonical keywords: `class` and `impl`.
   - `trait` is non-canonical and retained only in legacy-reference discussion until migration work is complete.
   - Active `jazz-next` parser behavior reserves top-level and module-body `class`/`impl` declarations and rejects them with deferred class/impl semantics diagnostics until the abstraction model is implemented.
   - Active `jazz-next` parser behavior rejects non-canonical top-level and module-body `trait` declarations with unsupported-syntax diagnostics that point future abstraction syntax back to `class`/`impl`.

4. **Collections**
   - List literal/type forms remain canonical: `[1, 2, 3]`, `[a]`.
   - Tuple literal/type forms remain canonical for fixed-arity values:
     `(1, True)`, `(Int, Bool)`.
   - Canonical combinator order is function-first:
     - `map f xs`
     - `filter p xs`

5. **Expression terminators and application**
   - Dot-separated root forms remain canonical: `expr.`
   - Whitespace application remains canonical: `f x y`
   - `$` remains canonical low-precedence right-associative application.

## Legacy Compatibility and Migration Notes

- Historical collection-first examples (`map xs f`, `filter xs p`) are non-canonical and should be rewritten in active docs.
- Historical `trait` examples are non-canonical and should be rewritten to `class` in active docs/spec text.
- During migration, legacy forms may remain in archival references, but new docs/tests must only introduce canonical forms.

## Non-Goals (This Item)

- Defining module loader/file-system resolution semantics.
- Defining effect-system semantics for `!`.
- Completing parse-only runtime/typechecker feature implementation.
- Defining backend strategy beyond currently locked governance decisions.

## Evidence Inputs Used for Decision

- `README.md` (top-level syntax claims and examples)
- `docs/jazz-language-state.md` (recorded mismatch inventory)
- `jazz-hs/src/Parser/Lang.hs` (legacy parser evidence)
- `jazz-hs/test/ParserSpec.hs` (legacy parser behavior evidence)
- `jazz-hs/static/Prelude.jz` (legacy reference syntax samples)
- `jazz-hs/src/Types.hs` (legacy builtin collection signature evidence)

## Progress Checklist

- [x] Canonical syntax decision recorded in a normative spec doc.
- [x] Legacy/non-canonical forms identified with migration notes.
- [x] `jazz-next/` parser tests aligned to the implemented function/module/lambda slice of the canonical contract.
- [x] `jazz-next/` implementation aligned to the implemented function/module/lambda slice of the canonical contract.
- [x] `jazz-next/` parser lowers canonical `class`/`impl` declaration forms as inert AST nodes.
- [x] `jazz-next/` parser rejects non-canonical `trait` declaration forms while preserving ordinary identifier uses.
- [ ] Summary docs fully converge with implementation behavior.
