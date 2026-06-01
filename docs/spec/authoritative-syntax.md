# Authoritative Syntax

Status: active (module/import and lambda slices are implemented in `jazz-next`; `class` declarations parse with signature-only method metadata, empty `impl` declarations parse and participate in environment validation, class method body/default syntax and `impl` bodies reject explicitly, non-canonical `trait` declarations reject explicitly with no compatibility path, and abstraction dispatch semantics remain pending)
Locked decisions: 2026-03-02
Primary plan: `docs/plans/spec-cleanup/2026-03-02/decisions/01-authoritative-syntax.md`

Implementation note (2026-06-01): `jazz-next` accepts canonical brace-bodied module declarations (`module A::B { ... }`), canonical lambdas (`\(x) -> expr`) including pattern-shaped parameters such as `_`, `(left, right)`, `[head | tail]`, `Just item`, and `whole @ Just item`, list literals/types, tuple literals, and concrete tuple signature types in the active parser/type/runtime/CLI path. The active parser also lowers top-level and module-body `class` declarations with signature-only method metadata plus `impl` declaration forms such as `class Eq { eq :: Self -> Self -> Bool. }` and `impl Eq(Int) { }`, preserving method signature payloads and concrete impl target types for later phases. Duplicate class declarations reject with `E1004`, duplicate class method signatures reject with `E1006`, duplicate concrete impl facts reject with `E1005`, and concrete constrained signatures require a visible class declaration plus matching concrete impl fact before normalizing to the monomorphic signature body. Class method body/default syntax and non-empty `impl` bodies reject explicitly; method dispatch/runtime abstraction semantics remain future work. Non-canonical `trait ... { ... }` declaration-shaped forms reject with diagnostics that point future abstraction syntax back to `class`/`impl`. `trait` has no active compatibility or deprecation-warning path; ordinary uses of those identifiers as binding names, signature names, or qualified aliases remain valid.

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
   - `trait` is non-canonical, is never accepted as an active compatibility alias, and is retained only in archival legacy-reference discussion.
   - Active `jazz-next` parser behavior lowers top-level and module-body `class` declarations with signature-only method metadata and `impl` declarations with concrete target types.
   - Active `jazz-next` parser behavior rejects class method body/default syntax, non-signature class body items, and non-empty `impl` bodies.
   - Active `jazz-next` analyzer/type behavior rejects duplicate class declarations, rejects duplicate class method signatures, rejects duplicate concrete impl facts, and validates concrete constrained signatures against known class/impl facts.
   - Active `jazz-next` runtime behavior keeps class/impl declarations inert; they do not produce values, dictionaries, method lookup, or dispatch.
   - Active `jazz-next` parser behavior rejects non-canonical top-level and module-body `trait` declarations with unsupported-syntax diagnostics that point future abstraction syntax back to `class`/`impl`.

   Deferred abstraction semantics:
   - keep method dispatch, dictionary passing, default methods, superclass
     constraints, overlap, orphans, and runtime behavior out of the first
     environment-validation slice.

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
- Historical `trait` examples are non-canonical archival evidence only and should be rewritten to `class` in active docs/spec text. Active `jazz-next` must not accept `trait`, even temporarily.
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
- [x] `jazz-next/` validates class/impl environments for duplicate class names, duplicate concrete impl facts, and concrete constrained signatures.
- [x] `jazz-next/` parser rejects non-empty `class`/`impl` declaration bodies instead of discarding deferred method syntax.
- [x] `jazz-next/` parser preserves signature-only class method metadata and rejects duplicate methods plus method body/default syntax.
- [x] `jazz-next/` parser rejects non-canonical `trait` declaration forms while preserving ordinary identifier uses.
- [ ] Summary docs fully converge with implementation behavior.
