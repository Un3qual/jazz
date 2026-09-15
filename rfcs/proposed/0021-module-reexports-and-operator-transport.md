# RFC 0021: Module re-exports and operator transport

Status: Proposed
Date: 2026-09-15
Supersedes: On acceptance, the no-re-export boundaries in RFCs 0017 and 0019 and the source-local custom-operator transport restriction retained by RFC 0020. All other decisions in those RFCs remain, including RFC 0019's method and instance rules.

## Decision

Allow explicit module exports to select imported declarations, including custom
operators. A public name denotes its original declaration through any number of
facades. A custom operator transports its callable binding, precedence, and
associativity together.

This is a proposed, unimplemented contract. The maintainer approved designing
these features together; the detailed syntax and rules below remain for review.
Current public documentation and behavior remain unchanged until implementation.

### Export and import syntax

Extend the existing export selectors; do not add a re-export statement:

```jazz
module Library::API (
  value Ops::answer,
  type Ops::Box(..),
  class Ops::Equal,
  value (Ops::%%)
) {
  import Library::Operations as Ops.
}
```

The public names are `answer`, `Box` and its selected constructors, `Equal` and
its ordinary method values, and `(%%)`. `Ops` identifies the source of the
export; it does not become part of the exported name. It must be an explicit
module import alias. Export selectors cannot introduce a new public spelling.

- Existing unqualified selectors may select a local declaration or an
  unqualified imported declaration. Local declarations take precedence, as in
  ordinary name resolution. Qualified selectors choose the named import.
- Namespace selectors retain their meaning: `value`, `type`, `constructor`,
  and `class`. Bare named selectors continue selecting all eligible same-text
  entries in the selected local or imported view. Namespace resolution is
  independent: a local value does not hide an imported type of the same name.
- Existing `type T`, `type T(..)`, and `type T(C1, C2)` selectors also work with
  imported types, including `type Ops::T(C1, C2)`. Constructor names in the group
  are interpreted in that type's selected view, not in the facade's local scope.
- `(%%)` and `value (%%)` select a custom operator value. Qualified forms are
  `(Ops::%%)` and `value (Ops::%%)`. Other namespace prefixes reject operators.
- An ordinary selective import may contain parenthesized custom operators:
  `import Library::API ((%%), answer).` Alias imports remain
  `import Library::API as API.` Aliases and selective lists remain mutually
  exclusive.
- Omitted export lists retain current behavior: export locally owned ordinary
  declarations, with no implicit re-exports and no implicit custom-operator
  exports. An operator becomes public only through an explicit selector.
  An empty export list still exposes no names.
- Built-in operator notation retains RFC 0020's fixed mapping to ordinary
  names such as `add`. Export/import those function names normally; `(+)` is
  not a custom-operator export selector.

### Visibility and original identity

A re-export can select only a declaration visible through the facade's imports.
It cannot recover a hidden name from supporting type or instance metadata.

Each exported value keeps the original binding reference and type scheme. Each
exported type or class keeps its original nominal identity. A facade introduces
no wrapper function, constructor, replacement class, or new implementation.
Runtime publication reuses the original cell, including laziness, captures,
failure behavior, and effect behavior.

`type Ops::T(..)` exports all constructors visible in `Ops` for that type,
not all constructors of its original definition. An abstract import stays
abstract. Selecting an unavailable constructor is an error. `class Ops::C`
selects that class and its public ordinary methods using the existing rule;
selecting one method alone need not expose the class name.

An exported abstract type retains its original definition and parameter kinds
for checking consumers, even when no exported value mentions it. This supporting
definition does not make its hidden constructors selectable.

Instances remain transitive and independent of name selection, as specified by
RFC 0019. Importing a facade with no public names can still bring implementations
into scope. Re-exporting or importing a class through multiple paths must not
duplicate its implementations or default-method cells. A consumer may call
`API::Equal::equal` and use `API::Equal` in constraints and impl heads; these
refer to the original class.

### Duplicate paths and conflicts

Identity, not the immediate provider module, determines whether two public
entries are the same declaration:

- Repeated imports, a direct import plus a facade, and two facades exposing the
  same original declaration are compatible in the same namespace.
- Compatible unqualified imports combine their visible constructor subsets for
  the same original type. If A exposes `T(C1, C2)` and facade B exposes `T(C1)`,
  importing both lets `type T(..)` re-export both constructors, in either import
  order. Qualified selectors use only the named alias's view: `type B::T(..)`
  still exposes only `C1`. Hidden supporting metadata never widens either view.
- Different original declarations exposed under the same unqualified name in
  the same namespace are an import collision. Qualification keeps them separate.
- Two export selectors producing the same public name and original declaration
  coalesce. Exact duplicate selector syntax keeps its existing parser rejection.
  Different declarations producing the same public name and namespace are an
  export error, even when selected through distinct aliases.
- Same-text names in different namespaces retain current behavior.
- Provider selection must not depend on `Map.union` bias or import traversal
  order. Diagnostics retain the participating source locations.

### Operator use and fixity

An unqualified import exposes `(%%)`, `a %% b`, `(a %%)`, and `(%% b)`.
An alias import exposes `(API::%%)`, `a API::%% b`, `(a API::%%)`, and
`(API::%% b)`, without exposing unqualified `%%`. The qualifier, `::`, and
operator spelling must be adjacent. The existing symbol vocabulary is unchanged.

All forms resolve to the same original ordinary function. Explicit type
application to a parenthesized operator value follows ordinary value rules.
Sections retain RFC 0020's argument order and evaluate the captured operand
once at section construction.

The defining module owns fixity. Qualified and unqualified uses apply the same
precedence and associativity; re-exports cannot alter them. The current
precedence range, tier aliases, and non-associative chain rules remain. Two
different operators at equal precedence follow the existing grouping rules.

An operator is exportable only when it has both a valid declaration and an
executable binding in the defining module. Exporting a declaration without a
binding fails. The binding may be an ordinary callable alias, closure, or
constrained function supported by the current language.

Different original operators with the same unqualified spelling conflict even
when their fixities happen to match. Multiple paths to the same operator are
compatible and carry the same defining fixity. Alias imports allow two
same-spelled operators with different fixities to coexist.

Imported operators cannot be rebound, re-signed, or assigned a new local fixity.
A local operator declaration with the same spelling as an unqualified imported
operator is an error. Import that dependency through an alias to define a
distinct local operator of the same spelling. Existing local declaration order,
rebinding behavior, and the requirement to declare local fixity before use
remain for operators that do not conflict with imports.

### Module discovery and diagnostics

Imports remain module-scoped, including imports written after a use. The compiler
discovers module headers and imports before parsing expression bodies, resolves
dependencies, then parses each body once with its imported operator environment.
Local operator declarations continue updating that environment in source order.
This does not require imports to move to the beginning of existing source files.

Discovery reads the token stream and uses the same header/import grammar as the
full parser. Its import result also supplies parser alias visibility, including
aliases imported after use, replacing the separate alias pre-scan. It does not
group expressions with provisional precedence. Full parsing owns body syntax,
binding IDs, and local operator declarations. Import discovery never follows an
import-looking token inside a nested expression,
string, character literal, or comment.

The dependency graph remains acyclic, traversed deterministically by module
name. A cycle through a facade is still an ordinary module cycle. Export
resolution occurs after dependencies are available, so no export fixpoint or
separate re-export graph is required.

Error selection follows the new dependency boundary: source loading/tokenizing
and header/import syntax precede dependency discovery; dependency failures
precede body parsing; public-export validation follows body/name discovery.
This can report a dependency failure before an unrelated importer-body syntax
error. Within each phase retain existing source order and structured spans.

Use the existing diagnostic families: `E4003` for cycles, `E4004` for module
syntax, current import collision/visibility codes for imported operators and
names, and `E4015` for invalid or conflicting export selections. Conflicts point
at the offending import/export/declaration and relate the earlier participant.
For conflicting exports, those locations are the later and earlier selectors,
including when their public names match but their qualifiers differ. Same-spelled
selectors in different namespaces retain distinct source locations.
Messages render the authored spelling, such as `API::%%`, never the internal
encoded operator binder. No new error-code family is needed.

### Complete example and acceptance

```jazz
module Library::Operations (value (%%), value answer) {
  operator %% precedence 6 left.
  (%%) :: Int -> Int -> Int.
  (%%) = \(left, right) -> left - right.
  answer = 42.
}
```

```jazz
module Library::API (value (Ops::%%), value Ops::answer) {
  import Library::Operations as Ops.
}
```

```jazz
module App::Main {
  import Library::API ((%%), answer).
  import Library::API as API.
  (10 %% 3, (%%) 10 3, (10 %%) 3, (%% 3) 10,
   10 API::%% 3, (API::%%) 10 3,
   (10 API::%%) 3, (API::%% 3) 10, 2 * 10 %% 3, answer).
}
```

The result is `(7, 7, 7, 7, 7, 7, 7, 7, 14, 42)`: precedence 6 binds
`%%` more tightly than the existing multiplication precedence 5.
With this left-associative declaration, `10 API::%% 3 API::%% 1` evaluates to `6`.

Acceptance also covers abstract/grouped type re-exports, imported class methods
and generic instances, diamond imports, private selections, conflicting origins,
alias isolation, non-associative operators, section evaluation, and Prelude-free
ordinary function dispatch. Existing successful programs retain their results
and effects. The dependency-first diagnostic order applies to all modules,
including those using no new selectors. Jazz is unreleased; this becomes the
single parsing path, without an opt-in flag or legacy parsing mode.

The hosted frontend must retain its existing supported domain and gain the new
selector and qualified-operator syntax, explicit imported-fixity input, and
corresponding canonical lowering. Differential tests compare complete values
and structured failures using the same supplied operator environment. The
Haskell module resolver continues owning graph loading and semantic analysis;
this proposal does not claim a Jazz-authored semantic compiler.

## Context

Jazz already has namespace-aware export inventories, nominal declaration
identities, explicit dependency interfaces, ordinary operator calls, and runtime
cells keyed by resolved references. Re-exports and operator transport extend
these same boundaries. Defining them together prevents incompatible answers to
visibility, original ownership, and duplicate import paths.

The significant ordering change is making imported fixity available before
expression parsing. Reusing the token stream and existing import parser is
smaller than introducing an unresolved-expression representation or parsing a
body twice. The existing acyclic module graph supplies the dependency order.

Reuse existing compiler records, names, references, selector constructors, and
operator tables. The implementation adds original-name and exported-fixity
metadata to their current owners; it does not require new export/target/header
record families, a new operator AST, or forwarding runtime objects.

## Consequences

Libraries can expose complete APIs through facade modules without wrappers or
identity changes. The implementation has three milestones: re-export identity
and publication; imported operator parsing and transport; combined conformance
and documentation. They share one contract and release boundary.

The implementation must preserve source diagnostics, namespace separation,
hidden support metadata, existing instance coherence, and dependency execution
rules. Supporting types and instances must not accidentally become selectable
names. Changes to default export visibility are deliberately limited to explicit
selectors, keeping existing hidden operator helpers private.

This batch adds explicit named re-exports and transport of existing custom
operator spellings. Whole-module wildcard re-exports, export renaming, new
operator characters, package resolution, cyclic modules, and effect-system
changes require separate contracts.

Implementation ownership, Haskell feature research, test cases, and execution
steps live in the [implementation plan](../../.codex/plans/2026-09-15-module-reexports-and-operator-transport.md).
