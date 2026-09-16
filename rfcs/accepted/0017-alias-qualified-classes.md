# RFC 0017: Alias-qualified classes

Status: Accepted
Date: 2026-09-09
Supersedes: None.

Scope amendment: During implementation the maintainer explicitly deferred all
bootstrap-related work. This batch implements the Haskell compiler only;
hosted syntax and lowering parity are deferred.

> **2026-09-15 amendment:** The deferred hosted frontend work and hosted regression obligations below are retired by [RFC 0022](0022-hosted-compiler-removal.md). The Haskell alias-qualified class contract remains in force.

## Decision

Aliased module imports expose public classes through the import alias in the
Haskell compiler.

After `import Lib::Facts as Facts.`, an exported class `Eq` is available as
`Facts::Eq`. Its method `equals` can be referenced as
`Facts::Eq::equals 1 1`. The same qualified class name is valid in a signature
constraint and as the class of an impl declaration, such as
`impl Facts::Eq(LocalType) { ... }.`. Existing constraint notation, concrete
impl requirements, method signatures, purity checks, and evidence selection
rules continue to apply.

The alias does not expose `Eq` or `Eq::equals` unqualified. A separately
visible class with that spelling continues to resolve to its own identity.
Class declarations still introduce an unqualified class owned by the declaring
module; `class Facts::Eq(a) { ... }.` is rejected.

## Context

Before this change, aliased imports exposed public values, constructors, and
type identities while excluding the capability namespace. Module analysis
consequently filtered out associated class facts and evidence candidates.
The module loader suite continues to verify that an alias-only import does
not enable unqualified class-method dispatch.

Unqualified imports can already expose public classes and their concrete
implementation evidence. Completing the alias form lets libraries use classes
from multiple modules without opening their names into the same source scope.

## Names, identity, and visibility

- A qualified class reference has exactly two adjacent components,
  `Alias::Class`. A qualified class-method expression has exactly three,
  `Alias::Class::method`. This introduces no arbitrary namespace traversal.
- The first component must resolve to an explicit import alias. Class lookup
  uses the dependency's public capability inventory; a same-text value or type
  cannot satisfy it. Existing two-component expression lookup is preserved.
- Qualified class references denote the original module-owned class identity.
  Aliases are source spellings, not new classes or implementation owners.
- Importing one module through two aliases, or both aliased and unqualified,
  must not duplicate the same implementation evidence. Distinct declarations
  that violate the existing duplicate/ambiguity rules remain errors.
- Only the evidence associated with selected public classes is imported, under
  the same policy as an unqualified import. Private classes remain inaccessible.
  Exported values retain their existing hidden type/evidence metadata behavior.
- An impl declared for an imported class is owned by the declaring module and
  uses the existing impl visibility policy. This proposal does not make impls
  transitively public or introduce a separate impl-import mechanism.
- Imported classes remain ineligible for re-export. Omitted export lists still
  export only owned declarations.

Method references must work in all contexts supported for ordinary
`Class::method`: direct calls, stored values, partial applications, explicit
type application, and constrained function bodies. Source qualification must
resolve before inference and runtime dispatch; neither phase should introduce
an independent alias lookup table or string-based fallback.

## Diagnostics and frontend agreement

Malformed or overlong qualification is a parser error. An unknown alias,
private or missing class, and a missing method are distinct failures, reported
at the relevant source component. Existing diagnostic families should be
retained where their meanings apply. A qualified class reference must never
fall back to an ambient or same-text class after lookup fails.

The Haskell parser and canonical-core lowering preserve existing
two-component names and point/range adapter contracts. A structured
representation must distinguish a module alias, a class, and a method rather
than hiding an additional separator inside an identifier.

The hosted Jazz frontend is outside this batch. Its implementation and corpus
are unchanged. Existing comparisons that exercise newly accepted qualification
or its parser diagnostics can diverge from Haskell until bootstrap work is
explicitly resumed; these are recorded as deferred parity work.

## Implementation boundaries and acceptance evidence

Extend the existing parser/name representations, resolver import validation
and rewriting, public import inventory, and module interface assembly.
Feed the resolved class and method identities into the existing type inference
and runtime evidence paths. Change those consumers only where needed to retain
the existing dispatch guarantees.

Acceptance requires behavior tests for:

- Qualified method execution, constrained signatures, and a qualified impl
  head using a local concrete type.
- Stored and partially applied methods, explicit instantiation, and evidence
  transport through imported functions.
- Two modules exporting same-spelled classes, plus aliased and unqualified
  access to one class without duplicate evidence.
- Private classes, unknown aliases/classes/methods, namespace mismatches,
  unqualified leakage, duplicate concrete impls, and continued rejection of
  re-exports and qualified class declarations.
- Haskell parser and canonical-core lowering, including malformed qualification
  and existing two-component syntax.

Run focused module, loader, parser, capability, and frontend conformance suites,
then the full supported compiler suite and repository quality gates. Existing
hosted suites run only as regression checks, with no bootstrap feature work. Update
`docs/language/modules.md`, `docs/language/capabilities.md`, and the relevant
reference grammar and resolution contracts with executable examples when the
implementation lands. Record the accepted implementation batch under
`.codex/plans/` and dispatch it through `.codex/execution/queue.md`.

## Alternatives

Re-exports would support facade modules but require a separate decision about
transitive ownership, constructor visibility, and evidence publication.
Cross-module operators require a separate import/fixity contract because
operators participate in parsing. Neither is required for qualified classes.

A method-call-only extension would be smaller but leave qualified classes
unusable in signatures and impl heads. The selected scope completes class
references across those existing language forms as one batch.

## Consequences

The cost is a syntax and resolver change in the Haskell frontend. No new class
features, effect system, package semantics, overlap/orphan policy, backend,
re-export syntax, or operator transport is included. Hosted frontend parity
remains deferred independently of the Haskell implementation.
