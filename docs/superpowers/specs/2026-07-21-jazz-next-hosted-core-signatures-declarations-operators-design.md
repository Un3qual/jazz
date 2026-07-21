# Jazz-Next Hosted Core Signatures, Declarations, and Operators Design

## Status

Approved in discussion on `2026-07-21` as the third child of the hosted
canonical-core milestone. Written-spec review remains the gate before the
implementation plan and queue promotion.

## Goal

Extend the Jazz-authored canonical-core lowerer through every expression and
statement transformation that does not require module extraction. The child
must match the active stage-0 `JazzNext.Compiler.Parser.Lower` boundary for
signature types and payloads, data/class/impl declarations, explicit type
application, `$` desugaring, and hidden operator-binding storage names.

The result remains a private differential-testing boundary. Production
compilation continues through the Haskell lowerer, and module lowering remains
the fourth child.

## Chosen Approach

Add one ordered `SignaturesDeclarationsOperatorsProfile` to the existing
profile-driven lowering kernel and expose one child-specific wrapper,
`lowerSignaturesDeclarationsOperatorsExpression`.

This preserves one recursive implementation for all landed expression forms.
The foundation and control-flow wrappers retain their existing boundaries,
while the new wrapper admits the third-child forms. Small total helpers lower
signature and declaration payloads; the existing `Maybe` result remains solely
to reject module/import trees that belong to child 4.

Two alternatives were rejected:

1. A standalone third-child lowerer would duplicate traversal and make parity
   fixes likely to drift between wrappers.
2. Replacing the ordered profile with a general capability record would make
   later combinations flexible, but the fixed four-child milestone does not
   need that additional representation or branching machinery.

## Ownership and API

The implementation remains under the active compiler path:

- `jazz-next/jazz/compiler/CoreTypes.jz` retains the already-complete canonical
  schema. No constructor change is expected unless a parity test reveals a
  genuine mismatch.
- `jazz-next/jazz/compiler/CoreLower.jz` owns the new profile, wrapper, and all
  pure lowering helpers.
- `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreParity.hs` owns only the
  generalized test runner needed to invoke the new wrapper.
- `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreSignaturesDeclarationsOperatorsSpec.hs`
  owns the fixed child fixture families.
- `jazz-next/jazz-next.cabal` registers the focused suite.

No public parser or production lowerer API changes in this child.

## Lowering Contract

### Expressions and profiles

The new profile includes every form already admitted by
`ControlFlowPatternsProfile`, then adds:

- `TypeApplicationExpression function span signatureType` lowers recursively
  to `CoreTypeApplicationExpression` with an unqualified `CoreSpan`;
- binary `$` lowers recursively to `CoreApplyExpression`; and
- blocks may contain the declaration forms defined below.

`lowerFoundationExpression` and `lowerControlFlowPatternsExpression` keep
returning `Nothing` for these forms. This makes each wrapper an observable
milestone boundary rather than an alias for the latest implementation.

### Names

Ordinary binding, declaration, parameter, constructor, capability, and method
names become `CoreSourceName` values.

Signature type and constraint names use the active stage-0 rule: a name with
exactly two non-empty segments separated by one `::` becomes
`CoreQualifiedName qualifier member`; every other spelling remains one
`CoreSourceName`. A `CoreSignatureNameToken` always contains a source name,
matching the tokenized fallback boundary.

A let binding or adjacent signature whose parsed name begins with
`$operator:` becomes
`CoreGeneratedName (CoreOperatorBinding storedName)`. The stored text is
preserved exactly; lowering neither decodes nor re-encodes the parser-owned
percent-escaped spelling. The same conversion helper serves bindings and
signatures so they cannot diverge.

### Signatures

The lowerer maps every `SurfaceSignatureType` constructor structurally:

- `Int`, `Float`, every fixed numeric width, `Bool`, `Char`, and `Text`;
- type variables and named types;
- named application, lists, tuples, unit-as-empty-tuple, and functions; and
- recursive combinations of those forms.

It also maps both structured payload forms, every constraint, and every
tokenized unsupported-signature constructor. Unsupported signature payloads
remain data; this stage does not reinterpret or diagnose them.

### Statements and declarations

The new profile lowers:

- ordinary and hidden operator `LetStatement` values recursively;
- ordinary and hidden operator `SignatureStatement` payloads;
- data names, type parameters, constructors, named constructor arguments, and
  opaque constructor arguments;
- class names, parameters, method names, spans, and signature payloads;
- impl capability names, signature-type arguments, method names, spans, and
  recursively lowered method bodies; and
- expression statements recursively.

Statement order and every unqualified source span are preserved exactly.

### All-or-nothing deferral

`ModuleStatement` and `ImportStatement` still return `Nothing`. That result
propagates through blocks, lets, lambdas, conditionals, cases, guards, impl
methods, applications, collections, and every other recursive owner. The
lowerer never drops a deferred statement or returns a partially lowered tree.

Module/import extraction, export metadata, path validation, structured
`E4005`/`E4006` failures, span qualification, and the composed facade remain
owned by child 4.

## Fixed Fixture Families

All three families execute twice and compare deterministic outputs.

### Direct positive family: 20 fixtures

1. `type-application-primitive`
2. `type-application-recursive-qualified`
3. `dollar-basic`
4. `dollar-nested-control-flow`
5. `ordinary-binding`
6. `operator-binding`
7. `signature-primitives`
8. `signature-numeric-widths`
9. `signature-recursive-shapes`
10. `signature-qualified-names`
11. `signature-constraints`
12. `unsupported-signature-token-inventory`
13. `operator-signature`
14. `data-empty`
15. `data-constructors`
16. `class-empty`
17. `class-methods`
18. `impl-empty`
19. `impl-methods`
20. `mixed-block`

Together these fixtures exercise every new constructor and transformation,
including every numeric signature width, all unsupported-signature tokens,
opaque data payloads, nested declaration expressions, exact `$` shape, and
exact hidden storage names.

### Hosted-parser-composed family: 16 sources

1. `explicit-type-primitive`
2. `explicit-type-applied-chain`
3. `dollar-right-associated`
4. `signature-primitives`
5. `signature-recursive-shapes`
6. `signature-qualified`
7. `signature-constrained`
8. `signature-unsupported-forall`
9. `data-nullary`
10. `data-parameterized`
11. `class-empty`
12. `class-method-signature`
13. `impl-empty`
14. `impl-method-body`
15. `operator-signature-binding`
16. `mixed-declarations-control-flow`

The stage-0 side independently tokenizes, parses, lowers, and structurally
adapts each source. The Jazz side uses the existing hosted lexer/parser before
the new lowering wrapper. Operator declarations themselves remain parser
context and therefore contribute no core statement, exactly as in stage 0.

### Deferred family: 8 direct fixtures

1. `module-root`
2. `import-root`
3. `module-in-if-branch`
4. `import-in-case-body`
5. `module-in-lambda-body`
6. `import-in-let-value`
7. `module-in-impl-method`
8. `import-in-operator-binding`

Each fixture must produce only `Nothing`, including when every sibling and
ancestor is otherwise supported.

## Verification

The child-specific suite must prove:

- exact stage-0 parity for the 20 direct fixtures;
- exact stage-0 parity for the 16 composed sources;
- exact generated operator-storage names in both bindings and signatures;
- exact qualified-name behavior inside signature types and constraints;
- exact `$`-to-application shape;
- repeated `Nothing` for all 8 deferred fixtures; and
- deterministic output across both executions of every family.

Regression verification includes the control-flow/pattern, foundation,
canonical-core, canonical-parser, hosted-parser operator, and repository-audit
suites. Closeout also requires a warning-clean development build, routine
Cabal `all`, `cabal check`, queue/docs validators, and `git diff --check`.

The opt-in exhaustive parser scale suites are deliberately excluded. Only the
bounded routine parser-scale component may run through Cabal `all`.

Tests assert canonical values and behavior. They do not inspect implementation
source text or duplicate lowering decisions in the Haskell runner.

## Non-Goals

This child does not:

- lower modules or imports into `CoreModule` metadata;
- validate declared or expected module paths;
- qualify spans with source paths;
- add the source-to-core facade or close the full parser corpus manifest;
- replace the production Haskell lowerer;
- change parsing, analysis, resolution, type inference, evaluation, or runtime
  behavior;
- introduce typed core, backend-neutral lowered IR, LLVM, objects, linking, or
  a native runtime;
- add host callbacks, lowering intrinsics, bytecode, or a VM;
- run exhaustive parser scale suites; or
- modify `jazz-hs/` or `jazz2/`.
