# Jazz-Next Generic Named Types Design

## Status

Approved in discussion and written-spec review on `2026-07-10`; implemented in
active `jazz-next` on `2026-07-10` by commit `b41a70a`.

The executable child id is `JN-BOOTSTRAP-GENERIC-NAMED-TYPES-001`. This
document defines the completed child. Its reviewed implementation plan records
the exact target paths, tests, architecture guard, and closeout verification.

## Goal

Add full rank-1 named generic types to Jazz source signatures and module
interfaces so bootstrap libraries can expose APIs such as:

```jazz
value :: Maybe(Char).
result :: Result(IOError, Text).
map :: (a -> b) -> List(a) -> List(b).
nested :: Result(IOError, List(Maybe(Token))).
```

The implementation must use one recursive surface signature type and one
recursive core signature type across ordinary signatures, constrained
signatures, class and impl metadata, explicit type applications, runtime hints,
and module interfaces. It must not add an intermediate bootstrap-only type
representation or couple the frontend to LLVM objects.

## Current Baseline

The active `jazz-next` compiler already provides:

- primitive, numeric-width, list, tuple, function, and Unit signature types;
- a small ordinary `SurfaceSignatureType` / `SignatureType` tree;
- a broader `SurfaceConstrainedSignatureType` / `ConstraintSignatureType` tree
  used by constrained signatures, impl targets, capability evidence, and
  runtime hints;
- inferred ordinary-binding schemes and per-use fresh instantiation;
- solver-backed constrained signatures and explicit type application;
- generic data declarations, constructor schemes, and `TDataType Name [Type]`;
- type-namespace-aware module resolution and typed export inventories; and
- imported scheme and data-type rebasing through module interfaces.

The duplicate signature trees are now the wrong long-term boundary. Extending
only one of them would make `Maybe`, `Result`, and collection APIs depend on an
intermediate representation that later compiler and LLVM work would have to
remove.

## Chosen Architecture

Keep `SurfaceSignatureType` as the only surface type-syntax tree and
`SignatureType` as the only core type-syntax tree. Delete
`SurfaceConstrainedSignatureType` and `ConstraintSignatureType`.

Preserve the existing dedicated primitive constructors so this child does not
reopen primitive aliases, numeric defaults, widths, or runtime matching. Add
the following recursive forms to both retained trees:

```haskell
SurfaceTypeVariable Identifier
SurfaceTypeName Identifier
SurfaceTypeApplication Identifier [SurfaceSignatureType]

TypeVariable Name
TypeName Name
TypeApplication Name [SignatureType]
```

The existing list, tuple, and function constructors remain recursive members
of the same retained trees.

`SignaturePayload` remains the policy wrapper:

```haskell
data SignaturePayload
  = SignatureType SignatureType
  | ConstrainedSignature [SignatureConstraint] SignatureType
  | UnsupportedSignature [SignatureToken]
```

`SignatureConstraint` arguments, `SImpl` targets, class method signatures,
capability facts, deferred constraints, and runtime hints all consume the
unified `SignatureType`. This is a representation unification, not a change to
which constraint, impl, or dispatch policies are accepted.

## Surface Grammar

Named type application uses the already-approved comma-parenthesized form:

```text
type-variable      := lower-identifier
type-head          := identifier | identifier "::" identifier
named-type         := type-head
type-application   := type-head "(" type ("," type)* ")"
type               := primitive
                    | type-variable
                    | named-type
                    | type-application
                    | "[" type "]"
                    | "(" type ("," type)* ")"
                    | type "->" type
```

Function types remain right-associative. Parentheses continue to override
association. `()` remains Unit, represented as the empty tuple type.

`List(a)` is an accepted built-in arity-one spelling and normalizes to the same
list node as `[a]`. It does not declare a second nominal type and does not create
a second runtime representation. `List()` is invalid empty-application syntax;
`List(a, b)` rejects as an arity error.

Application heads must be named constructors. Variable-headed forms such as
`f(a)` do not introduce higher-kinded variables. Empty application argument
lists and trailing commas are invalid. Partial applications such as bare
`Maybe` when `Maybe` declares one parameter are semantic arity errors.

The same grammar is accepted in:

- ordinary adjacent signatures;
- constrained signature bodies and constraint arguments;
- class method signatures;
- impl targets;
- explicit expression type applications; and
- list, tuple, and function type positions nested within any of the above.

## Name Resolution and Visibility

Primitive names continue to lower to their dedicated constructors. `List`
normalizes to the structural list constructor. Other upper-case names resolve
through the existing `TypeNamespace`.

Local data declarations retain current inference ordering: a local named type
must be registered before a signature uses it. Imported data types are preloaded
through the module interface and are visible according to the existing bare,
symbol-list, and alias import rules. This child does not add forward local type
references, implicit imports, type re-exports, wildcard imports, or hiding
imports.

Resolved user-defined type names carry their nominal `Name`, including module
origin. Two same-text types from different modules remain distinct. Alias-only
imports remain alias-qualified rather than becoming unqualified type names;
their type heads are written as `Alias::Type` in signatures and explicit type
applications.

Every named constructor application has exact arity:

- `Maybe(Char)` is valid when `Maybe` declares one parameter;
- `Result(IOError, Text)` is valid when `Result` declares two parameters;
- `Maybe(Char, Text)` rejects;
- `Result(IOError)` rejects; and
- `Unknown(Char)` rejects when no visible type declaration exists.

Zero-parameter user data types use their bare name. Applying arguments to a
zero-parameter type rejects.

## Rank-1 Variables and Schemes

Lower-case signature names are rank-1 type variables. All free signature
variables are implicitly quantified in deterministic first-occurrence order.
Repeated occurrences refer to the same inference variable:

```jazz
identity :: a -> a.
map :: (a -> b) -> List(a) -> List(b).
```

An adjacent generic signature defines a real `TypeScheme`. The binding right
hand side is checked with the declared variables held rigid: the implementation
cannot specialize a universally quantified variable or collapse two distinct
declared variables. After the definition passes, every later use receives a
fresh instantiation. Signed generic bindings therefore do not become
monomorphic after their first use. This applies to explicitly signed direct
constructor aliases as well; unsigned constructor aliases retain their existing
monomorphic policy.

Named applications lower to the existing inference representation:

```haskell
TypeApplication name arguments
  -> TDataType resolvedName loweredArguments
```

Repeated variables inside nested applications unify normally. Existing ADT
nominal identity and argument-by-argument unification remain authoritative.

This design is rank-1 only. A type variable cannot stand for a type constructor,
accept type arguments, or quantify inside another rank boundary. There are no
explicit `forall` binders.

## Constrained Signatures and Capabilities

Constrained signatures use the same retained `SignatureType` tree for both the
body and constraint arguments:

```jazz
equalsAll :: @{Eq(a)}: List(a) -> List(a) -> Bool.
```

Existing constraint policies remain unchanged:

- the class must be visible and have matching arity;
- duplicate explicit constraints reject;
- constrained variables must participate in the signature body under the
  existing bidirectional contract;
- concrete constraints still require visible concrete impl facts;
- inferred constraints still use the existing final defaulting and ambiguity
  rules; and
- qualified method requirements still use compiler-owned evidence.

For an explicitly signed binding, every class or primitive obligation inferred
from the implementation must be entailed by a matching declared constraint.
The compiler reports `E2009` at the signature when an implementation would
otherwise strengthen its public contract.

Impl targets and class method signatures use the unified tree without gaining
partial type constructors, higher-kinded parameters, default methods,
superclasses, or new overlap/orphan behavior. Until method-local quantification
is designed, a class method signature may mention only the enclosing class
parameters; other free variables reject at the method declaration.

The unification removes conversion helpers whose only purpose was copying
between `SignatureType` and `ConstraintSignatureType`. Semantic helpers for
substitution, rendering, variable collection, runtime matching, and capability
fact keys operate on the single retained tree.

## Explicit Type Application

Expression-level explicit type application accepts the unified type grammar:

```jazz
identity @Maybe(Char) value
map @(Result(IOError, Text)) transform values
```

It continues to apply arguments to generalized schemes in deterministic
quantified-variable order. Monomorphic targets, excess explicit arguments, and
incompatible arguments keep their existing diagnostic behavior.

Named explicit arguments resolve and validate exactly like named types in
adjacent signatures. This child does not add named selection of a quantified
variable or multiple arguments in one `@` form.

## Module Interfaces

The compiler module interface transports the inferred `TypeScheme`, whose
`ExpressionType` already represents nominal applications as
`TDataType Name [ExpressionType]`. The unified signature tree removes the
parallel constrained-type rebasing path; module rebasing must visit the one
retained `SignatureType` recursively wherever source signature metadata is
preserved.

Imported schemes freshen quantified variables per use. Named types are rebased
to their defining module origin, including nested applications, constraints,
class methods, impl targets, and runtime evidence.

Existing explicit export behavior remains unchanged. A value may continue to
expose an opaque nominal type under the current typed-interface rules; this
child does not implicitly export that type or its constructors. Generic values,
explicitly exported types, and constructors retain their existing independent
inventory entries.

## Runtime Hints and Dispatch

Runtime hints use the unified `SignatureType`. Scheme templates preserve
quantified positions as real `TypeVariable` nodes rather than fabricated named
data types. Only fully concrete instantiations become dispatch evidence. A
polymorphic scheme such as `Maybe(a)` therefore cannot masquerade as a nominal
type; after instantiation, `Maybe(Char)` may produce a concrete nominal hint.

Runtime hint substitution, rebasing, exact-match filtering, and qualified
method dispatch recurse through named applications using the same nominal
identity and arity as inference. This child must preserve all current primitive,
numeric-width, list, tuple, function, and ADT dispatch behavior.

The stage-0 Haskell interpreter may continue to store its internal type and
runtime metadata in Haskell values. Jazz-visible semantics and module
interfaces must remain representation-neutral.

## Diagnostics

Semantic named-type failures use deterministic `E2009` diagnostics at the
owning signature span or, for expression-level explicit type application, the
explicit type-argument span. Required messages distinguish:

- unknown or non-visible named type;
- expected versus received type-constructor arity;
- variable-headed type application;
- partial application of a nonzero-arity constructor; and
- unsupported rank or type-constructor polymorphism.

Malformed token grammar remains a parser diagnostic under the existing parser
contract. Existing signature mismatch, constrained-signature, explicit-type-
application, module-resolution, and runtime diagnostic codes and spans remain
unchanged.

The parser must not route a valid named generic surface through
`UnsupportedSignature`. That fallback remains only for still-unsupported
signature syntax so downstream diagnostics stay deterministic during later
type-surface work.

## Migration and Compatibility

Every currently accepted source signature must preserve its existing behavior.
Primitive, list, tuple, function, Unit, constrained, class/impl, runtime-hint,
and explicit-type-application tests change only where they assert removed AST
constructor names.

The migration is complete only when no production or test code references
`SurfaceConstrainedSignatureType`, `ConstraintSignatureType`, or their removed
constructors. No compatibility alias or duplicate mirror type remains.

The retained tree becomes the permanent frontend signature representation for
the interpreter and future compiler. Canonical typed core and later
backend-neutral lowered IR consume resolved nominal `ExpressionType` values;
LLVM lowering does not parse or reinterpret source signature syntax.

## Verification Contract

Focused coverage must include:

1. parser and lowering AST checks for concrete, variable, nested, list, tuple,
   function, constrained, impl-target, and explicit-application forms;
2. `[a]` and `List(a)` normalization to one list type;
3. deterministic failures for unknown names, wrong arity, partial
   applications, variable-headed applications, and malformed arguments;
4. signed rank-1 identity and multi-variable functions instantiated freshly at
   incompatible concrete types in the same scope;
5. repeated-variable consistency within and across nested applications;
6. generic ADT constructor compatibility and mismatch diagnostics;
7. constrained generic signatures and capability impl targets through the
   unified tree;
8. explicit type application with concrete nested named types;
9. cross-module generic scheme export/import with qualified nominal identity;
10. opaque named-type export behavior unchanged;
11. runtime-hint and qualified-dispatch regressions for generic ADTs; and
12. an architecture guard proving both removed type names are absent from
    `jazz-next/src`, `jazz-next/test`, and `jazz-next/jazz-next.cabal`.

The completed implementation must pass:

```text
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

Focused parser, binding-signature, module-resolution, loader, runtime, prelude,
and capability suites must also be recorded in the child plan and queue row.

## Non-Goals

This child does not add:

- Jazz definitions of `Maybe`, `Result`, or collection traversal libraries;
- text traversal, host I/O, process arguments, or stack-safe evaluation;
- the Jazz-authored lexer or its differential comparison format;
- higher-kinded or higher-rank types;
- type lambdas, explicit `forall`, aliases, variance, subtyping, or type-level
  computation;
- partial type-constructor application;
- packages, re-exports, wildcard imports, or new visibility rules;
- default methods, superclasses, or new overlap/orphan policy;
- backend-neutral lowered IR, LLVM IR generation, object generation, linking,
  or the native runtime; or
- a bytecode format or bytecode VM.

## Queue Handoff

After written-spec review, create one implementation plan and promote exactly
one `Ready Now` row:

```text
id: JN-BOOTSTRAP-GENERIC-NAMED-TYPES-001
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
```

The plan and queue row must carry identical target paths, deliverable wording,
verification commands, and plan-section metadata. `Maybe`/`Result` library
definitions remain the next separate bootstrap child and must not be bundled
into this row.
