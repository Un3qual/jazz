# RFC 0009: Typed-core closures and recursive callable groups

Status: Proposed
Date: 2026-08-09
Supersedes: None.

## Decision

Jazz will extend the opt-in typed-core producer and backend-neutral lowerer from
the closed scalar/direct-call profile to first-class closures and recursive
callable groups. The extension preserves the existing production boundary:
ordinary compile and run continue to use canonical core and the reference
interpreter, while an explicit producer request may return validated typed core
and validated lowered IR.

After this design gate, the work is promoted in two ordered implementation
slices:

1. non-recursive closures, beginning with closed named functions used as
   values and saturated higher-order calls, then adding capture,
   nested-lambda lifting, and partial application in the same child; and
2. self- and mutually recursive callable groups.

Each slice is a separate executor-ready child with its own complete accepted
and rejected fixture manifest. A later slice may start only after the preceding
slice preserves the complete scalar/direct-call manifest and passes both
contract validators.

### Callable shapes

Lowering classifies every executable function once for the complete bounded
program:

- a **direct shape** is capture-free, never used as a value, and called only
  through statically known saturated applications; or
- a **closure shape** is used as a value, is called through a callable value,
  captures a lexical value, participates in a closure-shaped recursive group,
  or needs partial application.

A lowered function definition has only one shape. The lowerer must not call an
environment-bearing function directly or maintain separate direct and closure
ABIs for one function identity. Existing direct shapes retain their current
multi-parameter direct-call lowering. Closure shapes receive an explicit
managed closure-environment parameter and are called through
`LoweredClosureCall`.

A capture-free function used as a value still has an explicit empty
`LoweredClosureEnvironmentLayout`. There is no null, implicit, target-specific,
or interpreter-owned environment representation. The create site constructs
the empty environment and then constructs the closure.

Callable types remain concrete and monomorphic in this profile. A
`TypedClosureRecipe` lowers recursively to a `LoweredClosureRepresentation`
whose signature contains the ordered concrete argument representations and
the concrete result representation. Closure parameters and closure results are
allowed; polymorphic representations, evidence-bearing executable nodes, and
unresolved representation parameters remain structured profile failures.

### Capture and partial application

A nested lambda is lifted to a module-local lowered function with a stable
identity derived from the module path and typed structural binder path. An
identity cannot depend on a host path, source spelling alone, a pointer, a
hash, or map/set iteration order.

The closure environment contains exactly the free lexical binders used by the
lifted body. Capture resolution uses the nearest lexical binder identity, not
textual-name matching. Fields are ordered by the first occurrence of each
resolved binder during the canonical left-to-right typed-expression traversal.
Changing host collection iteration cannot change the layout. Each function
body projects its captures from its environment; lowered functions contain no
lexical lookup.

The bounded profile permits scalar and closure-valued captures with concrete
representations. Managed text, lists, products, variants, runtime services,
type parameters, and capability-evidence values remain outside this RFC's
implementation slices.

Application consumes a callable signature in source order. A saturated known
direct shape emits a direct call. A saturated closure value emits a closure
call. Under-application creates a deterministic synthetic adapter closure: its
environment stores the target closure followed by the supplied operands in
application order, and its signature contains the remaining arguments.
Oversaturation is valid only when the first call's result is itself a concrete
closure; lowering then continues with the remaining operands. Other arity
mismatches remain ordered structured profile failures.

Synthetic adapter function and layout identities are derived from the owning
module and typed application path. Adapters use the same validation rules as
source lambdas and do not introduce a second callable representation.

### Recursive callable groups

Recursive membership must come from the compiler's shared recursive-binding
analysis. The typed-core producer and lowerer may not introduce a narrower
name-only call graph that disagrees with analyzer, inference, or interpreter
visibility.

A capture-free recursive group that never escapes as a value may retain direct
shapes and direct recursive calls. If any member requires a closure shape, the
whole strongly connected group is closure-shaped and uses one shared
environment layout. The layout contains the ordered union of external captures
only: group members are not stored in the environment. Union ordering follows
source statement order and then the canonical capture traversal for each
member.

At a group creation site, lowering constructs the shared environment once and
constructs member closures from it. Inside a member body, a reference to self
or a peer reconstructs that member's closure from the current shared
environment. This supports self and mutual recursion without cyclic heap
initialization, mutable environment fields, allocation placeholders, or new
runtime services. Closure identity is not observable in the accepted language
profile, so reconstructing an equivalent closure does not change Jazz
semantics.

Recursive groups continue to obey lexical declaration and rebinding semantics.
The first implementation accepts only concrete monomorphic function members
whose bodies otherwise fit the closure profile. Recursive aliases, patterns,
managed data, capability evidence, imports, and cross-module groups remain
structured profile failures unless a later accepted RFC promotes them.

This RFC does not select tail-call terminators. A recursive call is lowered as
an ordinary direct or closure call according to its function shape. Tail-call
classification and native stack guarantees require a separate child after
control-flow ownership is defined.

### Contract and failure ordering

The existing typed-core and lowered-IR constructors already represent typed
lambdas, closure recipes, environment layouts, environment parameters,
closure construction, projections, closure calls, and closure tail calls. The
implementation should extend production and lowering without adding an
implementation-specific sidecar or backend type. If a semantic constructor
change proves necessary, Haskell and Jazz schemas, validators, checked
comparison adapters, fixtures, and the lowered-IR version decision must change
together.

Failure precedence remains:

1. ordinary source diagnostics;
2. typed-core producer-profile failures;
3. typed-core invariant failures;
4. lowerer-profile failures; and
5. lowered-IR invariant failures.

Within a profile phase, failures are ordered by source statement order and
canonical expression pre-order. Supporting one closure or recursion form
removes only that form's previous unsupported failure. Existing failure kinds
remain available for callable, capture, arity, recursion, representation, and
non-local forms that are still outside the bounded profile. Failed production
or lowering does not return a partial artifact.

### Verification and promotion

The current scalar/direct-call fixtures and their exact typed and lowered
program expectations remain unchanged. Each closure/recursion child adds a
separate ordered manifest that runs production and lowering twice, compares
the complete result, validates successful Haskell artifacts, and sends the
same artifacts through the Jazz-authored typed-core and lowered-IR validators.

The combined manifests must cover at least:

- a closed named function passed to and called by a higher-order function;
- closure parameters and closure results;
- capture-free closure materialization with an empty environment;
- nested capture, shadowing, and deterministic multi-capture order;
- partial application and a callable oversaturated result;
- invalid arity and non-callable oversaturation;
- capture of an unsupported managed or unresolved value;
- capture-free direct self recursion;
- capture-free direct mutual recursion;
- escaping self recursion through a closure;
- a closure-shaped mutual group with external captures;
- recursive visibility and rebinding rejection; and
- repeatable failure ordering when several unsupported forms coexist.

Focused suites must retain exact typed-core and lowered-IR values, not merely
successful validation or rendered output. The full serialized compiler suite,
repository audit, documentation checks, execution-queue checks, and
`git diff --check` remain the promotion gate.

## Context

RFC 0005 established a single-pass typed-core producer whose implemented
profile accepts one resolved module of closed scalar expressions and concrete
non-capturing direct calls. RFC 0006 established a permanent lowered IR with
the closure and environment vocabulary needed by later production slices. The
current lowerer deliberately rejects callable values, captures, and recursion.

Closures and recursion must be designed together even though they are
implemented incrementally. A recursive function may escape as a value, a
capturing recursive function must retain its external lexical environment, and
mutually recursive peers must agree on how that environment is represented.
Treating recursion as only a name-call graph would work for a narrow direct
case but leave no sound path to first-class recursive functions.

The selected model uses the immutable IR already accepted by RFC 0006. A shared
external-capture environment plus peer-closure reconstruction avoids adding
mutation solely to tie a recursive knot. Separate implementation slices keep
the existing direct-call oracle useful while the callable ABI, capture
analysis, and recursive-group behavior are introduced.

## Alternatives considered

**Combine closures and recursion in one implementation child.** This reduces
queue ceremony but changes callable representation, capture analysis, function
identity, application lowering, and recursive initialization at once. Failures
would be harder to localize and the direct-call differential oracle would be
less useful. The RFC therefore fixes one model but requires ordered children.

**Store recursive peer closures inside a cyclic environment.** This closely
resembles some runtime implementations but requires mutable initialization,
allocation placeholders, or a new runtime service that the accepted IR does
not provide. Peer reconstruction from a shared external-capture environment is
semantically sufficient and keeps environments immutable.

**Lambda-lift every capture into explicit source-level parameters.** This can
avoid environments for known calls but does not represent escaping functions,
partial applications, or higher-order values at the permanent IR boundary. It
also creates a second ABI for the same callable semantics.

**Lower a native backend for the scalar/direct-call profile first.** That would
exercise only a temporary subset while closures, recursion, control flow,
modules, and managed data still determine the lasting ABI. Completing the
backend-neutral callable boundary first reduces backend rework.

## Consequences

- Closure and recursive-call semantics become explicit without changing the
  reference interpreter or ordinary compile/run path.
- Every closure has a concrete managed environment, including capture-free
  closures.
- Direct-call lowering remains available only where whole-profile use proves
  that no closure ABI is required.
- Partial application has an explicit backend-neutral representation and does
  not depend on interpreter `RuntimeValue` behavior.
- Recursive closure groups require no cyclic mutation or new runtime service,
  but may reconstruct peer closures repeatedly; allocation optimization is a
  later backend concern.
- Control flow, patterns, managed layouts, runtime services, imports,
  multi-module lowering, polymorphism, capability evidence, tail-call
  selection, LLVM emission, object generation, linking, and the native runtime
  remain unpromoted.
- Accepting this RFC authorizes implementation planning only. It does not make
  closure or recursion lowering part of shipped Jazz behavior until each
  implementation child and its verification gate land.
