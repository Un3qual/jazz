# Haskell and compiler practices that could simplify Jazz

Research completed 2026-09-12 against Jazz ff15d166 on codex/simplify-haskell-compiler.
This is a research recommendation record, not an approved implementation plan or
a change to public language behavior. No compiler code was changed.

The strongest remaining opportunities are narrower representations and shared
operations with explicit semantic differences. Jazz already uses many of the
relevant Haskell techniques. The recommendations below come from inspecting live
builders and consumers; the linked literature explains the principles, not a
claim that a paper prescribes these particular Jazz edits. No line savings or
performance improvements have been measured.

The local build plan identifies GHC 9.14.1, base 4.22.0.0, containers 0.8,
megaparsec 9.7.0, mtl 2.3.1 and transformers 0.6.1.2. Compiler comparisons were
checked at Elm 1bd5b36915a38335195ca7792fe3995f53d84d5e and Futhark
c3b8e3b9e109fc11e22dcf31c26a0eb3a2257931. The two earlier simplification passes
and the completed test repair were checked to avoid recommending work already done.

| Priority | Concrete opportunity                              | What becomes simpler                                       | Main constraint                                   |
| -------- | ------------------------------------------------- | ---------------------------------------------------------- | ------------------------------------------------- |
| 1        | Represent a statement's optional binding directly | Remove synchronized binder-list and scheme-map bookkeeping | Preserve binder identity and complete schemes     |
| 2        | Share the identical surface/core literal datatype | Remove duplicate constructors, instances and conversion    | Preserve numeric provenance and hosted encodings  |
| 3        | Share function-argument inference                 | One allocation/unification loop with two failure policies  | Preserve rollback and diagnostic state            |
| 4        | Share host-operation observation sequencing       | One begin/action/end helper across seven branches          | Preserve events, exceptions and exit order        |
| 5        | Parse a signature type's common head once         | Remove overlapping name/application parsing                | Preserve fallback, adjacency and failure behavior |
| 6        | Fold collections directly                         | Remove unnecessary intermediate-list conversions           | Preserve traversal order and required keys        |

**1. Use a datatype that represents a statement's actual binding cardinality.**

The general principle is to preserve information established by construction in
the type of the result, so later code needs fewer checks. Alexis King's
[Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
explains this distinction. Elm's
[canonical AST](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/compiler/src/AST/Canonical.hs)
also keeps facts on the nodes needing them and documents their later consumers.

Jazz's [StatementFacts](/Users/admin/.codex/worktrees/1b18/jazz-main/src/Jazz/Compiler/SemanticFacts.hs:119)
contains both a list of binder IDs and a map from binder IDs to generalized schemes.
Its [production builder](/Users/admin/.codex/worktrees/1b18/jazz-main/src/Jazz/Compiler/TypeInference/Analyzed.hs:294)
creates either an empty list/map or one binder and a singleton map. Import nodes
also use the empty shape. A field of type Maybe (CoreBinderId, AnalyzedScheme)
would encode that relationship directly.

This would simplify [result representation attachment](/Users/admin/.codex/worktrees/1b18/jazz-main/src/Jazz/Compiler/TypeInference/Analyzed.hs:265),
[runtime constructor creation](/Users/admin/.codex/worktrees/1b18/jazz-main/src/Jazz/Compiler/Runtime/Engine.hs:824)
and [checked-signature runtime hints](/Users/admin/.codex/worktrees/1b18/jazz-main/src/Jazz/Compiler/TypeInference/Capabilities.hs:1287).
The current invariant test at
[ModulePipelineContractSpec](/Users/admin/.codex/worktrees/1b18/jazz-main/test/Jazz/Compiler/Modules/ModulePipelineContractSpec.hs:969)
checks agreement between the collections; the proposed type would make that
particular agreement structural.

Retain ResolvedNodeFacts and complete schemes. A data declaration may contain many
constructor nodes, but that does not require many bindings on each node. This is
an internal artifact migration with several fixture consumers, not a reason to
weaken malformed-artifact checks generally.

**2. Share unchanged literal payloads across phases.**

[SurfaceLiteral](/Users/admin/.codex/worktrees/1b18/jazz-main/src/Jazz/Compiler/Parser/AST.hs:72) and
[Literal](/Users/admin/.codex/worktrees/1b18/jazz-main/src/Jazz/Compiler/AST.hs:104) have the same five cases and
payloads. SurfaceNumericType already aliases the same numeric representation.
[lowerSurfaceLiteral](/Users/admin/.codex/worktrees/1b18/jazz-main/src/Jazz/Compiler/Parser/Lower.hs:559)
only renames constructors.

A neutral literal datatype used by both ASTs would delete this duplicate
definition and conversion. Futhark's
[representation commentary](https://github.com/diku-dk/futhark/blob/c3b8e3b9e109fc11e22dcf31c26a0eb3a2257931/src/Futhark/IR/Syntax.hs)
uses ordinary type parameters where sufficient and reserves its more elaborate
representation machinery for nodes that need it. Sharing Jazz's identical leaf
payload is an application of that restraint.

Preserve arbitrary-precision integers, FractionalLiteralSource and numeric-width
hints. The [hosted parser's canonical encoder](/Users/admin/.codex/worktrees/1b18/jazz-main/test/Jazz/Compiler/Bootstrap/CanonicalParserComparison.hs:124)
must keep its existing external schema. Audit constructor-name/Show consumers;
a permanent compatibility layer could outweigh this small reduction. Surface
expressions and core expressions should remain distinct.

**3. Share the inference loop, with failure behavior supplied explicitly.**

John Hughes's [Why Functional Programming Matters, section 3](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf)
demonstrates reuse by separating a recursive operation from the functions that
vary between uses. This is directly applicable to
[applyKnownFunctionArguments](/Users/admin/.codex/worktrees/1b18/jazz-main/src/Jazz/Compiler/TypeInference/Capabilities.hs:1409)
and its error-reporting counterpart at line 1427.

Both fold over arguments, allocate a result variable, unify a function type and
resolve the result. One local loop can take a failure handler, while keeping the
two meaningful wrappers. The silent candidate path returns the state from before
allocating the failing argument's result variable; the reporting path retains
that variable and adds a diagnostic. Both paths retain prior successful arguments
and discard partial substitutions from the failed unification. The
candidate-signature dispatch immediately above can share the same operation.

Keep those checkpoints explicit, retain foldl' and the current first-failure
behavior, and avoid forcing a diagnostic on the silent path. A wholesale
StateT/Maybe conversion would not by itself express both policies. This is a
concrete reusable function, with no new dependency or extensible effect framework.

**4. Share the host observation protocol as an ordinary helper.**

[evalBuiltinWithHost](/Users/admin/.codex/worktrees/1b18/jazz-main/src/Jazz/Compiler/Runtime/Engine.hs:2072)
repeats beginHostOperation, a lifted host action and endHostOperation in seven
branches. A local observeHostOperation helper can own this sequencing while each
branch still interprets its specific result. The same higher-order decomposition
from [Hughes](https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf) applies.

Preserve the exact order: count/open, perform action, close, then interpret the
result or request exit. This helper must retain normal sequencing. Replacing it
with bracket, finally or exception masking would change behavior: a host action
that throws currently does not reach the close operation. Existing host-I/O,
profiling and exact-observation tests should verify the later refactor.

**5. Left-factor the signature grammar where the prefix is identical.**

Megaparsec's [author tutorial](https://markkarpov.com/tutorial/megaparsec.html)
explains that consuming alternatives need explicit backtracking, and warns against
long overlapping alternatives. Jazz's concrete Parser alias and structured-error
adapter already follow its guidance.

In [functionOperandTypeParser](/Users/admin/.codex/worktrees/1b18/jazz-main/src/Jazz/Compiler/Parser/Signature.hs:196),
the application alternative under try and the following named-type alternative
both invoke signatureTypeHeadParser. A combined named-or-applied parser could
read the head once, then decide whether to parse application arguments. This
would consolidate the two head-handling paths and their adjacent-parenthesis
guard.

This is a candidate to prototype, not a proven drop-in rewrite. Preserve the
current handling of whitespace before parentheses, failed application tails,
qualified-name adjacency, unsupported signature payloads, error locations and
consumed-input behavior. A blanket removal of try is not the recommendation.
The statement-level signature-versus-qualified-expression decision at
[Declaration.hs](/Users/admin/.codex/worktrees/1b18/jazz-main/src/Jazz/Compiler/Parser/Declaration.hs:322)
has additional context-sensitive rules and should remain separate.

**6. Use Foldable operations on their existing containers.**

The [base 4.22 Foldable documentation](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-Foldable.html#v:toList)
recommends folding a structure directly when conversion to a list is only
intermediate. Two remaining examples are
[inventoryHasSelector](/Users/admin/.codex/worktrees/1b18/jazz-main/src/Jazz/Compiler/ModuleExports.hs:169),
where any can consume the Set directly, and
[analyzeProgram](/Users/admin/.codex/worktrees/1b18/jazz-main/src/Jazz/Compiler/ModuleCompiler.hs:58),
where foldM can consume NonEmpty directly.

These are small readability improvements. Preserve iteration order. Map.toList
is different when the consumer needs keys: Foldable Map visits values, so those
conversions cannot generally be removed.

**Conventions worth adopting selectively.**

Use existing record field names for long constructors whose arguments have the
same types. For example,
[emptyResolvedNodeFacts](/Users/admin/.codex/worktrees/1b18/jazz-main/src/Jazz/Compiler/CoreIdentity.hs:99)
currently passes an owner, six Nothing values and an empty list positionally.
A complete named initializer would make each absent fact identifiable. This
increases line count slightly but improves readability without another abstraction.
The [Haskell Report](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#sect3.15.2)
defines labeled construction; recommending it at this site is a readability
judgment, not a language requirement.

Keep clear named arguments, type signatures and ordinary do/case expressions.
[Johan Tibell's style guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md#point-free-style)
warns against excessive point-free notation. Jazz's current HLint configuration
already declines automatic eta-reduction and lambda-removal rules. Retain Ormolu
and the current warning/Weeder gates; adopting a different project's formatting
preferences would create churn without removing compiler complexity.

For a non-obvious invariant shared across functions, put one short, named
explanation beside its owner and reference it from consumers. Simon Peyton Jones
describes this [GHC Notes practice](https://ghc.gitlab.haskell.org/homepage/blog/20160620-ContributingToGhc.html)
as a way to keep explanations near code without interrupting its flow. Good Jazz
subjects are inference preview rollback, signature-versus-qualified-name
disambiguation and recursive runtime cells. Preserve useful existing comments;
add an explanation when it records a missing reason, not to narrate obvious code.

**Compiler architecture practices already present and worth retaining.**

[Trees That Grow](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/trees-that-grow.pdf)
uses type-indexed extensions to share AST structure while allowing phase-specific
information. Jazz's [CoreNode and FactsAt](/Users/admin/.codex/worktrees/1b18/jazz-main/src/Jazz/Compiler/AST.hs:60)
already capture the current need with closed phase families. Preserve the nominal
roles, shared constructors and phase-specific facts. Full GHC-style extension
families for every constructor would need a concrete new consumer to pay for them.

The [Nanopass paper](https://www.cs.tufts.edu/comp/150FP/archive/kent-dybvig/nanopass.pdf)
supports passes with explicit input/output languages and focused responsibilities.
It also identifies repetitive unchanged traversals as a cost of splitting passes.
For Jazz, that supports named transformations and existing shared traversals, not
an automatic increase in passes or a universal visitor framework.

Maintain boundaries that discharge real work:
[surface lowering](/Users/admin/.codex/worktrees/1b18/jazz-main/src/Jazz/Compiler/Parser/Lower.hs:428) removes
multi-parameter lambdas and other syntax; signature payloads preserve unsupported
forms for diagnostics; [import exposure](/Users/admin/.codex/worktrees/1b18/jazz-main/src/Jazz/Compiler/ModuleGraph.hs:88)
deliberately permits malformed raw imports before refining them into checked
variants. Apply stronger types after the relevant validation boundary. Erasing
those raw shapes could remove useful diagnostics.

Preserve the [Draft/Attachment composition](/Users/admin/.codex/worktrees/1b18/jazz-main/src/Jazz/Compiler/TypeInference/Draft.hs:43),
ordered invariant failures and direct execution of analyzed trees. There is no
demonstrated simplification from adding ANF/CPS, another runtime IR or a generic
recursion-schemes framework to the current interpreter.

**Ideas requiring separate evidence.**

Moving from Haskell2010 to GHC2024 could remove repeated extension pragmas.
[GHC 9.14.1's edition guidance](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/exts/control.html)
supports explicit editions, but GHC2024 also changes enabled inference rules,
including MonoLocalBinds. Treat this as a separate compilation/tooling migration.
It ranks below the six concrete opportunities.

A Seq of inferred constraints could remove the count stored alongside the list in
[InferenceOutput](/Users/admin/.codex/worktrees/1b18/jazz-main/src/Jazz/Compiler/TypeInference/State.hs:97).
However, [containers documents](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Sequence.html)
that sequences are strict in their length and usually slower than lists for
stack-like operations. Prototype and measure before choosing it; fewer fields
alone do not establish a better representation.

Use State locally where it eliminates sequential plumbing. Mark Jones's
[Typing Haskell in Haskell](https://web.cecs.pdx.edu/~mpj/thih/thih.pdf)
is useful for this technique, but its simplified treatment is not a production
diagnostic architecture. Jazz's [previewInference and rejectPatternAttempt](/Users/admin/.codex/worktrees/1b18/jazz-main/src/Jazz/Compiler/TypeInference/State.hs:280)
have intentionally different rollback policies. Strictness also needs explicit
reasoning: [State.Strict documentation](https://hackage-content.haskell.org/package/transformers-0.6.2.0/docs/Control-Monad-Trans-State-Strict.html)
specifies strict sequencing without itself forcing the state; modify' forces
only weak head normal form. Global StrictData, blanket foldl' rewrites and
replacing lazy recursive maps are not justified here.

**Recommended implementation order and acceptance criteria.**

Start with the local shared inference and host helpers plus the literal type and
direct folds. Then migrate statement binding facts as one coordinated internal
representation change. Prototype signature prefix factoring separately because
parser commitment and diagnostic compatibility need close scrutiny.

For each change, compare total production code including new helpers/modules,
and retain it only if it removes duplication, a maintained invariant, or a clear
reading obstacle. Preserve language behavior, identities, diagnostic order/spans,
rollback, evaluation demand and observation events. Use the existing focused
semantic/parity tests and clean quality gate; add tests only for meaningful
coverage gaps. Any performance claim requires measurement. The preceding
66-suite result establishes the baseline, not verification of unimplemented ideas.

Research involved primary-source reading and static repository inspection.
Compiler tests and benchmarks were not rerun because production code was unchanged.
