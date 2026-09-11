# Jazz compiler comparative architecture audit

Date: 2026-09-10

## Executive conclusion

This report compares the active Jazz compiler source with fresh, pinned source snapshots of four substantially different Haskell-written compilers or language implementations: PureScript, Elm, Dhall, and Futhark. It is a code comparison only. It does not use prior Jazz audits, project documentation, plans, tests, issue history, or repository history as evidence for the findings below.

The comparison changes the blunt version of the original diagnosis. Jazz is not simply an abnormally large compiler. The selected Jazz compiler directory contains 37,171 physical lines in 88 Haskell files. PureScript's selected compiler core contains 31,646 lines in 121 files, Elm's compiler and builder contain 43,395 lines in 115 files, and Dhall's library core contains 32,719 lines in 68 files. Futhark is much larger at 140,695 lines in 348 files because it is an optimizing array compiler with several typed intermediate representations and many native backends.

Jazz's stronger problem is complexity density and ownership shape. Several independent concerns are represented as parallel structures and then rejoined by keys, attachments, tuples, maps, or reconstruction functions. The compiler also combines two architectural choices that the comparator projects keep more distinct:

1. an interpreter-oriented semantic model that wants rich source and type facts; and
2. a compiler-oriented pipeline that wants stable executable IRs and explicit artifact boundaries.

Jazz currently pays for both models without receiving the full simplification of either. The result is a compiler that is difficult to navigate even when its total size is comparable with other complete Haskell implementations.

The most credible reduction is therefore not “delete half of the compiler.” It is approximately 3,000–7,000 net lines, with a center estimate around 5,000, if the semantic fact bus, module-interface duplication, frontend ownership overlap, runtime plan duplication, and low-value carrier layers are removed or collapsed. The remaining code would still be a substantial compiler.

## Scope and method

### Jazz scope

The Jazz baseline is the active `src/Jazz/Compiler/**/*.hs` tree:

- 88 Haskell files
- 37,171 physical lines, including blank lines and comments
- compiler frontend, module analysis, type inference, runtime, and code-generation-adjacent code in that tree
- source inspected directly rather than through project documentation or previous audit conclusions

Relevant Jazz evidence includes the phase-indexed core representation in [AST.hs:60](../../src/Jazz/Compiler/AST.hs#L60), the inference state in [TypeInference/State.hs:128](../../src/Jazz/Compiler/TypeInference/State.hs#L128), the analyzed facts in [TypeInference/Analyzed.hs:223](../../src/Jazz/Compiler/TypeInference/Analyzed.hs#L223), the module interface in [ModuleInterface.hs:51](../../src/Jazz/Compiler/ModuleInterface.hs#L51), and the runtime engine in [Runtime/Engine.hs:475](../../src/Jazz/Compiler/Runtime/Engine.hs#L475).

### Comparator snapshots

The comparator repositories were cloned from their official repositories into a temporary read-only directory and inspected at these exact commits:

| Project    | Haskell source scope                                         | Files | Physical lines | Snapshot                                                                                                         |
| ---------- | ------------------------------------------------------------ | ----: | -------------: | ---------------------------------------------------------------------------------------------------------------- |
| Jazz       | `src/Jazz/Compiler`                                          |    88 |         37,171 | current worktree                                                                                                 |
| PureScript | compiler core, excluding Docs/Ide/Interactive/Publish/Linter |   121 |         31,646 | [`cb3c496`](https://github.com/purescript/purescript/tree/cb3c4965c8468d26c9b14cf0319db6dbd06ee4ff/src)          |
| PureScript | complete `src` tree, for context                             |   178 |         41,545 | [`cb3c496`](https://github.com/purescript/purescript/tree/cb3c4965c8468d26c9b14cf0319db6dbd06ee4ff/src)          |
| Elm        | compiler plus builder                                        |   115 |         43,395 | [`1bd5b36`](https://github.com/elm/compiler/tree/1bd5b36915a38335195ca7792fe3995f53d84d5e/compiler/src)          |
| Dhall      | `dhall/src/Dhall` plus `ghc-src/Dhall`                       |    68 |         32,719 | [`bcfb3dd`](https://github.com/dhall-lang/dhall-haskell/tree/bcfb3ddb17d4f55f7c4cf823c9d4072fa519edf2/dhall/src) |
| Futhark    | complete `src` tree                                          |   348 |        140,695 | [`f0e6988`](https://github.com/diku-dk/futhark/tree/f0e6988f2e68620a53976606fedaed8edc856244/src)                |

The line counts are orientation only. They are not a quality metric and are not apples-to-apples: Elm and PureScript include JavaScript compiler backends and build infrastructure, Dhall is primarily an evaluator/normalizer for a configuration language, and Futhark owns optimization and backend machinery that Jazz does not yet have.

## The architectural comparison

The key difference is how each codebase moves a program through phases and where semantic facts live.

```text
Jazz
source
  -> parser/CST and surface AST
  -> phase-indexed Core AST
  -> detached inference state + fact maps + attachment/reconstruction
  -> module interface records/maps/rebasing
  -> RuntimePlan / runtime engine / host outcomes
  -> observed value or host result

PureScript / Elm style
source
  -> parsed AST
  -> canonical/desugared/typed core
  -> explicit executable IR or backend input
  -> target code
  -> target runtime

Dhall style
source/imports
  -> one expression representation
  -> semantic Val
  -> type checking and normalization
  -> normalized expression/value

Futhark style
source
  -> checked source IR
  -> named typed optimization IRs
  -> memory/layout/backend IRs
  -> generic imperative code
  -> target backend
```

The other projects are not all solving the same problem. The useful comparison is not which one has the fewest modules; it is whether each representation has one stable job, whether facts travel with the thing they describe, and whether boundaries are visible in the types.

## Comparator findings

### PureScript: an explicit representation ladder keeps phase ownership visible

PureScript has a visibly staged compiler path in [`Make.hs`](https://github.com/purescript/purescript/blob/cb3c4965c8468d26c9b14cf0319db6dbd06ee4ff/src/Language/PureScript/Make.hs#L114): parse source, desugar and type-check modules, produce CoreFn, lower to CoreImp, and generate JavaScript. The main compiler pipeline does not need to infer which representation a value belongs to from a pile of maps. The representation names communicate phase ownership.

Its CoreFn expression type is an explicit post-typechecking form ([`CoreFn/Expr.hs`](https://github.com/purescript/purescript/blob/cb3c4965c8468d26c9b14cf0319db6dbd06ee4ff/src/Language/PureScript/CoreFn/Expr.hs#L18)), while CoreImp is a separate lower-level executable representation ([`CoreImp/AST.hs`](https://github.com/purescript/purescript/blob/cb3c4965c8468d26c9b14cf0319db6dbd06ee4ff/src/Language/PureScript/CoreImp/AST.hs#L61)). Module interfaces are also a durable artifact rather than a collection of independently recomputed facts: [`Externs.hs`](https://github.com/purescript/purescript/blob/cb3c4965c8468d26c9b14cf0319db6dbd06ee4ff/src/Language/PureScript/Externs.hs#L43) defines what is written and consumed across module boundaries.

PureScript is not small or simple. Its type checker has real state and substantial error handling ([`TypeChecker.hs`](https://github.com/purescript/purescript/blob/cb3c4965c8468d26c9b14cf0319db6dbd06ee4ff/src/Language/PureScript/TypeChecker.hs#L251)). The important contrast is that the stateful work ends in named, phase-specific output. Jazz's analogous facts remain spread through state, result records, maps, and later attachment/reconstruction, which makes every consumer understand more of the preceding pipeline.

The lesson for Jazz is not to copy PureScript's exact number of phases. It is to make the phase boundary a type boundary and to make the module interface a first-class serialized semantic product.

### Elm: a compact compile driver fronts explicit canonical and optimized forms

Elm's compiler entry point is comparatively small and legible. [`Compile.hs`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/compiler/src/Compile.hs#L42) coordinates a clear path from parsed source through canonicalization and optimization to JavaScript. The canonical syntax is a distinct representation ([`AST/Canonical.hs`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/compiler/src/AST/Canonical.hs#L33)), and the optimized form is a distinct graph representation ([`AST/Optimized.hs`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/compiler/src/AST/Optimized.hs#L127)). Dependency resolution and artifact/cache concerns live in the builder rather than leaking through every compiler phase ([`Build.hs`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/builder/src/Build.hs#L705)).

Elm still has many semantic passes, but the code is easier to navigate because the central question “what representation is this?” has an unambiguous answer. Canonicalization, constraint solving, graph optimization, artifact writing, and JavaScript generation are not all alternate views of the same phase-indexed node type.

The lesson for Jazz is to separate the analysis product from the execution product. A runtime or backend should not need to recover its executable shape by consulting facts that were accumulated for earlier analysis.

### Dhall: one expression model makes the interpreter boundary honest

Dhall is a different kind of system, so it is a useful counterexample rather than a direct template. Its core syntax is one parameterized expression representation ([`Syntax/Expr.hs`](https://github.com/dhall-lang/dhall-haskell/blob/bcfb3ddb17d4f55f7c4cf823c9d4072fa519edf2/dhall/src/Dhall/Syntax/Expr.hs#L30)). Evaluation produces semantic `Val` values ([`Eval.hs`](https://github.com/dhall-lang/dhall-haskell/blob/bcfb3ddb17d4f55f7c4cf823c9d4072fa519edf2/dhall/src/Dhall/Eval.hs#L497)); type checking and normalization consume that semantic model rather than a second compiler-specific instruction language ([`TypeCheck.hs`](https://github.com/dhall-lang/dhall-haskell/blob/bcfb3ddb17d4f55f7c4cf823c9d4072fa519edf2/dhall/src/Dhall/TypeCheck.hs#L186), [`Normalize.hs`](https://github.com/dhall-lang/dhall-haskell/blob/bcfb3ddb17d4f55f7c4cf823c9d4072fa519edf2/dhall/src/Dhall/Normalize.hs#L134)). Imports are expression-level terms with explicit resolution and caching logic ([`Import.hs`](https://github.com/dhall-lang/dhall-haskell/blob/bcfb3ddb17d4f55f7c4cf823c9d4072fa519edf2/dhall/src/Dhall/Import.hs#L1531)).

Dhall demonstrates what an interpreter-first architecture can buy: no RuntimePlan language is needed merely to execute the analyzed program, and there is no separate host runtime that has to consume compiler-owned scope facts. Its semantic model is itself the center of the system.

Dhall is not free of duplication or complexity. Its normalization machinery has multiple routes and its import/security semantics are substantial. It is also not a full multi-module optimizing compiler in the same sense as PureScript, Elm, or Futhark. The comparison supports a narrower conclusion: if Jazz wants an interpreter-first design, it should let a single executable semantic form be authoritative instead of preserving a compiler fact database plus a parallel runtime instruction model.

### Futhark: many IRs are justified when each one erases a different concern

Futhark is the strongest counterargument to “many intermediate representations are automatically over-engineering.” Its pass abstraction names both the input and output representation ([`Pass.hs`](https://github.com/diku-dk/futhark/blob/f0e6988f2e68620a53976606fedaed8edc856244/src/Futhark/Pass.hs#L25)), and its pipeline makes the lowering sequence explicit ([`Pipeline.hs`](https://github.com/diku-dk/futhark/blob/f0e6988f2e68620a53976606fedaed8edc856244/src/Futhark/Pipeline.hs#L118)). The repository owns separate representations for source-level array semantics, sequential and GPU-oriented forms, memory/layout stages, and imperative code; the representation inventory is visible in [`IR/Rep.hs`](https://github.com/diku-dk/futhark/blob/f0e6988f2e68620a53976606fedaed8edc856244/src/Futhark/IR/Rep.hs#L23). Internalization deliberately turns user modules into an executable internal program before later passes ([`Internalise.hs`](https://github.com/diku-dk/futhark/blob/f0e6988f2e68620a53976606fedaed8edc856244/src/Futhark/Internalise.hs#L19)), and a generic imperative code layer separates optimization from target-specific code generation ([`CodeGen/ImpCode.hs`](https://github.com/diku-dk/futhark/blob/f0e6988f2e68620a53976606fedaed8edc856244/src/Futhark/CodeGen/ImpCode.hs#L9)).

Futhark is much larger because it has real, irreducible concerns Jazz does not yet have: aggressive array optimization, memory allocation, layout, parallelism, GPU sequencing, and multiple native targets. Nevertheless, it is easier to reason about the cost of each IR because its type and name state what that IR is for. Jazz's phase-indexed AST is expensive partly because it continues to carry a broad semantic universe while detached maps supply the facts needed to make it meaningful. That is the opposite of Futhark's “each IR erases one concern” discipline.

The lesson is not “use seven IRs now.” It is “introduce a new representation only when it removes a concern, and make the pass boundary explicit when you do.”

## Ranked comparative findings

### [P1] Jazz is between two coherent architectures instead of choosing one

PureScript, Elm, and Futhark are compiler-first: their important transitions produce explicit executable or backend-facing representations. Dhall is interpreter-first: its semantic value model is the execution center. Jazz retains a rich analyzed AST and detached fact database, then builds a `RuntimePlan` and a runtime engine on top of those facts. That hybrid means both the analysis representation and execution representation remain alive and coupled.

The avoidable cost is architectural, not local function verbosity. Jazz should choose one of two near-term shapes:

- interpreter-first: make an analyzed, executable expression/value form authoritative and let runtime consume it directly; or
- compiler-first: make a small backend-neutral executable IR authoritative and lower the analyzed source form into it once.

Keeping both the fact database and the runtime instruction language as co-equal centers should be treated as transitional scaffolding, not the destination.

### [P1] The fact bus is a larger navigation tax than the comparator projects' typed phase boundaries

Jazz's semantic information is split between the phase-indexed AST, `CoreNodeId`-keyed maps, scope/type/constructor tables, inference state, and attachment/reconstruction code. A consumer often has to know which key identifies a node, which map contains the fact, when the map was populated, and whether an attached view is authoritative or derived. The core evidence is the AST/fact relationship in [AST.hs:60](../../src/Jazz/Compiler/AST.hs#L60) and [TypeInference/Analyzed.hs:223](../../src/Jazz/Compiler/TypeInference/Analyzed.hs#L223).

PureScript and Elm also carry annotations and symbol tables, but their post-analysis representations are more obviously phase-owned. Dhall moves through an expression and a semantic value. Futhark makes facts part of typed IRs and pass contracts. Jazz's maps are not inherently wrong; the problem is that they behave like a hidden inter-phase database rather than a narrowly scoped analysis context.

The reduction path is to publish one analyzed node or one executable node containing the facts its next consumer needs, then delete maps whose only purpose is to reattach those facts later. Keep genuinely global indexes—module names, source spans, or diagnostics—separate from per-node semantic payloads.

### [P1] Module interfaces are more duplicated and more mutable than the comparator artifact boundaries

Jazz module information is distributed among parallel interface structures, module analysis outputs, resolver decisions, visibility calculations, exported signatures, and rebasing/qualification logic. The module interface in [ModuleInterface.hs:51](../../src/Jazz/Compiler/ModuleInterface.hs#L51) and the import-analysis path in [ModuleAnalysis.hs:271](../../src/Jazz/Compiler/ModuleAnalysis.hs#L271) show that the boundary is assembled from multiple related products.

PureScript's externs and Elm's compiler/build artifacts make the cross-module result a recognizable product. Futhark internalizes modules into a single executable program before optimization. Dhall treats imports as expression terms and gives resolution an explicit cache/resolution boundary.

Jazz should have one authoritative module interface value containing exports, types, constructors, visibility, dependency identity, and the source information needed for diagnostics. Analysis should consume that value; it should not rebuild equivalent views from several tuples and maps. Qualification or rebasing should be a named transformation on the interface, not a separate convention repeated by callers.

### [P1] Frontend ownership overlaps between parser, CST, AST, and downstream raw-source walks

Jazz has more than one representation of source structure and more than one place where syntax is interpreted. The parser/CST, surface AST, core AST, token/source utilities, and downstream reparsing or raw-token walks all participate in determining meaning. This makes navigation hard because a feature can be “owned” by a syntax type, a parser helper, a desugaring function, or a later source inspection pass.

PureScript and Elm also preserve source locations and surface syntax, but their compiler paths make the transition into canonical forms a clear point. Dhall keeps a single expression syntax as the semantic center because its language needs no large lowering ladder.

The reduction is to make one frontend representation authoritative for semantic consumers. Keep CST/raw token data only for formatting and diagnostics, and lower to the semantic AST once. Any token-sensitive behavior that affects meaning should be resolved in the parser or desugarer rather than rediscovered in module/type/runtime code.

### [P1] Stateful inference is normal; the publication protocol around it is the avoidable part

PureScript's type checker shows that a substantial Haskell compiler can use stateful inference without being architecturally suspect. The issue in Jazz is the breadth of the `InferState` journal and the number of later products that depend on how that journal is interpreted. [TypeInference/State.hs:128](../../src/Jazz/Compiler/TypeInference/State.hs#L128) and [TypeInference/Analyzed.hs:223](../../src/Jazz/Compiler/TypeInference/Analyzed.hs#L223) indicate a pipeline where inference state, collected facts, and attached analyzed output are separate concerns.

The state should remain local to inference. Its public result should be a compact typed/analyzed program plus diagnostics and the explicit module interface updates. Consumers should not need to understand the inference journal, reconstruct facts from IDs, or distinguish several signature dialects. This is a boundary reduction, not a request to rewrite unification or remove useful type information.

### [P2] Runtime complexity is legitimate, but the compiler-to-runtime coupling is not

Jazz has real runtime requirements: lexical scope, closures, recursive bindings, constructors, host values, observation, and failure propagation. Those requirements justify an evaluator or a small VM. They do not justify making the runtime understand compiler-specific fact maps, module-analysis products, and a second instruction protocol at the same time.

The runtime engine in [Runtime/Engine.hs:475](../../src/Jazz/Compiler/Runtime/Engine.hs#L475) and the module runtime in [ModuleRuntime.hs:155](../../src/Jazz/Compiler/ModuleRuntime.hs#L155) show the coupling. Compared with Dhall's semantic-value evaluator, Jazz's runtime has to bridge a richer set of compiler artifacts. Compared with PureScript and Elm, Jazz keeps a hosted evaluator in the compiler rather than delegating execution to generated JavaScript.

The reduction path is to define a narrow runtime input: either the analyzed executable form or the backend-neutral executable IR. Runtime scope and closure state should be runtime-owned; static type/module facts should be consumed at the boundary and then disappear from the evaluator's internal protocol.

### [P2] The absence of a backend-neutral executable IR lets future concerns leak backward

PureScript's CoreImp and Futhark's generic imperative code show the value of a target-independent executable layer. Elm's optimized AST plays a similar role for its JavaScript backend. Jazz currently has analysis-oriented core data and runtime-oriented planning, but no clearly named, stable backend-neutral executable representation between them.

That absence encourages the compiler to put execution-specific decisions into analysis records and to make runtime plan construction compensate with more maps, tags, and case distinctions. It also makes future code generation harder because there is no clean place to lower into.

The fix is not to build a speculative optimizing IR. It is to define the smallest executable form the current language actually needs: literals, variables or slots, closures, calls, constructors, branching, recursive binding setup, and module entry points. If this form is only for the interpreter initially, it can later become the backend boundary without infecting the analyzer with target details.

### [P2] Jazz lacks a visible typed pass contract, while Futhark makes phase cost auditable

Futhark's `Pass fromrep torep` style makes it possible to ask what a pass consumes, what it produces, and what invariants it is responsible for. PureScript and Elm achieve a similar effect through named module types and phase-specific functions, even when they do not use a single generic pass abstraction.

Jazz's transitions are more often expressed through functions that accept and return broad records, maps, or phase-indexed nodes. The compiler therefore has implicit contracts: a field is populated “by this point,” a map key must be preserved, or a runtime plan assumes a particular analyzer ordering. These contracts are hard to discover from a module name or a type signature.

The reduction is to name the few real phase products and make their transitions explicit. A generic pass framework is not required; a small set of concrete types is preferable. The goal is auditable ownership, not abstraction for its own sake.

### [P2] The central modules are complexity hubs because they mediate data rather than own one transformation

In a compact compiler, a reader can follow “parse, analyze, lower, run.” Jazz's central modules mediate among several representations and therefore become routing hubs. The module resolver, module analysis, inference state/results, runtime plan, and driver API each know enough about adjacent phases to translate data between them. This is why the compiler feels longer than individual language features warrant.

The comparator projects split this knowledge more cleanly: Elm's builder owns build/cache orchestration, PureScript's Make owns build orchestration while CoreFn/CoreImp own compiler representations, Dhall's import layer owns imports, and Futhark's pipeline owns pass sequencing. Jazz should move orchestration out of semantic records and make each phase's input/output obvious.

### [P3] Several carrier layers can be deleted after the major boundary work

Once the fact bus and module-interface duplication are reduced, a second class of code should collapse naturally:

- wrapper records that carry one phase's data through several functions;
- parallel outcome/observation/host-result protocols;
- positional prelude conventions that duplicate named lookup;
- driver entry-point variants that differ only in plumbing;
- repeated helpers for converting among near-identical type/signature forms;
- pure and host scope walkers that implement the same lexical rules.

These are not good first targets because they are symptoms of the larger ownership problem. Deleting them before the boundaries are fixed would create churn without removing the underlying complexity.

## What the comparison says about the earlier line-reduction estimate

The code-size comparison supports revising the estimate upward from a few hundred lines, but it does not support claiming that most of Jazz's 37,171 lines are needless.

### Credible reduction bands

| Band                      | Likely change                                                                                      | Confidence  |
| ------------------------- | -------------------------------------------------------------------------------------------------- | ----------- |
| 500–1,500 lines           | remove duplicated carriers, trivial wrappers, and repeated conversion plumbing after local cleanup | high        |
| 1,000–2,500 lines         | collapse detached fact attachment and redundant analysis products                                  | medium-high |
| 700–1,800 lines           | replace parallel module-interface views and repeated visibility/rebasing paths                     | medium      |
| 500–1,500 lines           | collapse or simplify RuntimePlan/host/observation protocols and duplicate scope machinery          | medium      |
| **3,000–7,000 lines net** | **credible combined reduction after architectural consolidation**                                  | **medium**  |

The ranges overlap. They are not additive promises. The center estimate is roughly 5,000 net lines, with the lower end more likely if the team chooses a conservative interpreter-first path and the upper end more likely if it also introduces a clean executable IR and removes the old bridge layers.

Futhark is the useful warning against equating representation count with waste: its multiple IRs are justified because each removes a distinct array, memory, layout, or backend concern. Jazz should earn every representation by the same test. PureScript and Elm are the useful warning against allowing compiler orchestration to leak into every semantic data structure. Dhall is the useful warning against building a second execution language when a semantic value model would suffice.

## Recommended target shape

The smallest architecture that preserves Jazz's current direction is:

```text
Parser
  -> Surface AST
  -> Desugared/analyzed AST with local semantic payloads
  -> One authoritative ModuleInterface per module
  -> Small Executable IR
  -> Runtime evaluator or future backend

Global side channels only:
  source spans, diagnostics, module graph/cache, and explicitly global indexes
```

Specific constraints for that target:

- A node's ordinary type, binding, constructor, and call information should travel with the analyzed/executable node or with a narrowly scoped typed environment. It should not require a general-purpose map lookup for every consumer.
- The module interface should be one value and the only cross-module semantic product consumed by downstream phases.
- Parser/CST data should remain available for diagnostics but should not be a second semantic source of truth.
- Inference state should be private to inference; the public output should not expose its journal shape.
- Runtime should receive one executable input and own its lexical environment, closures, recursion setup, and host-call protocol.
- The executable form should be target-neutral and small enough that the interpreter can consume it directly.
- New representations should be added only when they erase a concrete concern, following the Futhark rule; a representation that merely renames the same information should not be added.

## Design conclusion

Compared with other Haskell-written compilers, Jazz's main architectural weakness is not that it has many files or that it uses advanced types. It is that program meaning is repeatedly moved sideways between representations rather than downward through a small number of owned phase products. Detached semantic facts, parallel module interfaces, repeated source interpretation, and compiler-specific runtime plans create a hidden protocol that every phase must understand.

PureScript and Elm show how to make a compiler-first ladder readable. Dhall shows how to make an interpreter-first semantic model direct. Futhark shows how to justify many IRs by giving each one a distinct invariant and a typed pass boundary. Jazz should take one coherent lesson from these examples: choose a center, make the data-flow edges explicit, and remove the bridges that exist only because two centers currently coexist.

net: -3,000 to -7,000 lines possible after architectural consolidation; approximately -5,000 is the best single estimate.

No source files were changed by this audit. Only this report was added.
