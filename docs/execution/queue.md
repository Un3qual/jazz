# Execution Queue

Read this file before scanning the rest of `docs/`. It is the dispatch source of truth for next-batch execution.

## Ready Now

`Ready Now` should contain implementation-first entries by default. `kind: docs` or `kind: coordination` items belong here only when they are the smallest verified unblocker for the next implementation batch.

| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |

Current executor status (`2026-07-30`): the pre-bootstrap language-quality gate
is complete, including synchronized ordinary pattern lambdas, ordered
multi-body `\|` clauses, intentional explicit `case` dispatch, structured
constructor fields, the authored-source feature audit, and six full
algorithmic workloads. Haskell-style function equations were removed from both
frontend implementations and the authored corpus.
The typed-core expression/direct-call child completed on `2026-07-30`: its
opt-in single-pass producer creates one resolved-module scalar/direct-call
typed-core profile and deterministically lowers it to validated backend-neutral
IR. Normal compile/run remains canonical-core/interpreter based, and permanent
mirrored contracts remain unchanged. Closure/recursion is the next ordered
design gate; it is not an implementation row. Control flow, multi-module
integration, LLVM, object/link, and native-runtime implementation remain
unpromoted.

## Next Curation Target

Use this section when `Ready Now` has no executable entry, or when a
coordination batch explicitly pre-seeds post-child candidates behind the current
ready row. Keep it to 1-3 promotion candidates, ordered by the recommended next
promotion. A candidate is not a queue row yet; the next curation pass should
either promote it into `Ready Now` with matching plan frontmatter or replace it
with a better source-backed candidate. Leave this table empty only when the
current executor status explicitly says there is no source-backed next curation
target and no named candidate currently.

| blocked_id | candidate_child_id | kind | source_contract | why_next | target_paths | verification | promotion_check |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `JN-BOOTSTRAP-INTERPRETER-PROFILE-PLAN-001` | `JN-BOOTSTRAP-TYPED-CORE-CLOSURE-RECURSION-DESIGN-001` | `docs` | [JN-BOOTSTRAP-INTERPRETER-PROFILE-PLAN-001](blocker-contracts.md#jn-bootstrap-interpreter-profile-plan-001) | Define the next typed-core/lowering boundary for closures and recursion after the completed scalar/direct-call profile. | `docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md`, `docs/superpowers/specs/2026-07-30-jazz-next-typed-core-expression-direct-call-design.md` | `bash scripts/check-execution-queue.sh`; `bash scripts/check-docs.sh` | Requires an approved closure/recursion design and a separate executor-ready child before implementation promotion. |

## Blocked

| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |
| `JN-BOOTSTRAP-INTERPRETER-PROFILE-PLAN-001` | Stage the hosted Jazz bootstrap interpreter profile | `Approved closure/recursion design and executor-ready child` | The hosted lexer, parser, canonical core, typed-core and lowered-IR contract foundations, language-quality gate, and opt-in scalar/direct-call typed-core producer/lowerer are complete. Normal compile/run remains canonical-core/interpreter based. Closure/recursion is next only as a design gate; control flow, multi-module integration, LLVM, object/link, and native-runtime implementation remain unpromoted. | [2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md](../superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md) | `2026-07-30` |
| `JN-MODULE-REBASE-PLAN-001` | Rebase module/import execution plan onto active `jazz-next` owners | `No accepted module child after explicit export lists` | Active-path ownership is rebased onto the current resolver/compiler/runtime path. `JN-MODULE-TYPED-EXPORT-INVENTORY-001` and `JN-MODULE-EXPLICIT-EXPORT-LIST-001` landed the shared inventory plus header allowlists, local/public separation, deterministic `E4015`, compiler import filtering, and runtime publication. Re-exports, alias-qualified classes, package semantics, and further stdlib/catalog growth remain blocked behind separate contracts. | [2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md](../plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md) | `2026-07-10` |
| `JN-USER-DEFINED-OPERATORS-PLAN-001` | Stage user-defined operator work beyond frozen v1 operators | `No accepted operator child after custom associativity` | Stage 2 fixed-tier declarations, same-source executable function bindings, operator-specific adjacent signatures, custom numeric precedence, and explicit `left`/`right`/`nonassoc` associativity are complete for parser declaration recognition, source-unit-local metadata, fixed-tier parsing, custom `precedence 1..99` parsing, diagnostics, ordinary callable `(op) = <expr>.` bindings, infix use, bare operator values, left sections, right sections, recursive local bindings, signature-constrained hidden bindings, non-associative same-precedence chain rejection, and module replay isolation without user-visible operator imports or exports. Runtime overload dispatch, cross-module operator binding APIs, and new built-in operators remain blocked until separate executable contracts exist. | [15-operator-fixity-and-sections.md](../plans/spec-clarification/2026-03-03/syntax/15-operator-fixity-and-sections.md) | `2026-07-08` |
| `JN-TYPE-GRAMMAR-CLOSURE-PLAN-001` | Plan type-system expansion beyond rebase closure | `No accepted type-solver child after runtime evidence` | The broad type-solver coordination contract is accepted and recorded as `JN-TYPE-SOLVER-CONTRACT-001`. Ordinary-binding schemes/per-use instantiation, solver-backed variable constrained-signature schemes, inferred class constraints from strict equality and qualified method requirements, final defaulting/ambiguity diagnostics, explicit type application, and compiler-owned runtime evidence have landed. No remaining accepted child is source-backed; the umbrella stays blocked until a new concrete type-system contract names target paths and focused verification. | [2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md](../plans/2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md) | `2026-07-08` |
| `JN-PATTERN-FUTURE-FORMS-PLAN-001` | Plan future pattern forms beyond the active subset | `Pattern synonyms explicitly deferred; no candidate child` | Literal, wildcard, variable, constructor, exact-length list, cons-like list, fixed-arity tuple, as-patterns, top-level case-arm or-patterns, top-level lambda-parameter or-patterns, and single `if` guards now execute in the active subset. Maintainer confirmed on `2026-06-30` that pattern synonyms should stay blocked for now; reopen only after a separate binder/type/runtime contract names target paths and focused verification. | [2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md](../plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md) | `2026-06-30` |
| `JN-PRIMITIVE-SURFACE-EXPANSION-PLAN-001` | Plan primitive semantics expansion beyond the v1 runtime subset | `No accepted primitive delta after host text I/O` | The v1 primitive/runtime subset includes integer and floating arithmetic/comparison/equality, direct typed integral-to-`Float`/`Float64` promotion, structural list/tuple/ADT equality, validated single-line `Char`/`Text` literals, signatures, literal patterns, strict `Char`/`Text` equality, source rendering, module transport, explicit-import scalar `Text` length/emptiness/uncons, strict UTF-8 host text I/O, arguments, and exit, plus `map`, `filter`, `hd`, `tl`, `print!`, numeric conversions, defaults, and suffixes. Indexing, slicing, classification, ordering, concatenation/builders, search, bytes/binary I/O, handles, implicit `Char`/`Text` conversion, implicit numeric mixed-width behavior, `Float16`/`Float32` promotion, and broader numeric solver work remain blocked until separate contracts are accepted. | [16-primitive-semantics-contract.md](../plans/spec-clarification/2026-03-03/runtime/16-primitive-semantics-contract.md) | `2026-07-11` |
| `JN-RUNTIME-PRODUCTIZE-CLOSURE-PLAN-001` | Productize interpreter-first execution beyond docs closure | `Additional runtime product delta beyond compile/run/help/stdin-selector baseline` | The runtime/driver active path now has diagnostic-only compile results, run-mode execution, module graph helpers, bundled-prelude reproducibility coverage, explicit CLI help output, and an explicit `-` stdin source selector for standalone compile and `--run`. The interpreter-first compile/run/help/stdin-selector baseline is closed. Additional runtime product work remains blocked until another concrete delta is accepted. | [2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md](../plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md) | `2026-06-29` |
| `JN-PURITY-EFFECT-TYPING-PLAN-001` | Plan full purity/effect typing beyond stub-v1 enforcement | `Remaining module-method/export and effect-system contracts` | Stub-v1 purity enforcement, `print!` support, local impl-method direct-call enforcement, solver-backed constrained-signature schemes, final defaulting, and compiler-owned runtime evidence are implemented and verified in `jazz-next`. Higher-order purity, effect types, cross-module purity graphs, runtime enforcement, inferred effects, and effect typing in signatures remain blocked until module-method/export behavior and a concrete effect-system contract are clearer. | [03-purity-bang-semantics.md](../plans/spec-cleanup/2026-03-02/decisions/03-purity-bang-semantics.md) | `2026-07-08` |
| `JN-ABSTRACTION-SEMANTICS-PLAN-001` | Plan class/impl abstraction semantics beyond parser boundaries | `Typed module export inventory landed; broader abstraction deltas remain blocked` | Active `jazz-next` supports explicit `Class::method` dispatch through visible local, prelude, and non-aliased imported class capability facts. `JN-MODULE-TYPED-EXPORT-INVENTORY-001` landed the architecture-only inventory follow-up without new syntax or impl policy. User-visible dictionaries, dictionary optimization, default methods, superclasses, overlap/orphans beyond duplicate visible facts, broader bundled-prelude method families, alias-qualified classes, and re-exports remain blocked until separate executable contracts define syntax, target paths, and focused verification. | [01-authoritative-syntax.md](../plans/spec-cleanup/2026-03-02/decisions/01-authoritative-syntax.md) | `2026-07-09` |

## Done

Completed items must be moved to [done-archive.md](done-archive.md) during the same closeout pass that marks them done. This table stays empty so `queue.md` remains a dispatcher instead of a changelog.

| id | closure evidence | completed_on |
| --- | --- | --- |

## Entry Template

```md
| `ITEM-ID` | Short task name | `P1` | `S` | `impl` | `yes` | `-` | [linked-plan.md](../plans/example.md) | `Task 2` | `jazz-next/src/JazzNext/Compiler/Foo.hs`, `jazz-next/test/JazzNext/Compiler/FooSpec.hs` | Concrete implementation deliverable. | Exact verification command(s). | `YYYY-MM-DD` |
```
