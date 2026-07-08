# Execution Queue

Read this file before scanning the rest of `docs/`. It is the dispatch source of truth for next-batch execution.

## Ready Now

`Ready Now` should contain implementation-first entries by default. `kind: docs` or `kind: coordination` items belong here only when they are the smallest verified unblocker for the next implementation batch.

| id | title | priority | size | kind | autonomous_ready | depends_on | plan | plan_section | target_paths | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |

Current executor status (`2026-07-08`): `Ready Now` is empty after the final
accepted type-solver child, compiler-owned runtime evidence, landed. The
accepted children from the remaining-slices plan are complete: ordinary binding
schemes/per-use instantiation, solver-backed variable constrained signatures,
inferred class constraints, final defaulting/ambiguity diagnostics, explicit
type application, and compiler-owned runtime evidence. There is no
source-backed next curation target and no named candidate currently. Keep
default methods, superclasses, module method import/export behavior,
orphan/overlap changes, dictionary optimization, primitive mixed-width
behavior, typed integer-to-float promotion, and pattern synonyms blocked until
separate accepted contracts name target paths and focused verification.

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

## Blocked

| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |
| `JN-MODULE-REBASE-PLAN-001` | Rebase module/import execution plan onto active `jazz-next` owners | `Concrete future stdlib/catalog API or module behavior contract; none currently after stdlib/prelude validation` | Active-path ownership is now rebased onto `ModuleResolver.hs`, `Driver.hs`, `CLI/Main.hs`, and loader/module tests. Dependency-module expression isolation, explicit symbol-list visibility, alias-import unqualified visibility, `Alias::symbol` qualified alias lookup, default bundled-prelude module graph helpers, bundled-prelude reproducibility coverage, and explicit no-prelude module graph ownership coverage are landed. Current module/import and stdlib execution semantics are closed for the active subset. `JN-STDLIB-PRELUDE-NEXT-API-CONTRACT-001` found no source-backed future stdlib/catalog API or module behavior to promote; keep the umbrella blocked until a concrete API/runtime contract names target paths and focused verification. | [2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md](../plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md) | `2026-06-24` |
| `JN-USER-DEFINED-OPERATORS-PLAN-001` | Stage user-defined operator work beyond frozen v1 operators | `No accepted operator child after custom associativity` | Stage 2 fixed-tier declarations, same-source executable function bindings, operator-specific adjacent signatures, custom numeric precedence, and explicit `left`/`right`/`nonassoc` associativity are complete for parser declaration recognition, source-unit-local metadata, fixed-tier parsing, custom `precedence 1..99` parsing, diagnostics, ordinary callable `(op) = <expr>.` bindings, infix use, bare operator values, left sections, right sections, recursive local bindings, signature-constrained hidden bindings, non-associative same-precedence chain rejection, and module replay isolation without user-visible operator imports or exports. Runtime overload dispatch, cross-module operator binding APIs, and new built-in operators remain blocked until separate executable contracts exist. | [15-operator-fixity-and-sections.md](../plans/spec-clarification/2026-03-03/syntax/15-operator-fixity-and-sections.md) | `2026-07-08` |
| `JN-TYPE-GRAMMAR-CLOSURE-PLAN-001` | Plan type-system expansion beyond rebase closure | `No accepted type-solver child after runtime evidence` | The broad type-solver coordination contract is accepted and recorded as `JN-TYPE-SOLVER-CONTRACT-001`. Ordinary-binding schemes/per-use instantiation, solver-backed variable constrained-signature schemes, inferred class constraints from strict equality and qualified method requirements, final defaulting/ambiguity diagnostics, explicit type application, and compiler-owned runtime evidence have landed. No remaining accepted child is source-backed; the umbrella stays blocked until a new concrete type-system contract names target paths and focused verification. | [2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md](../plans/2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md) | `2026-07-08` |
| `JN-PATTERN-FUTURE-FORMS-PLAN-001` | Plan future pattern forms beyond the active subset | `Pattern synonyms explicitly deferred; no candidate child` | Literal, wildcard, variable, constructor, exact-length list, cons-like list, fixed-arity tuple, as-patterns, top-level case-arm or-patterns, top-level lambda-parameter or-patterns, and single `if` guards now execute in the active subset. Maintainer confirmed on `2026-06-30` that pattern synonyms should stay blocked for now; reopen only after a separate binder/type/runtime contract names target paths and focused verification. | [2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md](../plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md) | `2026-06-30` |
| `JN-PRIMITIVE-SURFACE-EXPANSION-PLAN-001` | Plan primitive semantics expansion beyond the v1 runtime subset | `No accepted primitive delta after direct typed integer-to-Float64 promotion` | The v1 primitive/runtime subset is implemented for integer arithmetic, same concrete `Float`/`Float16`/`Float32`/`Float64` arithmetic, same concrete `Float`/`Float16`/`Float32`/`Float64` comparison/equality, direct binary Float64-domain integer-literal arithmetic targeting, direct binary typed integral-to-`Float`/`Float64` promotion for arithmetic/comparison/equality, structural list/tuple equality when nested element types are equality-supported, structural ADT equality when every declared constructor payload type is equality-supported, callable equality/inequality rejection, equality/inequality, `map`, `filter`, `hd`, `tl`, `print!`, explicit target-named numeric conversions through the active prelude/kernel boundary, default `toInt`/`toFloat` prelude aliases, numeric width signature ownership, default Float64 fractional literal values, direct annotated `Float16`/`Float32` fractional literal bindings, and parser-owned lowercase `f16`/`f32`/`f64` fractional literal suffixes. Implicit mixed-width behavior, `Float16`/`Float32` promotion, operator-value or section promotion, user-defined operator behavior, and broader numeric solver work remain blocked until separate contracts are accepted. | [16-primitive-semantics-contract.md](../plans/spec-clarification/2026-03-03/runtime/16-primitive-semantics-contract.md) | `2026-06-30` |
| `JN-RUNTIME-PRODUCTIZE-CLOSURE-PLAN-001` | Productize interpreter-first execution beyond docs closure | `Additional runtime product delta beyond compile/run/help/stdin-selector baseline` | The runtime/driver active path now has diagnostic-only compile results, run-mode execution, module graph helpers, bundled-prelude reproducibility coverage, explicit CLI help output, and an explicit `-` stdin source selector for standalone compile and `--run`. The interpreter-first compile/run/help/stdin-selector baseline is closed. Additional runtime product work remains blocked until another concrete delta is accepted. | [2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md](../plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md) | `2026-06-29` |
| `JN-PURITY-EFFECT-TYPING-PLAN-001` | Plan full purity/effect typing beyond stub-v1 enforcement | `Remaining module-method/export and effect-system contracts` | Stub-v1 purity enforcement, `print!` support, local impl-method direct-call enforcement, solver-backed constrained-signature schemes, final defaulting, and compiler-owned runtime evidence are implemented and verified in `jazz-next`. Higher-order purity, effect types, cross-module purity graphs, runtime enforcement, inferred effects, and effect typing in signatures remain blocked until module-method/export behavior and a concrete effect-system contract are clearer. | [03-purity-bang-semantics.md](../plans/spec-cleanup/2026-03-02/decisions/03-purity-bang-semantics.md) | `2026-07-08` |
| `JN-ABSTRACTION-SEMANTICS-PLAN-001` | Plan class/impl abstraction semantics beyond parser boundaries | `No accepted abstraction child after runtime evidence; future dictionary/default-method/superclass/solver/module-method contracts still blocked` | Active `jazz-next` now parses and lowers canonical `class` declarations with explicit lowercase parameter metadata plus signature-only method metadata, concrete `impl` declarations with method binding metadata, rejects duplicate class method signatures with `E1006`, rejects duplicate impl method bindings with `E1007`, rejects class method body/default syntax, rejects non-binding or non-concrete method-bearing impl bodies, rejects duplicate class names and duplicate concrete impl facts, validates concrete constrained signatures against known class/impl facts using declared class arity, executes explicit `Class::method` references by visible concrete impl method bodies including typed selection across multiple concrete candidates, validates concrete impl method bodies against the substituted class method signature, attaches compiler-owned runtime evidence to concrete method candidates, exposes bundled-prelude `Eq(Int).equals`, `Eq(Bool).equals`, `Eq(Float).equals`, `Eq(Float16).equals`, `Eq(Float32).equals`, and `Eq(Float64).equals` method bodies through default prelude loading under the narrow `Float`/`Float64` alias-overlap policy, and permanently rejects non-canonical `trait` declarations. User-visible dictionaries, dictionary optimization, default methods, superclasses, overlap/orphans beyond duplicate visible facts, broader bundled-prelude method families, and module export/import behavior for methods remain blocked until separate executable contracts define syntax, target paths, and focused verification. | [01-authoritative-syntax.md](../plans/spec-cleanup/2026-03-02/decisions/01-authoritative-syntax.md) | `2026-07-08` |

## Done

Completed items must be moved to [done-archive.md](done-archive.md) during the same closeout pass that marks them done. This table stays empty so `queue.md` remains a dispatcher instead of a changelog.

| id | closure evidence | completed_on |
| --- | --- | --- |

## Entry Template

```md
| `ITEM-ID` | Short task name | `P1` | `S` | `impl` | `yes` | `-` | [linked-plan.md](../plans/example.md) | `Task 2` | `jazz-next/src/JazzNext/Compiler/Foo.hs`, `jazz-next/test/JazzNext/Compiler/FooSpec.hs` | Concrete implementation deliverable. | Exact verification command(s). | `YYYY-MM-DD` |
```
