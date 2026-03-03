# Additional Spec Gap Matrix (Post Items 1-12a)

## Purpose

Record unresolved language-spec areas that are still unclear after existing cleanup/clarification plans, with concrete code/spec evidence and non-overlap checks.

## Gap Matrix

| Domain | Why Still Unclear | Primary Evidence | Non-Overlap Justification |
| --- | --- | --- | --- |
| Binding + signature coherence | `ELet`/`ETypeSignature` behavior is only partially encoded and order-sensitive; signature tests are marked broken. | `jazz-hs/src/Analyzer/TypeInference.hs` (`inferExpr` handling of `ELet`/`ETypeSignature`), `jazz-hs/test/Analyzer/TypeInferenceSpec.hs` (commented "Currently broken" signature tests) | Existing item #7 defines type grammar and constrained syntax, but not declaration ordering/shadowing/recursion/signature-binding semantics. |
| `if` expression language surface | `EIf` exists in AST/codegen, but parser has no `if` grammar path; reserved words include `if`/`else`; inference has no `EIf` branch. | `jazz-hs/src/AST.hs`, `jazz-hs/src/CodeGen/Javascript.hs`, `jazz-hs/src/Parser/Lang.hs`, `jazz-hs/src/Parser/Lib.hs`, `jazz-hs/src/Analyzer/TypeInference.hs` | Existing item #6 focuses parse-only accepted features; `if` is the inverse drift (AST/codegen-only), requiring separate control-flow surface clarification. |
| Operator/fixity/sections policy | Operator table is hardcoded, user-defined operators are TODO, and right-partial section support relies on synthetic lambda naming hacks. | `jazz-hs/src/Parser/Operator.hs`, `jazz-hs/src/Parser/Lang.hs`, `jazz-hs/test/ParserSpec.hs` | Existing items #1/#7 clarify surface syntax and type arrows, but not long-term operator extensibility/fixity governance. |
| Primitive semantic contract | Runtime semantics of equality and primitive operations remain backend-dependent (`==` in JS codegen), while interpreter-first strategy is now planned. | `jazz-hs/src/CodeGen/Javascript.hs`, `jazz-hs/src/Types.hs`, `docs/plans/spec-clarification/2026-03-02/runtime/12-backend-target-strategy.md` | Existing item #12 chooses backend direction; this gap is the missing language-level primitive semantics that must survive backend changes. |
| `jazz2` authority and migration role | `jazz2` encodes a different AST direction but is unfinished; no explicit policy states whether it is active target, archive, or reference-only. | `jazz2/src/Jazz/AST.hs`, `jazz2/src/Jazz/Parser.hs`, `jazz2/src/Jazz/Parser/Lexer.hs`, `docs/jazz-language-state.md` | Existing docs note `jazz2` is unfinished, but no governance contract prevents future spec drift between `jazz-hs` and `jazz2`. |

## Recommended Execution Order

1. Binding + signature coherence (`13`) because it affects typechecker stability and declaration semantics globally.
2. `if` surface decision (`14`) to remove AST/parser/control-flow inconsistency.
3. Operator/fixity policy (`15`) to stop parser hacks from becoming de facto language behavior.
4. Primitive semantic contract (`16`) to stabilize runtime meaning across backend transitions.
5. `jazz2` authority policy (`17`) to prevent governance drift while other work proceeds.

## Progress

- [x] Domain list assembled from current code and specs
- [x] Evidence anchored to concrete files
- [x] Non-overlap with items `1-12a` documented
- [ ] Maintainer sign-off on domain priority order
