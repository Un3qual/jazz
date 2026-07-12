# Jazz-Next Stack-Safe Evaluation Design

**Date:** 2026-07-11

**Status:** Implemented on 2026-07-11

**Parent:**
[`JN-BOOTSTRAP-INTERPRETER-PROFILE-PLAN-001`](2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md)

## Goal

Make Jazz tail recursion safe for bootstrap-scale compiler traversals without
creating a bytecode interpreter, a virtual machine, or an intermediate form
that future LLVM code generation would replace.

The stage-0 interpreter remains the executable reference implementation. LLVM
will consume a separate backend-neutral lowered IR after the canonical typed
core. The evaluation controls and continuations in this design are private
interpreter machinery and never cross that backend boundary.

## Current Failure Mode

`jazz-next/src/JazzNext/Compiler/Runtime.hs` currently has two mutually recursive expression-evaluation paths:

- the pure path calls `evalValueWithModulePath`, `applyRuntimeFunction`,
  `evalPatternCase`, and block-scope evaluation recursively; and
- the host path repeats the same shape through `evalValueWithHost`,
  `applyRuntimeFunctionWithHost`, `evalPatternCaseWithHost`, and host-aware
  block-scope evaluation.

A Jazz closure application therefore consumes another Haskell call frame when
its body tail-calls a closure. Selecting an `if` branch or case arm does the
same. Imported closures retain their defining module path correctly, but they
still re-enter the recursive evaluator. Fixing only one entry path would leave
host-backed bootstrap programs or pure programs vulnerable and would allow the
two semantic implementations to drift further.

`jazz-next/src/JazzNext/Compiler/ModuleRuntime.hs` already owns dependency-order program evaluation, and
`jazz-next/src/JazzNext/Compiler/Driver.hs` already owns compile/run presentation. Neither module should gain
an evaluator or recursion protocol.

## Decision

`jazz-next/src/JazzNext/Compiler/Runtime.hs` will own one explicit expression-evaluation machine shared by pure
and host-backed entry points.

The machine has two conceptual controls:

- evaluate an expression with its lexical environment, defining module path,
  builtin-resolution mode, and runtime hints; or
- return a runtime value through the remaining continuation frames.

Continuation frames record only work that must happen after a nested,
non-tail evaluation: an application argument, an already-evaluated function,
the remaining elements of a list or tuple, a binary operand, a condition or
case decision, a type/result-hint obligation, or a block/scope continuation.
The concrete constructors remain private to `jazz-next/src/JazzNext/Compiler/Runtime.hs`.

The machine advances one control transition at a time in
`ExceptT Diagnostic (RuntimeHostEvaluationT m)`. Pure public helpers run the
same machine with the disabled host and `Identity`. Host-backed helpers provide
the configured `RuntimeHost`. Pure and host scope setup may keep their existing
binding-cell and deferred-host-cache responsibilities, but every expression
body and callable application enters the shared machine.

This is a CEK-style interpreter loop, not bytecode:

- it does not assign opcodes;
- it does not serialize instructions;
- it does not become an input to LLVM lowering; and
- it does not duplicate the future lowered-IR or native-runtime contracts.

## Evaluation Order

The machine preserves the current call-by-value, left-to-right behavior:

1. An application evaluates the function expression, then the argument, then
   applies the resulting callable.
2. Lists and tuples evaluate elements from left to right.
3. Binary expressions evaluate their left operand before their right operand.
   Declared operators preserve the current function-then-left-then-right
   sequencing.
4. An `if` evaluates its condition before selecting exactly one branch.
5. A pattern case evaluates its scrutinee once, tests arms in source order, and
   evaluates guards only for structurally matching arms.
6. A block evaluates declarations and non-terminal statements in source order
   before its terminal expression.
7. Qualified-method dispatch evaluates and applies captured arguments in their
   existing order.

No transition may speculate, duplicate, reorder, or replay an expression.

## Tail Positions

An expression inherits its caller's continuation, rather than pushing a new
continuation frame, in these exact positions:

- the root expression passed to an evaluator entry point;
- a closure body after the callable and argument have been evaluated and the
  argument hint has been applied;
- the selected branch of an `if`;
- the selected body of a pattern-case arm after its scrutinee, pattern, and
  optional guard have been processed;
- the terminal expression of an expression-valued block;
- the final application selected by qualified-method dispatch; and
- any of the positions above when the closure came through an ordinary
  binding, operator binding, capability method, prelude environment, or
  imported module export.

These positions are not tail positions:

- function and argument expressions;
- list and tuple elements;
- binary operands and partially applied declared operators;
- `if` conditions;
- pattern-case scrutinees and guards;
- binding right-hand sides while a scope still has statements to process; and
- non-terminal block expressions.

Runtime type obligations must not accidentally turn every typed recursive
call into a Haskell continuation chain. Argument obligations are applied before
entering a closure. Result obligations travel as interpreter-owned return
policy and are discharged in the same semantic order as today. The
implementation may normalize redundant compatible obligations, but it may not
weaken a check, change numeric defaulting, or reorder wrappers visible to
existing runtime behavior.

## Scope and Module Ownership

`jazz-next/src/JazzNext/Compiler/Runtime.hs` owns:

- evaluation controls and continuation frames;
- the transition loop;
- callable application;
- tail-position transfer;
- result-hint return policy; and
- expression-level integration with `RuntimeHost`.

`jazz-next/src/JazzNext/Compiler/ModuleRuntime.hs` continues to own:

- dependency-order module evaluation;
- construction of imported runtime environments;
- publication of typed module exports; and
- selection of the entry module's output.

An imported closure already captures its defining module path and environment.
Applying it transfers the machine control to that captured context without
adding a module-specific recursion layer. Module dependency traversal itself
is finite loader work and is not redefined as user-level tail recursion.

`jazz-next/src/JazzNext/Compiler/Driver.hs` remains an unchanged public façade unless implementation evidence
reveals an adapter-only change is required. It must not own stack limits,
continuations, or host-operation scheduling.

## RuntimeHost Ordering

The existing `RuntimeHost` contract remains the only host capability seam.
Stack-safe evaluation must preserve all of these guarantees:

- a host operation runs only when control reaches its expression;
- function and argument effects retain left-to-right order;
- condition, scrutinee, and guard effects happen before the selected tail
  transfer;
- effects in unselected branches and arms never run;
- every reached effect runs exactly once;
- deferred host cells retain their dynamic-scope cache identity; and
- fatal interpreter diagnostics remain separate from ordinary `IOError`
  values.

Tail transfer occurs only after all eager prerequisite work and host effects
for that call site have completed. The evaluator must not defer effects to
`jazz-next/src/JazzNext/Compiler/Driver.hs` or batch them around the loop.

## Diagnostics and Resource Behavior

The implementation preserves existing runtime diagnostic codes, messages,
module-path context, runtime-hint behavior, and source rendering. The
evaluation loop propagates `Diagnostic` through the existing `ExceptT`
channel; it does not catch arbitrary Haskell exceptions or translate host
programming errors into Jazz diagnostics.

The accepted guarantee is that the required tail-recursive workloads do not
grow the Haskell call stack. Non-tail work is represented by explicit
interpreter continuations and may consume memory proportional to nesting.
This child does not introduce an arbitrary language-visible recursion limit or
a new diagnostic code. Operating-system termination on total memory exhaustion
is outside the same recoverability boundary as other process-wide resource
failures.

## Regression Contract

The implementation child must add behavioral tests at these minimum depths:

- **50,000 pure tail calls:** an ordinary recursively bound closure traverses
  through closure application and conditional or case selection without a
  host stack overflow;
- **20,000 host-path tail calls:** a program forced through host-backed
  evaluation completes the recursive calls while preserving a small,
  deterministic host-operation trace; and
- **20,000 module-export tail calls:** an entry module invokes a recursive
  closure exported by a dependency, preserving the closure's defining module
  path and returning the expected result.

The depths are semantic regression floors, not user-visible recursion limits
or performance service-level objectives. Tests may use a generous timeout only
as a hang guard and must distinguish a timeout, a leaked Haskell exception, a
Jazz diagnostic, and a successful result.

Focused parity tests must also prove:

- selected `if` and case branches are stack-safe;
- terminal block expressions are stack-safe;
- ordinary, operator, qualified-method, and imported closure application keep
  existing results;
- pure and host-backed evaluation return the same existing diagnostics for
  representative failures; and
- host effects retain exact ordering and execute once across tail transfers.

The focused implementation targets are:

- `jazz-next/src/JazzNext/Compiler/Runtime.hs`;
- `jazz-next/src/JazzNext/Compiler/ModuleRuntime.hs` only if an adapter is
  required to enter the shared evaluator;
- `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RecursionTests.hs`;
- `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/HostIOTests.hs`;
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`; and
- `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`.

`jazz-next/src/JazzNext/Compiler/Driver.hs` is an inspected boundary, not a planned implementation target.

## Verification

The implementation child must pass:

```bash
cd jazz-next
cabal test runtime-semantics-spec loader-spec --test-show-details=failures
bash scripts/test-warning-config.sh
cd ..
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

Before implementation, the plan must record RED evidence for the three depth
regressions against the recursive evaluator and GREEN evidence after the shared
machine lands.

Implementation verification completed on `2026-07-11`. The shared machine
passes 50,000 pure closure calls, 10,000 selected case-arm calls, 10,000 typed
closure calls, 20,000 host-backed calls with its pre-recursion effect executed
exactly once, and 20,000 calls through an imported closure. Rendered pure/host
diagnostics match for representative failures. The focused runtime and loader
suites and the full `jazz-next/scripts/test-warning-config.sh` matrix pass, and
boundary searches confirm that machine types remain private to `jazz-next/src/JazzNext/Compiler/Runtime.hs`
with no bytecode, opcode, lowered-IR, or LLVM execution coupling.

## Non-Goals

This child does not add or design:

- bytecode, opcodes, a bytecode VM, or an instruction serializer;
- backend-neutral lowered IR;
- LLVM IR generation, optimization, object emission, linking, or a native
  runtime;
- lexer or parser implementation;
- new effect syntax or effect inference;
- concurrency or asynchronous evaluation;
- lazy evaluation; or
- a general module-loader rewrite.

The future native compiler remains:

```text
source -> surface AST -> canonical typed core -> backend-neutral lowered IR
       -> LLVM IR -> object files -> native linker -> native executable
```

The interpreter machine remains beside that pipeline as the stage-0 reference
executor and development oracle.
