# RFC 0004: Hosted canonical compiler

Status: Accepted
Date: 2026-07-31
Supersedes: Hosted lexer, parser, and core decisions dated 2026-07-11, 2026-07-12, 2026-07-16, 2026-07-17, 2026-07-20, and 2026-07-21.

## Decision

Jazz-authored lexing, parsing, and canonical-core lowering are compiler-private
modules under `jazz/compiler/`. They run as ordinary Jazz code through stage 0
and are verified by exact differential comparison with the Haskell compiler.
They do not call Haskell compiler callbacks or use lexer-, parser-, or lowerer-
specific host intrinsics.

The hosted lexer returns a deterministic language-owned result containing the
logical source path, complete token sequence or structured lexical failure,
raw lexemes, decoded values, and one-based spans. The test-only Haskell adapter
normalizes stage-0 results into that same ordinary Jazz schema; rendered output
is evidence, not a second protocol.

The hosted parser:

- consumes canonical lexer tokens at its primary boundary;
- returns one complete surface AST or one structured failure;
- remains fail-fast, with no recovery or partial tree;
- preserves lexical and parser failures as distinct source-facade outcomes;
- uses one compiler-local generic parser kernel and explicit immutable grammar
  context; and
- keeps expression, pattern, signature, declaration, operator, program, and
  facade ownership separate without publishing a standard-library parser API.

The complete hosted parser matches stage 0 across the fixed 365-fixture corpus.
Its grammar includes expressions, declarations, modules, imports and exports,
types and signatures, lambdas, control flow, patterns, and fixed and source-
local operators. Scale scheduling follows RFC 0008.

The hosted canonical-core lowerer consumes the hosted surface AST and mirrors
the current pre-inference core boundary. It preserves desugaring, generated
names, signature and declaration payloads, module/import metadata, export
selectors, expected-module-path validation, and source-qualified spans. Module
lowering has only the structured counterparts of `E4005` (multiple module
declarations) and `E4006` (declared path mismatch) at this boundary.

Expression lowering is total over the fixed surface schema. The composed
source facade calls the hosted parser once and preserves lexical, parser,
module-lowering, and successful results as distinct values. The accepted
parser corpus currently has 196 successful fixtures, all covered by repeated
hosted source-to-core parity.

Stage 0 remains the production lexer, parser, and lowerer. The Jazz-authored
pipeline is a hosted canonical boundary exercised through differential tests
until a separate integration RFC promotes it. This RFC does not host module
resolution, analysis, type inference, evidence elaboration, runtime execution,
or backend generation.

## Context

Porting the compiler as one unit would make semantic drift difficult to locate.
Redesigning core at the same time would also remove the stable oracle needed to
show that the Jazz-authored frontend preserves behavior.

The selected sequence fixed canonical value schemas first, then implemented
grammar and lowering in reviewable slices while retaining one Haskell semantic
reference. Complete structured comparison catches differences that acceptance-
only tests or rendered diagnostics would miss.

## Consequences

- Hosted frontend changes must preserve deterministic, complete structured
  parity or deliberately revise the shared contract in both implementations.
- Test adapters translate already-produced values; they may not reproduce
  lexer, parser, or lowering decisions.
- Public parser recovery, a general parser library, and production cutover each
  require separate decisions.
- Canonical core remains the interpreter input and is not annotated in place
  with post-inference backend data.
- Hosted parity is real compiler progress but is not a claim that the complete
  compiler or native toolchain is self-hosted.
