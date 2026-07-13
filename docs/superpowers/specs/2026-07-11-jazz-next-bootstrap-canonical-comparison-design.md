# Jazz-Next Bootstrap Canonical Lexer Comparison Design

## Status

Approved in discussion on `2026-07-12`. This document locks the comparison
contract for the first Jazz-authored compiler component. Implementation is split
into two ordered children: the canonical reference adapter and parity harness,
followed by the Jazz-authored lexer. The children are suitable for stacked pull
requests because the lexer consumes the comparison contract without changing
it.

## Goal

Define one deterministic, language-owned representation for lexer results so
the Haskell stage-0 lexer and a Jazz-authored lexer can be compared exactly over
the accepted and rejected parser fixture corpus.

The contract must survive the later LLVM transition. It therefore consists of
ordinary Jazz ADTs, lists, `Text`, `Char`, and `Int` values rendered by the
existing generic runtime-value renderer. It does not introduce a serializer
primitive, bytecode, a VM, lowered IR, LLVM-specific values, or an artifact
format.

## Chosen Architecture

`CanonicalLexResult` is the semantic interface. Both implementations normalize
their output into that interface:

```text
Haskell source text -> stage-0 lexer -> test-only Haskell adapter --+
                                                                 |
Jazz source text -> Jazz lexer -> canonical Jazz ADTs ------------+->
  generic runtime-value renderer -> exact text comparison
```

The Jazz lexer may use private state and token types internally. Its public
comparison adapter returns only the canonical ADTs. The Haskell adapter is the
only comparison component allowed to know the Haskell `TokenKind`, lexer-error,
Megaparsec, or diagnostic representations.

Both sides are rendered by `renderRuntimeValue`. The rendered text is evidence,
not a second semantic schema. No hand-written JSON, line protocol, or custom
Jazz serializer is added.

## Canonical Value Schema

The Jazz-owned schema is:

```text
CanonicalLexResult
  = CanonicalLexSuccess(CanonicalSourcePath, List(CanonicalToken))
  | CanonicalLexFailure(CanonicalSourcePath, CanonicalLexError)

CanonicalSourcePath
  = CanonicalSourcePath(Text normalizedPath)

CanonicalToken
  = CanonicalToken(CanonicalTokenKind, Text rawLexeme, CanonicalSpan)

CanonicalSpan
  = CanonicalSpan(Int line, Int column)

CanonicalTokenKind
  = IdentifierKind(Text name)
  | KeywordKind(CanonicalKeyword)
  | IntegerKind(Text canonicalDecimal)
  | CharacterKind(Char decodedValue)
  | TextKind(Text decodedValue)
  | OperatorKind(Text symbol)
  | PunctuationKind(CanonicalPunctuation)
```

`CanonicalKeyword` enumerates `module`, `import`, `as`, `data`, `if`, `else`,
and `case`. `CanonicalPunctuation` enumerates arrow, at sign, equals, colon,
double colon, dot, braces, parentheses, brackets, comma, and lambda marker.
Symbolic operators remain `OperatorKind` values and carry their spelling.

Every token carries its exact raw lexeme in addition to its semantic payload.
This distinguishes keyword, punctuation, operator, leading-zero integer, and
escape spellings without weakening decoded-value comparison. Character and text
tokens carry both the raw source lexeme and the decoded scalar or scalar
sequence.

Integer payloads use canonical base-10 `Text`, not Jazz `Int`. The Haskell lexer
accepts arbitrary-precision integer literals, so routing that payload through a
fixed-width runtime integer would make the comparison lossy. Leading zeroes are
preserved only by `rawLexeme`; `canonicalDecimal` contains the mathematical
value's unsigned decimal spelling.

## Lexical Failure Schema

Lexical failure is structured rather than recovered from Megaparsec prose:

```text
CanonicalLexError
  = CanonicalLexError(
      Text code,
      CanonicalLexErrorReason reason,
      CanonicalSpan span
    )

CanonicalLexErrorReason
  = UnexpectedCharacter(Char value)
  | UnexpectedEndOfInput
  | InvalidCharacterLength(Int actualScalars)
  | UnterminatedLiteral(CanonicalLiteralKind)
  | RawNewline(CanonicalLiteralKind)
  | InvalidEscape(Char escape)
  | UnterminatedUnicodeEscape
  | MalformedUnicodeEscape(Text digits)
  | NonScalarUnicodeEscape(Text digits)
  | InvalidLiteralCharacter(CanonicalLiteralKind, Char value)
  | InvalidIntegerLiteral(Text rawDigits)

CanonicalLiteralKind
  = CharacterLiteral
  | TextLiteral
```

The active lexer code remains `E0001`. The span is the 1-based start position
of the failing token, matching stage 0. Error payloads preserve the offending
scalar or raw digit text where one exists. Haskell exception names,
Megaparsec's rendered expectation sets, and complete rendered `Diagnostic`
summaries are excluded.

The stage-0 lexer must expose its lexical reason as structured data before it
is converted to the existing public `Diagnostic`. The compatibility
`tokenize :: Text -> Either Diagnostic [Token]` entry point remains intact; the
comparison adapter must not parse `diagnosticSummary` to reconstruct a reason.

## Paths, Coordinates, and Source Text

Fixture paths are logical, nonempty, relative paths using `/` separators.
Normalization removes empty and `.` segments and rejects absolute paths and
`..` segments. An invalid path is a parity-harness input error, not a lexical
failure produced by either lexer. Ambient checkout paths never enter a
canonical result.

Line and column values count Unicode scalar values and remain 1-based. A tab
advances to the next eight-column stop, matching the current stage-0 lexer.
Source text is not line-ending-normalized before lexing. LF and CRLF inputs are
separate fixtures so both implementations must account for the same consumed
scalars and positions.

## Fixture Corpus and Parity Rules

The parity suite owns an explicit fixture manifest. Every fixture has a unique
stable name, normalized logical path, exact source `Text`, and an accepted or
rejected parser classification. Classification is corpus metadata; neither
lexer may infer it by running the parser before comparison.

The manifest must include every current accepted and rejected parser source
fixture that reaches `tokenize` or `parseSurfaceProgram`. Shared fixture values
should be consumed by the existing parser tests where practical so the parity
corpus does not become a stale duplicate. New parser fixtures must either use
the shared manifest or add a matching manifest entry in the same change.

Both lexers receive the same normalized path and unmodified source text.

- When lexing succeeds, compare the complete token sequence exactly: order,
  token kind, kind payload, raw lexeme, decoded value, line, and column. This
  comparison still runs when the parser later rejects the fixture.
- When lexing fails, compare path, code, structured reason and payload, line,
  and column exactly.
- Whitespace and comments do not produce tokens, but they must influence later
  spans identically.
- Parser ASTs, parser diagnostics, name resolution, and type information are
  outside this contract.

The corpus includes focused families for every token constructor, comments,
spaces, tabs, LF, CRLF, Unicode scalar literals, all supported escapes,
leading-zero and arbitrary-precision integers, operator runs, malformed
literals, malformed Unicode escapes, and unexpected characters. Stable named
generated cases may supplement the manifest, but they do not replace its
human-readable boundary cases.

## Determinism and Evolution

Each Jazz fixture is evaluated and rendered twice; both renderings must be
byte-identical before either is compared with stage 0. Fixture iteration order
is manifest order, and failure reporting names the stable fixture identifier.

The Haskell adapter is total over the current `TokenKind` and structured lexer
reason constructors. Adding either constructor must cause an exhaustiveness
failure until its canonical mapping and fixtures are supplied.

The schema has no version field in this tranche. A constructor or payload
change is an intentional contract change reviewed alongside both adapters.
Versioning becomes useful only when more than one canonical schema must coexist;
there is no evidence for that complexity yet.

## Permanent Bootstrap Support APIs

The Jazz lexer needs to construct lexemes, classify source scalars, and decode
Unicode escapes. The second child may therefore add the smallest permanent
Jazz-level `Char` and `Text` APIs required by the lexer, including text
construction, scalar/code-point conversion, and deterministic scalar
classification.

Those APIs must sit behind the existing builtin catalog and Jazz stdlib module
boundary. They are semantic runtime services that the future native runtime
will implement; they are not Haskell callbacks embedded in lexer code. Their
tests must cover Unicode scalar rejection and the classification behavior used
by the stage-0 lexer. No lexer-specific kernel primitive is permitted.

## Ordered Implementation Children

### Child 1: `JN-BOOTSTRAP-CANONICAL-COMPARISON-001`

Build the contract and reference side:

1. preserve structured stage-0 lexical reasons before diagnostic rendering;
2. add the Jazz canonical ADT module;
3. add test-only canonical Haskell values and a total adapter;
4. construct both sides as ordinary `RuntimeValue` trees and render them with
   `renderRuntimeValue`;
5. add logical-path normalization and the explicit parser fixture manifest;
6. prove schema rendering, path rejection, failure mapping, arbitrary-precision
   integers, escaping, full current corpus adaptation, and repeated-output
   determinism.

This child does not implement a Jazz lexer or add runtime primitives.

### Child 2: `JN-BOOTSTRAP-JAZZ-LEXER-001`

Consume the fixed contract:

1. add only the permanent `Char`/`Text` support APIs required for lexing;
2. implement the lexer state machine in 2-space-indented `.jz` modules;
3. return canonical success and structured lexical failure values;
4. run exact differential parity over the complete manifest and focused
   generated families;
5. prove large tail-recursive source traversal does not consume one Haskell
   stack frame per Jazz step.

The second pull request is based on the first and must not revise the canonical
schema to accommodate implementation shortcuts. If implementation reveals a
real schema defect, fix and review it in the first branch before rebasing the
second.

## Verification

Each child runs its focused suite plus:

```text
cabal test --project-dir=jazz-next all
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

The lexer child additionally runs exact accepted/rejected corpus parity,
repeated deterministic rendering, and a large synthetic traversal case.

## Non-Goals

This design does not add a Jazz parser, canonical parser AST, parser-diagnostic
parity, bytecode, a VM, backend-neutral lowered IR, LLVM lowering, object
emission, linking, or a native runtime. It does not make the Haskell reference
lexer import Jazz modules or make Jazz compiler code import Haskell compiler
types.
