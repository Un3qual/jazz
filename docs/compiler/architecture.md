---
title: Compiler architecture
description: Understand the active Jazz compiler, runtime, module, and hosted-source boundaries.
sidebar_position: 1
---

# Compiler architecture

Jazz is currently a Haskell compiler and interpreter with an increasing body of
Jazz-authored compiler code. The Cabal package exposes a `jazz` executable; its
Haskell implementation library is private and is not a supported embedding API.

## Major boundaries

- `src/Jazz/Compiler/Parser/` owns lexing, surface grammar, structured parser
  failures, and lowering into canonical core.
- `Analyzer`, focused type-inference modules, pattern analysis, capability
  facts, and purity analysis establish the semantic program.
- `ModuleResolver` builds a dependency-first graph of retained core modules.
  `ModuleCompiler` checks modules against explicit dependency interfaces, and
  `ModuleRuntime` evaluates against explicit runtime exports.
- `DiagnosticCatalog` owns stable codes and metadata. Presentation-neutral
  diagnostics are rendered only at the reporting boundary.
- `Runtime` evaluates canonical core through an injectable runtime host.
  Production supplies files, streams, arguments, and exit; tests install
  deterministic hosts.
- `Driver` coordinates Prelude preparation, compile diagnostics, warning
  policy, module compilation, optional evaluation, and runtime observations.

Active Jazz-authored sources live in `jazz/stdlib/` and `jazz/compiler/`.
Standard-library modules cannot depend on compiler modules; repository tests
enforce that direction. Production-shaped programs live in `programs/`, while
focused fixtures and component suites live in `test/`.

## Hosted compiler boundary

The Jazz-authored lexer and parser cover the accepted surface and are checked
for exact repeated parity with the Haskell stage-0 front end. Jazz-authored
canonical-core schemas and lowering cover the complete accepted parser corpus.
Typed-core and backend-neutral lowered-IR schemas also have paired Haskell and
Jazz validators.

These hosted components are compiler-internal bootstrap boundaries. Normal
compile and run still use the Haskell parser, semantic pipeline, canonical core,
and interpreter. See [bootstrapping](bootstrapping.md) for the promotion rule and
[pipeline](pipeline.md) for phase order.
