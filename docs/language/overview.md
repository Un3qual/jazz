---
title: Language overview
description: Understand the implemented Jazz programming model and its current boundaries.
sidebar_position: 1
---

# Language overview

Jazz programs are expression-oriented and statically typed. Source units
contain period-terminated signatures, bindings, declarations, imports, and
expressions. Functions are curried internally, while tuple-style lambda
parameters provide compact multi-argument syntax.

Implemented language foundations include:

- integers, finite-width numeric types, booleans, characters, text, lists,
  tuples, functions, and user-defined algebraic data types;
- type inference, signatures, generic named types, constrained signatures, and
  explicit type application;
- lambdas, ordered pattern lambdas, conditionals, guarded cases, and blocks;
- modules with imports, aliases, symbol lists, explicit typed exports, cycle
  detection, and deterministic resolution;
- built-in and source-local operators with precedence and associativity; and
- `class`/`impl` capability declarations with a bounded concrete dispatch
  profile.

The current runtime is an interpreter. Typed core and backend-neutral lowered
IR exist as validated opt-in compiler boundaries, but native code generation
is [planned](../project/roadmap.md). Read [source and blocks](source-and-blocks.md)
next, or use the [expression grammar](../reference/expression-grammar.md) as a
compact reference.
