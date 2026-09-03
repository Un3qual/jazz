---
title: Jazz programming language overview
description: Learn the implemented Jazz programming model, including expressions, immutable values, functions, types, and modules.
sidebar_position: 1
---

Jazz programs are expressions built from immutable values. Static types rule
out invalid combinations before evaluation, while inference keeps most local
types implicit. A program's final expression supplies its result.

Functions are values and support partial application. Algebraic data types
model alternatives explicitly; patterns select and decompose those
alternatives. Conditionals, cases, and blocks are expressions, so every branch
that can produce a result must agree on its type.

Names are resolved lexically and declaration order matters. Modules add named
boundaries around declarations and dependencies. Capability constraints let
polymorphic code require operations such as equality or ordering without
choosing one concrete type in advance.

Jazz currently evaluates programs with an interpreter. Native code generation
is [planned](../project/roadmap.md), but does not change the language model
described here.

Continue with [source and blocks](source-and-blocks.md), or use the
[expression grammar](../reference/expression-grammar.md) when you need exact
notation.
