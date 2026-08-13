---
title: Source and blocks
description: Learn Jazz source units, statement terminators, comments, and block evaluation.
sidebar_position: 2
---

Jazz processes declarations and evaluates expressions in source order. Earlier
bindings are available to later expressions; ordinary forward references are
not. The final top-level expression is the result of a standalone program.

Fragment:

<!-- jazz-example: fragment -->

```jazz
answer :: Int.
answer = 40 + 2.
answer.
```

Blocks introduce a lexical scope. Their statements run in order, bindings are
visible to later statements in the block, and the last expression becomes the
block's value.

Fragment:

<!-- jazz-example: fragment -->

```jazz
{
  left = 20.
  right = 22.
  left + right.
}
```

A later binding with the same name replaces it only for subsequent statements
in that scope. Existing values and closures keep the environment they captured.
Warnings can report same-scope rebinding, shadowing of an outer name, and an
unused block binding.

A module owns an entire source unit and cannot be nested. Imports, data types,
capabilities, and operators belong at source or module scope; expression blocks
contain ordinary bindings, signatures, and expressions. See the
[expression grammar](../reference/expression-grammar.md) for the exact forms
and [lexical grammar](../reference/lexical-grammar.md) for comments and
terminators.
