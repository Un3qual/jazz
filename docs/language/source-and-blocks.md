---
title: Source and blocks
description: Learn Jazz source units, statement terminators, comments, and block evaluation.
sidebar_position: 2
---

# Source and blocks

A Jazz source unit is a sequence of statements. Every signature, binding,
declaration, import, and expression statement ends with `.`. Whitespace is not
significant beyond separating tokens, and `#` starts a line comment.

Fragment:

```jazz
answer :: Int.
answer = 40 + 2.
answer.
```

Braces form expression blocks. Bindings become visible to later statements in
the same lexical block, and the last expression is the block value.

Fragment:

```jazz
{
  left = 20.
  right = 22.
  left + right.
}
```

Bindings use `name = expression.`; there is no required `let` keyword. A later
same-scope binding with the same name replaces the earlier one. Optional
warnings can report rebinding, outer-scope shadowing, and unused ordinary block
bindings.

Top-level and module bodies may contain declarations that nested expression
blocks reject, including modules, imports, data declarations, capability
declarations, and operator declarations. See the exact
[lexical grammar](../reference/lexical-grammar.md).
