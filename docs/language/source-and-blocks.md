---
title: Source and blocks
description: Learn Jazz source units, statement terminators, comments, and block evaluation.
sidebar_position: 2
---

# Source and blocks

A Jazz source unit contains either ordinary top-level forms or one
brace-bodied module declaration. Ordinary signatures, bindings, imports, data
declarations, `class` and `impl` declarations, operator declarations,
operator signatures and bindings, and expression statements end with `.`.
Whitespace is not significant beyond separating tokens, and `#` starts a line
comment.

A module declaration has the form `module A::B { ... }`. It ends at `}` with
no trailing `.`, must be the first top-level form, and owns the remainder of
the source unit. Module declarations are forbidden inside module bodies and
nested expression blocks.

Fragment:

<!-- jazz-example: fragment -->

```jazz
answer :: Int.
answer = 40 + 2.
answer.
```

Braces form expression blocks. Bindings become visible to later statements in
the same lexical block, and the last expression is the block value.

Fragment:

<!-- jazz-example: fragment -->

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

Direct module-body forms are imports, data declarations, `class` and `impl`
declarations, operator declarations, operator signatures and bindings,
ordinary signatures and bindings, and expression statements. Nested expression
blocks accept ordinary signatures, bindings, and expressions, but reject
modules, imports, data, capability, and operator declarations. See the exact
[lexical grammar](../reference/lexical-grammar.md).
