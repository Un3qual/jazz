# Operator Values and Haskell-Style Sections Design

Date: 2026-03-15

## Goal

Align `jazz-next` with the language goal that operators are first-class function values while preserving Haskell-style section semantics and the existing precedence model.

## Approved Decisions

1. Operators must be usable as first-class values:
   - `(+)`
   - `(/)`
   - `(==)`
2. Haskell-style sections stay intact:
   - `(2 +)` captures the left operand.
   - `(+ 2)` captures the right operand.
3. Arithmetic and infix parsing must continue to follow standard precedence and associativity rules comparable to Haskell:
   - application binds tighter than infix,
   - `*` and `/` bind tighter than `+` and `-`,
   - comparisons bind looser than arithmetic,
   - `$` stays lowest and right-associative.
4. Internal parser/core nodes may remain specialized if they are only an implementation device, not a distinct semantic model.

## Language Semantics

The intended semantic equivalences are:

- `1 + 2` behaves like `((+) 1) 2`
- `(1 +) 2` behaves like `((+) 1) 2`
- `(+ 1) 2` behaves like `((\x -> x + 1)) 2`
- If `plus = (+).`, then `plus 1 2` behaves like `(+) 1 2`

The important non-equivalence is:

- `(+ 2)` is a right section
- `((+) 2)` is ordinary partial application of the operator value

For commutative operators those may happen to produce the same results, but for operators such as `/` or `-` they must differ. That difference is part of the language design and must remain observable.

## AST and Lowering Boundary

`jazz-next` currently has explicit binary and section nodes. That is acceptable as a parser/lowering representation, but the semantic model should move closer to ordinary function application.

The planned direction is:

1. Add an explicit operator-value form for bare parenthesized operators such as `(+)`.
2. Keep explicit section forms for left/right sections where they simplify parsing.
3. Canonicalize infix expressions and left sections into operator-value application during lowering or canonicalization:
   - `a + b` -> `EApply (EApply (EOperatorValue "+") a) b`
   - `(a +)` -> `EApply (EOperatorValue "+") a`
4. Keep right sections as a dedicated form for now because `(+ b)` is not equivalent to `EApply (EOperatorValue "+") b`.

This preserves the parser clarity of explicit section nodes while removing the current semantic split where infix operators behave unlike ordinary function values.

## Runtime Model

Runtime should support first-class operator values in the same broad style as builtins:

1. Evaluating a bare operator value yields a callable runtime value.
2. Applying that value captures arguments until the operator is saturated.
3. Saturation dispatches through the existing primitive operator evaluator.
4. Over-application should continue to fail deterministically through the normal runtime application path.

Existing right-section runtime behavior can remain, but it should be layered on top of the same primitive operator table and callable-value protocol rather than acting like a separate language feature.

## Type Inference Model

Type inference should give bare operator values curried function types derived from the existing operator rule table:

- `(+)` -> `Int -> Int -> Int`
- `(<)` -> `Int -> Int -> Bool`
- `(==)` -> `t -> t -> Bool` with the existing runtime-supported equality restrictions

Infix expressions should inherit those types through canonicalization. Left sections should then type-check like ordinary one-argument partial applications, while right sections keep their dedicated typing path to preserve Haskell-style flipped argument order.

## Parser and Precedence

The current precedence climber already reflects the frozen operator table and should remain the foundation. The required parser change is not a precedence rewrite; it is support for bare operator values inside parentheses without confusing them with sections or grouped infix expressions.

Required parse distinctions:

- `(+)` -> operator value
- `(+ 2)` -> right section
- `(2 +)` -> left section
- `(1 + 2)` -> grouped infix expression

Additional precedence coverage should explicitly lock:

- `1 + 2 * 3`
- `f x + g y * z`
- `(+) 1 2 * 3`
- `f $ g x + h y`

That keeps the parser behavior close to conventional functional-language expectations and prevents drift while this feature is added.

## Testing Strategy

The implementation should be driven test-first and validated at four layers:

1. Parser tests:
   - bare operator value parsing
   - grouped expression vs section vs operator value disambiguation
   - application tighter than infix
2. Lowering/canonicalization tests:
   - infix lowers to operator application semantics
   - left sections lower to operator partial application
3. Compile/type tests:
   - bare operator values compile with expected uses
   - right sections remain distinct for non-commutative operators
4. Runtime tests:
   - `(+) 1 2`, `(1 +) 2`, and `1 + 2` agree
   - `(/ 2) 10` and `((/) 2) 10` differ as expected

## Non-Goals

This change does not attempt to:

- remove all operator-specific AST forms immediately,
- implement user-defined operators,
- redesign operator diagnostics beyond what is needed for semantic parity,
- revisit the locked precedence table.

## Expected Outcome

After this change, `jazz-next` should support the user-facing rule that operators behave like functions, while still preserving Haskell-style section syntax and the existing locked fixity/precedence contract.
