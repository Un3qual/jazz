---
title: Modules
description: Organize Jazz code with module paths, imports, exports, and deterministic resolution.
sidebar_position: 7
---

Modules give declarations a stable namespace and make dependencies explicit.
Each module is compiled after its dependencies, while only the entry module's
top-level expressions are evaluated.

`Example::Main` imports a value owned by `Example::Greeting`:

<!-- jazz-example: executable path=examples/modules/src/Example/Greeting.jz -->

```jazz
module Example::Greeting (value greeting) {
  greeting = "Hello from a Jazz module".
}
```

<!-- jazz-example: executable path=examples/modules/src/Example/Main.jz -->

```jazz
module Example::Main {
  import Example::Greeting.

  greeting.
}
```

Run it with:

```bash
cabal run jazz -- --run --entry-module Example::Main \
  --module-root examples/modules/src
```

Expected output:

<!-- jazz-example-output: case=module -->

```text
"Hello from a Jazz module"
```

An import can expose all public names, select particular names, or introduce a
qualifier. Export lists distinguish values, types, constructors, and
capabilities. Typed selectors can also re-export declarations from explicit
unqualified imports. Omitted lists and bare selectors export owned declarations
only.

Module names map to paths beneath ordered module roots. Resolution rejects an
ambiguous match or dependency cycle rather than choosing one implicitly. See
[module resolution](../reference/module-resolution.md) for exact path, import,
export, and Prelude rules.

## Using an imported class through an alias

An aliased class keeps its defining module's identity. Use `Alias::Class` in a
constraint or impl head, and `Alias::Class::method` to call a method.

<!-- jazz-example: executable path=examples/modules/src/Example/Equality.jz -->

```jazz
module Example::Equality (class Equal) {
  class Equal(a) {
    equal :: a -> a -> Bool.
  }.

  impl Equal(Int) {
    equal = \(left, right) -> left == right.
  }.
}
```

<!-- jazz-example: executable path=examples/modules/src/Example/Compare.jz -->

```jazz
module Example::Compare {
  import Example::Equality as Equality.

  same :: @{Equality::Equal(a)}: a -> a -> Bool.
  same = \(left, right) -> Equality::Equal::equal left right.

  data Marker = Marker.
  impl Equality::Equal(Marker) {
    equal = \(left, right) -> True.
  }.

  (same 1 1, Equality::Equal::equal 1 2, Equality::Equal::equal Marker Marker).
}
```

Run it with:

```bash
cabal run jazz -- --run --entry-module Example::Compare \
  --module-root examples/modules/src
```

<!-- jazz-example-output: case=qualified-class -->

```text
(True, False, True)
```

The alias does not make `Equal` available unqualified. Private classes stay
private.

## Publishing a library facade

A facade selects public declarations from its dependencies without changing
their identity. First define a value:

<!-- jazz-example: executable path=examples/modules/src/Example/Numbers.jz -->

```jazz
module Example::Numbers (value answer) {
  answer = 42.
}
```

The facade publishes that value and the `Equal` class from the earlier example:

<!-- jazz-example: executable path=examples/modules/src/Example/API.jz -->

```jazz
module Example::API (value answer, class Equal) {
  import Example::Numbers.
  import Example::Equality.
}
```

Clients use the existing import forms, including aliases:

<!-- jazz-example: executable path=examples/modules/src/Example/UseAPI.jz -->

```jazz
module Example::UseAPI {
  import Example::API as API.

  if API::Equal::equal API::answer 42 then API::answer else 0.
}
```

Run it with:

```bash
cabal run jazz -- --run --entry-module Example::UseAPI \
  --module-root examples/modules/src
```

<!-- jazz-example-output: case=module-reexports -->

```text
42
```

Re-exports require typed selectors: `value answer`, `class Equal`, `type Box`,
`type Box(..)`, or `constructor Box`. Bare `(answer)` still selects only an
owned declaration. An owned declaration wins over an imported one in the same
namespace. Alias-only imports and ambient Prelude declarations cannot satisfy
an export selector.

`type Box` exports the original type abstractly. `type Box(..)` includes only
constructors visible through the facade's explicit imports; it cannot recover
private constructors. `type Box(First, Second)` selects particular visible
constructors of that original type. A constructor-only export retains its type
metadata without making the owner type publicly nameable.

Explicit `class Equal` re-exports include the dependency's public methods and
implementation evidence, plus the facade's own implementations of that class.
Repeated routes to the same declaration or implementation do not create
duplicates. Distinct conflicting declarations or implementations remain errors.
Importing a class without explicitly re-exporting it keeps it out of the
facade's public API.
