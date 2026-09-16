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
capabilities. Explicit selectors can also re-export visible imported declarations.
Selecting a class also selects its public ordinary method values.
Every import supplies the dependency's transitive implementations, even if its
export list exposes no names; aliases and selections do not hide instances.

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
private. An explicit `class Alias::Equal` export can publish the imported class
and its public methods under their original names.

## Facades and custom operators

A facade selects imported declarations in its module header. The qualifier must
be an explicit import alias; it identifies the source without becoming part of
the public name. Exporting a value, type, or class preserves its original identity.
No wrapper function or replacement declaration is created.

<!-- jazz-example: executable path=examples/modules/src/Example/OperatorLibrary.jz -->

```jazz
module Example::OperatorLibrary (value (%%), value answer) {
  operator %% precedence 6 left.
  (%%) :: Int -> Int -> Int.
  (%%) = \(left, right) -> left - right.
  answer = 42.
}
```

<!-- jazz-example: executable path=examples/modules/src/Example/OperatorAPI.jz -->

```jazz
module Example::OperatorAPI (value (Ops::%%), value Ops::answer) {
  import Example::OperatorLibrary as Ops.
}
```

<!-- jazz-example: executable path=examples/modules/src/Example/OperatorConsumer.jz -->

```jazz
module Example::OperatorConsumer {
  import Example::OperatorAPI ((%%), answer).
  import Example::OperatorAPI as API.
  (10 %% 3, (%%) 10 3, (10 %%) 3, (%% 3) 10,
   10 API::%% 3, (API::%%) 10 3,
   (10 API::%%) 3, (API::%% 3) 10, 2 * 10 %% 3, answer).
}
```

Run `Example::OperatorConsumer` with the same module root as the earlier examples.

<!-- jazz-example-output: case=module-operators -->

```text
(7, 7, 7, 7, 7, 7, 7, 7, 14, 42)
```

Unqualified selectors choose local declarations before unqualified imports,
independently in each namespace. `type API::Box(..)` selects only constructors
visible through `API`; an abstract type stays abstract. Use `type API::Box(C1)`
to select a visible subset. Constructors and methods come from the selected type
or class's view. Hidden supporting definitions cannot be re-exported.

Multiple paths to the same original declaration are compatible. Different
original declarations with the same public name in one namespace conflict.
Unqualified imports combine visible constructor subsets for the same type;
qualified exports retain only the named alias's subset.

Omitting the export list publishes locally owned ordinary declarations.
Imported names and custom operators require explicit export selectors. `()`
publishes no names while still carrying transitive implementations.
