---
title: Modules
description: Organize Jazz code with module paths, imports, exports, and deterministic resolution.
sidebar_position: 7
---

# Modules

Module names use `::`, and their source paths mirror the name below a module
root. `Example::Main` resolves as `Example/Main.jz`.

The checked module example consists of these two files:

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

Imports may load all public names, select names with `import A::B (x, y).`, or
bind a qualifier with `import A::B as B.`. Symbol lists and aliases cannot be
combined. Explicit module export lists can select `value`, `type`,
`constructor`, and `class` namespaces. Omitted lists export owned declarations;
`()` exports none. Imported declarations cannot be re-exported.

Resolution is deterministic, rejects ambiguity and cycles, compiles
dependencies before importers, and evaluates only the entry module's expression
statements. See [module resolution](../reference/module-resolution.md).
