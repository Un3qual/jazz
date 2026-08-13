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
capabilities. A module can export declarations it owns, but cannot re-export an
imported declaration.

Module names map to paths beneath ordered module roots. Resolution rejects an
ambiguous match or dependency cycle rather than choosing one implicitly. See
[module resolution](../reference/module-resolution.md) for exact path, import,
export, and Prelude rules.
