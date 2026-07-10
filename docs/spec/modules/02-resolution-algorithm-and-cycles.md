# Module Resolution Algorithm and Cycles

Status: active module resolution contract
Primary plan: `docs/plans/spec-clarification/2026-03-02/modules/09-module-loader-and-import-resolution.md`
Depends on: `docs/spec/modules/01-file-layout-and-package-roots.md`

## Scope

This document defines the deterministic v1 module resolution algorithm for active `jazz-next` module-graph compilation and execution. It documents existing resolver behavior and does not introduce new compiler behavior.

Name binding for qualified imports, loader replay/runtime behavior, cache policy, and future package metadata remain separate module spec slices.

## Inputs

Resolution starts with:

- one non-empty entry module path such as `App::Main`;
- an ordered module-root list from CLI or driver configuration;
- the canonical module-path-to-file mapping from the file-layout spec;
- a source lookup function that returns file contents or no match for a candidate path.

CLI module-graph mode parses `--entry-module <A::B>` before resolver traversal. Invalid or empty entry paths fail before any source lookup.

## Candidate Lookup

For a requested module path, the resolver builds candidate paths by joining each deduplicated module root with the module-relative `.jz` file path.

Candidate roots are deduplicated before lookup while preserving first occurrence. Duplicate roots therefore cannot create ambiguity by themselves.

Candidate lookup outcomes:

| matching candidates | outcome |
| --- | --- |
| `0` | `E4001` unresolved import |
| `1` | selected source path and source text |
| `>1` | `E4002` ambiguous import |

`E4001` diagnostics include the rendered module path, importer context when the missing module was imported by another module, and the candidate paths that were checked.

`E4002` diagnostics include the rendered module path, importer context when present, and every matching candidate path in deterministic module-root order.

The bundled prelude is not a module-root fallback. Prelude loading is a driver concern layered around module graph compilation and execution.

## Graph Traversal

Resolution is depth-first with deterministic import ordering.

For each module:

1. If the module is already resolved, reuse the resolved result and do not traverse it again.
2. If the module is already in the active call stack, stop with `E4003`.
3. Load the module source using candidate lookup.
4. Parse the source and collect module declarations, imports, exports, unqualified references, and qualified references. Parse failure while loading a module is `E4004`.
5. Validate module declarations:
   - a source file with no module declaration is allowed; it uses the resolved path as module identity;
   - exactly one matching top-level brace-bodied declaration is allowed;
   - multiple declarations are `E4005`;
   - a declaration/path mismatch is `E4006`.
6. Deduplicate imported module paths and sort them by rendered `A::B` text.
7. Resolve dependencies in that sorted order.
8. Validate import bindings after dependencies have exported-name inventories.
9. Add the current module after all dependencies.

The final resolved module list is dependency-first. For example, resolving `App::Main` that imports `Lib::Util` yields `Lib::Util` before `App::Main`.

Source import order is not the traversal contract. Duplicate imports collapse to one dependency edge, and distinct imports traverse in lexical order by rendered module path.

## Cycle Detection

Cycles are detected against the active call stack before loading a module source again.

`E4003` diagnostics render the minimal repeated cycle from the first repeated module through the current stack and back to that module.

Example:

```text
A::One imports B::Two
B::Two imports A::One
```

The diagnostic trace is:

```text
A::One -> B::Two -> A::One
```

Resolved modules outside the active stack do not create cycles when referenced again; they are reused.

## Pseudocode

```text
resolve(entry):
  state = visit([], empty_state, entry)
  return state.resolved_modules

visit(call_stack, state, module_path):
  if module_path in state.resolved_set:
    return state

  if module_path in call_stack:
    fail E4003 with minimal cycle trace

  source = load_one_candidate(module_path)
  parsed = parse_and_validate_module(source, module_path)
  imports = sort(render_path, unique(parsed.imports))

  next_stack = module_path : call_stack
  for import_path in imports:
    state = visit(next_stack, state, import_path)

  validate_import_bindings(parsed, state.exports)
  return state + resolved module_path
```

## Truth Table

| case | result |
| --- | --- |
| Entry path is empty | pre-resolution entry-path diagnostic |
| Requested module has no matching file under any root | `E4001` |
| Requested module matches files under two distinct roots | `E4002` |
| Requested module imports an ancestor in the active stack | `E4003` |
| Loaded module source does not parse | `E4004` |
| Loaded module has multiple module declarations | `E4005` |
| Loaded module declaration does not match resolved path | `E4006` |
| Loaded module imports the same dependency twice | one dependency traversal |
| Two imports render in reverse source order | traversal still uses lexical rendered-path order |
| Already resolved module is imported again by another module | resolved result is reused |

Implementation evidence (2026-05-30): `ModuleResolutionSpec.hs` now locks the
active `jazz-next` harness for lexical rendered-path traversal when source
imports appear in reverse order, duplicate import collapse, already-resolved
shared dependency reuse across branches, and nested minimal cycle traces.

## Non-Goals

This resolution slice does not define:

- loader replay, compile/run output, or dependency expression execution;
- import binding/shadowing details beyond the dependency graph needed for resolution;
- cache invalidation or memoized source lookup policy;
- package metadata or automatic root discovery;
- standard-library module-root discovery.

Those behaviors belong to later module spec slices.
