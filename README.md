# Jazz

<img src="https://github.com/un3qual/jazz/blob/main/jazz_logo.png?raw=true" width="100">

### Story

Jazz is a functional language that takes inspiration from Elixir and Haskell. I love writing Haskell, but for beginners, the category theory oriented type system is confusing[^1]. Rather than monads, semigroupoids, etc, I will have a more standard approach to typeclasses, with classes like Collection, Orderable, etc.

### Implemented Today (verified)

- Strong, static typing for the core expression subset
- Type inference for the core expression subset
- Immutable bindings
- First-class functions
- Functions are curried by default
- Bundled-prelude runtime helpers (`map`, `filter`, `hd`, `tl`, `print!`) in the active CLI/run path
- Stub purity enforcement for direct `!` callee calls in binding bodies
- Dot-terminated source forms and brace-bodied module declarations

### Planned / Aspirational

- ADTs as end-to-end runtime/compiler behavior
- Pattern matching as end-to-end runtime/compiler behavior
- Tuple code generation/runtime behavior
- Full module/import loader semantics
- LLVM backend and performance target
- Readability/usability goals such as "easy to understand syntax"

Detailed status/evidence matrix: [docs/feature-status.md](docs/feature-status.md)  
Implementation baseline details: [docs/jazz-language-state.md](docs/jazz-language-state.md)
Active compiler implementation path: `jazz-next/`

### Repository Governance (Spec Authority)

- `jazz2` is a reference-only design source for future ideas and is non-normative for current language behavior.
- The canonical language authority is `docs/spec/*` plus the implemented subset behavior/tests in `jazz-next/`; treat `jazz-hs/` as historical reference evidence only.
- Active compiler implementation work lands in `jazz-next/`.
- This top-level README is a project pitch and usage overview, not the normative language specification.
- Semantic language changes require a decision record or RFC before implementation.
- Non-semantic/internal changes may be implementation-first only when docs and tests are updated in the same change.
- Governance policy details: `docs/spec/governance/spec-authority-policy.md`.

### Examples

#### Implemented Today

#### Hello World

`jazz-next/` loads its bundled prelude by default in CLI mode, so user-facing helpers such as `print!`, `map`, `filter`, `hd`, and `tl` come from that prelude rather than from direct compiler-owned names.

##### Jazz

```
print! "Hello, world".
```

##### Javascript

```js
console.log("Hello, world");
```

#### Currying and Partial Application

```
// Reference type signatures for built in functions
// (+) :: Num -> Num -> Num
// map :: (a -> b) -> [a] -> [b]

// These examples illustrate that functions are curried by default and support partial application. I will provide type signatures for these functions for reference, but they are not needed and infer correctly
add10 :: Int -> Int.
add10 = (+10).
add10List :: [Int] -> [Int].
add10List = map add10.
// Note that I don't have to write:
// add10List = \(xs) -> map add10 xs

nums = [1,2,3,4,5].
print! $ add10List nums.
// $ is an operator used to avoid parenthesis. Technically, it is a right associative, 0 precidence function application operator.
// Without the $, I would need to write `print! (add10List nums)`, because the parser thinks I mean (in Java syntax) `print!(add10List, nums)`.

```

#### Planned / Aspirational Examples (not fully implemented end-to-end)

The following examples show intended direction but are not all fully implemented through parse/analyze/codegen/runtime in the current compiler subset.

#### Array operations

##### Jazz

```
myArr = [1, 2, 3, 4, 5].
evens = filter (\(i) -> mod(i, 2) == 0) myArr.
powersOf2 = map (\(i) -> Num.pow(2, i)) myArr.
// Is the same as
powersOf2 = map (\Num.pow(2, \0)) myArr.
```

##### Javascript

```js
const myArr = [1, 2, 3, 4, 5];

let evens = [];
for (let i in myArr) {
  if (i % 2 == 0) evens.push(i);
}

// Functional approach
let powersOf2 = myArr.map((i) => Math.pow(2, i));
```

#### Functions

In Jazz, functions are pure by default and are declared with assignment to a lambda. Impure functions must be denoted with `!` and cannot be called from pure functions (stub-v1 direct-call enforcement in `jazz-next`; full effect typing remains planned).

- Multiline functions must use curly braces
- Impure functions must end with a `!` e.g:
  - `println! = \(str) -> ...`

##### Jazz

```
// Implicit types
isEven = \(i) -> mod(i, 2) == 0.
// Explicit types
isEven :: Integer -> Bool.
isEven = \(i) -> mod(i, 2) == 0.

// Multiline functions
greet! = \(ignored) -> {
  name = getLine!.
  println!("Hello, ${name}").
}.

greet2! = \(name) -> {
  println! "Hello, ${name}".
}.

// Pure greeting function that just returns the string
greet3 = \(name) -> {
  "Hello, ${name}".
}.
```

##### Javascript

```js
function isEven(i) {
  return i % 2 == 0;
}
// Or
const isEven = (i) => i % 2 == 0;
```

#### Modules

##### Jazz

```
module Person::Organs::Heart {
  beat = # do stuff
}
```

##### Javascript

```js
// In file "./Person/Organs/Heart.js"
export class Heart {
  function beat() {
    // Do stuff
  }
}
```

[^1]: A monad is just a monoid in the category of endofunctors, what's the problem?
