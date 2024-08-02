# Jazz
<img src="https://github.com/un3qual/jazz/blob/main/jazz_logo.png?raw=true" width="100">

### Story
Jazz is a functional language that takes inspiration from Elixir and Haskell. I love writing Haskell, but for beginners, the category theory oriented type system is confusing[^1]. Rather than monads, semigroupoids, etc, I will have a more standard approach to typeclasses, with classes like Collection, Orderable, etc. 

### Features
- ADTs
- Easy to understand syntax
- Performant. Will generate LLVM IR in the future.
- Strong, static typing
- Incredible type inferrence
- Immutable variables
- First class functions
- Pattern matching
- Functions are curried by default
- First class functions
- Tuples

### Examples
#### Hello World
##### Jazz
```
print! "Hello, world".
```
##### Javascript
```js
console.log("Hello, world")
```

#### Currying and Partial Application
```
// Reference type signatures for built in functions
// (+) :: Num -> Num -> Num
// map :: (a -> b) -> [a] -> [b]

// These examples illustrate that functions are curried by default and support partial application. I will provide type signatures for these functions for reference, but they are not needed and infer correctly
add10 :: Integer -> Integer.
add10 = (+10).
add10List :: [Integer] -> [Integer].
add10List = map add10.
// Note that I don't have to write:
// add10List = \(xs) -> map add10 xs

nums = [1,2,3,4,5].
print! $ add10List nums.
// $ is an operator used to avoid parenthesis. Technically, it is a right associative, 0 precidence function application operator.
// Without the $, I would need to write `print! (add10List nums)`, because the parser thinks I mean (in Java syntax) `print!(add10List, nums)`.

```


#### Array operations
##### Jazz
```
myArr = [1, 2, 3, 4, 5].
evens = filter myArr \(i) -> mod(i, 2) == 0.
powersOf2 = map myArr \Num.pow(2, \0).
// Is the same as
powersOf2 = map myArr \(i) -> Num.pow(2, i).
```
##### Javascript
```js
const myArr = [1, 2, 3, 4, 5]

let evens = []
for (let i in myArr) {
  if (i % 2 == 0) evens.push(i)
}

// Functional approach
let powersOf2 = myArr.map(i => Math.pow(2, i))
```

#### Functions
In Jazz, functions are pure by default and are declared with assignment to a lambda. Impure functions must be denoted with `!` and can not be called from pure functions.

- Multiline functions must use curly braces
- Impure functions must end with a `!` e.g:
  - ```println! = \(str) -> ...```
- 
##### Jazz
```
// Implicit types
isEven = \(i) -> mod(i, 2) == 0.
// Explicit types
isEven :: Integer -> Bool.
isEven = \(i) -> mod(i, 2) == 0.

// Multiline functions
greet! = \() -> {
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
  return i % 2 == 0
}
// Or
const isEven = i => i % 2 == 0
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
