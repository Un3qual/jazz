# Jazz
<img src="https://github.com/un3qual/jazz/blob/main/jazz_logo.png?raw=true" width="100">

### Notes for grader
Some of these features are not implemented yet. Some features will parse, but will not generate JS for reasons I will discuss in the Future Goals section.
Additionally, I did not use a grammer like yacc, I used parser-combinators.

If you have any trouble running or understanding the code, please send me an email and we can hop on Zoom and I can walk you through everything.
#### Tests
To run the tests, cd into the `jazz-hs` folder and run `stack test` from the command line. 
I did not end up using a code coverage tool because the way that I run the tests, and the way the parser works, it is impossible to easily test individual functions in the parser.

#### Running the Compiler
Inside the `jazz-hs` directory run `stack build` to build the binary. Then, run `./run.sh <path>`. I have included sample programs in the `jazz-hs/ExamplePrograms` folder. The `ComplexProgram.jz` file is a showcase of the features of the language in a program that runs successfully. The rest of the programs showcase some possible errors and their error messages. 
Note that if you get a permission error when attempting to execute `run.sh`, run `chmod +x ./run.sh` first. 

#### Future Goals
I have enjoyed this project so much, and I plan on continuing work on it for years to come. I made the code generator so minimal, because once this assingment is graded, I will remove all of the code generation code and write an interpreter instead. Then I'll bootstrap/self-host the compiler by writing all of the parsing, analysis, etc. code in Jazz itself and have it generate LLVM IR. To start, I need to make the parser able to read multiple files, add a module registry, add a dependency/library system, write the standard library, and more, but I'm so excited to do it.

It will certainly be a challenge, because there are many functional concepts that don't easily translate to assembly concepts (and LLVM is SSA so I need phi nodes all over). For example, with functions being curried by default, when writing a function like `\(x, y, z) -> ...`, it is transformed at parse-time into nested lambda expressions, because the AST node for lambda is `ELambda FunParam Expr`, with `Expr` being the body, so in Java-like-pseudocode, the function from before is `ELambda(x, ELambda(y, ELambda(z, ...)))`. This presents an issue because to translate that to assembly, I would likely need to be throwing function pointers everywhere which would be a nightmare. I'm not sure if I want the language to have a VM or not, which would help to some extend, but it just adds another layer of complication. 

#### About the Analyzer
I don't have a lot of analysis stages because the one stage I have is so complex. It is an (almost) complete [Algorithm W](https://en.wikipedia.org/wiki/Hindley–Milner_type_system#Algorithm_W) implementation. The result is that you almost never need to actually write type signatures. Of course it's best practice to do-so for others working on your code (and for yourself in the future), but it isn't needed. The type system is so good that it takes care of a number of other analysis stages for free, like handing not in scope errors, use-before-definiton errors, and more. 

#### General Notes
I spent too much time on the parser early on in the project in an effort to build a complete language that can have a good standard library, but that meant that I got really close to the deadline with only having a parser written, so a lot of the language features will parse successfully, but will throw `not implemented` errors during type inferrence, optimization, or codegen. Also, right now lines must end with `.`. This was a lazy fix to make parsing easier, but I intend on removing it in the future.

#### Easter Egg
Try running the parser on a Go program[^1].

### Story
Jazz is a functional language that takes inspiration from Elixir and Haskell. I love writing Haskell, but for beginners, the category theory oriented type system is confusing[^2]. Rather than monads, semigroupoids, etc, I will have a more standard approach to typeclasses, with classes like Collection, Orderable, etc. 

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

[^1]: http://redd.it/hpl7br (sorry jazzy)
[^2]: A monad is just a monoid in the category of endofunctors, what's the problem?