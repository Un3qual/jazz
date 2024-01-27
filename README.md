# Jazz
![Jazz logo](https://github.com/un3qual/jazz/blob/main/image.jpg?raw=true)

### Notes for grader
I discussed with Prof. Toal about doing the project in Haskell and compiling to LLVM instead of Javascript with Ohm, so I am unable to do some of the js-specific requirements for grading HW1.
### Story
Jazz is a functional language that takes inspiration from Elixir and Haskell.

### Features
- ADTs
- Easy to understand Elixir-like (ruby-like) syntax
- Performant. Currently the language is interpreted, but will hopefully compile down to LLVM IR in the future
- Strong, static typing
- Type inferrence
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
println "Hello, world"
```
##### Javascript
```js
console.log("Hello, world")
```

#### Array operations
##### Jazz
```
myArr = [1, 2, 3, 4, 5]
evens = filter myArr \i -> mod(i, 2) == 0
powersOf2 = map myArr &Num.pow(2, i)
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
In Jazz, functions are pure by default and are declared with `def`. Impure functions must be denoted with `WIP` and can not be called from pure functions. Function syntax and requirements are currently a work in progress.
##### Jazz
```
# Implicit types
def isEven(i) = mod(i, 2) == 0
# Explicit types
def isEven(i: Int): Bool = mod(i, 2) == 0

# Multiline functions
defio greet = {
  name = getLine
  println("Hello, ${name}")
}
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
mod Person::Organs::Heart {
  def beat() = # do stuff
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