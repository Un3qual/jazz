# Homework 3

## Problems

### Problem 1
a. Syntax error

b. Syntax error

c. Static semantic

d. Static semantic

e. Static semantic

f. Not a compile time error

g. Not a compile time error (valid syntax)

h. Not a compile time error (valid syntax)

i. Syntax error

j. Assuming you are using a current Java version that supports `instanceof` pattern matching, this is valid syntax

k. If Java version `[10, 13)`, syntax error, because it thinks it's three string literals next to each other, if Java version 13 or greater, semantic error because text blocks must start with a newline. If Java version less than 10, there is no `var` keyword, so it will be a syntax error

l. Syntax error

m. Not a compile time error (valid syntax) if using Java version 12 or later, which allows this type of syntax

### Problem 2
a. Variables must be defined in the scope that they are used and undefined variables evaluate `undefined`. Also, `undefined (integer infix operator) valid_number` evaluates to `NaN`.

b. Variables must be defined in the scope that they are used, usage of undefined variables throws an error.

c. Variables must be defined in the scope that they are used. Undefined variables point to random location in memory, and can be garbage data.

d. Usage of a variable in its own declaration potentially points to random location in memory (undefined behavhior)

e. Compiler allows undefined variables to be used, but throws an error when used in their own declaration.

### Problem 3
Example code, for a pseudo language where function parameters are always passed by reference:
```
var a = 1
fn f(x, y) {
  print(x, y)
}

fn g(x) {
  x *= 10
  return x
}

f(a, g(a))
```

This could potentially print either `10 10`, if the second parameter of `f` is evaluated first, or `1 10` if the left parameter of `f` is evaluated first.

### Problem 4
Carlos allows structs to have a parameter of its own type, as long as it is nested in another type, such as an optional, function, or list. However, it does not allow a struct, `S`, to have a parameter of just type `S`, because parameters are stored inline and would result in a struct of infinite size.

This is handled in the analyzer phase of the compiler. When creating the semantics with Ohm, when a `TypeDecl` block is found (which is only for structs, and occurs inside the struct block, for each field's type), it ensures that a field of the type of the struct is not declared.

### Problem 5
Python:
```python
def list_min(lst, c_min=None):
    if not lst:
        return c_min
    if c_min is None or lst[0] < c_min:
        c_min = lst[0]
    return list_min(lst[1:], c_min)

lst = [5, 1, 8, 3, 2]
print(list_min(lst))
```
Erlang:
```erlang
list_min(List)                                       -> list_min(List, undefined).
list_min([], Min)                                    -> Min;
list_min([H|T], Min) when Min =:= undefined; H < Min -> list_min(T, H);
list_min([_|T], Min)                                 -> list_min(T, Min).

list_min([5, 1, 8, 3, 2]).
```

### Problem 6
The issue with the second function is that it is passing the result of the update function to `setTimeout` (which would be undefined), not a reference to the function, so `setTimeout` doesn't have a function to call. His function would throw an error when `setTimeout` tries to call its first parameter.

### Problem 7
1. This file does not declare what package it's in
2. On the right side of the `=` operator, the diamond operator could be used to avoid declaring the type parameters of the HashMap twice
3. Instead of returning the `0` literal, a constant should be declared, and the method should return the constant
4. The class seems to be a utility class that does not need a constructor, so the constructor should be made private and should throw an error explaining that this class should not be initialized. If it is not a utility class, and should be initialized, then a comment should be added explaining why the constructor is empty