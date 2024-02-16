# Homework 1
1. 
2. drawn
3. drawn
4. drawn
5. Give complete grammar rules that make the expression `-2**2`
  a. Evaluate to 4
  ```
    Arithmetic {
      Exp = PowExp
      PriExp
        = "-" PriExp -- neg
        | number
      PowExp
        = PriExp "**" PowExp  -- pow
        | PriExp
      number = digit+
    }
  ```
  b. Evaluate to -4
  ```
    Arithmetic {
      Exp = PriExp
      PriExp
        = "-" PriExp -- neg
        | PowExp
      PowExp
        = PriExp "**" PowExp -- pow
        | number
      number = digit+
    }
  ```
  c. Be a syntax error, while allowing `(-2)**2` and `-(2**2)` to be legal?
  ```
  Arithmetic {
    Exp = PowExp
    PowExp
      = PriExp "**" PowExp  -- pow
      | PriExp
      | NegExp
    PriExp
      = "(" Exp ")" -- paren
      | number
    NegExp
      = "-" PriExp -- neg
      | PriExp
    number = digit+            
  }
  ```
  6. 