# TSFP

## Overview

The project is an interpreter implemented in Haskell for a minimalist programming language that combines the mathematical purity of lambda calculus with modern programming language features It features:

- Pure lambda calculus core
- Hindley-Milner type inference
- Applicative order evaluation
- Static typing with detailed error reporting
- First-class functions and closures

## Language Features

### Syntax

The language supports these core constructs:

```
expr ::= var                      # Variables
       | \var.expr               # Lambda abstraction
       | (expr expr)             # Function application
       | var = expr              # Definition
```

### Type System

Lambda+ uses Hindley-Milner type inference to automatically deduce the most general type of every expression. Types include:

- Type variables (polymorphic types)
- Function types (t1 -> t2)

### Examples

Here's a small program demonstrating the core features:

```
# Basic function definitions
id = \x.x
compose = \f.\g.\x.(f (g x))

# Church encoding of booleans
true = \x.\y.x
false = \x.\y.y
not = \p.(p false true)

# Function application
test = (compose not not)
```

When executed, the interpreter shows both inferred types and evaluations:

```
id : t0 -> t0

compose : (t1 -> t2) -> (t3 -> t1) -> t3 -> t2

true : t4 -> t5 -> t4

false : t6 -> t7 -> t7

not : (t8 -> t9 -> t10) -> t9 -> t8

test : t11 -> t11
Evaluating test...
= (compose not not)
= \x.x
```

### More Complex Example

Here's a more sophisticated example showing higher-order functions and type inference:

```
# Function combinators
const = \x.\y.x
flip = \f.\x.\y.(f y x)

# List operations (Church encoding)
nil = \c.\n.n
cons = \h.\t.\c.\n.(c h (t c n))
isEmpty = \l.(l (\h.\t.false) true)

# Example usage
list = (cons true (cons false nil))
anyTrue = \l.(l (\h.\t.(or h t)) false)
test = (anyTrue list)
```
