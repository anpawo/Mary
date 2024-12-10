# **GLaDOS**  
_A LISP-like language interpreter with support for arithmetic operations, conditionals, functions, and user-defined variables._

---

## **Table of Contents**
1. [About the Project](#about-the-project)
2. [Features](#features)
3. [Installation](#installation)
4. [Usage](#usage)
   - [CLI Usage](#cli-usage)
   - [REPL Mode](#repl-mode)
   - [Examples](#examples)
5. [Supported Syntax](#supported-syntax)

---

## **About the Project**
GLaDOS is a simple interpreter for a LISP-like language, built in Haskell. It processes S-expressions, evaluates them, and outputs the results. The project supports basic arithmetic operations, conditionals, and allows users to define variables and functions.

The interpreter provides two modes:
- **REPL Mode**: An interactive session for testing expressions.
- **File Input/Command-Line**: Parse and evaluate expressions from files or single commands.

---

## **Features**
- **Arithmetic Operations**: Supports `+`, `-`, `*`, `div`, and `mod`.
- **Booleans**: True (`#t`) and False (`#f`), with support for conditions.
- **Conditionals**: `if` expressions to evaluate different branches.
- **Functions**: Built-in and user-defined functions.
- **Variable Definitions**: Create reusable variables using `define`.
- **Lambdas**: Supports anonymous functions.

---

## **Installation**
1. Ensure you have [Haskell's GHC](https://www.haskell.org/ghc/) and [Cabal](https://www.haskell.org/cabal/) installed.
2. Go to the repository: 
    ```bash
    https://github.com/EpitechPromo2027/B-FUN-500-PAR-5-2-glados-zacharie.rodde
    ```
3. Download the latest release by clicking on `Releases` then `Assets` and downloading `glados_V2_defense_part_1`
4. Run the executable:
   ```bash
   ./glados_V2_defense_part_1
   ```

---

## **Usage**

### CLI Usage
GLaDOS can evaluate expressions directly from the command line or from a file. Here’s the general usage:

```bash
Usage: glados [OPTIONS] [EXPRESSION]

Options:
  -f, <file>      # Evaluate expressions from the given file.
  -repl                  # Evaluate expressions with a Read Evaluate Print Loop (REPL)
  -h                     # Show this help text.

Examples:
  ./glados "(+ 1 2)"     # Evaluate the expression and print the result.
  ./glados -f expr.txt   # Evaluate all expressions in 'expr.txt'.
  ./glados -repl         # Start REPL mode.
  ./glados -h            # Show this help text.
```

### REPL Mode
To start an interactive session:
```bash
./glados -repl
```

Type expressions directly, and GLaDOS will evaluate and display the result. Exit by typing `quit`.

### Examples
```bash
$ ./glados "(define x 10)"
$ ./glados "(+ x 5)"
15

$ ./glados "(if (< x 15) (* x 2) (/ x 2))"
20
```

## **Supported Syntax**

### **1. Arithmetic Operations**
```lisp
(+ 1 2)          >> 3
(- 5 3)          >> 2
(* 4 2)          >> 8
(div 10 2)       >> 5
(mod 10 3)       >> 1
```

### **2. Booleans**
```lisp
#t               >> true
#f               >> false
(< 3 5)          >> #t
(eq? 4 4)        >> #t
```

### **3. Conditionals**
```lisp
(if #t 1 0)      >> 1
(if (< 3 5) 2 4) >> 2
```

### **4. Variable Definitions**
```lisp
(define x 10)    >> Define x as 10
(+ x 5)          >> 15
```

### **5. Functions**
```lisp
(define square (lambda (x) (* x x)))  >> Define square function
(square 3)                            >> 9
```
