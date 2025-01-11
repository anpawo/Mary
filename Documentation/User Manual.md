# User Manual for GLaDOS Programming Language

Welcome to the user manual for the GLaDOS programming language. This document will guide you through installing, running, and using GLaDOS effectively. Whether you're a beginner or an advanced user, this manual covers everything from syntax basics to advanced features.

---

## Table of Contents

1. [Getting Started](#getting-started)

   - Installation
   - Running GLaDOS
   - Hello, World!

2. [Syntax and Basics](#syntax-and-basics)

   - S-Expressions
   - Variables and Bindings
   - Built-in Functions

3. [Control Structures](#control-structures)

   - Conditionals (`if`, `cond`)
   - Loops and Recursion

4. [Functions](#functions)

   - Defining Functions
   - Higher-Order Functions

5. [Advanced Features](#advanced-features)

   - Custom Operators
   - Closures

6. [Error Handling](#error-handling)

   - Common Errors
   - Debugging Tips

7. [Examples](#examples)

   - Factorial Function
   - Fibonacci Sequence
   - Custom Data Structures

---

## 1. Getting Started

### Installation

1. Download the latest release of GLaDOS from the [official repository](#https://github.com/EpitechPromo2027/B-FUN-500-PAR-5-2-glados-zacharie.rodde).
Download the latest release by clicking on `Releases` then `Assets` and downloading the release you want.

2. Ensure you have [Haskell](https://www.haskell.org/) installed on your machine.

3. Run the executable:
   ```bash
   ./<name_of_the_release_you_downloaded>
   ```

### Running GLaDOS

- **Executing a Script**: Run a script file using:
  ```bash
  ./glados <options> <file.mary | file.txt>
  ```

### Hello, World!

Save the following code in a file named `hello.mary`:

```mary
function main () -> void {
    print("Hello, World!");
}
```

Run it:

```bash
./glados hello.mary
```

Output:

```
Hello, World!
```

---

## 2. Syntax and Basics

Our language is iterative, and takes direct inspiration from Python, JavaScript as well as C. In the following chapters we will see how to write our language.

### Variables and Bindings

Create variables using the syntax:

```
let x = 10;
```

Access and manipulate the variable:

```
let y = x + 5;
print(y); // Outputs 15
```

### Built-in Functions

Some built-in functions include:

- Arithmetic: `+`, `-`, `*`, `/`
- Comparisons: `<`, `>`, `==`, `<=`, `>=`
- Logical: `is`, `or`, `not`

---

## 3. Control Structures

### Conditionals

#### `if`

The `if` construct evaluates a condition:

```glados
if (x < 5) then {
    print("Less than 5");
} else {
    print("Greater or equal to 5");
}
```

---

### Loops and Recursion

GLaDOS allows for both recursion and loops. Example:

An example of a loop:
```
function my_add(int a, int b) -> int {
    int i = 0;

    while i < b then {
        a = a + 1;
        i = i + 1;
    }
    return a;
}

print(my_add(2, 5)); // Outputs 7
```

And recursion looks like this :
```
function factorial(int n) -> int {
    if (n == 0) then {
        return 1;
    }
    return n * factorial(n - 1);
}

print(factorial(5)); // Outputs 120
```

---

## 4. Functions

### Defining Functions

Define a function using the `function` keyword:

```glados
function add_two(int x) -> int {
    return x + 2;
}

print(add_two(5)); // Outputs 7
```

### Higher-Order Functions

Functions can be passed as arguments:

```glados
function apply(function f, int x) -> int {
    return f(x);
}

print(apply(add_two, 10)); // Outputs 12
```

---

## 5. Advanced Features

### Custom Operators

Define custom operators using `let-op` (if supported):

```lisp
(let-op ** (lambda (x y) (* x x y y)))
(print (** 2 3))  ; Outputs 13
```

### Closures

Functions can close over variables:

```lisp
(let counter (lambda ()
  (let count 0)
  (lambda ()
    (let count (+ count 1)))))

(let c (counter))
(print (c))  ; Outputs 1
(print (c))  ; Outputs 2
```

---

## 6. Error Handling

### Common Errors

- **Undefined Variable**:

  ```lisp
  (+ x 5)  ; Error: Variable x is not defined
  ```

- **Malformed Expression**:

  ```lisp
  (+ 1)    ; Error: Insufficient arguments
  ```

### Debugging Tips

- Check for mismatched parentheses.

---

## 7. Examples

### Factorial Function

```lisp
(defun factorial (n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
```

### Fibonacci Sequence

```lisp
(defun fibonacci (n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))
```

### Custom Data Structures

Using lists:

```lisp
(let my-list '(1 2 3 4 5))
(print (car my-list))   ; Outputs 1
(print (cdr my-list))   ; Outputs (2 3 4 5)
```

---

Thank you for using GLaDOS !
