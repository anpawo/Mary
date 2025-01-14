# User Manual for GLaDOS Programming Language

Welcome to the user manual for the GLaDOS programming language. This document will guide you through installing, running, and using GLaDOS effectively. Whether you're a beginner or an advanced user, this manual covers everything from syntax basics to advanced features.

---

## Table of Contents

- [User Manual for GLaDOS Programming Language](#user-manual-for-glados-programming-language)
  - [Table of Contents](#table-of-contents)
  - [1. Getting Started](#1-getting-started)
    - [Installation](#installation)
    - [Running GLaDOS](#running-glados)
    - [Hello, World!](#hello-world)
  - [2. Syntax and Basics](#2-syntax-and-basics)
    - [Variables and Bindings](#variables-and-bindings)
    - [Built-in Functions](#built-in-functions)
  - [3. Control Structures](#3-control-structures)
    - [Conditionals](#conditionals)
      - [`if`](#if)
    - [Loops and Recursion](#loops-and-recursion)
  - [4. Functions](#4-functions)
    - [Defining Functions](#defining-functions)
    - [Higher-Order Functions](#higher-order-functions)
  - [5. Advanced Features](#5-advanced-features)
    - [Custom Operators](#custom-operators)
    - [Closures](#closures)
  - [6. Error Handling](#6-error-handling)
    - [Common Errors](#common-errors)
    - [Debugging Tips](#debugging-tips)
  - [7. Examples](#7-examples)
    - [Factorial Function](#factorial-function)
    - [Fibonacci Sequence](#fibonacci-sequence)
    - [Custom Data Structures](#custom-data-structures)

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

```c
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

Our language is iterative, and takes direct inspiration from Python, TypeScript as well as C. In the following chapters we will see how to write our language.
The following examples are non-exhaustive and you are encouraged to read the [Formal description of our language's grammar](#https://github.com/EpitechPromo2027/B-FUN-500-PAR-5-2-glados-zacharie.rodde/blob/Documentation/Documentation/Formal%20description%20of%20our%20language%27s%20grammar.md)
by clicking the link or going to the [official repository](#https://github.com/EpitechPromo2027/B-FUN-500-PAR-5-2-glados-zacharie.rodde)
and clicking on `Documentation` and then on `Formal description of our language's grammar.md`.

This file contains detailed descriptions of our language's grammar using the [BNF notation](#https://letmegooglethat.com/?q=BNF+notation).

### Variables and Bindings

Create variables using the syntax:

```c
x: int = 10;
```

Access and manipulate the variable:

```c
y: int = x + 5;
print(y); // Outputs 15
```

### Mandatory use of a `Main` function

The use of a `main` function is mandatory. The structure of the `main` is the same as any other function except for it's name that needs to remain `main`.
//todo description du fonctionnnement d'un main et pk obligatoire ?

```f#
function main () -> <return type> {
    <main body>
}
```

### Built-in Functions

built-in functions include in Mary:

- **Arithmetic Operators**

  - (+) (Addition): Adds two numbers and returns the result.
  Example: 2 + 3 → 5.

  - (-) (Subtraction): Subtracts the second number from the first and returns the result.
  Example: 5 - 3 → 2.

  - (*) (Multiplication): Multiplies two numbers and returns the product.
  Example: 4 * 3 → 12.

  - (/) (Division): Divides the first number by the second and returns the quotient.
  Example: 10 / 2 → 5.

- **Comparison Operators**

  - is: Checks if the first value is a string and matches the second string. Returns true or false.
  Example: 5 is "number" → false.

  - (==) (Equality): Compares two values for equality. Works with any data type.
  Example: 3 == 3 → true.

  - (<) (Less Than): Checks if the first number is less than the second.
  Example: 2 < 5 → true.

- **Input/Output Functions**

  - print: Outputs a value to the standard output (e.g., console or terminal).
  Example: print("Hello, world!") → Displays Hello, world!.

  - eprint: Outputs a value to the standard error (useful for debugging or error messages).
  Example: eprint("Error occurred!").

  - getline: Reads a line of input from the user and returns it as a string.
  Example: name = getline() → Waits for user input and stores it in name.

  - exit: Stops the program and exits with the specified status code.
  Example: exit(0) → Terminates the program successfully.

- **Struct Manipulation**

  - (.) (Member Access): Accesses a member of a struct, object, or similar data type.
  Example: user.name retrieves the name property from the user object.
  
  - set: Sets a property on a struct or object to a given value.
  Example: set(user, "age", 25) → Assigns 25 to the age property of user.

- **Array and String Operations**

  - length: Returns the number of elements in an array or the number of characters in a string.
  Example: length([1, 2, 3]) → 3.
  Example: length("hello") → 5.

  - insert: Inserts an element into an array or string at the specified position.
  Example: insert([1, 2, 3], 1, 99) → [1, 99, 2, 3].
  Example: insert("hello", 1, 'x') → "hxello".

  - append: Adds an element to the end of an array or string.
  Example: append([1, 2], 3) → [1, 2, 3].
  Example: append("hi", 'a') → "hia".

  - at: Retrieves the element at the specified index in an array or string.
  Example: at([1, 2, 3], 1) → 2.
  Example: at("hello", 4) → 'o'.

  - concat: Combines two arrays or strings into one.
  Example: concat([1, 2], [3, 4]) → [1, 2, 3, 4].
  Example: concat("hi", " there") → "hi there".
  
  - pop: Removes and returns the element at the specified index in an array or string.
  Example: pop([1, 2, 3], 1) → 2 (leaves [1, 3]).
  Example: pop("hello", 0) → 'h' (leaves "ello").

  - del: Deletes the element at the specified index in an array.
  Example: del([1, 2, 3], 1) → [1, 3].

- **Type Conversion Functions**

  - toInt: Converts a value to an integer if possible.
  Example: toInt("42") → 42.

  - toFloat: Converts a value to a floating-point number if possible.
  Example: toFloat("3.14") → 3.14.

  - toChar: Converts a value to a character if possible.
  Example: toChar(65) → 'A' (ASCII value).

  - toString: Converts any value to a string representation.
  Example: toString(42) → "42".

---

## 3. Control Structures

### Conditionals

#### `if`

The `if` construct evaluates a condition:

```f#
if x < 5 then {
    print("Less than 5");
} else {
    print("Greater or equal to 5");
}
```

---

### Loops and Recursion

GLaDOS allows for both recursion and loops. Example:

An example of a loop:
```f#
function my_add(a: int, b: int) -> int {
    i: int = 0;

    while i < b then {
        a = a + 1;
        i = i + 1;
    }
    return a;
}

print(my_add(2, 5)); // Outputs 7
```

And recursion looks like this :
```f#
function factorial(n: int) -> int {
    if n == 0 then {
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

```f#
function add_two(x: int) -> int {
    return x + 2;
}

print(add_two(5)); // Outputs 7
```

### Higher-Order Functions

Functions can be passed as arguments:

```f#
function apply(f: (int) -> int, x: int) -> int {
    return f(x);
}

print(apply((add_two), 10)); // Outputs 12
```

---

## 5. Advanced Features

### Custom Operators

Define custom operators using `operator`:

```f#
operator !! precedence 10 (l: list, index: int) -> null | any {
    if index < 0 then {
        return NULL;
    } else {
        if index == 0 then {
            return l.data;
        }
        return l.next !! (index - 1);
    }
}
```

### Closures

This part is currently underway

Functions can close over variables:

```f#
function counter() -> function {
    plus: (int, int) -> int = (+);
    plus(1, 2); // 3
}
```

---

## 6. Error Handling

### Common Errors

- **Undefined Variable**:

  ```f#
  y: int = x + 5; // Error: Variable x is not defined
  ```

- **Malformed Expression**:

  ```f#
  z: int = 1 + ; // Error: Syntax error
  ```

### Debugging Tips

- Check for mismatched brackets.
- Ensure all variables are defined before use.

---

## 7. Examples

### Factorial Function

```f#
function factorial(n: int) -> int {
    if n == 0 then {
        return 1;
    }
    return n * factorial(n - 1);
}
```

### Fibonacci Sequence

```f#
function fibonacci(n: int) -> int {
    if n <= 1 then {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}
```

### Custom Data Structures

Using lists and structures:

Lists:
```c
import list;

print(1..5) // [1, 2, 3, 4, 5]
```

Structures:
  - Definition:

```c
struct date {
    d: int,
    m: int,
    y: int
}

struct person {
    name: str,
    age: date
}
```
  - Accessing a defined field:
```f#
function getname(p: struct person) -> str {
    return p.name;
}
```

---

Thank you for using GLaDOS !

