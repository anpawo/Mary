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

---

## **Data Types**

### **CharType**
Represents a single character.
- Example: `'a'`, `'1'`, `'$'`
- **Usage:**
  ```c
  c: char = 'x';
  ```

### **BoolType**
Represents a Boolean value (`true` or `false`).
- **Usage:**
  ```c
  isReady: bool = true;
  ```

### **IntType**
Represents an integer value (whole number).
- **Usage:**
  ```c
  count: int = 42;
  ```

### **FloatType**
Represents a floating-point number (decimal values).
- **Usage:**
  ```c
  pi: float = 3.14;
  ```

### **StrType**
Represents a string (a sequence of characters). Equivalent to an array of `char`.
- **Usage:**
  ```c
  message: str = "Hello, world!";
  ```

### **ArrType**
Represents an array of elements of a specific type.
- **Usage:**
  ```c
  arr: arr[int] = [1, 2, 3];
  words: arr[str] = ["hello", "world"];
  ```

### **AnyType**
Represents a dynamic type that can hold any value
- **Usage:**
  ```c
  x: any = 10;
  x = "Dynamic typing";
  ```

### **4. ConstraintType**
Represents a type constraint for variables, often combining multiple allowed types.
- **Usage:**
  ```c
  x: int | float = 3.5;
  ```

---

## **Variable and Binding Syntax**

Declare and initialize variables using the following syntax:

```c
<variable> ::= <variable_name>: <type> = <value>;
```

### **Examples:**

1. **Integer Variable:**
   ```c
   age: int = 25;
   ```

2. **String Variable:**
   ```c
   name: str = "Ryan";
   ```

3. **Boolean Variable:**
   ```c
   isStudent: bool = true;
   ```

4. **Array of Floats:**
   ```c
   temperatures: arr[float] = [36.5, 37.0, 36.8];
   ```

---

## **Function**

### **Writing Functions**

In Mary, functions are defined using the `function` keyword, followed by the function name, parameters, return type, and body.

**Syntax:**
```c
function ::= function <function_name>(<parameters>) -> <return_type> {
  <function_body>
}
```

**Example**
```c
function test_loop(a: int) -> int {
  i: int = 1;
  b: int = 1;

  while i < a + 1 then {
      b = b * i;
      i = i + 1;
  }
  return b;
}
```

### **Key Points for Writing Functions:**
1. **Parameter Declaration:** Specify each parameter's name and type inside the parentheses.
2. **Return Type:** Use `->` followed by the type of the value the function returns. Use `void` if no value is returned.
3. **Body:** Enclose the function logic within `{}`.
4. **Return Statement:** Use `return` to provide the output of the function.

You need to create a function named `main` to execute the code you wrote

```c
function main() -> int {
  res: int = test_loop(6);
  return res;
}
```

## **Built-in Functions**

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

## **Condition**

### Using if

The `if` construct evaluates a condition and executes the corresponding block of code.

**Syntax:**

```
condition ::= if < bool | fonction<bool> > then { <body> } else { <body> }
```

Example:
```c
if x < 5 then {
    print("Less than 5");
} else {
    print("Greater or equal to 5");
}
```

Explanation:

  if x < 5: Checks if the variable x is less than 5.

  then: Introduces the block to execute if the condition is true.

  else: Executes an alternative block if the condition is false.

---

## **Loops and Recursion**

### Using Loops

A loop repeatedly executes a block of code as long as the condition evaluates to true.

**Syntax:**

```
loop ::= while < bool | fonction<bool> > then { <body> }
```

Example:
```c
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

Explanation:

  The while loop increments a by 1 until i equals b.

  The function returns the final value of a.

### Using Recursion

Recursion occurs when a function calls itself to solve a problem.

Example:
```c
function factorial(n: int) -> int {
    if n == 0 then {
        return 1;
    }
    return n * factorial(n - 1);
}

print(factorial(5)); // Outputs 120
```

Explanation:

  The base case checks if n is 0 and returns 1.
  For other values, the function recursively calls itself with n - 1, multiplying the results.

---

## **Custom Data Structures**

In Mary, structures are defined using the struct keyword, where each structure contains fields.

**Syntax:**

```c
structure ::= struct <struct_name> { field }
```
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
To access a field of a structure, the `.` operator is used:

```f#
function getname(p: struct person) -> str {
    return p.name;
}
```

---

## **Custom Operators**

In Mary, we can define custom operators using `operator` keyword:

**Syntax:**

```c
operator ::= operator <operator_symbol> precedence <int> (<parameters>) -> <return_type> {
  <operator_body>
}
```

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
---

## **Constraint type**

In Mary, we can create our own custom types.

Example: 
```
type number2: float | int;
```

---

## **Pass Functions as argument**

Functions can be passed as arguments:

```f#
function add_two(x: int) -> int {
    return x + 2;
}

print(add_two(5)); // Outputs 7

function apply(f: (int) -> int, x: int) -> int {
    return f(x);
}

print(apply((add_two), 10)); // Outputs 12
```

