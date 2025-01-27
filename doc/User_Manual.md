# User Manual for Mary Programming Language

Welcome to the user manual for the Mary programming language. This document will guide you through installing, running, and using GLaDOS effectively. Whether you're a beginner or an advanced user, this manual covers everything from syntax basics to advanced features.

---

## Table of Contents

- [User Manual for Mary Programming Language](#user-manual-for-mary-programming-language)
  - [Table of Contents](#table-of-contents)
  - [1. Getting Started](#1-getting-started)
    - [Installation](#installation)
    - [Running GLaDOS](#running-glados)
    - [Hello, World!](#hello-world)
  - [2. Syntax and Basics](#2-syntax-and-basics)
  - [3. Data Types](#3-data-types)
    - [CharType](#chartype)
    - [BoolType](#booltype)
    - [IntType](#inttype)
    - [FloatType](#floattype)
    - [StrType](#strtype)
    - [ArrType](#arrtype)
    - [AnyType](#anytype)
    - [ConstraintType](#constrainttype)
  - [4. Variable and Binding Syntax](#4-variable-and-binding-syntax)
  - [5. Function](#5-function)
    - [Writing Functions](#writing-functions)
    - [Built-in Functions](#built-in-functions)
  - [6. Custom Operators](#6-custom-operators)
  - [7. Pass Functions as argument](#7-pass-functions-as-argument)
  - [8. Lambda Functions](#8-lambda-functions)
    - [Lambdas are closures](#lambdas-are-closures)
  - [9. Condition](#9-condition)
    - [Using if](#using-if)
  - [10. Loops and Recursion](#10-loops-and-recursion)
    - [Using Loops](#using-loops)
    - [Using Recursion](#using-recursion)
  - [11. Import](#11-import)
  - [12. Custom Data Structures](#12-custom-data-structures)
  - [13. Constraint type](#13-constraint-type)

---

## 1. Getting Started

### Installation

1. Download the latest release of GLaDOS from the [official repository](https://github.com/EpitechPromo2027/B-FUN-500-PAR-5-2-glados-zacharie.rodde).
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
The following examples are non-exhaustive and you are encouraged to read the [Formal description of our language's grammar](https://github.com/EpitechPromo2027/B-FUN-500-PAR-5-2-glados-zacharie.rodde/blob/Documentation/Documentation/Formal%20description%20of%20our%20language%27s%20grammar.md)
by clicking the link or going to the [official repository](https://github.com/EpitechPromo2027/B-FUN-500-PAR-5-2-glados-zacharie.rodde)
and clicking on `Documentation` and then on `Formal description of our language's grammar.md`.

This file contains detailed descriptions of our language's grammar using the [BNF notation](https://letmegooglethat.com/?q=BNF+notation).

---

## 3. Data Types

### CharType
Represents a single character.
- Example: `'a'`, `'1'`, `'$'`
- **Usage:**
  ```c
  c: char = 'x';
  ```

### BoolType
Represents a Boolean value (`true` or `false`).
- **Usage:**
  ```c
  isReady: bool = true;
  ```

### IntType
Represents an integer value (whole number).
- **Usage:**
  ```c
  count: int = 42;
  ```

### FloatType
Represents a floating-point number (decimal values).
- **Usage:**
  ```c
  pi: float = 3.14;
  ```

### StrType
Represents a string (a sequence of characters). Equivalent to an array of `char`.
- **Usage:**
  ```c
  message: str = "Hello, world!";
  ```

### ArrType
Represents an array of elements of a specific type.
- **Usage:**
  ```c
  arr: arr[int] = [1, 2, 3];
  words: arr[str] = ["hello", "world"];
  ```

### AnyType
Represents a dynamic type that can hold any value
- **Usage:**
  ```c
  x: any = 10;
  x = "Dynamic typing";
  ```

### ConstraintType
Represents a type constraint for variables, often combining multiple allowed types.
- **Usage:**
  ```c
  x: int | float = 3.5;
  ```

---

## 4. Variable and Binding Syntax

Declare and initialize variables using the following syntax:

```BNF
<variable> ::= <variable_name>: <type> "=" <value>;
```

### Examples:

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

## 5. Function

### Writing Functions

In Mary, functions are defined using the `function` keyword, followed by the function name, parameters, return type, and body.

**Syntax:**
```BNF
<function> ::= "function" <function_name> "(" <parameters> ")" "->" <return_type> "{"
  <function_body>
"}"
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

### Key Points for Writing Functions:
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

### Built-in Functions

Built-in functions include in Mary:

- **Arithmetic Operators**

  - (+) (Addition): Adds two numbers and returns the result.

    `Example: 2 + 3 → 5.`

  - (-) (Subtraction): Subtracts the second number from the first and returns the result.

    `Example: 5 - 3 → 2.`

  - (*) (Multiplication): Multiplies two numbers and returns the product.
  
    `Example: 4 * 3 → 12.`

  - (/) (Division): Divides the first number by the second and returns the quotient.
  
    `Example: 10 / 2 → 5.`

- **Comparison Operators**

  - is: Checks if the first value matches the type specified in the second parameter.
  
    `Example: 5 is "int" → true.`

  - (==) (Equality): Compares two values for equality. Works with any data type.
  
    `Example: 3 == 3 → true.`

  - (<) (Less Than): Checks if the first number is less than the second.
  
    `Example: 2 < 5 → true.`

- **Input/Output Functions**

  - print: Outputs a value to the standard output (e.g., console or terminal).
  
    `Example: print("Hello, world!") → Displays Hello, world!.`

  - eprint: Outputs a value to the standard error (useful for debugging or error messages).
  
    `Example: eprint("Error occurred!").`

  - getline: Reads a line of input from the user and returns it as a string.
  
    `Example: name = getline() → Waits for user input and stores it in name.`

  - exit: Stops the program and exits with the specified status code.
  
    `Example: exit(0) → Terminates the program successfully.`

- **Struct Manipulation**

  - (.) (Member Access): Accesses a member of a struct.
  
    `Example: user.name retrieves the name property from the user struct.`
  
  - set: Sets a property on a struct.
    
    `Example: set(user, "age", 25) → Assigns 25 to the age property of user.`

- **Array and String Operations**

  - length: Returns the number of elements in an array or the number of characters in a string.
  
    `Example: length([1, 2, 3]) → 3.`

    `Example: length("hello") → 5.`

  - insert: Inserts an element into an array or string at the specified position.

    `Example: insert([1, 2, 3], 1, 99) → [1, 99, 2, 3].`

    `Example: insert("hello", 1, 'x') → "hxello".`

  - append: Adds an element to the end of an array or string.

    `Example: append([1, 2], 3) → [1, 2, 3].`

    `Example: append("hi", 'a') → "hia".`

  - at: Retrieves the element at the specified index in an array or string.

    `Example: at([1, 2, 3], 1) → 2.`

    `Example: at("hello", 4) → 'o'.`

  - concat: Combines two arrays or strings into one.

    `Example: concat([1, 2], [3, 4]) → [1, 2, 3, 4].`
  
    `Example: concat("hi", " there") → "hi there".`
  
  - pop: Removes and returns the element at the specified index in an array or string.

    `Example: pop([1, 2, 3], 1) → 2 (leaves [1, 3]).`

    `Example: pop("hello", 0) → 'h' (leaves "ello").`

  - del: Deletes the element at the specified index in an array.

    `Example: del([1, 2, 3], 1) → [1, 3].`

- **Type Conversion Functions**

  - toInt: Converts a value to an integer if possible.
  
    `Example: toInt("42") → 42.`

  - toFloat: Converts a value to a floating-point number if possible.
  
    `Example: toFloat("3.14") → 3.14.`

  - toChar: Converts a value to a character if possible.
  
    `Example: toChar(65) → 'A' (ASCII value).`

  - toString: Converts any value to a string representation.
  
    `Example: toString(42) → "42".`

---

## 6. Custom Operators

In Mary, we can define custom operators using `operator` keyword, where precedence refers to the priority order of the operator:

For example, in `Mary` the `+` operator has a precedence of 6 and the `*` operator has a precedence of 7. This means that the operation `1 + 2 * 3` is equivalent to ` 1 + (2 * 3)` because `*` will be executed before `+` as 7 is greater than 6.

**Syntax:**

```BNF
<operator> ::= "operator" <operator_symbol> "precedence" <int> "(" <parameters> ")" "->" <return_type> "{"
  <operator_body>
"}"
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

## 7. Pass Functions as argument

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

---

## 8. Lambda Functions

Lambda functions can be defined using `\` and then defining the function.

**Syntax:**

```BNF
<lambda> ::= "\" <arguments> "->" <subexpression>
```

It is also possible to associate a lambda function to an identifier.

**Syntax:**
```BNF
<lambda> ::= <identifier> ":" <arguments> "->" <return_type> "=" <subexpression> ";"
```

Example:
```cpp
import list

function test(c: int) -> (int) -> int { //this function returns a lambda function
    a: int = 1;
    f: (int, int) -> int = (+);
    return \b -> a + f(b, c);
}

function main() -> void {
    f: (int) -> int = test(3);      //this is a lambda associated to the "f" identifier
    f2: () -> int = \-> f(4);       //this is a lambda associated to the "f2" identifier
    print(f(2));                    //Outputs 6
    print(f2());                    //Outputs 8
    print(map(\x -> x * 2, 1..10)); //map takes here as parameter a lambda and applies it to each element of the list [1, .. , 10]
                                    //Outputs [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
}

```

### Lambdas are closures

It is important to note that lambda functions are closures, meaning they can capture and remember variables from the environment in which they were created.

Example:

```cpp
// The lambda function "remembers" the value of `factor` even after make_multiplier finishes executing
function make_multiplier(factor: int) -> (int) -> int {
    return \x -> x * factor;
}

function main () -> void {
    double: (int) -> int = make_multiplier(2);  // Create a multiplier function with factor = 2
    triple: (int) -> int = make_multiplier(3);  // Create a multiplier function with factor = 3
    
    print(double(5));                           // Output: 10 (5 * 2)
    print(triple(5));                           // Output: 15 (5 * 3)
}
```

---

## 9. Condition

### Using if

The `if` construct evaluates a condition and executes the corresponding block of code.

**Syntax:**

```BNF
<if> ::= "if" <boolean expression> "then" "{" <body> "}" "else" "{" <body> "}"
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

## 10. Loops and Recursion

### Using Loops

A loop repeatedly executes a block of code as long as the condition evaluates to true.

**Syntax:**

```BNF
<while_loop> ::= "while" <boolean_expression> "then" "{" <body> "}"
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

## 11. Import

In Mary, we can import file or lib using `import` keyword:

```f#
import math

function main() -> int {
    print(3 ** 3);
    print(factorial(6));
    print(fibonacci(7));
    print(pow(3, 4));
    return 0;
}

```

---

## 12. Custom Data Structures

In Mary, structures are defined using the struct keyword, where each structure possibly contains fields.

It is also possible to define a structure with no fields by using 
```
atom empty
```

**Syntax:**

```BNF
<structure> ::= "struct" <struct_name> "{" <fields> "}"
```
```c
struct person {
    name: str,
    age: int
}

personStruct : struct person = { name = "john", age = 30 };
```
To access a field of a structure, the `.` operator is used:

```f#
function getname(p: struct person) -> str {
    return p.name;
}
```

---

## 13. Constraint type

In Mary, we can create our own custom types.

**Syntax:**

```BNF
<constraint> ::= "type" <name> ":" <type> "|" <type> ";"
```

Example: 
```
type number2: float | int;
```
