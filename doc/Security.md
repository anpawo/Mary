# Security Features in the Mary Programming Language

Welcome to the overview of the security features in the Mary programming language. This document explains how Mary ensures security and reliability through its design and implementation. 

As a strongly typed language, Mary’s compiler enforces several rules to enhance code clarity and minimize errors, such as:

- Ensuring variables are properly initialized during declaration.
- Preventing the wrong usage of a function.

---

## Key Security Features

### 1. Strong Typing and Explicit Function Signatures

Functions in Mary must adhere to strict typing rules, ensuring they:

- **Return the Correct Type**
  The compiler enforces the specified return type, generating an error if a mismatch occurs:
  ```c
  function getNumber(n: int) -> bool {
      return n;
  }
  // error: Invalid return type, expected 'bool' but got 'int'.
  ```

- **Accept Correct Parameters**
  Functions must be called with the exact number and types of arguments as defined in their signature. Compile-time checks prevent incorrect usage:
  ```c
  function factorial(n: int) -> int {
      if n == 1 then
      {
          return n;
      }
      else
      {
          return n * factorial(n - 1);
      }
  }
  factorial(5,2);
  // error: invalid number of arguments for the function 'factorial', expected 1 but found 2.
  ```

### 2. Variable Type Integrity

Variables in Mary are strictly typed, meaning:

- **Type Safety**
  Variables must be declared with a specific type, and only values of that type can be assigned:
  ```c
    a: int = 'c';
    //error: invalid type for the variable 'a', expected 'int' got 'char'.
  ```

- **Proper Initialization**
  Every variable must be assigned an initial value during declaration. This prevents:
    - Accessing undefined values, which could lead to runtime errors or unpredictable behavior.
    - Security vulnerabilities from uninitialized variables being used as placeholders or default values.
  ```c
    a: int;
    //error: must initialize variable.
  ```
By enforcing these rules, Mary reduces risks associated with undefined behavior and ensures reliable, secure code execution.

