## Inspirations

Mary, a security-focused programming language, draws inspiration from both TypeScript and C, blending modern type safety and developer-friendly features with low-level precision and reliability. Here's how these two languages influence Mary's core security features:


### 1. TypeScript: Strong Typing and Developer-Friendly Error Checks

Mary incorporates TypeScript's principles of strong typing, emphasizing predictability, maintainability, and security:

  - **Explicit Type Declarations**

    Mary enforces explicit type annotations for variables and functions, similar to TypeScript. This minimizes ambiguity and reduces the risk of type mismatches:

    ```typescript
    let num: number = 42; // TypeScript
    a: int = 42;          // Mary
    ```

  - **Compile-Time Checks**

    Just as TypeScript detects type mismatches and missing parameters at compile time, Mary applies strict typing rules to ensure:

    - Functions are invoked with the correct number and types of arguments.
    - Return values conform to their declared signatures.

    ```typescript
    function double(n: number): int {
        return n * 2;
    }
    double("hello"); // TypeScript: Argument of type 'string' not assignable to 'number'

    function double(n: int) -> str {
        return n * 2;
    }
    double("hello"); // Mary: expected 'int' but got 'str'
    ```

  - **Ease of Debugging**

    By surfacing errors early in the development process, Mary helps developers catch potential bugs before runtime. This reduces security risks like unexpected type coercion or incorrect parameter usage.

### 2. C: Precision, Low-Level Control, and Robustness

From C, Mary inherits a focus on precision, control, and efficiency, while addressing common pitfalls for better security:

  - **Type Safety**
    Like C, Mary enforces strict type integrity but avoids unsafe practices, such as implicit type conversions:
    ```c
    int x = 3.14; // C: implicit narrowing conversion
    a: int = 3.14; // Mary: error, expected 'int' but got 'float'
    ```

  - **Mandatory Initialization**
    Mary eliminates risks associated with uninitialized variables by requiring all variables to be initialized at the time of declaration. This prevents undefined behavior and potential security vulnerabilities:

    ```c
    int a; // C: may contain garbage value
    a: int; // Mary: error, must initialize variable
    ```

  - **Static Typing at Compile-Time**

    Similar to C, Mary's static typing system ensures type checks occur at compile time, enhancing performance and security by avoiding runtime overhead.

### Conclusion

By combining the user-friendly safeguards of TypeScript with the precision and efficiency of C, Mary delivers a programming language that prioritizes secure, reliable, and maintainable code. Its strong typing, strict function signatures, and mandatory variable initialization create a robust environment for building secure applications, balancing developer productivity with precise control.