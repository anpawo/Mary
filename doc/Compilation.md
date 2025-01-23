# Compilation Process in the Mary Programming Language

Welcome to the overview of the compilation process in the Mary programming language.
The compilation process in Mary involves a series of transformations that take the original source code and convert it into machine-readable bytecode. The key steps include tokenization, AST generation, bytecode generation, and execution. Each stage plays a vital role in ensuring the program runs efficiently and correctly, with strong type safety and error handling provided by the compiler. Let’s explore these stages in more detail.

---

## Stages

### 1. Tokenization

The first step in the compilation process is tokenization, where the source code is broken down into smaller units called tokens. These tokens represent the fundamental components of the program, such as keywords, variables, operators, and literals. The tokenizer parses the code into these tokens to facilitate further analysis.

Example:

Original source code:

```c
function factorial(n: int) -> int {
    if n == 1 then {
        return n;
    } else {
        return n * factorial(n - 1);
    }
}
```

After tokenization, the code is split into individual tokens like:

```
FunctionKw  Identifier {identifier = TextId {textIdName = "factorial"}}  ParenOpen  Identifier {identifier = TextId {textIdName = "n"}}  Colon  Type {typing = IntType}  ParenClose  Arrow  Type {typing = IntType}  CurlyOpen  IfKw  Identifier {identifier = TextId {textIdName = "n"}}  Identifier {identifier = OperatorId {opIdName = "=="}}  Literal {literal = IntLit {intLiteralValue = 1}}  ThenKw  CurlyOpen  ReturnKw  Identifier {identifier = TextId {textIdName = "n"}}  SemiColon  CurlyClose  ElseKw  CurlyOpen  ReturnKw  Identifier {identifier = TextId {textIdName = "n"}}  Identifier {identifier = OperatorId {opIdName = "*"}}  Identifier {identifier = TextId {textIdName = "factorial"}}  ParenOpen  Identifier {identifier = TextId {textIdName = "n"}}  Identifier {identifier = OperatorId {opIdName = "-"}}  Literal {literal = IntLit {intLiteralValue = 1}}  ParenClose  SemiColon  CurlyClose  CurlyClose
```

These tokens represent the structure of the program, breaking down the function definition, conditionals, and operations into their components.

### 2. Abstract Syntax Tree (AST)

After tokenization, the compiler constructs an Abstract Syntax Tree (AST). The AST is a hierarchical tree structure that represents the syntactic structure of the program. It organizes the tokens in a way that reflects the logical structure and relationships between different components of the program, such as functions, variables, conditionals, and expressions.

Example:

For the factorial function in the example, the corresponding AST would look like:

```
Function {fnName = "factorial",
  fnArgs = [(int,"n")],
  fnRetType = int,
  fnBody = [
    IfThenElse {ifCond = FunctionCall {fnCallName = "==", fnCallArgs = [VariableCall {varCallName = "n"},Lit 1]},
    thenExpr = [Return {retValue = VariableCall {varCallName = "n"}}],
    elseExpr = [Return {retValue = FunctionCall {fnCallName = "*", fnCallArgs = [VariableCall {varCallName = "n"}, FunctionCall {fnCallName = "factorial", fnCallArgs = [FunctionCall {fnCallName = "-", fnCallArgs = [VariableCall {varCallName = "n"},Lit 1]}]}]}}]}]}
```

The AST structure simplifies the understanding of the program's logic by breaking it into smaller, logical pieces that can be processed more easily by the compiler.

### 3. Bytecode Generation

After the AST is built, the compiler generates bytecode, which is an intermediate representation of the program. Bytecode is a lower-level representation of the program that is more efficient for execution.

In this stage, the compiler translates the logical structure of the AST into bytecode instructions that represent the actual operations and flow of control in the program.

Example:

The bytecode for the factorial function might look like:
```
factorial = 
	Store "n"
	Load "n"
	Push 1
	Push function ==
	Call
	JumpIfFalse 2
	Load "n"
	Ret
	Load "n"
	Load "n"
	Push 1
	Push function -
	Call
	Push function factorial
	Call
	Push function *
	Call
	Ret
```

This bytecode is a list of instructions that the virtual machine will understand and execute.

### 4. Execution

Once the bytecode has been generated, the execution stage begins. The virtual machine reads the bytecode and executes the instructions step by step.

Example:

For the bytecode of a main function that print factorial of 5:

```
main = 
	Push 5
	Push function factorial
	Call
	Store "res"
	Load "res"
	Push function print
	Call
	Push 0
	Ret
```
The virtual machine will:

  - **Push 5:**
  Pushes the literal value 5 onto the stack, which is the argument for the factorial function.

  - **Push function factorial:**
  Pushes the factorial function onto the stack.

  - **Call:**
  Calls the factorial function with 5 as the argument. This will begin the recursive evaluation of factorial(5).

  - **Store "res":**
  Stores the result of factorial(5) in the variable res in the environment.

  - **Load "res":**
  Loads the value of res from the environment. This is the result of the factorial calculation, which will be printed.

  - **Push function print:**
  Pushes the print function onto the stack, preparing to print the result.

  - **Call:**
  Calls the print function with res as the argument. This outputs the value stored in res.

  - **Push 0:**
  Pushes the literal value 0 onto the stack, which represents the exit code for the main function.

  - **Ret:**
  Returns 0, signaling the end of the main function and indicating successful execution of the program.
