Below is a formal description of our programming language `Mary`.
This documentation uses the [BNF notation](#https://letmegooglethat.com/?q=BNF+notation).


first paragraph to define basic things of the language
```BNF
<symbol> ::= "+" | "-" | "*" | "/" | "%" | "=" | "<" | ">" | "|" | "^" | "&" | "~" | "!" | "$" | "." | ":"
; Defines a unique name for a custom operator.

<symbol_identifier> ::= <symbol>+
; Defines mathematical operators.

<letter> ::= "a" |... | "z" | "A" | ... | "Z"
; Represents any uppercase or lowercase letter.

<digit> ::= "0" | ... | "9"
; Represents a single digit.

<character> ::= " " | ... | "~"
; Represents any printable character in the ASCII table

<space> ::= (" " | "\t" | "\n")+
; Represents a single space character.

<string> ::=  "\"" <character>* "\""
; A string is enclosed in double quotes and consists of apossibly empty list of characters

<number> ::= "-"? <digit>+
; Represents an integer value.

<float> ::= "-"? <digit>+ "." <digit>+
; A floating-point number contains digits, a decimal point, and additional digits.

<bool> ::= "true" | "false"
; Represents a boolean value.

<null> ::= "NULL"
; Represents the NULL value

<identifier> ::= <letter> (<letter> | <digit>)*
; An identifier is a sequence of letters or digits starting with a letter.
```

```BNF

<function> ::= "function" <space> <identifier> <space>? "(" <arguments>? ")" <space>? "->" <space>? <return_type> <space>? "{" <space>? <body> <space>? "}"
; Defines a function with a name, arguments, return type, and body.

<arguments> ::= <variable> ( <space>? "," <space>? <variable>)*
; Defines a list of arguments, which can be variables or function types, separated by commas.

<variable> ::= <identifier> <space>? ":" <space>? <type>
; Declares a variable with a name and type.

<type> ::= "int" | "float" | "string" | "bool" | "char" | "arr[" <space>? <type> <space>? "]" | "null" | <struct_type> | <function_type> | <constraint_type> | <identifier>
; Defines all supported types in Mary, including basic, structured, and constrained types.
; Identifier in types are either named constraints or structures with respectively the prefix type/struct

<return_type> ::= <type> | "void" 
; Specifies the return type of a function.

<function_type> ::= "(" (<type> ("," <type>)*)? ")" "->" <return_type>
; Represents a function type with its arguments and return type.

<constraint_type> ::= <named_constraint> | <unnamed_constraint>
; Represents a previously defined type constraint.

<named_constraint> ::= "type" <space> <identifier>

<unnamed_constraint> ::= (<type> "|" (<type> | <unnamed_constraint>))

<struct_type> ::= "struct" <space> <identifier>
; Refers to a specific struct type by its name.

<body> ::= (<space>? <expression> <space>?)+
; The body of a function contains one or more statements separated by whitespace.

<expression> ::= <declaration> | <constraint_def> | <binary_expression> | <control_structure> | <return_statement>
; A expression can be a declaration, a constraint definition, a binary expression, a control structure, or a return expression.

<declaration> ::= <variable> <space>? "=" <space>? <sub_expression> ";"
; Declares and initializes a variable.

<sub_expression> ::= <identifier> | <function_call> | <literal>
; Represents any value that can be assigned to a variable.

<function_call> ::= <identifier> <space>? "(" ((<space> <identifier>)? | ( <space>? <identifier> <space>? "," <space>? <identifier>)+) <space>? ")"
; Represents a call to a defined function.



<literal>::= //todo



<file> ::= (<function> | <operator> | <struct_definition> | <constraint>)+
; A Mary file can contain one or more definitions including functions, operator definitions, structure definitions, or type constraints.

<constraint> ::= "type" <space> <identifier> <space> "=" <space> <type> <space> "|" <space> <type> <space>? ";"
; Defines a type constraint as a union of two or more types.


<binary_expression> ::= <binary_operation> | <bool> | <number> | <float> | <identifier>
; Binary expressions include operations and simple values.

<binary_operation> ::= <binary_expression> <space> <binary_operator> <space> <binary_expression>
; Describes a binary operation using two expressions and an operator.

<control_structure> ::= <if_statement> | <while_loop>
; Control structures include conditional statements and loops.

<if_statement> ::= "if" <space> <binary_expression> <space> "then" <space> "{" <body> "}" ( <space> "else" <space> "{" <body> "}")?
; Represents an if-else conditional structure.

<while_loop> ::= "while" <space> <binary_expression> <space> "then" <space> "{" <body> "}"
; Represents a while loop structure.

<return_statement> ::= "return" (<space> <value>)? ";"
; Represents a return expression with an optional value.

<binary_operator> ::= "==" | "<" | ">" | "|" | "^" | "&" | "~"
; Defines binary comparison and logical operators.

<struct_def> ::= "struct" <space> <identifier> <space>? "{" <variable> ("," <variable>)* "}"
; Defines a struct with a name and one or more variables.

<operator> ::= "operator" <operator_name> "precedence" <number> "(" <arguments> ")" <space> "->" <space>? <return_type> <space>? "{" <body> "}"
; Defines a custom operator with precedence, arguments, a return type, and a body.
```
<!-- todo mettre les operateurs [ '!', '$' , '.', '=', ':',] -->
