Below is a formal description of our programming language `Mary`.
This documentation uses the [BNF notation](#https://letmegooglethat.com/?q=BNF+notation).

```BNF
<file> ::= (<function> | <operator_definition> | <struct_definition> | <constraint_def>)+
; A Mary file can contain one or more definitions including functions, operator definitions, structure definitions, or type constraints.

<function> ::= "function" <whitespace> <function_name> <whitespace>? "(" <arguments> ")" <whitespace> "->" <whitespace> <return_type> <whitespace> "{" <body> "}"
; Defines a function with a name, arguments, return type, and body.

<whitespace> ::= " "
; Represents a single space character.

<function_name> ::= <letter> (<letter> | <digit>)*
; Function names must start with a letter and can be followed by letters or digits.

<function_call> ::= <function_name> "(" <arguments> ")" ";"
; Represents a call to a defined function.

<character> ::= .. Any ASCII character ..
; Represents any character in the ASCII table

<letter> ::= "a" | "b" | ... | "z" | "A" | "B" | ... | "Z"
; Represents any uppercase or lowercase letter.

<digit> ::= "0" | "1" | ... | "9"
; Represents a single digit.

<string> ::=  "\"" <character>* "\""
; A string is enclosed in double quotes and consists of apossibly empty list of characters

<float> ::= "-"? <digit>+ "." <digit>+
; A floating-point number contains digits, a decimal point, and additional digits.

<bool> ::= "true" | "false"
; Represents a boolean value.

<number> ::= "-"? <digit>+
; Represents an integer value.

<arguments> ::= <variable> | <function_type> ("," <whitespace>? <variable> | <function_type>)*
; Defines a list of arguments, which can be variables or function types, separated by commas.

<variable> ::= <identifier> ":" <type>
; Declares a variable with a name and type.

<identifier> ::= <letter> (<letter> | <digit>)*
; An identifier is a sequence of letters or digits starting with a letter.

<constraint_def> ::= "type" <whitespace> <identifier> <whitespace> "=" <whitespace> <type> <whitespace> "|" <whitespace> <type> <whitespace>? ";"
; Defines a type constraint as a union of two or more types.

<constraint> ::= <identifier>
; Represents a previously defined type constraint.

<type> ::= "int" | "float" | "string" | "void" | "bool" | "char" | "arr[" <type> "]" | "null" | <struct_type> | <function_type> | <constraint>
; Defines all supported types in Mary, including basic, structured, and constrained types.

<return_type> ::= "int" | "float" | "string" | "void" | "bool" | "char" | "arr[" <type> "]" | <struct_type> | <constraint>
; Specifies the return type of a function.

<function_type> ::= <identifier> ":" "(" <arguments> ")" "->" <return_type>
; Represents a function type with its arguments and return type.

<struct_type> ::= <identifier> ":" "struct " <identifier>
; Refers to a specific struct type by its name.

<body> ::= (<whitespace> <statement> <whitespace>)+
; The body of a function contains one or more statements separated by whitespace.

<statement> ::= <declaration> | <constraint_def> | <binary_expression> | <control_structure> | <return_statement>
; A statement can be a declaration, a constraint definition, a binary expression, a control structure, or a return statement.

<declaration> ::= <variable> "=" <value> ";"
; Declares and initializes a variable.

<value> ::= <identifier> | <function_call> | <string> | <number> | <float> | <bool>
; Represents any value that can be assigned to a variable.

<binary_expression> ::= <binary_operation> | <bool> | <number> | <float> | <identifier>
; Binary expressions include operations and simple values.

<binary_operation> ::= <binary_expression> <whitespace> <binary_operator> <whitespace> <binary_expression>
; Describes a binary operation using two expressions and an operator.

<control_structure> ::= <if_statement> | <while_loop>
; Control structures include conditional statements and loops.

<if_statement> ::= "if" <whitespace> <binary_expression> <whitespace> "then" <whitespace> "{" <body> "}" ( <whitespace> "else" <whitespace> "{" <body> "}")?
; Represents an if-else conditional structure.

<while_loop> ::= "while" <whitespace> <binary_expression> <whitespace> "then" <whitespace> "{" <body> "}"
; Represents a while loop structure.

<return_statement> ::= "return" (<whitespace> <value>)? ";"
; Represents a return statement with an optional value.

<operator> ::= "+" | "-" | "*" | "/" | "%"
; Defines mathematical operators.

<binary_operator> ::= "==" | "<" | ">" | "|" | "^" | "&" | "~"
; Defines binary comparison and logical operators.

<struct_def> ::= "struct" <whitespace> <identifier> <whitespace>? "{" <variable> ("," <variable>)* "}"
; Defines a struct with a name and one or more variables.

<operator_def> ::= "operator" <operator_name> "precedence" <number> "(" <arguments> ")" <whitespace> "->" <whitespace>? <return_type> <whitespace>? "{" <body> "}"

<operator_name> ::= .. any word or combination of ASCII characters that is not already assigned ..

```
<!-- todo mettre les operateurs [ '|', '^', '&', '~', '!', '$' , '.', '=', ':',] -->
<!-- todo ajouter les constraints dans tout les types -->
