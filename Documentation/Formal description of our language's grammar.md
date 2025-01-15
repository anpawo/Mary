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

<function_type> ::= <identifier> ":" "(" <arguments> ")" "->" <return_type>

<struct_type> ::= <identifier> ":" "struct " <name_of_struct>

<name_of_struct> ::= <letter> (<letter> | <digit>)*

<body> ::= (<whitespace> <statement> <whitespace>)+

<statement> ::= <declaration> | <binary_expression> | <control_structure> | <return_statement>

<declaration> ::= <variable> "=" <value> ";"

<value> ::= <identifier> | <function_call> | <string> | <number> | <float> | <bool>

<binary_expression> ::= <binary_operation> | <bool> | <number> | <float> | <identifier>

<binary_operation> ::= <binary_expression> <whitespace> <binary_operator> <whitespace> <binary_expression>

<control_structure> ::= <if_statement> | <while_loop>

<if_statement> ::= "if" <whitespace> <binary_expression> <whitespace> "then" <whitespace> "{" <body> "}" ( <whitespace> "else" <whitespace> "{" <body> "}")?

<while_loop> ::= "while" <whitespace> <binary_expression> <whitespace> "then" <whitespace> "{" <body> "}"

<return_statement> ::= "return" (<whitespace> <value>)? ";"

<operator> ::= "+" | "-" | "*" | "/" | "%"

<binary_operator> ::= "==" | "<" | ">" | "|" | "^" | "&" | "~"

<struct_def> ::= "struct" <whitespace> <identifier> <whitespace>? "{" <variable> ("," <variable>)* "}"

<operator_def> ::= "operator" <operator_name> "precedence" <number> "(" <arguments> ")" <whitespace> "->" <whitespace>? <return_type> <whitespace>? "{" <body> "}"

<operator_name> ::= .. any word or combination of ASCII characters that is not already assigned ..

```
<!-- todo mettre les operateurs [ '|', '^', '&', '~', '!', '$' , '.', '=', ':',] -->
<!-- todo ajouter les constraints dans tout les types -->
