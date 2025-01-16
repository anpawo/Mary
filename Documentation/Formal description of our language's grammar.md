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

<identifier> ::= <letter> (<letter> | <digit>)*
; An identifier is a sequence of letters or digits starting with a letter.
```

```BNF

<function> ::= "function" <space> <identifier> <space>? "(" <arguments>? ")" <space>? "->" <space>? <return_type> <space>? "{" <space>? <body> <space>? "}"
; Defines a function with a name, arguments, return type, and body.

<operator> ::= "operator" <space>? <symbol_identifier> <space>? ("precedence" <int>)? <space>? "(" <arguments>? ")" <space>? "->" <space>? <return_type> <space>? "{" <space>? <body> <space>? "}"
; Defines a function with a name, arguments, return type, and body.

<arguments> ::= <variable> (<space>? "," <space>? <variable>)*
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

<expression> ::= <declaration> | <return> | <if> | <while> | <sub_expression> ";"
; A expression can be a declaration, a constraint definition, a binary expression, a control structure, or a return expression.

<declaration> ::= <variable> <space>? "=" <space>? <sub_expression>
; Declares and initializes a variable.

<sub_expression> ::= <identifier> | <function_call> | <literal> | <operator_call>
; Represents any value that can be assigned to a variable.

<operator_call> ::= <sub_expression> <space>? <symbol_identifier> <space>? <sub_expression>

<function_call> ::= <identifier> <space>? "(" ((<space> <sub_expression>)? | ( <space>? <sub_expression> <space>? "," <space>? <sub_expression>)+) <space>? ")"
; Represents a call to a defined function.

<literal>::= <char> | <string> | <int> | <float> | <bool> | <null> | <array> | <struct>

<char> ::= "'" <character> "'"

<string> ::=  "\"" <character>* "\""
; A string is enclosed in double quotes and consists of a possibly empty list of characters

<int> ::= "-"? <digit>+
; Represents an integer value.

<float> ::= "-"? <digit>+ "." <digit>+
; A floating-point number contains digits, a decimal point, and additional digits.

<bool> ::= "true" | "false"
; Represents a boolean value.

<null> ::= "NULL"
; Represents the NULL value

<array> ::= <type> "[" <array_elements>? "]"

<array_elements> ::= <sub_expression> | (<sub_expression> <space>? "," <space>? <array_elements>)

<struct> ::= <identifier> "{" <struct_elements>? "}"

<lambda> ::= "\\" <arguments> "->" <subexpression>

<struct_elements> ::= (<identifier> "=" <space>? <sub_expression>) | (<identifier> "=" <sub_expression> <space>? "," <space>? <struct_elements>)

<return> ::= "return" <space> <sub_expression>
; Represents a return expression with an optional value.

<if> ::= "if" <space> <boolean_subexpression> <space> "then" <space>? "{" <body> "}" (<space>? "else" <space>? "{" <body> "}")?
; Represents an if-else conditional structure.

<boolean_subexpression> ::= <sub_expression>
; the result of the subexpression must be a boolean

<while> ::= "while" <space> <boolean_subexpression> <space> "then" <space>? "{" <body> "}"
; Represents a while loop structure.

<file> ::= (<function> | <operator> | <struct_def> | <atom_def> | <type_def>)+
; A Mary file can contain one or more definitions including functions, operator definitions, structure definitions, or type constraints.

<struct_def> ::= "struct" <space> <identifier> <space>? "{" <space>? (<variable> ("," <space>? <variable>)*)? "}"
; Defines a struct with some fields.

<atom_def> ::= "atom" <space> <identifier>

<type_def> ::= "type" <space> <identifier> <space>? "=" <space>? <type> (<space>? "|" <space>? <type>)*
```
