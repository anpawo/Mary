# Formal language Description in Backus-Naur Form (BNF)
Below is a formal description of our programming language `Mary`.
This documentation uses the [BNF notation](#https://letmegooglethat.com/?q=BNF+notation).

## Defining the basic elements of the language
```BNF
<symbol> ::= "+" | "-" | "*" | "/" | "%" | "=" | "<" | ">" | "|" | "^" | "&" | "~" | "!" | "$" | "." | ":"
; Specifies valid symbols used as unique operators in the language.

<symbol_identifier> ::= <symbol>+
; Represents a series of one or more symbols, forming custom mathematical operators.

<letter> ::= "a" |... | "z" | "A" | ... | "Z"
; Matches any uppercase or lowercase letter.

<digit> ::= "0" | ... | "9"
; Matches any single numeric digit.

<character> ::= " " | ... | "~"
; Matches any printable ASCII character.

<space> ::= (" " | "\t" | "\n")+
; Matches one or more space characters, including tabs and newlines.

<identifier> ::= <letter> (<letter> | <digit>)*
; Represents a sequence starting with a letter, followed by letters or digits.
```

## Defining the functionalities of the language
```BNF
<function> ::= "function" <space> <identifier> <space>? "(" <arguments>? ")" <space>? "->" <space>? <return_type> <space>? "{" <space>? <body> <space>? "}"
; Specifies a function with a name, parameters, a return type, and a body.

<operator> ::= "operator" <space>? <symbol_identifier> <space>? ("precedence" <int>)? <space>? "(" <arguments>? ")" <space>? "->" <space>? <return_type> <space>? "{" <space>? <body> <space>? "}"
; Defines a custom operator with optional precedence, parameters, a return type, and a body.

<arguments> ::= <variable> (<space>? "," <space>? <variable>)*
; Represents a list of arguments separated by commas.

<variable> ::= <identifier> <space>? ":" <space>? <type>
; Declares a variable with an identifier and a type.

<type> ::= "int" | "float" | "string" | "bool" | "char" | "arr[" <space>? <type> <space>? "]" | "null" | <struct_type> | <function_type> | <constraint_type> | <identifier>
; Enumerates all supported types, including primitives, structured types, and constraints.

<return_type> ::= <type> | "void"
; Specifies the return type of a function or void if none.

<function_type> ::= "(" (<type> ("," <type>)*)? ")" "->" <return_type>
; Defines a function type, including argument types and a return type.

<constraint_type> ::= <named_constraint> | <unnamed_constraint>
; Refers to a type constraint, either named or unnamed.

<named_constraint> ::= "type" <space> <identifier>
; Defines a named type constraint.

<unnamed_constraint> ::= (<type> "|" (<type> | <unnamed_constraint>))
; Describes an unnamed constraint.

<struct_type> ::= "struct" <space> <identifier>
; Refers to a named structure type.

<body> ::= (<space>? <expression> <space>?)+
; Represents the body of a function, an operator, a while loop or an if condition.
: The body consists of one or more expressions.

<expression> ::= <declaration> | <return> | <if> | <while> | <sub_expression> ";"
; Represents a declaration, a control structure ("if" or "while"), or a sub-expression.

<declaration> ::= <variable> <space>? "=" <space>? <sub_expression>
; Declares and initializes a variable.

<sub_expression> ::= <identifier> | <function_call> | <literal> | <operator_call>
; Represents any valid assignable or computable entity.

<operator_call> ::= <sub_expression> <space>? <symbol_identifier> <space>? <sub_expression>
; Defines the use of an operator in an expression.

<function_call> ::= <identifier> <space>? "(" ((<space> <sub_expression>)? | (<space>? <sub_expression> <space>? "," <space>? <sub_expression>)+) <space>? ")"
; Represents calling a defined function.

<literal>::= <char> | <string> | <int> | <float> | <bool> | <null> | <array> | <struct>
; Matches a literal value, including characters, strings, integers, floats, booleans, the null value, arrays, or structures.

<char> ::= "'" <character> "'"
; Represents a single character enclosed in single quotes.

<string> ::=  "\"" <character>* "\""
; Represents a string enclosed in double quotes.

<int> ::= "-"? <digit>+
; Matches an integer, optionally negative.

<float> ::= "-"? <digit>+ "." <digit>+
; Matches a floating-point number, optionally negative.

<bool> ::= "true" | "false"
; Represents a boolean value.

<null> ::= "NULL"
; Represents a null value.

<array> ::= <type> "[" <array_elements>? "]"
; Declares an array with optional elements.

<array_elements> ::= <sub_expression> | (<sub_expression> <space>? "," <space>? <array_elements>)
; Defines the elements of an array.

<struct> ::= <identifier> "{" <struct_elements>? "}"
; Represents a structure with optional fields.

<struct_elements> ::= (<identifier> "=" <space>? <sub_expression>) | (<identifier> "=" <sub_expression> <space>? "," <space>? <struct_elements>)
; Specifies the fields in a struct definition.

<lambda> ::= "\\" <arguments> "->" <subexpression>
; Declares an anonymous function or lambda.

<return> ::= "return" <space> <sub_expression>
; Defines a return statement with a value.

<if> ::= "if" <space> <boolean_subexpression> <space> "then" <space>? "{" <body> "}" (<space>? "else" <space>? "{" <body> "}")?
; Describes an if-else conditional statement.

<boolean_subexpression> ::= <sub_expression>
; Represents a sub-expression expected to return a boolean value.

<while> ::= "while" <space> <boolean_subexpression> <space> "then" <space>? "{" <body> "}"
; Represents a while loop construct.

<file> ::= (<function> | <operator> | <struct_def> | <atom_def> | <type_def>)+
; Specifies the structure of a valid file, containing one or more definitions.

<struct_def> ::= "struct" <space> <identifier> <space>? "{" <space>? (<variable> ("," <space>? <variable>)*)? "}"
; Declares a struct with its fields.

<atom_def> ::= "atom" <space> <identifier>
; Defines an atom.

<type_def> ::= "type" <space> <identifier> <space>? "=" <space>? <type> (<space>? "|" <space>? <type>)*
; Defines a type alias or a union of types.
