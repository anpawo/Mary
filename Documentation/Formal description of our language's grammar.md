Below is a formal description of our programming language `Mary`.
This documentation uses the [BNF notation](#https://letmegooglethat.com/?q=BNF+notation).

```BNF
<function> ::= "function" <function_name> "(" <arguments> ")" "->" <type> "{" <body> "}"

<function_name> ::= <letter> (<letter> | <digit>)*

<function_call> ::= <function_name> "(" <arguments> ")" ";"

<letter> ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"

<word> ::= <letter>+

<string> ::=  "\"" <word>+ "\""

<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

<float> ::= <digit>+ "." <digit>+

<bool> ::= "true" | "false"

<number> ::= <digit>+

<arguments> ::= <variable> | <function> ("," <variable> | <function_type>)*

<variable> ::= <identifier> ":" <type>

<identifier> ::= <letter> (<letter> | <digit>)*

<type> ::= "int" | "float" | "string" | "void" | "bool" | "char" | "arr[" <type> "]" | "null" | <struct_type> | <function_type>

<return_type> ::= "int" | "float" | "string" | "void" | "bool" | "char" | "arr[" <type> "]" | <struct_type>

<function_type> ::= <identifier> ":" "(" <arguments> ")" "->" <return_type>

<struct_type> ::= <identifier> ":" "struct " <name_of_struct>

<name_of_struct> ::= <letter> (<letter> | <digit>)*

<body> ::= <statement>+

<statement> ::= <declaration> | <expression> | <control_structure> | <return_statement>

<declaration> ::= <variable> "=" <expression> ";"

<expression> ::= <identifier> | <function_call> | <string> | <number> | <float> | <bool>

<return_statement> ::= "return" <expression>? ";"

//todo -----------------------------------------

<binary_operation> ::= <expression> <operator> <expression>

<operator> ::= "+" | "-" | "*" | "/" | "%"

<control_structure> ::= <if_statement> | <while_loop> | <for_loop>

<if_statement> ::= "if" <expression> "{" <body> "}" [ "else" "{" <body> "}" ]

<while_loop> ::= "while" <expression> "{" <body> "}"

```
