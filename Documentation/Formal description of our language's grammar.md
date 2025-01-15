Below is a formal description of our programming language `Mary`.
This documentation uses the [BNF notation](#https://letmegooglethat.com/?q=BNF+notation).

```BNF
<function> ::= "function" <whitespace> <function_name> <whitespace> "(" <arguments> ")" <whitespace> "->" <whitespace> <type> <whitespace> "{" <body> "}"

<whitespace> ::= " "

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

<binary_operator> ::= "==" | "<" | ">" | "<=" | ">="
```
<!-- todo ajouter les constraints dans tout les types -->