La bnf pour definir une fonction 

```BNF
<function> ::= "function" <function name> "(" <arguments> ")" "->" < type> "{" <function body> "}"

<function name> ::= <letter> (<letter> | <digit>)*

<letter> ::= 'a'-'z' | 'A'-'Z'

<digit> ::= '0'-'9'

<arguments> ::= <variable> | <function> ("," <variable> | <function type>)*

<variable> ::= <identifier> ":" <type>

<identifier> ::= <letter> (<letter> | <digit>)*

<type> ::= "int" | "float" | "string" | "void" | "bool" | "char" | "arr[" <type> "]" | "null" | <struct type> | <function type>

<return type> ::= "int" | "float" | "string" | "void" | "bool" | "char" | "arr[" <type> "]" | <struct type>

<function type> ::= <identifier> ":" "(" <arguments> ")" "->" <return type>

<struct type> ::= <identifier> ":" "struct " <name_of_struct>

<name_of_struct> ::= <letter> (<letter> | <digit>)*

<function body> ::= //todo
```
