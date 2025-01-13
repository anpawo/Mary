La bnf pour definir une fonction 

```BNF
<function> ::= "function" <function name> "(" <arguments> ")" "->" <return type> "{" <function body> "}"

<function name> ::= <letter> (<letter> | <digit>)*

<letter> ::= 'a'-'z' | 'A'-'Z'

<digit> ::= '0'-'9'

<arguments> ::= <variable> | <function> ("," <variable> | <function>)*

<variable> ::= <identifier> ":" <type>

<function> ::= //todo

<identifier> ::= <letter> (<letter> | <digit>)*

<return type> ::= "int" | "float" | "string" | "void" | "bool" | "char" | "arr[" <type> "]" | //todo type function

<type> ::= "int" | "float" | "string" | "void" | "bool" | "char" | "arr[" <type> "]" | "null" | //todo type fonction

<function body> ::= //todo
```

<!-- //todo notes a moi meme: -verifier si on peut nommer une fonction ou identifiant en commencant a par un digit 
    - demander si on peut "arr[arr[int]]"
    -->

