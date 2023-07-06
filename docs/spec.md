# The Zephyr Programming Language Specification

## General elements

### Alphabet and digit

```
<alphabet>          ::= "a" .. "z" | "A" .. "Z"
<binary_digit>      ::= "0" | "1"
<octadecimal_digit> ::= "0" .. "7"
<decimal_digit>     ::= "0" .. "9"
<hexadecimal_digit> ::= "0" .. "9" | "a" .. "f" | "A" .. "F"
```

## Lexical items

### Comment

There is two type of comment: range comment and line comment.

- Line comment start with `//` and valid in the line.
- Range comment start with `/*` and end with `*/`. This is valid through multiple lines.

```
// Line comment
This line is not a comment

/*
    You can write comment inside /* */
    Here is also comment
*/
```

### Identifier

An identifier contain ascii character, underline and number. Start with number is not
allowed on this specification.

```
<identifier>      ::= <identifier_head> { <identifier_rest> }
<identifier_head> ::= <alphabet> | "_"
<identifier_rest> ::= <alphabet> | "_" | <decimal_digit>
```

```
a
ident12
_ident12

12ident // start with numeric is not valid
```

### Operator and symbols

Below is operators and symbols which the lexer recognize.

```
+ - * / % & | ^ << >> && || ! < > <= >= == !=
= += -= *= /= %= &= |= ^= <<= >>=
```

```
( ) [ ] { } : ; , .
```

### keywords

```
function return let
```

### Integer iteral

An integer is a list of alphabet or numeric. 
You can separate digit with `_`, but end with it is not allowed.

```
<integer_lit>     ::= <binary_lit> | <decimal_lit> | <octadecimal_lit> | <hexadecimal_lit>
<binary_lit>      ::= "0" ( "b" | "B" ) <binary_digits>
<decimal_lit>     ::= "0" | <decimal_digits>
<octadecimal_lit> ::= "0" ( "o" | "O" ) <octadecimal_digits>
<hexadecimal_lit> ::= "0" ( "x" | "X" ) <hexadecimal_digits>
<binary_digits>       ::= <binary_digit> { [ "_" ] <binary_digit> }
<decimal_digits>      ::= <decimal_digit> { [ "_" ] <decimal_digit> }
<octadecimal_digits>  ::= <octadecimal_digit> { [ "_" ] <octadecimal_digit> }
<hexadecimal_digits>  ::= <hexadecimal_digit> { [ "_" ] <hexadecimal_digit> }
```

```
0b0010_1101
30
0o76_13
0x0a23

42_ // end with _ is invalid
_42 // this is identifier, not integer literal
```

## Declarative item

```
<declarative> ::= <function_decl>
```

### Function Declaration

```
<function_decl> ::= "function" <function_name> "(" <function_args> ")" <function_body>
<function_name> ::= <identifier>
<function_args> ::= <function_arg> "," <function_args>
                  | <function_arg>
<function_arg>  ::= <identifier>
<function_body> ::= "{" { <statement> } "}"
```

## Statement

```
<statement> ::= <let_stmt>
              | <expression_stmt>
              | <return_stmt>
```

### Let statement

```
<let_stmt> ::= "let" <identifier> [ "=" <expression> ] ";"
```

### Expression statement

```
<expression_stmt> ::= <expression> ";"
```

### Return statement

```
<return_stmt> ::= "return" <expression> ";"
```
