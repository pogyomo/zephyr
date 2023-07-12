# The Zephyr Programming Language Specification

## General elements

### Alphabet and digit

```
<alphabet>          ::= "a" .. "z" | "A" .. "Z"
<binary-digit>      ::= "0" | "1"
<octadecimal-digit> ::= "0" .. "7"
<decimal-digit>     ::= "0" .. "9"
<hexadecimal-digit> ::= "0" .. "9" | "a" .. "f" | "A" .. "F"
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
<identifier>      ::= <identifier-head> { <identifier-rest> }
<identifier-head> ::= <alphabet> | "_"
<identifier-rest> ::= <alphabet> | "_" | <decimal-digit>
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
<integer-lit>     ::= <binary-lit> | <decimal-lit> | <octadecimal-lit> | <hexadecimal-lit>
<binary-lit>      ::= "0" ( "b" | "B" ) <binary-digits>
<decimal-lit>     ::= "0" | <decimal-digits>
<octadecimal-lit> ::= "0" ( "o" | "O" ) <octadecimal-digits>
<hexadecimal-lit> ::= "0" ( "x" | "X" ) <hexadecimal-digits>
<binary-digits>       ::= <binary-digit> { [ "_" ] <binary-digit> }
<decimal-digits>      ::= <decimal-digit> { [ "_" ] <decimal-digit> }
<octadecimal-digits>  ::= <octadecimal-digit> { [ "_" ] <octadecimal-digit> }
<hexadecimal-digits>  ::= <hexadecimal-digit> { [ "_" ] <hexadecimal-digit> }
```

```
0b0010_1101
30
0o76_13
0x0a23

42_ // end with _ is invalid
_42 // this is identifier, not integer literal
```

### Types

```
<types> ::= "u8"
          | "i8"
          | <pointer>
<pointer> ::= "*" <types>
```

## Declarative item

```
<declarative> ::= <function-decl>
                | <struct-decl>
```

### Function Declaration

```
<function-decl> ::= "function" <function-name> "(" [ <function-args> ] ")" <function-body>
<function-name> ::= <identifier>
<function-args> ::= <function-arg> "," <function-args>
                  | <function-arg>
<function-arg>  ::= <identifier> ":" <types>
<function-body> ::= "{" { <statement> } "}"
```

### Struct Declaration

```
<struct-decl> ::= "struct" <identifier> "{" <struct-fields> "}"
<struct-fields> ::= <struct-field>
                  | <struct-field> "," <struct-fields>
<struct-field> ::= <identifier> ":" <types>
```

```
struct Hoge {
    a: u8,
    b: i8,
}
```

### Union Declaration

```
<union-decl> ::= "union" <identifier> "{" <union-fields> "}"
<union-fields> ::= <union-field>
                  | <union-field> "," <union-fields>
<union-field> ::= <identifier> ":" <types>
```

```
union Hoge {
    a: u8,
    b: i8,
}
```

## Statement

```
<statement> ::= <let-stmt>
              | <expression-stmt>
              | <return-stmt>
```

### Let statement

```
<let-stmt> ::= "let" <identifier> ":" <types> [ "=" <expression> ] ";"
```

### Expression statement

```
<expression-stmt> ::= <expression> ";"
```

### Return statement

```
<return-stmt> ::= "return" [ <expression> ] ";"
```

## Expression

```
<expression> ::= <logical-or-expression>
<logical-or-expression> ::= <logical-and-expression>
                          | <logical-and-expression> "||" <logical-and-expression>
<logical-and-expression> ::= <inclusive-or-expression>
                           | <inclusive-or-expression> "&&" <inclusive-or-expression>
<inclusive-or-expression> ::= <exclusive-or-expression>
                            | <exclusive-or-expression> "|" <exclusive-or-expression>
<exclusive-or-expression> ::= <and-expression>
                            | <and-expression> "^" <and-expression>
<and-expression> ::= <equality-expression>
                   | <equality-expression> "&" <equality-expression>
<equality-expression> ::= <relative-expression>
                        | <relative-expression> "==" <relative-expression>
                        | <relative-expression> "!=" <relative-expression>
<relative-expression> ::= <shift-expression>
                        | <shift-expression> "<" <shift-expression>
                        | <shift-expression> ">" <shift-expression>
                        | <shift-expression> "<=" <shift-expression>
                        | <shift-expression> ">=" <shift-expression>
<shift-expression> ::= <additive-expression>
                     | <additive-expression> "<<" <additive-expression>
                     | <additive-expression> ">>" <additive-expression>
<additive-expression> ::= <multiplicative-expression>
                        | <multiplicative-expression> "+" <multiplicative-expression>
                        | <multiplicative-expression> "-" <multiplicative-expression>
<multiplicative-expression> ::= <multiplicative-expression>
                              | <multiplicative-expression> "*" <multiplicative-expression>
                              | <multiplicative-expression> "/" <multiplicative-expression>
                              | <multiplicative-expression> "%" <multiplicative-expression>
<unary-expression> ::= <primary-expression>
                     | "-" <unary-expression>
<primary-expression> ::= <identifier>
                       | <integer-lit>
                       | <function-call-expression>
                       | "(" <expression> ")"
<function-call-expression> ::= <identifier> "(" [ <function-call-expression-args> ] ")"
<function-call-expression-args> ::= <expression>
                                  | <expression> "," <expression>
```
