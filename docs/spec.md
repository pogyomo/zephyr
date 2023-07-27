# The Zephyr Programming Language Specification

## Table Of Contents

- [General Element](#general-elements)
    - [Alphabet And Digit](#alphabet-and-digit)
- [Lexical Items](#lexical-items)
    - [Comment](#comment)
    - [Identifier](#identifier)
    - [Operators And Symbols](#operators-and-symbols)
    - [Keywords](#keywords)
    - [Integer Literal](#integer-literal)
    - [Types](#types)
- [Declarative Items](#declarative-items)
    - [Function Declaration](#function-declaration)
    - [Struct Declaration](#struct-declaration)
    - [Union Declaration](#union-declaration)
- [Statement](#statement)
    - [Let Statement](#let-statement)
    - [While Statement](#while-statement)
    - [If Statement](#if-statement)
    - [Expression Statement](#expression-statement)
    - [Return Statement](#return-statement)
- [Expression](#expression)

## General Elements

### Alphabet And Digit

```
<alphabet>          ::= "a" .. "z" | "A" .. "Z"
<binary-digit>      ::= "0" | "1"
<octadecimal-digit> ::= "0" .. "7"
<decimal-digit>     ::= "0" .. "9"
<hexadecimal-digit> ::= "0" .. "9" | "a" .. "f" | "A" .. "F"
```

## Lexical Items

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

### Operators and symbols

Below is operators and symbols which the lexer recognize.

```
+ - * / % & | ^ << >> && || ! < > <= >= == !=
= += -= *= /= %= &= |= ^= <<= >>=
```

```
( ) [ ] { } : ; , .
```

### Keywords

```
function return let struct union true false u8 i8 u16 i16 bool
```

### Integer Literal

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
          | "u16"
          | "i16"
          | "bool"
          | <typename>
          | <pointer>
<typename> ::= <identifier>
<pointer> ::= "*" <types>
```

## Declarative Items

```
<declarative> ::= <function-decl>
                | <struct-decl>
                | <union-decl>
```

### Function Declaration

```
<function-decl> ::= "function" <function-name> "(" [ <function-args> ] ")" [ ":" <types> ] <function-body>
<function-name> ::= <identifier>
<function-args> ::= <function-arg> "," <function-args>
                  | <function-arg>
<function-arg>  ::= <identifier> ":" <types>
<function-body> ::= <block-stmt>
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
              | <block-stmt>
              | <while-stmt>
              | <if-stmt>
              | <expression-stmt>
              | <return-stmt>
```

### Let Statement

```
<let-stmt> ::= "let" <identifier> [ ":" <types> ] [ "=" <expression> ] ";"
```

### Block Statement

```
<block-stmt> ::= "{" { <statement> } "}"
```

### While Statement

```
<while-stmt> ::= "while" <expression> <block-stmt>
```

### If Statement

```
<if-stmt> ::= "if" <expression> <block-stmt> [ "else" ( <block-stmt> | <if-stmt> ) ]
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
                          | <logical-and-expression> "||" <logical-or-expression>
<logical-and-expression> ::= <inclusive-or-expression>
                           | <inclusive-or-expression> "&&" <logical-and-expression>
<inclusive-or-expression> ::= <exclusive-or-expression>
                            | <exclusive-or-expression> "|" <inclusive-or-expression>
<exclusive-or-expression> ::= <and-expression>
                            | <and-expression> "^" <exclusive-or-expression>
<and-expression> ::= <equality-expression>
                   | <equality-expression> "&" <and-expression>
<equality-expression> ::= <relative-expression>
                        | <relative-expression> "==" <equality-expression>
                        | <relative-expression> "!=" <equality-expression>
<relative-expression> ::= <shift-expression>
                        | <shift-expression> "<" <relative-expression>
                        | <shift-expression> ">" <relative-expression>
                        | <shift-expression> "<=" <relative-expression>
                        | <shift-expression> ">=" <relative-expression>
<shift-expression> ::= <additive-expression>
                     | <additive-expression> "<<" <shift-expression>
                     | <additive-expression> ">>" <shift-expression>
<additive-expression> ::= <multiplicative-expression>
                        | <multiplicative-expression> "+" <additive-expression>
                        | <multiplicative-expression> "-" <additive-expression>
<multiplicative-expression> ::= <unary-expression>
                              | <unary-expression> "*" <multiplicative-expression>
                              | <unary-expression> "/" <multiplicative-expression>
                              | <unary-expression> "%" <multiplicative-expression>
<unary-expression> ::= <dotaccess-expression>
                     | "-" <unary-expression>
                     | "&" <unary-expression>
                     | "*" <unary-expression>
<dotaccess-expression> ::= <primary-expression>
                         | <primary-expression> "." <dotaccess-expression>
<primary-expression> ::= <identifier>
                       | <integer-lit>
                       | "true"
                       | "false"
                       | <function-call-expression>
                       | "(" <expression> ")"
<function-call-expression> ::= <identifier> "(" [ <function-call-expression-args> ] ")"
<function-call-expression-args> ::= <expression>
                                  | <expression> "," <expression>
```
