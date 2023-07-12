use std::{str::CharIndices, iter::Peekable};
use zephyr_span::Span;
use zephyr_token::{Token, TokenKind, IntBase};

/// A struct which generate tokens from string.
#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    chars: Peekable<CharIndices<'a>>,
    len: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer { chars: input.char_indices().peekable(), len: input.len() }
    }
}

impl<'a> Lexer<'a> {
    fn consume_whitespace(&mut self) {
        while let Some((_, ch)) = self.chars.peek() {
            if ch.is_whitespace() {
                self.chars.next();
            } else {
                break;
            }
        }
    }

    fn consume(&mut self, ch: char) -> bool {
        if let Some((_, c)) = self.chars.peek() {
            if *c == ch {
                self.chars.next();
                return true;
            }
        }
        false
    }

    fn consume_integer(&mut self, radix: u32, head: Option<char>) -> String {
        let mut integer = String::new();
        if let Some(head) = head {
            integer.push(head);
        }
        while let Some((_, ch)) = self.chars.peek() {
            if ch.is_digit(radix) || *ch == '_' {
                let ch = *ch;
                self.chars.next();
                integer.push(ch);
            } else {
                break;
            }
        }
        integer
    }

    fn consume_identifier(&mut self, head: Option<char>) -> String {
        let mut ident = String::new();
        if let Some(head) = head {
            ident.push(head);
        }
        while let Some((_, ch)) = self.chars.peek() {
            if ch.is_ascii_alphanumeric() || *ch == '_' {
                let ch = *ch;
                self.chars.next();
                ident.push(ch);
            } else {
                break;
            }
        }
        ident
    }

    fn offset(&mut self) -> usize {
        self.chars.peek().map(|v| v.0).unwrap_or(self.len)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.consume_whitespace();

        let start = self.offset();
        match self.chars.next().map(|v| v.1)? {
            '+' => {
                if self.consume('=') {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::AddAssign))
                } else {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::Plus))
                }
            }
            '-' => {
                if self.consume('=') {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::SubAssign))
                } else {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::Minus))
                }
            }
            '*' => {
                if self.consume('=') {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::MulAssign))
                } else {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::Star))
                }
            }
            '/' => {
                if self.consume('=') {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::DivAssign))
                } else {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::Slash))
                }
            }
            '%' => {
                if self.consume('=') {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::ModAssign))
                } else {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::Percent))
                }
            }
            '&' => {
                if self.consume('&') {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::And))
                } else if self.consume('=') {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::AndAssign))
                } else {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::BitAnd))
                }
            }
            '|' => {
                if self.consume('|') {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::Or))
                } else if self.consume('=') {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::OrAssign))
                } else {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::BitOr))
                }
            }
            '^' => {
                if self.consume('=') {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::XorAssign))
                } else {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::BitXor))
                }
            }
            '<' => {
                if self.consume('<') {
                    if self.consume('=') {
                        Some(Token::new(Span::new(start, self.offset() - start), TokenKind::LshAssign))
                    } else {
                        Some(Token::new(Span::new(start, self.offset() - start), TokenKind::LShift))
                    }
                } else if self.consume('=') {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::LE))
                } else {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::LT))
                }
            }
            '>' => {
                if self.consume('>') {
                    if self.consume('=') {
                        Some(Token::new(Span::new(start, self.offset() - start), TokenKind::RshAssign))
                    } else {
                        Some(Token::new(Span::new(start, self.offset() - start), TokenKind::RShift))
                    }
                } else if self.consume('=') {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::GE))
                } else {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::GT))
                }
            }
            '!' => {
                if self.consume('='){
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::NE))
                } else {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::Not))
                }
            }
            '=' => {
                if self.consume('=') {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::EQ))
                } else {
                    Some(Token::new(Span::new(start, self.offset() - start), TokenKind::Assign))
                }
            }
            '(' => Some(Token::new(Span::new(start, self.offset() - start), TokenKind::LParen)),
            ')' => Some(Token::new(Span::new(start, self.offset() - start), TokenKind::RParen)),
            '{' => Some(Token::new(Span::new(start, self.offset() - start), TokenKind::LCurly)),
            '}' => Some(Token::new(Span::new(start, self.offset() - start), TokenKind::RCurly)),
            '[' => Some(Token::new(Span::new(start, self.offset() - start), TokenKind::LSquare)),
            ']' => Some(Token::new(Span::new(start, self.offset() - start), TokenKind::RSquare)),
            ':' => Some(Token::new(Span::new(start, self.offset() - start), TokenKind::Colon)),
            ';' => Some(Token::new(Span::new(start, self.offset() - start), TokenKind::Semicolon)),
            '.' => Some(Token::new(Span::new(start, self.offset() - start), TokenKind::Period)),
            ',' => Some(Token::new(Span::new(start, self.offset() - start), TokenKind::Comma)),
            '0' => {
                let radix = if self.consume('x') || self.consume('X') {
                    IntBase::Hexadecimal
                } else if self.consume('b') || self.consume('B') {
                    IntBase::Binary
                } else if self.consume('o') || self.consume('O') {
                    IntBase::Octadecimal
                } else {
                    return Some(Token::new(
                        Span::new(start, self.offset() - start),
                        TokenKind::Integer(IntBase::Decimal, String::from("0"))
                    ));
                };
                let value = self.consume_integer(radix.into(), None);
                Some(Token::new(
                    Span::new(start, self.offset() - start),
                    TokenKind::Integer(radix, value)
                ))
            }
            ch if ch.is_digit(10) => {
                let value = self.consume_integer(IntBase::Decimal.into(), Some(ch));
                Some(Token::new(
                    Span::new(start, self.offset() - start),
                    TokenKind::Integer(IntBase::Decimal, value)
                ))
            }
            ch if ch.is_ascii_alphabetic() || ch == '_' => {
                let ident = self.consume_identifier(Some(ch));
                let kind = match ident.as_str() {
                    "function" => TokenKind::Function,
                    "return" => TokenKind::Return,
                    "let" => TokenKind::Let,
                    "u8" => TokenKind::U8,
                    "i8" => TokenKind::I8,
                    _ => TokenKind::Identifier(ident),
                };
                Some(Token::new(Span::new(start, self.offset() - start), kind))
            }
            ch => Some(Token::new(Span::new(start, self.offset() - start), TokenKind::Unexpected(ch))),
        }
    }
}

#[cfg(test)]
mod test {
    use zephyr_span::Span;
    use zephyr_token::{TokenKind, Token, IntBase};
    use crate::Lexer;

    #[test]
    fn identifier() {
        let input = "a ident12 _ident12";
        let tokens = Lexer::new(input).collect::<Vec<_>>();
        assert_eq!(tokens, vec![
            Token::new(Span::new(0, 1),  TokenKind::Identifier("a".to_string())),
            Token::new(Span::new(2, 7),  TokenKind::Identifier("ident12".to_string())),
            Token::new(Span::new(10, 8), TokenKind::Identifier("_ident12".to_string())),
        ]);
    }

    #[test]
    fn integer() {
        let input = "0b0010_1101 30 0o76_13 0x0a23";
        let tokens = Lexer::new(input).collect::<Vec<_>>();
        assert_eq!(tokens, vec![
            Token::new(Span::new(0, 11), TokenKind::Integer(IntBase::Binary, String::from("0010_1101"))),
            Token::new(Span::new(12, 2), TokenKind::Integer(IntBase::Decimal, String::from("30"))),
            Token::new(Span::new(15, 7), TokenKind::Integer(IntBase::Octadecimal, String::from("76_13"))),
            Token::new(Span::new(23, 6), TokenKind::Integer(IntBase::Hexadecimal, String::from("0a23"))),
        ]);
    }

    #[test]
    fn operator() {
        let input = "+ - * / % & | ^ << >> && || ! < > <= >= == != = += -= *= /= %= &= |= ^= <<= >>=";
        let tokens = Lexer::new(input).collect::<Vec<_>>();
        assert_eq!(tokens, vec![
            Token::new(Span::new(0, 1),  TokenKind::Plus),
            Token::new(Span::new(2, 1),  TokenKind::Minus),
            Token::new(Span::new(4, 1),  TokenKind::Star),
            Token::new(Span::new(6, 1),  TokenKind::Slash),
            Token::new(Span::new(8, 1),  TokenKind::Percent),
            Token::new(Span::new(10, 1), TokenKind::BitAnd),
            Token::new(Span::new(12, 1), TokenKind::BitOr),
            Token::new(Span::new(14, 1), TokenKind::BitXor),
            Token::new(Span::new(16, 2), TokenKind::LShift),
            Token::new(Span::new(19, 2), TokenKind::RShift),
            Token::new(Span::new(22, 2), TokenKind::And),
            Token::new(Span::new(25, 2), TokenKind::Or),
            Token::new(Span::new(28, 1), TokenKind::Not),
            Token::new(Span::new(30, 1), TokenKind::LT),
            Token::new(Span::new(32, 1), TokenKind::GT),
            Token::new(Span::new(34, 2), TokenKind::LE),
            Token::new(Span::new(37, 2), TokenKind::GE),
            Token::new(Span::new(40, 2), TokenKind::EQ),
            Token::new(Span::new(43, 2), TokenKind::NE),
            Token::new(Span::new(46, 1), TokenKind::Assign),
            Token::new(Span::new(48, 2), TokenKind::AddAssign),
            Token::new(Span::new(51, 2), TokenKind::SubAssign),
            Token::new(Span::new(54, 2), TokenKind::MulAssign),
            Token::new(Span::new(57, 2), TokenKind::DivAssign),
            Token::new(Span::new(60, 2), TokenKind::ModAssign),
            Token::new(Span::new(63, 2), TokenKind::AndAssign),
            Token::new(Span::new(66, 2), TokenKind::OrAssign),
            Token::new(Span::new(69, 2), TokenKind::XorAssign),
            Token::new(Span::new(72, 3), TokenKind::LshAssign),
            Token::new(Span::new(76, 3), TokenKind::RshAssign),
        ]);
    }

    #[test]
    fn symbol() {
        let input = "( ) [ ] { } : ; , .";
        let tokens = Lexer::new(input).collect::<Vec<_>>();
        assert_eq!(tokens, vec![
            Token::new(Span::new(0, 1),  TokenKind::LParen),
            Token::new(Span::new(2, 1),  TokenKind::RParen),
            Token::new(Span::new(4, 1),  TokenKind::LSquare),
            Token::new(Span::new(6, 1),  TokenKind::RSquare),
            Token::new(Span::new(8, 1),  TokenKind::LCurly),
            Token::new(Span::new(10, 1), TokenKind::RCurly),
            Token::new(Span::new(12, 1), TokenKind::Colon),
            Token::new(Span::new(14, 1), TokenKind::Semicolon),
            Token::new(Span::new(16, 1), TokenKind::Comma),
            Token::new(Span::new(18, 1), TokenKind::Period),
        ]);
    }

    #[test]
    fn keyword() {
        let input = "function return let u8 i8";
        let tokens = Lexer::new(input).collect::<Vec<_>>();
        assert_eq!(tokens, vec![
            Token::new(Span::new(0, 8),  TokenKind::Function),
            Token::new(Span::new(9, 6),  TokenKind::Return),
            Token::new(Span::new(16, 3), TokenKind::Let),
            Token::new(Span::new(20, 2), TokenKind::U8),
            Token::new(Span::new(23, 2), TokenKind::I8),
        ])
    }
}
