use zephyr_span::Span;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

impl Token {
    pub fn new(span: Span, kind: TokenKind) -> Token {
        Token { span, kind }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenKind {
    // Special
    Unexpected(char),

    // Literals
    Identifier(String),
    Integer(IntBase, String),

    // Operators
    /// "+"
    Plus,
    /// "-"
    Minus,
    /// "*"
    Star,
    /// "/"
    Slash,
    /// "%"
    Percent,
    /// "&"
    BitAnd,
    /// "|"
    BitOr,
    /// "^"
    BitXor,
    /// "<<"
    LShift,
    /// ">>"
    RShift,
    /// "&&"
    And,
    /// "||"
    Or,
    /// "!"
    Not,
    /// "<"
    LT,
    /// ">"
    GT,
    /// "<="
    LE,
    /// ">="
    GE,
    /// "=="
    EQ,
    /// "!="
    NE,
    /// "="
    Assign,
    /// "+="
    AddAssign,
    /// "-="
    SubAssign,
    /// "*="
    MulAssign,
    /// "/="
    DivAssign,
    /// "%="
    ModAssign,
    /// "&="
    AndAssign,
    /// "|="
    OrAssign,
    /// "^="
    XorAssign,
    /// "<<="
    LshAssign,
    /// ">>="
    RshAssign,

    // Symbols
    /// "("
    LParen,
    /// ")"
    RParen,
    /// "["
    LSquare,
    /// "]"
    RSquare,
    /// "{"
    LCurly,
    /// "}"
    RCurly,
    /// ":"
    Colon,
    /// ";"
    Semicolon,
    /// ","
    Comma,
    /// "."
    Period,

    // Keywords
    /// "function"
    Function,
    /// "return"
    Return,
    /// "let"
    Let,
    /// "struct"
    Struct,
    /// "union"
    Union,
    /// "u8"
    U8,
    /// "i8"
    I8,
    /// "u16"
    U16,
    /// "i16"
    I16,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntBase {
    Binary,
    Octadecimal,
    Decimal,
    Hexadecimal,
}

impl From<IntBase> for u32 {
    fn from(value: IntBase) -> Self {
        match value {
            IntBase::Binary => 2,
            IntBase::Octadecimal => 8,
            IntBase::Decimal => 10,
            IntBase::Hexadecimal => 16,
        }
    }
}
