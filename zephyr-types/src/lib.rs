use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Types {
    U8,
    I8,
    U16,
    I16,
    Bool,
    TypeName(String),
    Pointer(Box<Types>),

    /// This type is used at function return type if the function doesn't return anything
    Void,

    /// This type is used for integer literal: which doesn't have exact type at compile time
    Integer,
}

impl Types {
    pub fn amb_eq(&self, other: &Types) -> bool {
        use Types::*;

        match (self, other) {
            (Pointer(l), Pointer(r)) => l.amb_eq(&*r),
            (Integer, o) | (o, Integer) => {
                match o {
                    U8 => true,
                    I8 => true,
                    U16 => true,
                    I16 => true,
                    Integer => true,
                    _ => false,
                }
            }
            _ => self.eq(other),
        }
    }
}

impl Display for Types {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Types::U8 => write!(f, "u8"),
            Types::I8 => write!(f, "i8"),
            Types::U16 => write!(f, "u16"),
            Types::I16 => write!(f, "i16"),
            Types::Bool => write!(f, "bool"),
            Types::TypeName(name) => write!(f, "{name}"),
            Types::Pointer(to) => write!(f, "*{to}"),
            Types::Integer => write!(f, "integer"),
            Types::Void => write!(f, "void"),
        }
    }
}
