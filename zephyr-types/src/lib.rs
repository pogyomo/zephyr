#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Types {
    U8,
    I8,
    TypeName(String),
    Pointer(Box<Types>),
}
