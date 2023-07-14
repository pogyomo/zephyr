#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Types {
    U8,
    I8,
    U16,
    I16,
    TypeName(String),
    Pointer(Box<Types>),
}
