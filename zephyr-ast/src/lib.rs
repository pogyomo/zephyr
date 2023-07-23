mod decl;
mod expr;
mod stmt;

pub use decl::*;
pub use expr::*;
pub use stmt::*;

pub struct Program(pub Vec<Declarative>);

#[macro_export]
macro_rules! impl_from {
    ($target:ident, $($from:ident),*) => {$(
        impl From<$from> for $target {
            fn from(from: $from) -> $target {
                $target::$from(from)
            }
        }
    )*};
}
