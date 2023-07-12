use derive_new::new;
use zephyr_span::{Span, Spannable};
use zephyr_types::Types;
use crate::Statement;
use crate::impl_from;

impl_from!(Declarative, FunctionDecl);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Declarative {
    FunctionDecl(FunctionDecl),
}

/// function name(args) body
#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionDecl {
    span: Span,
    pub name: FunctionDeclName,
    pub args: Vec<FunctionDeclArg>,
    pub body: FunctionDeclBody,
}

impl Spannable for FunctionDecl {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionDeclName {
    span: Span,
    pub name: String,
}

impl Spannable for FunctionDeclName {
    fn span(&self) -> Span {
        self.span
    }
}

/// name: type
#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionDeclArg {
    span: Span,
    pub name: String,
    pub r#type: Types,
}

impl Spannable for FunctionDeclArg {
    fn span(&self) -> Span {
        self.span
    }
}

/// {
///     stmt1;
///     stmt2;
///     ...
///     stmtn;
/// }
#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionDeclBody {
    span: Span, // This span include '{' and '}'
    body: Vec<Statement>,
}

impl Spannable for FunctionDeclBody {
    fn span(&self) -> Span {
        self.span
    }
}
