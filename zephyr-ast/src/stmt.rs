use zephyr_span::{Spannable, Span};

use crate::Expression;

macro_rules! impl_from {
    ($target:ident, $($from:ident),*) => {$(
        impl From<$from> for $target {
            fn from(from: $from) -> $target {
                $target::$from(from)
            }
        }
    )*};
}

impl_from!(Statement, ExprStmt, ReturnStmt);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Statement {
    LetStmt(LetStmt),
    ExprStmt(ExprStmt),
    ReturnStmt(ReturnStmt),
}

impl Spannable for Statement {
    fn span(&self) -> zephyr_span::Span {
        match self {
            Statement::LetStmt(lets) => lets.span(),
            Statement::ExprStmt(expr) => expr.span(),
            Statement::ReturnStmt(ret) => ret.span(),
        }
    }
}

/// let name [ = expr ];
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LetStmt {
    span: Span,
    pub name: LetStmtName,
    pub expr: Option<Expression>,
}

impl Spannable for LetStmt {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LetStmtName {
    span: Span,
    pub name: String,
}

impl Spannable for LetStmtName {
    fn span(&self) -> Span {
        self.span
    }
}

/// expr;
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExprStmt {
    span: Span,
    pub expr: Expression,
}

impl Spannable for ExprStmt {
    fn span(&self) -> Span {
        self.span
    }
}

/// return expr;
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ReturnStmt {
    span: Span,
    pub expr: Expression,
}

impl Spannable for ReturnStmt {
    fn span(&self) -> Span {
        self.span
    }
}
