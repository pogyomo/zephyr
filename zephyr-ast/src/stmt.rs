use derive_new::new;
use zephyr_span::{Spannable, Span};
use zephyr_types::Types;
use crate::{Expression, impl_from};

impl_from!(Statement, LetStmt, ExprStmt, ReturnStmt);

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

/// let name [ : type ] [ = expr ];
#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LetStmt {
    span: Span,
    pub name: LetStmtName,
    pub r#type: Option<LetStmtType>,
    pub expr: Option<Expression>,
}

impl Spannable for LetStmt {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
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

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LetStmtType {
    span: Span,
    pub r#type: Types,
}

impl Spannable for LetStmtType {
    fn span(&self) -> Span {
        self.span
    }
}

/// expr;
#[derive(new)]
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

/// return [ expr ];
#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ReturnStmt {
    span: Span,
    pub expr: Option<Expression>,
}

impl Spannable for ReturnStmt {
    fn span(&self) -> Span {
        self.span
    }
}
