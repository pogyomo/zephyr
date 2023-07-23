use derive_new::new;
use zephyr_span::{Span, Spannable};
use crate::impl_from;

impl_from!(Expression, SurrExpr, IdentExpr, IntExpr, UnaryExpr, InfixExpr, FuncCallExpr);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expression {
    SurrExpr(SurrExpr),
    IdentExpr(IdentExpr),
    IntExpr(IntExpr),
    FuncCallExpr(FuncCallExpr),
    UnaryExpr(UnaryExpr),
    InfixExpr(InfixExpr),
}

impl Spannable for Expression {
    fn span(&self) -> Span {
        match self {
            Expression::SurrExpr(surr) => surr.span(),
            Expression::IdentExpr(ident) => ident.span(),
            Expression::IntExpr(int) => int.span(),
            Expression::FuncCallExpr(call) => call.span(),
            Expression::UnaryExpr(unary) => unary.span(),
            Expression::InfixExpr(infix) => infix.span(),
        }
    }
}

/// A expression which be surrounded by `(` and `)`
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SurrExpr {
    span: Span,
    pub expr: Box<Expression>,
}

impl SurrExpr {
    pub fn new(span: Span, expr: Expression) -> Self {
        Self { span, expr: Box::new(expr) }
    }
}

impl Spannable for SurrExpr {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IdentExpr {
    span: Span,
    pub name: String,
}

impl Spannable for IdentExpr {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct IntExpr {
    span: Span,
    pub value: u64,
}

impl Spannable for IntExpr {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FuncCallExpr {
    span: Span,
    pub name: FuncCallExprName,
    pub args: Vec<Expression>,
}

impl Spannable for FuncCallExpr {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FuncCallExprName {
    span: Span,
    pub name: String,
}

impl Spannable for FuncCallExprName {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnaryExpr {
    pub expr: Box<Expression>,
    pub op: UnaryOp,
}

impl UnaryExpr {
    pub fn new(expr: Expression, op: UnaryOp) -> Self {
        Self { expr: Box::new(expr), op }
    }
}

impl Spannable for UnaryExpr {
    fn span(&self) -> Span {
        self.expr.span() + self.op.span()
    }
}

#[derive(new)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnaryOp {
    span: Span,
    pub kind: UnaryOpKind,
}

impl Spannable for UnaryOp {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnaryOpKind {
    /// "-"
    Minus,
    /// "&"
    Ref,
    /// "*"
    Deref,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InfixExpr {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub op: InfixOp,
}

impl InfixExpr {
    pub fn new(lhs: Expression, rhs: Expression, op: InfixOp) -> Self {
        Self { lhs: Box::new(lhs), rhs: Box::new(rhs), op }
    }
}

impl Spannable for InfixExpr {
    fn span(&self) -> Span {
        self.lhs.span() + self.rhs.span()
    }
}

#[derive(new)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct InfixOp {
    span: Span,
    pub kind: InfixOpKind,
}

impl Spannable for InfixOp {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum InfixOpKind {
    /// "+"
    Add,
    /// "-"
    Sub,
    /// "*"
    Mul,
    /// "/"
    Div,
    /// "%"
    Mod,
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
    /// "."
    Dot,
}
