use derive_new::new;
use zephyr_span::{Span, Spannable};
use crate::impl_from;

impl_from!(Expression, IdentExpr, IntExpr, UnaryExpr, InfixExpr);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expression {
    IdentExpr(IdentExpr),
    IntExpr(IntExpr),
    FuncCallExpr(FuncCallExpr),
    UnaryExpr(UnaryExpr),
    InfixExpr(InfixExpr),
}

impl Spannable for Expression {
    fn span(&self) -> Span {
        match self {
            Expression::IdentExpr(ident) => ident.span(),
            Expression::IntExpr(int) => int.span(),
            Expression::FuncCallExpr(call) => call.span(),
            Expression::UnaryExpr(unary) => unary.span(),
            Expression::InfixExpr(infix) => infix.span(),
        }
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

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnaryExpr {
    pub expr: Box<Expression>,
    pub op: UnaryOp,
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
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InfixExpr {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub op: InfixOp,
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
}
