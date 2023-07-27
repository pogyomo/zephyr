use derive_new::new;
use zephyr_span::{Spannable, Span};
use zephyr_types::Types;
use crate::{Expression, impl_from};

impl_from!(Statement, BlockStmt, LetStmt, WhileStmt, IfStmt, ExprStmt, ReturnStmt);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Statement {
    BlockStmt(BlockStmt),
    LetStmt(LetStmt),
    WhileStmt(WhileStmt),
    IfStmt(IfStmt),
    ExprStmt(ExprStmt),
    ReturnStmt(ReturnStmt),
}

impl Spannable for Statement {
    fn span(&self) -> zephyr_span::Span {
        match self {
            Statement::BlockStmt(block) => block.span(),
            Statement::LetStmt(lets) => lets.span(),
            Statement::WhileStmt(stmt) => stmt.span(),
            Statement::IfStmt(stmt) => stmt.span(),
            Statement::ExprStmt(expr) => expr.span(),
            Statement::ReturnStmt(ret) => ret.span(),
        }
    }
}

/// {
///     stmt1
///     stmt2
///     ...
///     stmtn
/// }
#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BlockStmt {
    span: Span,
    pub stmts: Vec<Statement>,
}

impl Spannable for BlockStmt {
    fn span(&self) -> Span {
        self.span
    }
}

/// let name [ : type ] [ = expr ];
#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LetStmt {
    span: Span,
    pub name: LetStmtName,
    pub r#type: LetStmtType,
    pub expr: Option<Expression>,
}

impl Spannable for LetStmt {
    fn span(&self) -> Span {
        self.span
    }
}

/// while expr body
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct WhileStmt {
    span: Span,
    pub expr: Expression,
    pub body: Box<BlockStmt>,
}

impl WhileStmt {
    pub fn new(span: Span, expr: Expression, body: BlockStmt) -> Self {
        Self { span, expr, body: Box::new(body) }
    }
}

impl Spannable for WhileStmt {
    fn span(&self) -> Span {
        self.span
    }
}

/// if expr body1 [ else ( body | if... )]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IfStmt {
    span: Span,
    pub expr: Expression,
    pub body: Box<BlockStmt>,
    pub r#else: Option<Box<ElseStmt>>,
}

impl IfStmt {
    pub fn new(span: Span, expr: Expression, body: BlockStmt, r#else: Option<ElseStmt>) -> Self {
        Self {
            span, expr,
            body: Box::new(body),
            r#else: r#else.map(|v| Box::new(v))
        }
    }
}

impl Spannable for IfStmt {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ElseStmt {
    Else(BlockStmt),
    ElseIf(IfStmt),
}

impl Spannable for ElseStmt {
    fn span(&self) -> Span {
        match self {
            ElseStmt::Else(blk) => blk.span(),
            ElseStmt::ElseIf(stmt) => stmt.span(),
        }
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
