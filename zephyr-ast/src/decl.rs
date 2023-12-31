use derive_new::new;
use zephyr_span::{Span, Spannable};
use zephyr_types::Types;
use crate::BlockStmt;
use crate::impl_from;

impl_from!(Declarative, FunctionDecl, StructDecl, UnionDecl);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Declarative {
    FunctionDecl(FunctionDecl),
    StructDecl(StructDecl),
    UnionDecl(UnionDecl),
}

impl Spannable for Declarative {
    fn span(&self) -> Span {
        match self {
            Declarative::FunctionDecl(func) => func.span(),
            Declarative::StructDecl(strct) => strct.span(),
            Declarative::UnionDecl(uni) => uni.span(),
        }
    }
}

/// function name(args) body
#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionDecl {
    span: Span,
    pub name: FunctionDeclName,
    pub args: Vec<FunctionDeclArg>,
    pub ret_type: Option<FunctionDeclRetType>,
    pub body: BlockStmt,
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

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionDeclRetType {
    span: Span,
    pub r#type: Types,
}

impl Spannable for FunctionDeclRetType {
    fn span(&self) -> Span {
        self.span
    }
}

/// struct name {
///     field1,
///     field2,
///     ...
///     fieldn,
/// }
#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct StructDecl {
    span: Span,
    pub name: StructDeclName,
    pub fields: Vec<StructDeclField>,
}

impl Spannable for StructDecl {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct StructDeclName {
    span: Span,
    pub name: String,
}

impl Spannable for StructDeclName {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct StructDeclField {
    span: Span,
    pub name: String,
    pub r#type: Types,
}

impl Spannable for StructDeclField {
    fn span(&self) -> Span {
        self.span
    }
}

/// union name {
///     field1,
///     field2,
///     ...
///     fieldn,
/// }
#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnionDecl {
    span: Span,
    pub name: UnionDeclName,
    pub fields: Vec<UnionDeclField>,
}

impl Spannable for UnionDecl {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnionDeclName {
    span: Span,
    pub name: String,
}

impl Spannable for UnionDeclName {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(new)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnionDeclField {
    span: Span,
    pub name: String,
    pub r#type: Types,
}

impl Spannable for UnionDeclField {
    fn span(&self) -> Span {
        self.span
    }
}
