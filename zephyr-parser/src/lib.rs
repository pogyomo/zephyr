use std::num::ParseIntError;
use thiserror::Error;
use zephyr_ast::{
    Expression, Declarative, Statement, InfixExpr, InfixOp, InfixOpKind, UnaryExpr, UnaryOp, UnaryOpKind,
    IntExpr, IdentExpr, FuncCallExpr, FuncCallExprName, LetStmt, ReturnStmt, ExprStmt, LetStmtName, 
    FunctionDecl, FunctionDeclName, FunctionDeclArg, SurrExpr, LetStmtType, StructDecl, 
    UnionDecl, StructDeclField, StructDeclName, UnionDeclField, UnionDeclName, FunctionDeclRetType,
    BoolExpr, BlockStmt, WhileStmt, IfStmt, ElseStmt, Program
};
use zephyr_span::{Span, Spannable};
use zephyr_token::{Token, TokenKind};
use zephyr_types::Types;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("expected {expect}, but no token available")]
    ExpectedToken { span: Span, expect: &'static str },
    #[error("failed to parse integer: {reason}")]
    FailedToParseInteger { span: Span, reason: ParseIntError },
    #[error("unexpected token found: expected {expect}")]
    UnexpectedToken { span: Span, expect: &'static str },
}

impl Spannable for ParseError {
    fn span(&self) -> Span {
        match *self {
            ParseError::ExpectedToken { span, .. } => span,
            ParseError::FailedToParseInteger { span, .. } => span,
            ParseError::UnexpectedToken { span, .. } => span,
        }
    }
}

pub struct Parser<I> {
    tokens: I,
    curr: Option<Token>,
    last_span: Span,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn new(mut tokens: I) -> Self {
        let curr = tokens.next();
        Self { tokens, curr, last_span: Span::new(0, 0) }
    }

    pub fn parse(mut self) -> Result<Program, ParseError> {
        match self.curr {
            Some(ref token) => {
                self.last_span = token.span; // Update last available token's span
            }
            None => return Ok(Program(Vec::new())),
        }

        let mut decls = Vec::new();
        while self.curr.is_some() {
            decls.push(self.parse_decl()?);
        }
        Ok(Program(decls))
    }
}

impl<I: Iterator<Item = Token>> Parser<I> {
    fn parse_type(&mut self) -> Result<(Types, Span), ParseError> {
        let token = self.consume_or_err("*, u8 or i8")?;
        match token.kind {
            TokenKind::Star => {
                let (types, span) = self.parse_type()?;
                Ok((
                    Types::Pointer(Box::new(types)),
                    span,
                ))
            }
            TokenKind::U8 => {
                Ok((Types::U8, token.span))
            }
            TokenKind::I8 => {
                Ok((Types::I8, token.span))
            }
            TokenKind::U16 => {
                Ok((Types::U16, token.span))
            }
            TokenKind::I16 => {
                Ok((Types::I16, token.span))
            }
            TokenKind::Bool => {
                Ok((Types::Bool, token.span))
            }
            TokenKind::Identifier(name) => {
                Ok((Types::TypeName(name), token.span))
            }
            _ => Err(ParseError::UnexpectedToken {
                span: token.span, expect: "*, u8 or i8"
            })
        }
    }
}

impl<I: Iterator<Item = Token>> Parser<I> {
    fn parse_decl(&mut self) -> Result<Declarative, ParseError> {
        let token = self.peek_or_err("function")?;
        match token.kind {
            TokenKind::Function => Ok(self.parse_func_decl()?.into()),
            TokenKind::Struct => Ok(self.parse_struct_decl()?.into()),
            TokenKind::Union => Ok(self.parse_union_decl()?.into()),
            _ => Err(ParseError::UnexpectedToken {
                span: token.span, expect: "function"
            })
        }
    }

    fn parse_func_decl(&mut self) -> Result<FunctionDecl, ParseError> {
        let mut span = self.consume_or_err("function")?.span;

        let token = self.consume_or_err("identifier")?;
        let name = match token.kind {
            TokenKind::Identifier(name) => {
                span += token.span;
                FunctionDeclName::new(token.span, name)
            }
            _ => return Err(ParseError::UnexpectedToken {
                span: token.span, expect: "identifier"
            })
        };

        let token = self.consume_or_err("(")?;
        match token.kind {
            TokenKind::LParen => span += token.span,
            _ => return Err(ParseError::UnexpectedToken {
                span: token.span, expect: "("
            })
        }

        let mut args = Vec::new();
        loop {
            let token = self.consume_or_err("identifier or )")?;
            match token.kind {
                TokenKind::Identifier(name) => {
                    span += token.span;

                    let token = self.consume_or_err(":")?;
                    match token.kind {
                        TokenKind::Colon => span += token.span,
                        _ => return Err(ParseError::UnexpectedToken {
                            span: token.span, expect: ":"
                        })
                    }

                    let (types, type_span) = self.parse_type()?;
                    args.push(FunctionDeclArg::new(type_span, name, types));
                    span += type_span;
                }
                TokenKind::RParen => {
                    span += token.span;
                    break;
                }
                _ => return Err(ParseError::UnexpectedToken {
                    span: token.span, expect: "identifier"
                })
            }

            let token = self.consume_or_err(", or )")?;
            match token.kind {
                TokenKind::Comma => span += token.span,
                TokenKind::RParen => {
                    span += token.span;
                    break;
                }
                _ => return Err(ParseError::UnexpectedToken {
                    span: token.span, expect: ", or )"
                })
            }
        }

        let token = self.peek_or_err(":")?;
        let ret_type = match token.kind {
            TokenKind::Colon => {
                self.consume();
                let (types, type_span) = self.parse_type()?;
                Some(FunctionDeclRetType::new(type_span, types))
            }
            _ => None,
        };
        
        let body = self.parse_block_stmt()?;
        span += body.span();

        Ok(FunctionDecl::new(span, name, args, ret_type, body))
    }

    fn parse_struct_decl(&mut self) -> Result<StructDecl, ParseError> {
        let mut span = self.consume_or_err("struct")?.span;

        let token = self.consume_or_err("identifier")?;
        let name = match token.kind {
            TokenKind::Identifier(name) => {
                span += token.span;
                StructDeclName::new(token.span, name)
            }
            _ => return Err(ParseError::UnexpectedToken {
                span: token.span, expect: "{"
            })
        };

        let token = self.consume_or_err("{")?;
        match token.kind {
            TokenKind::LCurly => span += token.span,
            _ => return Err(ParseError::UnexpectedToken {
                span: token.span, expect: "{"
            })
        }

        let mut fields = Vec::new();
        loop {
            let token = self.consume_or_err("identifier or }")?;
            let mut field_span = token.span;
            match token.kind {
                TokenKind::RCurly => {
                    span += token.span;
                    break;
                }
                TokenKind::Identifier(name) => {
                    let token = self.consume_or_err(":")?;
                    match token.kind {
                        TokenKind::Colon => field_span += token.span,
                        _ => return Err(ParseError::UnexpectedToken {
                            span: token.span, expect: ":"
                        })
                    }

                    let (types, type_span) = self.parse_type()?;
                    field_span += type_span;
                    fields.push(StructDeclField::new(field_span, name, types))
                }
                _ => return Err(ParseError::UnexpectedToken {
                    span: token.span, expect: "} or identifier"
                })
            }

            let token = self.consume_or_err(", or }")?;
            match token.kind {
                TokenKind::RCurly => {
                    span += token.span;
                    break;
                }
                TokenKind::Comma => (),
                _ => return Err(ParseError::UnexpectedToken {
                    span: token.span, expect: "} or ,"
                })
            }
        }

        Ok(StructDecl::new(span, name, fields))
    }

    fn parse_union_decl(&mut self) -> Result<UnionDecl, ParseError> {
        let mut span = self.consume_or_err("union")?.span;

        let token = self.consume_or_err("identifier")?;
        let name = match token.kind {
            TokenKind::Identifier(name) => {
                span += token.span;
                UnionDeclName::new(token.span, name)
            }
            _ => return Err(ParseError::UnexpectedToken {
                span: token.span, expect: "{"
            })
        };

        let token = self.consume_or_err("{")?;
        match token.kind {
            TokenKind::LCurly => span += token.span,
            _ => return Err(ParseError::UnexpectedToken {
                span: token.span, expect: "{"
            })
        }

        let mut fields = Vec::new();
        loop {
            let token = self.consume_or_err("identifier or }")?;
            let mut field_span = token.span;
            match token.kind {
                TokenKind::RCurly => {
                    span += token.span;
                    break;
                }
                TokenKind::Identifier(name) => {
                    let token = self.consume_or_err(":")?;
                    match token.kind {
                        TokenKind::Colon => field_span += token.span,
                        _ => return Err(ParseError::UnexpectedToken {
                            span: token.span, expect: ":"
                        })
                    }

                    let (types, type_span) = self.parse_type()?;
                    field_span += type_span;
                    fields.push(UnionDeclField::new(field_span, name, types))
                }
                _ => return Err(ParseError::UnexpectedToken {
                    span: token.span, expect: "} or identifier"
                })
            }

            let token = self.consume_or_err(", or }")?;
            match token.kind {
                TokenKind::RCurly => {
                    span += token.span;
                    break;
                }
                TokenKind::Comma => (),
                _ => return Err(ParseError::UnexpectedToken {
                    span: token.span, expect: "} or ,"
                })
            }
        }

        Ok(UnionDecl::new(span, name, fields))
    }
}

impl<I: Iterator<Item = Token>> Parser<I> {
    fn parse_stmt(&mut self) -> Result<Statement, ParseError> {
        let token = self.peek_or_err("let or return")?;
        match token.kind {
            TokenKind::LCurly => Ok(self.parse_block_stmt()?.into()),
            TokenKind::Let    => Ok(self.parse_let_stmt()?.into()),
            TokenKind::While  => Ok(self.parse_while_stmt()?.into()),
            TokenKind::If     => Ok(self.parse_if_stmt()?.into()),
            TokenKind::Return => Ok(self.parse_return_stmt()?.into()),
            _ => Ok(self.parse_expr_stmt()?.into()),
        }
    }

    fn parse_block_stmt(&mut self) -> Result<BlockStmt, ParseError> {
        let mut span = self.consume_or_err("{")?.span;

        let mut stmts = Vec::new();
        loop {
            let token = self.peek_or_err("any token")?;
            match token.kind {
                TokenKind::RCurly => {
                    span += token.span;
                    self.consume();
                    break;
                }
                _ => stmts.push(self.parse_stmt()?),
            }
        }
        Ok(BlockStmt::new(span, stmts))
    }

    fn parse_let_stmt(&mut self) -> Result<LetStmt, ParseError> {
        let mut span = self.consume_or_err("let")?.span;

        let token = self.consume_or_err("identifier")?;
        let name = match token.kind {
            TokenKind::Identifier(name) => {
                span += token.span;
                LetStmtName::new(token.span, name)
            }
            _ => return Err(ParseError::UnexpectedToken {
                span: token.span, expect: "identifier"
            })
        };

        let token = self.peek_or_err(":")?;
        let types = match token.kind {
            TokenKind::Colon => {
                span += token.span;
                self.consume();

                let (types, type_span) = self.parse_type()?;
                let types = LetStmtType::new(type_span, types);
                span += type_span;
                Some(types)
            }
            _ => None,
        };


        let token = self.consume_or_err("; or =")?;
        match token.kind {
            TokenKind::Assign => {
                span += token.span;
            }
            TokenKind::Semicolon => {
                span += token.span;
                return Ok(LetStmt::new(span, name, types, None));
            }
            _ => return Err(ParseError::UnexpectedToken {
                span: token.span, expect: "; or ="
            })
        }

        let expr = self.parse_expr()?;
        span += expr.span();

        let token = self.consume_or_err(";")?;
        match token.kind {
            TokenKind::Semicolon => {
                span += token.span;
                Ok(LetStmt::new(span, name, types, Some(expr)))
            }
            _ => Err(ParseError::UnexpectedToken {
                span: token.span, expect: ";"
            })
        }
    }

    fn parse_while_stmt(&mut self) -> Result<WhileStmt, ParseError> {
        let mut span = self.consume_or_err("while")?.span;

        let expr = self.parse_expr()?;

        let body = self.parse_block_stmt()?;
        span += body.span();

        Ok(WhileStmt::new(span, expr, body))
    }

    fn parse_if_stmt(&mut self) -> Result<IfStmt, ParseError> {
        let mut span = self.consume_or_err("if")?.span;

        let expr = self.parse_expr()?;

        let body = self.parse_block_stmt()?;
        span += body.span();

        if let Some(token) = self.peek() {
            match token.kind {
                TokenKind::Else => self.consume(),
                _ => return Ok(IfStmt::new(span, expr, body, None)),
            };
        } else {
            return Ok(IfStmt::new(span, expr, body, None))
        }

        let token = self.peek_or_err("if or anything")?;
        match token.kind {
            TokenKind::If => {
                let elseif = ElseStmt::ElseIf(self.parse_if_stmt()?);
                span += elseif.span();
                Ok(IfStmt::new(span, expr, body, Some(elseif)))
            }
            _ => {
                let elseif = ElseStmt::Else(self.parse_block_stmt()?);
                span += elseif.span();
                Ok(IfStmt::new(span, expr, body, Some(elseif)))
            }
        }
    }

    fn parse_return_stmt(&mut self) -> Result<ReturnStmt, ParseError> {
        let mut span = self.consume_or_err("return")?.span;

        let token = self.peek_or_err(";")?;
        match token.kind {
            TokenKind::Semicolon => {
                span += token.span;
                self.consume();
                return Ok(ReturnStmt::new(span, None))
            }
            _ => (),
        }

        let expr = self.parse_expr()?;
        span += expr.span();

        let token = self.consume_or_err(";")?;
        match token.kind {
            TokenKind::Semicolon => {
                span += token.span;
                Ok(ReturnStmt::new(span, Some(expr)))
            }
            _ => Err(ParseError::UnexpectedToken {
                span: token.span, expect: ";"
            })
        }
    }

    fn parse_expr_stmt(&mut self) -> Result<ExprStmt, ParseError> {
        let expr = self.parse_expr()?;
        let Some(token) = self.consume() else {
            return Err(ParseError::ExpectedToken {
                span: self.last_span, expect: ";"
            });
        };
        match token.kind {
            TokenKind::Semicolon => Ok(ExprStmt::new(expr.span() + token.span, expr)),
            _ => Err(ParseError::UnexpectedToken {
                span: token.span, expect: ";"
            })
        }
    }
}

impl<I: Iterator<Item = Token>> Parser<I> {
    fn parse_expr(&mut self) -> Result<Expression, ParseError> {
        self.parse_logical_or_expr()
    }

    fn parse_logical_or_expr(&mut self) -> Result<Expression, ParseError> {
        let mut lhs = self.parse_logical_and_expr()?;
        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::Or => {
                    let span = token.span;
                    self.consume();
                    let rhs = self.parse_logical_and_expr()?;
                    lhs = InfixExpr::new(lhs, rhs, InfixOp::new(span, InfixOpKind::Or)).into();
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_logical_and_expr(&mut self) -> Result<Expression, ParseError> {
        let mut lhs = self.parse_inclusive_or_expr()?;
        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::And => {
                    let span = token.span;
                    self.consume();
                    let rhs = self.parse_inclusive_or_expr()?;
                    lhs = InfixExpr::new(lhs, rhs, InfixOp::new(span, InfixOpKind::And)).into();
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_inclusive_or_expr(&mut self) -> Result<Expression, ParseError> {
        let mut lhs = self.parse_exclusive_or_expr()?;
        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::BitOr => {
                    let span = token.span;
                    self.consume();
                    let rhs = self.parse_exclusive_or_expr()?;
                    lhs = InfixExpr::new(lhs, rhs, InfixOp::new(span, InfixOpKind::BitOr)).into();
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_exclusive_or_expr(&mut self) -> Result<Expression, ParseError> {
        let mut lhs = self.parse_and_expr()?;
        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::BitXor => {
                    let span = token.span;
                    self.consume();
                    let rhs = self.parse_and_expr()?;
                    lhs = InfixExpr::new(lhs, rhs, InfixOp::new(span, InfixOpKind::BitXor)).into();
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_and_expr(&mut self) -> Result<Expression, ParseError> {
        let mut lhs = self.parse_equality_expr()?;
        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::BitAnd => {
                    let span = token.span;
                    self.consume();
                    let rhs = self.parse_equality_expr()?;
                    lhs = InfixExpr::new(lhs, rhs, InfixOp::new(span, InfixOpKind::BitAnd)).into();
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_equality_expr(&mut self) -> Result<Expression, ParseError> {
        let mut lhs = self.parse_relative_expr()?;
        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::EQ => {
                    let span = token.span;
                    self.consume();
                    let rhs = self.parse_relative_expr()?;
                    lhs = InfixExpr::new(lhs, rhs, InfixOp::new(span, InfixOpKind::EQ)).into();
                }
                TokenKind::NE => {
                    let span = token.span;
                    self.consume();
                    let rhs = self.parse_relative_expr()?;
                    lhs = InfixExpr::new(lhs, rhs, InfixOp::new(span, InfixOpKind::NE)).into();
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_relative_expr(&mut self) -> Result<Expression, ParseError> {
        let mut lhs = self.parse_shift_expr()?;
        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::LT => {
                    let span = token.span;
                    self.consume();
                    let rhs = self.parse_shift_expr()?;
                    lhs = InfixExpr::new(lhs, rhs, InfixOp::new(span, InfixOpKind::LT)).into();
                }
                TokenKind::GT => {
                    let span = token.span;
                    self.consume();
                    let rhs = self.parse_shift_expr()?;
                    lhs = InfixExpr::new(lhs, rhs, InfixOp::new(span, InfixOpKind::GT)).into();
                }
                TokenKind::LE => {
                    let span = token.span;
                    self.consume();
                    let rhs = self.parse_shift_expr()?;
                    lhs = InfixExpr::new(lhs, rhs, InfixOp::new(span, InfixOpKind::LE)).into();
                }
                TokenKind::GE => {
                    let span = token.span;
                    self.consume();
                    let rhs = self.parse_shift_expr()?;
                    lhs = InfixExpr::new(lhs, rhs, InfixOp::new(span, InfixOpKind::GE)).into();
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_shift_expr(&mut self) -> Result<Expression, ParseError> {
        let mut lhs = self.parse_additive_expr()?;
        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::LShift => {
                    let span = token.span;
                    self.consume();
                    let rhs = self.parse_additive_expr()?;
                    lhs = InfixExpr::new(lhs, rhs, InfixOp::new(span, InfixOpKind::LShift)).into();
                }
                TokenKind::RShift => {
                    let span = token.span;
                    self.consume();
                    let rhs = self.parse_additive_expr()?;
                    lhs = InfixExpr::new(lhs, rhs, InfixOp::new(span, InfixOpKind::RShift)).into();
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_additive_expr(&mut self) -> Result<Expression, ParseError> {
        let mut lhs = self.parse_multiplicative_expr()?;
        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::Plus => {
                    let span = token.span;
                    self.consume();
                    let rhs = self.parse_multiplicative_expr()?;
                    lhs = InfixExpr::new(lhs, rhs, InfixOp::new(span, InfixOpKind::Add)).into();
                }
                TokenKind::Minus => {
                    let span = token.span;
                    self.consume();
                    let rhs = self.parse_multiplicative_expr()?;
                    lhs = InfixExpr::new(lhs, rhs, InfixOp::new(span, InfixOpKind::Sub)).into();
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_multiplicative_expr(&mut self) -> Result<Expression, ParseError> {
        let mut lhs = self.parse_unary_expr()?;
        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::Star => {
                    let span = token.span;
                    self.consume();
                    let rhs = self.parse_unary_expr()?;
                    lhs = InfixExpr::new(lhs, rhs, InfixOp::new(span, InfixOpKind::Mul)).into();
                }
                TokenKind::Slash => {
                    let span = token.span;
                    self.consume();
                    let rhs = self.parse_unary_expr()?;
                    lhs = InfixExpr::new(lhs, rhs, InfixOp::new(span, InfixOpKind::Div)).into();
                }
                TokenKind::Percent => {
                    let span = token.span;
                    self.consume();
                    let rhs = self.parse_unary_expr()?;
                    lhs = InfixExpr::new(lhs, rhs, InfixOp::new(span, InfixOpKind::Mod)).into();
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_unary_expr(&mut self) -> Result<Expression, ParseError> {
        if let Some(token) = self.peek() {
            match token.kind {
                TokenKind::Minus => {
                    let span = token.span;
                    self.consume();
                    let expr = self.parse_unary_expr()?;
                    Ok(UnaryExpr::new(expr, UnaryOp::new(span, UnaryOpKind::Minus)).into())
                }
                TokenKind::BitAnd => {
                    let span = token.span;
                    self.consume();
                    let expr = self.parse_unary_expr()?;
                    Ok(UnaryExpr::new(expr, UnaryOp::new(span, UnaryOpKind::Ref)).into())
                }
                TokenKind::Star => {
                    let span = token.span;
                    self.consume();
                    let expr = self.parse_unary_expr()?;
                    Ok(UnaryExpr::new(expr, UnaryOp::new(span, UnaryOpKind::Deref)).into())
                }
                _ => self.parse_dotaccess_expr(),
            }
        } else {
            self.parse_dotaccess_expr()
        }
    }

    fn parse_dotaccess_expr(&mut self) -> Result<Expression, ParseError> {
        let mut lhs = self.parse_primary_expr()?;
        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::Period => {
                    let span = token.span;
                    self.consume();
                    let rhs = self.parse_primary_expr()?;
                    lhs = InfixExpr::new(lhs, rhs, InfixOp::new(span, InfixOpKind::Dot)).into();
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_primary_expr(&mut self) -> Result<Expression, ParseError> {
        let token = self.consume_or_err("true, false, (, identifier or integer")?;
        match token.kind {
            TokenKind::Identifier(name) => self.parse_func_call(name, token.span),
            TokenKind::Integer(base, body) => {
                let body = body.chars().filter(|c| *c != '_').collect::<String>();
                match u64::from_str_radix(body.as_str(), base.into()) {
                    Ok(value) => Ok(IntExpr::new(token.span, value).into()),
                    Err(e) => Err(ParseError::FailedToParseInteger {
                        span: token.span, reason: e
                    })
                }
            }
            TokenKind::True => Ok(BoolExpr::new(token.span, true).into()),
            TokenKind::False => Ok(BoolExpr::new(token.span, false).into()),
            TokenKind::LParen => {
                let mut span = token.span;
                let expr = self.parse_expr()?;
                let token = self.consume_or_err(")")?;
                match token.kind {
                    TokenKind::RParen => {
                        span += token.span;
                        Ok(SurrExpr::new(span, expr).into())
                    }
                    _ => Err(ParseError::UnexpectedToken {
                        span: token.span, expect: ")"
                    })
                }
            }
            _ => Err(ParseError::UnexpectedToken {
                span: token.span, expect: "true, false, (, identifier or integer"
            })
        }
    }

    fn parse_func_call(&mut self, name: String, start_span: Span) -> Result<Expression, ParseError> {
        if let Some(token) = self.peek() {
            match token.kind {
                TokenKind::LParen => self.consume(),
                _ => return Ok(IdentExpr::new(start_span, name).into()),
            };
        }
        let mut args = Vec::new();
        let end_span = loop {
            let token = self.peek_or_err(")")?;
            match token.kind {
                TokenKind::RParen => {
                    let span = token.span;
                    self.consume();
                    break span;
                }
                _ => (),
            }

            args.push(self.parse_expr()?);

            let token = self.consume_or_err(", or )")?;
            match token.kind {
                TokenKind::Comma => (),
                TokenKind::RParen => break token.span,
                _ => return Err(ParseError::UnexpectedToken {
                    span: token.span, expect: ", or )"
                })
            };
        };
        let name = FuncCallExprName::new(start_span, name);
        Ok(FuncCallExpr::new(start_span + end_span, name, args).into())
    }
}

impl<I: Iterator<Item = Token>> Parser<I> {
    fn consume(&mut self) -> Option<Token> {
        let ret = std::mem::replace(&mut self.curr, self.tokens.next());
        match self.curr {
            Some(ref token) => self.last_span = token.span,
            None => (),
        }
        ret
    }

    fn consume_or_err(&mut self, expect: &'static str) -> Result<Token, ParseError> {
        let span = self.last_span;
        self.consume().ok_or(ParseError::ExpectedToken { span, expect })
    }

    fn peek(&mut self) -> Option<&Token> {
        self.curr.as_ref()
    }

    fn peek_or_err(&mut self, expect: &'static str) -> Result<&Token, ParseError> {
        let span = self.last_span;
        self.peek().ok_or(ParseError::ExpectedToken { span, expect })
    }
}

#[cfg(test)]
mod test {
    use zephyr_ast::{
        FunctionDecl, FunctionDeclName, LetStmt, LetStmtName, IntExpr, ReturnStmt,
        InfixExpr, IdentExpr, InfixOp, InfixOpKind, SurrExpr, LetStmtType, BlockStmt
    };
    use zephyr_span::Span;
    use zephyr_token::{Token, TokenKind, IntBase};
    use zephyr_types::Types;
    use crate::Parser;

    #[test]
    fn function() {
        // function main() { let a: u8 = 10 * (2 << 3); return a % 10; }
        let input = vec![
            Token::new(Span::new(0, 8),  TokenKind::Function),
            Token::new(Span::new(9, 4),  TokenKind::Identifier("main".to_string())),
            Token::new(Span::new(13, 1), TokenKind::LParen),
            Token::new(Span::new(14, 1), TokenKind::RParen),
            Token::new(Span::new(16, 1), TokenKind::LCurly),
            Token::new(Span::new(18, 3), TokenKind::Let),
            Token::new(Span::new(22, 1), TokenKind::Identifier("a".to_string())),
            Token::new(Span::new(23, 1), TokenKind::Colon),
            Token::new(Span::new(25, 2), TokenKind::U8),
            Token::new(Span::new(28, 1), TokenKind::Assign),
            Token::new(Span::new(30, 2), TokenKind::Integer(IntBase::Decimal, "10".to_string())),
            Token::new(Span::new(33, 1), TokenKind::Star),
            Token::new(Span::new(35, 1), TokenKind::LParen),
            Token::new(Span::new(36, 1), TokenKind::Integer(IntBase::Decimal, "2".to_string())),
            Token::new(Span::new(38, 2), TokenKind::LShift),
            Token::new(Span::new(41, 1), TokenKind::Integer(IntBase::Decimal, "3".to_string())),
            Token::new(Span::new(42, 1), TokenKind::RParen),
            Token::new(Span::new(43, 1), TokenKind::Semicolon),
            Token::new(Span::new(45, 6), TokenKind::Return),
            Token::new(Span::new(52, 1), TokenKind::Identifier("a".to_string())),
            Token::new(Span::new(54, 1), TokenKind::Percent),
            Token::new(Span::new(56, 2), TokenKind::Integer(IntBase::Decimal, "10".to_string())),
            Token::new(Span::new(58, 1), TokenKind::Semicolon),
            Token::new(Span::new(60, 1), TokenKind::RCurly),
        ];

        let decl = Parser::new(input.into_iter()).parse_decl().unwrap();
        assert_eq!(decl, FunctionDecl::new(
            Span::new(0, 8) + Span::new(60, 1),
            FunctionDeclName::new(Span::new(9, 4), "main".to_string()),
            Vec::new(),
            None,
            BlockStmt::new(
                Span::new(16, 1) + Span::new(60, 1),
                vec![
                    LetStmt::new(
                        Span::new(18, 3) + Span::new(43, 1),
                        LetStmtName::new(Span::new(22, 1), "a".to_string()),
                        Some(LetStmtType::new(Span::new(25, 2), Types::U8)),
                        Some(InfixExpr::new(
                            IntExpr::new(Span::new(30, 2), 10).into(),
                            SurrExpr::new(
                                Span::new(35, 1) + Span::new(42, 1),
                                InfixExpr::new(
                                    IntExpr::new(Span::new(36, 1), 2).into(),
                                    IntExpr::new(Span::new(41, 1), 3).into(),
                                    InfixOp::new(Span::new(38, 2), InfixOpKind::LShift)
                                ).into(),
                            ).into(),
                            InfixOp::new(Span::new(33, 1), InfixOpKind::Mul)
                        ).into())
                    ).into(),
                    ReturnStmt::new(
                        Span::new(45, 6) + Span::new(58, 1),
                        Some(InfixExpr::new(
                            IdentExpr::new(Span::new(52, 1), "a".to_string()).into(),
                            IntExpr::new(Span::new(56, 2), 10).into(),
                            InfixOp::new(Span::new(54, 1), InfixOpKind::Mod)
                        ).into())
                    ).into()
                ]
            )
        ).into())
    }

    #[test]
    fn expression() {
        // 10 * (20 + 10 << 2)
        let input = vec![
            Token::new(Span::new(0, 2),  TokenKind::Integer(IntBase::Decimal, "10".to_string())),
            Token::new(Span::new(3, 1),  TokenKind::Star),
            Token::new(Span::new(5, 1),  TokenKind::LParen),
            Token::new(Span::new(6, 2),  TokenKind::Integer(IntBase::Decimal, "20".to_string())),
            Token::new(Span::new(9, 1),  TokenKind::Plus),
            Token::new(Span::new(11, 2), TokenKind::Integer(IntBase::Decimal, "10".to_string())),
            Token::new(Span::new(14, 2), TokenKind::LShift),
            Token::new(Span::new(17, 1), TokenKind::Integer(IntBase::Decimal, "2".to_string())),
            Token::new(Span::new(18, 1), TokenKind::RParen),
        ];
        let expr = Parser::new(input.into_iter()).parse_expr().unwrap();
        assert_eq!(expr, InfixExpr::new(
            IntExpr::new(Span::new(0, 2), 10).into(),
            SurrExpr::new(
                Span::new(5, 1) + Span::new(18, 1),
                InfixExpr::new(
                    InfixExpr::new(
                        IntExpr::new(Span::new(6, 2),  20).into(),
                        IntExpr::new(Span::new(11, 2), 10).into(),
                        InfixOp::new(Span::new(9, 1), InfixOpKind::Add)
                    ).into(),
                    IntExpr::new(Span::new(17, 1), 2).into(),
                    InfixOp::new(Span::new(14, 2), InfixOpKind::LShift)
                ).into(),
            ).into(),
            InfixOp::new(Span::new(3, 1), InfixOpKind::Mul)
        ).into())
    }
}
