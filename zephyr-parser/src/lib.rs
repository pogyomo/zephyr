use std::num::ParseIntError;
use thiserror::Error;
use zephyr_ast::{
    Expression, Declarative, Statement, InfixExpr, InfixOp, InfixOpKind, UnaryExpr, UnaryOp, UnaryOpKind,
    IntExpr, IdentExpr, FuncCallExpr, FuncCallExprName, LetStmt, ReturnStmt, ExprStmt, LetStmtName, 
    FunctionDecl, FunctionDeclName, FunctionDeclArg, FunctionDeclBody
};
use zephyr_span::{Span, Spannable};
use zephyr_token::{Token, TokenKind};

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("expected {expect}, but no token available")]
    ExpectedToken { span: Span, expect: &'static str },
    #[error("failed to parse integer: {reason}")]
    FailedToParseInteger { span: Span, reason: ParseIntError },
    #[error("unexpected token found: expected {expect}")]
    UnexpectedToken { span: Span, expect: &'static str },
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

    pub fn parse(mut self) -> Result<Vec<Declarative>, ParseError> {
        match self.curr {
            Some(ref token) => {
                self.last_span = token.span; // Update last available token's span
            }
            None => return Ok(Vec::new()),
        }

        let mut decls = Vec::new();
        while self.curr.is_some() {
            println!("{:#?}", self.curr);
            decls.push(self.parse_decl()?);
        }
        Ok(decls)
    }
}

impl<I: Iterator<Item = Token>> Parser<I> {
    fn parse_decl(&mut self) -> Result<Declarative, ParseError> {
        let token = self.peek_or_err()?;
        match token.kind {
            TokenKind::Function => Ok(self.parse_func_decl()?.into()),
            _ => Err(ParseError::UnexpectedToken {
                span: token.span, expect: "function"
            })
        }
    }

    fn parse_func_decl(&mut self) -> Result<FunctionDecl, ParseError> {
        let mut span = self.consume_or_err()?.span;

        let token = self.consume_or_err()?;
        let name = match token.kind {
            TokenKind::Identifier(name) => {
                span += token.span;
                FunctionDeclName::new(token.span, name)
            }
            _ => return Err(ParseError::UnexpectedToken {
                span: token.span, expect: "identifier"
            })
        };

        let token = self.consume_or_err()?;
        match token.kind {
            TokenKind::LParen => span += token.span,
            _ => return Err(ParseError::UnexpectedToken {
                span: token.span, expect: "("
            })
        }

        let mut args = Vec::new();
        loop {
            let token = self.consume_or_err()?;
            match token.kind {
                TokenKind::Identifier(name) => {
                    span += token.span;
                    args.push(FunctionDeclArg::new(token.span, name));
                }
                TokenKind::RParen => {
                    span += token.span;
                    break;
                }
                _ => return Err(ParseError::UnexpectedToken {
                    span: token.span, expect: "identifier"
                })
            }

            let token = self.consume_or_err()?;
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

        let token = self.consume_or_err()?;
        let mut body_span = token.span;
        match token.kind {
            TokenKind::LCurly => {
                span += token.span;
            }
            _ => return Err(ParseError::UnexpectedToken {
                span: token.span, expect: "{"
            })
        }

        let mut body = Vec::new();
        loop {
            let token = self.peek_or_err()?;
            match token.kind {
                TokenKind::RCurly => {
                    body_span += token.span;
                    span += token.span;
                    self.consume();
                    break;
                }
                _ => (),
            }
            let stmt = self.parse_stmt()?;
            body_span += stmt.span();
            body.push(stmt);
        }

        Ok(FunctionDecl::new(span, name, args, FunctionDeclBody::new(body_span, body)))
    }
}

impl<I: Iterator<Item = Token>> Parser<I> {
    fn parse_stmt(&mut self) -> Result<Statement, ParseError> {
        let token = self.peek_or_err()?;
        match token.kind {
            TokenKind::Let    => Ok(self.parse_let_stmt()?.into()),
            TokenKind::Return => Ok(self.parse_return_stmt()?.into()),
            _ => Ok(self.parse_expr_stmt()?.into()),
        }
    }

    fn parse_let_stmt(&mut self) -> Result<LetStmt, ParseError> {
        let mut span = self.consume_or_err()?.span;

        let token = self.consume_or_err()?;
        let name = match token.kind {
            TokenKind::Identifier(name) => {
                span += token.span;
                LetStmtName::new(token.span, name)
            }
            _ => return Err(ParseError::UnexpectedToken {
                span: token.span, expect: "identifier"
            })
        };

        let token = self.consume_or_err()?;
        match token.kind {
            TokenKind::Assign => {
                span += token.span;
            }
            TokenKind::Semicolon => {
                span += token.span;
                return Ok(LetStmt::new(span, name, None));
            }
            _ => return Err(ParseError::UnexpectedToken {
                span: token.span, expect: "; or ="
            })
        }

        let expr = self.parse_expr()?;
        span += expr.span();

        let token = self.consume_or_err()?;
        match token.kind {
            TokenKind::Semicolon => {
                span += token.span;
                Ok(LetStmt::new(span, name, Some(expr)))
            }
            _ => Err(ParseError::UnexpectedToken {
                span: token.span, expect: ";"
            })
        }
    }

    fn parse_return_stmt(&mut self) -> Result<ReturnStmt, ParseError> {
        let mut span = self.consume_or_err()?.span;

        let token = self.peek_or_err()?;
        match token.kind {
            TokenKind::Semicolon => {
                span += token.span;
                return Ok(ReturnStmt::new(span, None))
            }
            _ => (),
        }

        let expr = self.parse_expr()?;
        span += expr.span();

        let token = self.consume_or_err()?;
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
                _ => self.parse_primary_expr(),
            }
        } else {
            self.parse_primary_expr()
        }
    }

    fn parse_primary_expr(&mut self) -> Result<Expression, ParseError> {
        let Some(token) = self.consume() else {
            return Err(ParseError::ExpectedToken {
                span: self.last_span, expect: "identifier or integer"
            });
        };
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
            _ => Err(ParseError::UnexpectedToken {
                span: token.span, expect: "identifier or integer"
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
            let token = self.peek_or_err()?;
            match token.kind {
                TokenKind::RParen => break token.span,
                _ => (),
            }

            args.push(self.parse_expr()?);

            let token = self.consume_or_err()?;
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

    fn consume_or_err(&mut self) -> Result<Token, ParseError> {
        let span = self.last_span;
        self.consume().ok_or(ParseError::ExpectedToken {
            span, expect: "any token"
        })
    }

    fn peek(&mut self) -> Option<&Token> {
        self.curr.as_ref()
    }

    fn peek_or_err(&mut self) -> Result<&Token, ParseError> {
        let span = self.last_span;
        self.peek().ok_or(ParseError::ExpectedToken {
            span, expect: "any token"
        })
    }
}
