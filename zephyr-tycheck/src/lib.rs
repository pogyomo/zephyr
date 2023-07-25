use std::collections::HashMap;

use thiserror::Error;
use zephyr_ast::{Program, Expression, Declarative, FunctionDecl, Statement, ElseStmt, LetStmtType};
use zephyr_span::{Span, Spannable};
use zephyr_types::Types;

#[derive(Debug, Error)]
pub enum TypeCheckError {
    #[error("no such variable exist: {name}")]
    NoSuchVariableExist { name: String, span: Span },
    #[error("no such function exist: {name}")]
    NoSuchFunctionExist { name: String, span: Span },
    #[error("type of lhs and rhs is different: lhs is {lhs_ty} but rhs is {rhs_ty}")]
    InfixOperationWithDifferentType { lhs_ty: Types, rhs_ty: Types, lhs: Span, rhs: Span },
    #[error("different type of function argument: expect {expect}, but got {got}")]
    DifferentTypeOfFunctionArgument { expect: Types, got: Types, span: Span },
    #[error("if condition has non boolean expression: got {got}")]
    IfConditionHasNonBooleanExpression { got: Types, span: Span },
    #[error("while condition has non boolean expression: got {got}")]
    WhileConditionHasNonBooleanExpression { got: Types, span: Span },
    #[error("variable must have either type or initial value")]
    VariableDeclarationWithoutTypesAndValue { span: Span },
    #[error("the function return type and return statement's type different: expect {expect}, but got {got}")]
    DifferentTypeOfReturnValue { expect: Types, got: Types, span: Span },
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ValTable<'a> {
    outer: Option<&'a ValTable<'a>>,
    tbl: HashMap<String, Types>,
} 

impl<'a> ValTable<'a> {
    fn new() -> Self {
        Self { outer: None, tbl: HashMap::new() }
    }

    fn from_outer(outer: &'a ValTable) -> Self {
        Self { outer: Some(outer), tbl: HashMap::new() }
    }

    fn add(&mut self, name: String, ty: Types) {
        self.tbl.insert(name, ty);
    }

    fn val_type(&self, name: &str) -> Option<&Types> {
        match self.tbl.get(name) {
            Some(ty) => Some(ty),
            None => match self.outer {
                Some(ref outer) => outer.val_type(name),
                None => None,
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FuncTable(HashMap<String, (Types, Vec<Types>)>);

impl From<&Program> for FuncTable {
    fn from(value: &Program) -> Self {
        let mut tbl = FuncTable(HashMap::new());
        for decl in value.0.iter() {
            match decl {
                Declarative::FunctionDecl(func) => {
                    let ret_type = func.ret_type
                        .as_ref()
                        .map(|v| v.r#type.clone())
                        .unwrap_or(Types::Void);
                    let arg_type = func.args
                        .iter()
                        .map(|v| v.r#type.clone())
                        .collect::<Vec<_>>();
                    tbl.0.insert(func.name.name.clone(), (ret_type, arg_type));
                }
                _ => (),
            }
        }
        tbl
    }
}

impl FuncTable {
    fn ret_type(&self, name: &str) -> Option<&Types> {
        self.0.get(name).map(|v| &v.0)
    }

    fn arg_type(&self, name: &str) -> Option<&Vec<Types>> {
        self.0.get(name).map(|v| &v.1)
    }
}

/// Check types of given program and add correct type to objects.
pub fn type_check(prog: Program) -> Result<Program, TypeCheckError> {
    let mut result = Vec::new();
    let func_tbl = FuncTable::from(&prog);
    for decl in prog.0.into_iter() {
        match decl {
            Declarative::FunctionDecl(func) => {
                let func = type_check_func_decl(func, &func_tbl)?;
                result.push(Declarative::FunctionDecl(func));
            }
            _ => result.push(decl),
        }
    }
    Ok(Program(result))
}

fn type_check_func_decl(mut decl: FunctionDecl, func_tbl: &FuncTable) -> Result<FunctionDecl, TypeCheckError> {
    // Bring arguments to the scope
    let mut val_tbl = ValTable::new();
    for arg in decl.args.iter() {
        val_tbl.add(arg.name.clone(), arg.r#type.clone());
    }

    let mut stmts = Vec::new();
    for stmt in decl.body.stmts.into_iter() {
        // If return statement exist, check the returned value's type is same as we expected.
        if let Statement::ReturnStmt(ref ret) = stmt {
            match (ret.expr.clone(), decl.ret_type.clone()) {
                (Some(expr), Some(fun_ret_ty)) => {
                    let ret_ty = typeof_expr(&expr, &val_tbl, func_tbl)?;
                    if !ret_ty.amb_eq(&fun_ret_ty.r#type) {
                        return Err(TypeCheckError::DifferentTypeOfReturnValue {
                            expect: fun_ret_ty.r#type.clone(),
                            got: ret_ty,
                            span: ret.span(),
                        })
                    }
                }
                (Some(expr), None) => {
                    let ret_ty = typeof_expr(&expr, &val_tbl, func_tbl)?;
                    return Err(TypeCheckError::DifferentTypeOfReturnValue { 
                        expect: Types::Void,
                        got: ret_ty,
                        span: ret.span()
                    })
                }
                (None, Some(fun_ret_ty)) => {
                    return Err(TypeCheckError::DifferentTypeOfReturnValue { 
                        expect: fun_ret_ty.r#type.clone(),
                        got: Types::Void,
                        span: ret.span()
                    })
                }
                (None, None) => (),
            }
        }

        let stmt = type_check_stmt(stmt, &mut val_tbl, func_tbl)?;
        stmts.push(stmt);
    }

    decl.body.stmts = stmts;
    Ok(decl)
}

fn type_check_stmt(stmt: Statement, val_tbl: &mut ValTable, func_tbl: &FuncTable) -> Result<Statement, TypeCheckError> {
    match stmt {
        Statement::BlockStmt(mut block) => {
            let mut val_tbl = ValTable::from_outer(val_tbl);

            let mut stmts = Vec::new();
            for stmt in block.stmts.into_iter() {
                stmts.push(type_check_stmt(stmt, &mut val_tbl, func_tbl)?);
            }

            block.stmts = stmts;
            Ok(block.into())
        }
        Statement::IfStmt(mut _if) => {
            let cond_type = typeof_expr(&_if.expr, val_tbl, func_tbl)?;
            if cond_type != Types::Bool {
                return Err(TypeCheckError::IfConditionHasNonBooleanExpression {
                    got: cond_type, span: _if.expr.span()
                })
            }

            let block = type_check_stmt((*_if.body).into(), val_tbl, func_tbl)?;
            match block {
                Statement::BlockStmt(block) => _if.body = Box::new(block),
                _ => unreachable!(),
            }

            if let Some(els) = _if.r#else {
                match *els {
                    ElseStmt::Else(blk) => {
                        let block = match type_check_stmt(blk.into(), val_tbl, func_tbl)? {
                            Statement::BlockStmt(block) => block,
                            _ => unreachable!(),
                        };
                        _if.r#else = Some(Box::new(ElseStmt::Else(block)));
                    }
                    ElseStmt::ElseIf(elif) => {
                        let elif = match type_check_stmt(elif.into(), val_tbl, func_tbl)? {
                            Statement::IfStmt(elif) => elif,
                            _ => unreachable!(),
                        };
                        _if.r#else = Some(Box::new(ElseStmt::ElseIf(elif)))
                    }
                }
            }

            Ok(_if.into())
        }
        Statement::LetStmt(mut _let) => {
            let lhs_type = _let.r#type.as_ref().map(|v| v.r#type.clone());
            let rhs_type = match _let.expr {
                Some(ref expr) => Some(typeof_expr(expr, val_tbl, func_tbl)?),
                None => None,
            };
            match (lhs_type, rhs_type) {
                (Some(lt), Some(rt)) => {
                    if lt.amb_eq(&rt) {
                        val_tbl.add(_let.name.name.clone(), lt);
                    }
                    Ok(_let.into())
                }
                (Some(ty), None) => {
                    val_tbl.add(_let.name.name.clone(), ty);
                    Ok(_let.into())
                }
                (None, Some(ty)) => {
                    val_tbl.add(_let.name.name.clone(), ty.clone());

                    // FIXME: We temporary assign u8 to the variable if rhs is come from integer
                    //        literal. Is there any other good types?
                    let ty = match ty {
                        Types::Integer => Types::U8,
                        _ => ty,
                    };

                    // FIXME: Span of this type is empty.
                    _let.r#type = Some(LetStmtType::new(Span::new(0, 0), ty));

                    Ok(_let.into())
                }
                _ => Err(TypeCheckError::VariableDeclarationWithoutTypesAndValue {
                    span: _let.span()
                })
            }
        }
        Statement::WhileStmt(mut _while) => {
            let cond_type = typeof_expr(&_while.expr, val_tbl, func_tbl)?;
            if cond_type != Types::Bool {
                return Err(TypeCheckError::IfConditionHasNonBooleanExpression {
                    got: cond_type, span: _while.expr.span()
                })
            }

            let block = type_check_stmt((*_while.body).into(), val_tbl, func_tbl)?;
            match block {
                Statement::BlockStmt(block) => _while.body = Box::new(block),
                _ => unreachable!(),
            }

            Ok(_while.into())
        }
        _ => Ok(stmt),
    }
}

#[allow(dead_code)]
fn typeof_expr(expr: &Expression, val_tbl: &ValTable, func_tbl: &FuncTable) -> Result<Types, TypeCheckError> {
    match expr {
        Expression::SurrExpr(surr) => typeof_expr(&surr.expr, val_tbl, func_tbl),
        Expression::BoolExpr(_) => Ok(Types::Bool),
        Expression::IntExpr(_) => Ok(Types::Integer),
        Expression::IdentExpr(ident) => {
            val_tbl.val_type(ident.name.as_str())
                .map(|v| v.clone())
                .ok_or(TypeCheckError::NoSuchVariableExist {
                    name: ident.name.clone(), span: ident.span(),
                })
        }
        Expression::UnaryExpr(unary) => typeof_expr(&unary.expr, val_tbl, func_tbl),
        Expression::InfixExpr(infix) => {
            let lhs = typeof_expr(&infix.lhs, val_tbl, func_tbl)?;
            let rhs = typeof_expr(&infix.rhs, val_tbl, func_tbl)?;
            if lhs.amb_eq(&rhs) {
                // If lhs and rhs return integer and either of these has concrete type, return it.
                match (lhs, rhs) {
                    (Types::Integer, Types::Integer) => Ok(Types::Integer),
                    (Types::Integer, rhs) => Ok(rhs),
                    (lhs, Types::Integer) => Ok(lhs),
                    (lhs, _) => Ok(lhs),
                }
            } else {
                Err(TypeCheckError::InfixOperationWithDifferentType { 
                    lhs_ty: lhs, rhs_ty: rhs,
                    lhs: infix.lhs.span(),
                    rhs: infix.rhs.span()
                })
            }
        }
        Expression::FuncCallExpr(call) => {
            let ret_type = func_tbl
                .ret_type(call.name.name.as_str())
                .ok_or(TypeCheckError::NoSuchFunctionExist {
                    name: call.name.name.clone(),
                    span: call.span(),
                })?;
            let arg_type = func_tbl
                .arg_type(call.name.name.as_str())
                .ok_or(TypeCheckError::NoSuchFunctionExist {
                    name: call.name.name.clone(),
                    span: call.span(),
                })?;
            for (expr, expect) in call.args.iter().zip(arg_type.iter()) {
                let got = typeof_expr(expr, val_tbl, func_tbl)?;
                if !expect.amb_eq(&got) {
                    return Err(TypeCheckError::DifferentTypeOfFunctionArgument {
                        expect: expect.clone(),
                        got,
                        span: expr.span()
                    });
                }
            }
            Ok(ret_type.clone())
        }
    }
}
