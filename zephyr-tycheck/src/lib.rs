use std::collections::HashMap;
use thiserror::Error;
use zephyr_ast::{
    Program, Expression, Declarative, FunctionDecl, Statement, ElseStmt, LetStmtType, InfixOpKind, UnaryOpKind
};
use zephyr_span::{Span, Spannable};
use zephyr_types::Types;

#[derive(Debug, Error)]
pub enum TypeCheckError {
    #[error("no such function exist: {name}")]
    NoSuchFunctionExist { name: String, span: Span },
    #[error("invalid application of infix operator")]
    InvalidApplicationOfInfixOperator { 
        expect: Vec<Types>, got_lhs: Types, got_rhs: Types, lhs: Span, rhs: Span
    },
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
    #[error("the type {got} doesn't have field")]
    TheTypeDoesntHaveField { got: Types, span: Span },
    #[error("struct or union's field can only be accessed via identifier")]
    InvalidAccessToField { span: Span },
    #[error("no such struct or union exist: {name}")]
    NoSuchStructOrUnionExist { name: String, span: Span },
    #[error("no such field {field} exist on {name}")]
    NoSuchFieldExist { name: String, field: String, span: Span },
    #[error("can't take nagetion for {got}: expect i8 or i16")]
    CantTakeNegationForTheType { got: Types, span: Span },
    #[error("can't deref non-pointer type: got {got}")]
    CantDerefNonPointerType { got: Types, span: Span },
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

#[derive(Debug, Clone, PartialEq, Eq)]
struct StructTable(HashMap<String, HashMap<String, Types>>);

impl From<&Program> for StructTable {
    fn from(value: &Program) -> Self {
        let mut tbl = StructTable(HashMap::new());
        for decl in value.0.iter() {
            match decl {
                Declarative::StructDecl(decl) => {
                    tbl.0.insert(decl.name.name.clone(), HashMap::new());
                    for field in decl.fields.iter() {
                        tbl.0.get_mut(decl.name.name.as_str())
                            .unwrap()
                            .insert(field.name.clone(), field.r#type.clone());
                    }
                }
                _ => (),
            }
        }
        tbl
    }
}

impl StructTable {
    fn is_exist(&self, name: &str) -> bool {
        self.0.get(name).is_some()
    }

    fn field_type(&self, struct_name: &str, field_name: &str) -> Option<&Types> {
        self.0.get(struct_name)?.get(field_name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct UnionTable(HashMap<String, HashMap<String, Types>>);

impl From<&Program> for UnionTable {
    fn from(value: &Program) -> Self {
        let mut tbl = UnionTable(HashMap::new());
        for decl in value.0.iter() {
            match decl {
                Declarative::UnionDecl(decl) => {
                    tbl.0.insert(decl.name.name.clone(), HashMap::new());
                    for field in decl.fields.iter() {
                        tbl.0.get_mut(decl.name.name.as_str())
                            .unwrap()
                            .insert(field.name.clone(), field.r#type.clone());
                    }
                }
                _ => (),
            }
        }
        tbl
    }
}

impl UnionTable {
    fn is_exist(&self, name: &str) -> bool {
        self.0.get(name).is_some()
    }

    fn field_type(&self, union_name: &str, field_name: &str) -> Option<&Types> {
        self.0.get(union_name)?.get(field_name)
    }
}

/// Check types of given program and add correct type to objects.
pub fn type_check(prog: Program) -> Result<Program, TypeCheckError> {
    let mut result = Vec::new();
    let func_tbl = FuncTable::from(&prog);
    let struct_tbl = StructTable::from(&prog);
    let union_tbl = UnionTable::from(&prog);
    for decl in prog.0.into_iter() {
        match decl {
            Declarative::FunctionDecl(func) => {
                let func = type_check_func_decl(func, &func_tbl, &struct_tbl, &union_tbl)?;
                result.push(Declarative::FunctionDecl(func));
            }
            _ => result.push(decl),
        }
    }
    Ok(Program(result))
}

fn type_check_func_decl(
    mut decl: FunctionDecl, func_tbl: &FuncTable, struct_tbl: &StructTable, union_tbl: &UnionTable
) -> Result<FunctionDecl, TypeCheckError> {
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
                    let ret_ty = typeof_expr(&expr, &val_tbl, func_tbl, struct_tbl, union_tbl)?;
                    if !ret_ty.amb_eq(&fun_ret_ty.r#type) {
                        return Err(TypeCheckError::DifferentTypeOfReturnValue {
                            expect: fun_ret_ty.r#type.clone(),
                            got: ret_ty,
                            span: ret.span(),
                        })
                    }
                }
                (Some(expr), None) => {
                    let ret_ty = typeof_expr(&expr, &val_tbl, func_tbl, struct_tbl, union_tbl)?;
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

        let stmt = type_check_stmt(stmt, &mut val_tbl, func_tbl, struct_tbl, union_tbl)?;
        stmts.push(stmt);
    }

    decl.body.stmts = stmts;
    Ok(decl)
}

fn type_check_stmt(
    stmt: Statement, val_tbl: &mut ValTable,
    func_tbl: &FuncTable, struct_tbl: &StructTable, union_tbl: &UnionTable
) -> Result<Statement, TypeCheckError> {
    match stmt {
        Statement::BlockStmt(mut block) => {
            let mut val_tbl = ValTable::from_outer(val_tbl);

            let mut stmts = Vec::new();
            for stmt in block.stmts.into_iter() {
                stmts.push(type_check_stmt(stmt, &mut val_tbl, func_tbl, struct_tbl, union_tbl)?);
            }

            block.stmts = stmts;
            Ok(block.into())
        }
        Statement::IfStmt(mut _if) => {
            let cond_type = typeof_expr(&_if.expr, val_tbl, func_tbl, struct_tbl, union_tbl)?;
            if cond_type != Types::Bool {
                return Err(TypeCheckError::IfConditionHasNonBooleanExpression {
                    got: cond_type, span: _if.expr.span()
                })
            }

            let block = type_check_stmt((*_if.body).into(), val_tbl, func_tbl, struct_tbl, union_tbl)?;
            match block {
                Statement::BlockStmt(block) => _if.body = Box::new(block),
                _ => unreachable!(),
            }

            if let Some(els) = _if.r#else {
                match *els {
                    ElseStmt::Else(blk) => {
                        let block = match type_check_stmt(blk.into(), val_tbl, func_tbl, struct_tbl, union_tbl)? {
                            Statement::BlockStmt(block) => block,
                            _ => unreachable!(),
                        };
                        _if.r#else = Some(Box::new(ElseStmt::Else(block)));
                    }
                    ElseStmt::ElseIf(elif) => {
                        let elif = match type_check_stmt(elif.into(), val_tbl, func_tbl, struct_tbl, union_tbl)? {
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
                Some(ref expr) => Some(typeof_expr(expr, val_tbl, func_tbl, struct_tbl, union_tbl)?),
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
            let cond_type = typeof_expr(&_while.expr, val_tbl, func_tbl, struct_tbl, union_tbl)?;
            if cond_type != Types::Bool {
                return Err(TypeCheckError::IfConditionHasNonBooleanExpression {
                    got: cond_type, span: _while.expr.span()
                })
            }

            let block = type_check_stmt((*_while.body).into(), val_tbl, func_tbl, struct_tbl, union_tbl)?;
            match block {
                Statement::BlockStmt(block) => _while.body = Box::new(block),
                _ => unreachable!(),
            }

            Ok(_while.into())
        }
        Statement::ExprStmt(ref expr) => {
            typeof_expr(&expr.expr, val_tbl, func_tbl, struct_tbl, union_tbl)?;
            Ok(stmt)
        }
        Statement::ReturnStmt(ref ret) => {
            match ret.expr {
                Some(ref expr) => {
                    typeof_expr(expr, val_tbl, func_tbl, struct_tbl, union_tbl)?;
                }
                _ => (),
            }
            Ok(stmt)
        }
    }
}

#[allow(dead_code)]
fn typeof_expr(
    expr: &Expression, val_tbl: &ValTable,
    func_tbl: &FuncTable, struct_tbl: &StructTable, union_tbl: &UnionTable
) -> Result<Types, TypeCheckError> {
    match expr {
        Expression::SurrExpr(surr) => typeof_expr(&surr.expr, val_tbl, func_tbl, struct_tbl, union_tbl),
        Expression::BoolExpr(_) => Ok(Types::Bool),
        Expression::IntExpr(_) => Ok(Types::Integer),
        Expression::IdentExpr(ident) => {
            match val_tbl.val_type(ident.name.as_str()) {
                Some(val) => Ok(val.clone()),
                None => Ok(Types::TypeName(ident.name.clone()))
            }
        }
        Expression::UnaryExpr(unary) => {
            let ty = typeof_expr(&unary.expr, val_tbl, func_tbl, struct_tbl, union_tbl)?;
            match unary.op.kind {
                UnaryOpKind::Minus => match &ty {
                    Types::I8 | Types::I16 => Ok(ty),
                    _ => Err(TypeCheckError::CantTakeNegationForTheType {
                        got: ty, span: unary.expr.span()
                    })
                }
                UnaryOpKind::Deref => match &ty {
                    Types::Pointer(to) => Ok(*to.clone()),
                    _ => Err(TypeCheckError::CantDerefNonPointerType {
                        got: ty, span: unary.expr.span()
                    })
                }
                UnaryOpKind::Ref => Ok(Types::Pointer(Box::new(ty))),
            }
        }
        Expression::InfixExpr(infix) => {
            use InfixOpKind::*;
            use Types::*;

            // Get type of lhs and rhs
            let lhs = typeof_expr(&infix.lhs, val_tbl, func_tbl, struct_tbl, union_tbl)?;
            let rhs = typeof_expr(&infix.rhs, val_tbl, func_tbl, struct_tbl, union_tbl)?;

            // Check the type of lhs and rhs is same and application of the operator is valid
            match infix.op.kind {
                Add | Sub | Mul | Div | Mod | BitAnd | BitOr | BitXor | LShift | RShift => match (&lhs, &rhs) {
                    (U8, Integer) | (I8, Integer) | (U16, Integer) | (I16, Integer) => Ok(lhs),
                    (Integer, U8) | (Integer, I8) | (Integer, U16) | (Integer, I16) => Ok(rhs),
                    (U8, U8) | (I8, I8) | (U16, U16) | (I16, I16) | (Integer, Integer) => Ok(lhs),
                    _ => Err(TypeCheckError::InvalidApplicationOfInfixOperator {
                        expect: vec![U8, I8, U16, I16],
                        got_lhs: lhs, got_rhs: rhs,
                        lhs: infix.lhs.span(), rhs: infix.rhs.span(),
                    })
                }
                And | Or => match (&lhs, &rhs) {
                    (Bool, Bool) => Ok(lhs),
                    _ => Err(TypeCheckError::InvalidApplicationOfInfixOperator {
                        expect: vec![Bool],
                        got_lhs: lhs, got_rhs: rhs,
                        lhs: infix.lhs.span(), rhs: infix.rhs.span(),
                    })
                }
                LT | GT | LE | GE | EQ | NE => match (&lhs, &rhs) {
                    (U8, Integer) | (I8, Integer) | (U16, Integer) | (I16, Integer) => Ok(Bool),
                    (Integer, U8) | (Integer, I8) | (Integer, U16) | (Integer, I16) => Ok(Bool),
                    (U8, U8) | (I8, I8) | (U16, U16) | (I16, I16) | (Integer, Integer) => Ok(Bool),
                    _ => Err(TypeCheckError::InvalidApplicationOfInfixOperator {
                        expect: vec![U8, I8, U16, I16],
                        got_lhs: lhs, got_rhs: rhs,
                        lhs: infix.lhs.span(), rhs: infix.rhs.span(),
                    })
                }
                Dot => match (&lhs, &rhs) {
                    (TypeName(name), _) => {
                        let field = match *infix.rhs {
                            Expression::IdentExpr(ref field) => field.name.as_str(),
                            _ => return Err(TypeCheckError::InvalidAccessToField { 
                                span: infix.rhs.span() 
                            })
                        };

                        if let Some(struct_ty) = struct_tbl.field_type(name, field) {
                            return Ok(struct_ty.clone());
                        }
                        if let Some(union_ty) = union_tbl.field_type(name, field) {
                            return Ok(union_ty.clone());
                        }

                        if struct_tbl.is_exist(name) || union_tbl.is_exist(name) {
                            Err(TypeCheckError::NoSuchFieldExist { 
                                name: name.clone(),
                                field: field.to_string(),
                                span: infix.rhs.span() 
                            })
                        } else {
                            Err(TypeCheckError::NoSuchStructOrUnionExist { 
                                name: name.clone(),
                                span: infix.lhs.span(),
                            })
                        }
                    }
                    _ => Err(TypeCheckError::TheTypeDoesntHaveField {
                        got: lhs, span: infix.span(),
                    })
                }
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
                let got = typeof_expr(expr, val_tbl, func_tbl, struct_tbl, union_tbl)?;
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
