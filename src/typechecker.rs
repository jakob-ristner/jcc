use std::{
    collections::{HashMap, VecDeque},
    io::Write,
};

use crate::ast::*;

pub fn typecheck(program: Program<Expr>) -> Program<TypedExpr> {
    let mut env = Environment::new();
    let andExpression = Expr::EAnd(Box::new(Expr::ELitBool(true)), Box::new(Expr::ELitInt(3)));

    // infer_expr(Box::new(andExpression), &env).unwrap();
    //
    todo!();
}

struct Environment {
    function_sigs: HashMap<Ident, Type>,
    variable_context: VecDeque<HashMap<Ident, Type>>,
}

impl Environment {
    fn new() -> Self {
        let variable_context = VecDeque::new();
        Self {
            function_sigs: HashMap::new(),
            variable_context,
        }
    }

    fn add_function(&mut self, name: Ident, ty: Type) {
        self.function_sigs.insert(name, ty);
    }

    fn get_fun_type(&self, name: &Ident) -> Result<Type, TypeError> {
        self.function_sigs
            .get(name)
            .copied()
            .ok_or(TypeError::UndefinedFunction(name.clone()))
    }

    fn add_variable(&mut self, name: Ident, ty: Type) -> Result<(), TypeError> {
        if self.variable_context.front().unwrap().contains_key(&name) {
            return Err(TypeError::VariableAlreadyDefined(name));
        }
        self.variable_context.front_mut().unwrap().insert(name, ty);
        Ok(())
    }

    fn get_var_type(&self, name: &Ident) -> Result<Type, TypeError> {
        for scope in &self.variable_context {
            if let Some(ty) = scope.get(name) {
                return Ok(*ty);
            }
        }
        Err(TypeError::UndefinedVariable(name.clone()))
    }

    fn push_scope(&mut self) {
        self.variable_context.push_front(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.variable_context.pop_front();
    }
}

fn type_of(expr: &TypedExpr) -> Type {
    match *expr {
        TypedExpr::TELitInt(t, _) => t,
        TypedExpr::TELitBool(t, _) => t,
        TypedExpr::TELitString(t, _) => t,
        TypedExpr::TELitDouble(t, _) => t,
        TypedExpr::TEIdent(t, _) => t,
        TypedExpr::TEAdd(t, _, _, _) => t,
        TypedExpr::TEMul(t, _, _, _) => t,
        TypedExpr::TEOr(t, _, _) => t,
        TypedExpr::TEAnd(t, _, _) => t,
        TypedExpr::TERel(t, _, _, _) => t,
        TypedExpr::TEUnop(t, _, _) => t,
        TypedExpr::TEApp(t, _, _) => t,
    }
}

fn infer_expr(expr: Box<Expr>, env: &Environment) -> Result<TypedExpr, TypeError> {
    match *expr {
        Expr::ELitInt(i) => Ok(TypedExpr::TELitInt(Type::TInt, i)),
        Expr::ELitBool(b) => Ok(TypedExpr::TELitBool(Type::TBool, b)),
        Expr::ELitString(s) => Ok(TypedExpr::TELitString(Type::TString, s)),
        Expr::ELitDouble(d) => Ok(TypedExpr::TELitDouble(Type::TDouble, d)),
        Expr::EIdent(ident) => Ok(TypedExpr::TEIdent(env.get_var_type(&ident)?, ident)),
        Expr::EAdd(lhs, op, rhs) => {
            let lhs = infer_expr(lhs, env)?;
            let rhs = infer_expr(rhs, env)?;
            let tl = type_of(&lhs);
            let tr = type_of(&rhs);
            if tl != tr {
                return Err(TypeError::TypeMismatch(tl, tr));
            } else if tl != Type::TInt && tl != Type::TDouble {
                return Err(TypeError::InvalidArithmeticType(tl));
            }
            Ok(TypedExpr::TEAdd(tl, Box::new(lhs), op, Box::new(rhs)))
        }
        Expr::EMul(lhs, op, rhs) => {
            let lhs = infer_expr(lhs, env)?;
            let rhs = infer_expr(rhs, env)?;
            let tl = type_of(&lhs);
            let tr = type_of(&rhs);
            if tl != tr {
                return Err(TypeError::TypeMismatch(tl, tr));
            } else if tl != Type::TInt && tl != Type::TDouble {
                return Err(TypeError::InvalidArithmeticType(tl));
            }
            Ok(TypedExpr::TEMul(tl, Box::new(lhs), op, Box::new(rhs)))
        }
        Expr::EAnd(lhs, rhs) => {
            let lhs = infer_expr(lhs, env)?;
            let rhs = infer_expr(rhs, env)?;
            let tl = type_of(&lhs);
            let tr = type_of(&rhs);
            if tl != Type::TBool {
                return Err(TypeError::InvalidLogicalType(tl));
            } else if tr != Type::TBool {
                return Err(TypeError::InvalidLogicalType(tr));
            }
            Ok(TypedExpr::TEAnd(Type::TBool, Box::new(lhs), Box::new(rhs)))
        }
        Expr::EOr(lhs, rhs) => {
            let lhs = infer_expr(lhs, env)?;
            let rhs = infer_expr(rhs, env)?;
            let tl = type_of(&lhs);
            let tr = type_of(&rhs);
            if tl != Type::TBool {
                return Err(TypeError::InvalidLogicalType(tl));
            } else if tr != Type::TBool {
                return Err(TypeError::InvalidLogicalType(tr));
            }
            Ok(TypedExpr::TEOr(Type::TBool, Box::new(lhs), Box::new(rhs)))
        }
        _ => unimplemented!(),
    }
}

#[derive(Debug)]
enum TypeError {
    InvalidArithmeticType(Type),
    InvalidLogicalType(Type),
    TypeMismatch(Type, Type),
    VariableAlreadyDefined(Ident),
    UndefinedVariable(Ident),
    UndefinedFunction(Ident),
    InvalidArgumentCount(Ident),
}
