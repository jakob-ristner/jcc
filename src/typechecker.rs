use std::collections::{HashMap, VecDeque};

use crate::ast::*;

pub type Signature = (Type, Vec<Type>);

pub fn typecheck(program: Program<Expr>) -> Result<Program<TypedExpr>, TypeError> {
    let mut env = Environment::new();

    // Top level function pass
    for (ty, name, args, _) in &program {
        let sig = (ty.clone(), args.iter().map(|(ty, _)| ty.clone()).collect());
        env.add_function(name.clone(), sig);
    }

    check_program(program, &mut env)
}

#[derive(Debug)]
struct Environment {
    function_sigs: HashMap<Ident, Signature>,
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

    fn add_function(&mut self, name: Ident, sig: Signature) {
        self.function_sigs.insert(name, sig);
    }

    fn get_fun_type(&self, name: &Ident) -> Result<Signature, TypeError> {
        self.function_sigs
            .get(name)
            .cloned()
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
        TypedExpr::TEBinop(t, _, _, _) => t,
        TypedExpr::TEUnop(t, _, _) => t,
        TypedExpr::TEApp(t, _, _) => t,
    }
}

fn check_program(
    program: Program<Expr>,
    env: &mut Environment,
) -> Result<Program<TypedExpr>, TypeError> {
    program
        .into_iter()
        .map(|fun| check_function(fun, env))
        .collect()
}

fn check_function(
    (ty, name, args, body): Fun<Expr>,
    env: &mut Environment,
) -> Result<Fun<TypedExpr>, TypeError> {
    env.push_scope();
    for (arg_ty, arg_name) in &args {
        if *arg_ty == Type::TVoid {
            return Err(TypeError::VoidArgument(name.clone(), arg_name.clone()));
        }
        env.add_variable(arg_name.clone(), *arg_ty)?;
    }
    let typed_body = body
        .into_iter()
        .map(|stm| check_statement(stm, env))
        .collect::<Result<_, TypeError>>()?;
    env.pop_scope();
    Ok((ty, name, args, typed_body))
}

fn check_statement(
    stm: Box<Stm<Expr>>,
    env: &mut Environment,
) -> Result<Box<Stm<TypedExpr>>, TypeError> {
    //generate empty match arms for stm
    match *stm {
        Stm::SEmpty => Ok(Box::new(Stm::SEmpty)),
        Stm::SExp(expr) => Ok(Box::new(Stm::SExp(infer_expr(expr, env)?))),
        Stm::SBlock(block) => {
            env.push_scope();
            let typed_block: Vec<_> = block
                .into_iter()
                .map(|stm| check_statement(stm, env))
                .collect::<Result<_, _>>()?;
            env.pop_scope();
            Ok(Box::new(Stm::SBlock(typed_block)))
        }
        _ => unimplemented!(),
    }
}

fn infer_expr(expr: Box<Expr>, env: &Environment) -> Result<Box<TypedExpr>, TypeError> {
    match *expr {
        Expr::ELitInt(i) => Ok(Box::new(TypedExpr::TELitInt(Type::TInt, i))),
        Expr::ELitBool(b) => Ok(Box::new(TypedExpr::TELitBool(Type::TBool, b))),
        Expr::ELitString(s) => Ok(Box::new(TypedExpr::TELitString(Type::TString, s))),
        Expr::ELitDouble(d) => Ok(Box::new(TypedExpr::TELitDouble(Type::TDouble, d))),
        Expr::EIdent(ident) => Ok(Box::new(TypedExpr::TEIdent(
            env.get_var_type(&ident)?,
            ident,
        ))),
        Expr::EBinop(lhs, op, rhs) => {
            let lhs = infer_expr(lhs, env)?;
            let rhs = infer_expr(rhs, env)?;
            let tl = type_of(&lhs);
            let tr = type_of(&rhs);
            if tl != tr {
                return Err(TypeError::BinaryOpError(tl, op, tr));
            }
            match op {
                Binop::Add
                | Binop::Sub
                | Binop::Mul
                | Binop::Div
                | Binop::Mod
                | Binop::Lt
                | Binop::Le
                | Binop::Gt
                | Binop::Ge => {
                    if tl != Type::TInt && tl != Type::TDouble {
                        return Err(TypeError::BinaryOpError(tl, op, tr));
                    }
                }
                Binop::And | Binop::Or => {
                    if tl != Type::TBool {
                        return Err(TypeError::BinaryOpError(tl, op, tr));
                    }
                }
                Binop::Eq | Binop::Ne => {}
            }
            Ok(Box::new(TypedExpr::TEBinop(tl, lhs, op, rhs)))
        }
        Expr::EUnop(op, expr) => {
            let expr = infer_expr(expr, env)?;
            let t = type_of(&expr);
            match op {
                Unop::Neg => {
                    if t != Type::TInt && t != Type::TDouble {
                        return Err(TypeError::UnaryOpError(op, t));
                    }
                }
                Unop::Not => {
                    if t != Type::TBool {
                        return Err(TypeError::UnaryOpError(op, t));
                    }
                }
            }
            Ok(Box::new(TypedExpr::TEUnop(t, op, expr)))
        }

        Expr::EApp(name, args) => {
            let typed_args: Vec<_> = args
                .into_iter()
                .map(|arg| infer_expr(arg, env))
                .collect::<Result<_, _>>()?;
            let (fun_type, arg_types) = env.get_fun_type(&name)?;
            if typed_args.len() != arg_types.len() {
                return Err(TypeError::IncorrectArgCount(
                    name,
                    arg_types.len(),
                    typed_args.len(),
                ));
            }
            for (arg, ty) in typed_args.iter().zip(arg_types.iter()) {
                if type_of(arg) != *ty {
                    return Err(TypeError::IncorrectArgType(name, type_of(arg), *ty));
                }
            }
            Ok(Box::new(TypedExpr::TEApp(fun_type, name, typed_args)))
        }
    }
}

#[derive(Debug)]
pub enum TypeError {
    VoidArgument(Ident, Ident),
    BinaryOpError(Type, Binop, Type),
    UnaryOpError(Unop, Type),
    UndefinedVariable(Ident),
    IncorrectArgCount(Ident, usize, usize),
    IncorrectArgType(Ident, Type, Type),
    VariableAlreadyDefined(Ident),
    UndefinedFunction(Ident),
}
