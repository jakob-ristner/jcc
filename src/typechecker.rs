use std::collections::{HashMap, VecDeque};
use std::error::Error;
use std::fmt;

use crate::ast::*;

pub type Signature = (Type, Vec<Type>);

pub fn typecheck(program: Program<Expr>) -> Result<Program<TypedExpr>, TypeError> {
    let mut env = Env::new();

    // Top level function pass
    for (ty, name, args, _) in &program {
        let sig = (ty.clone(), args.iter().map(|(ty, _)| ty.clone()).collect());
        if env.get_fun_type(&name).is_ok() {
            return Err(TypeError::FunctionAlreadyDefined(name.clone()));
        }
        env.add_function(name.clone(), sig);
    }

    check_program(program, &mut env)
}

#[derive(Debug)]
struct Env {
    return_found: bool,
    function_sigs: HashMap<Ident, Signature>,
    variable_context: VecDeque<HashMap<Ident, Type>>,
}

impl Env {
    fn new() -> Self {
        let variable_context = VecDeque::new();
        let mut function_sigs = HashMap::new();
        function_sigs.insert("printInt".to_string(), (Type::TVoid, vec![Type::TInt]));
        function_sigs.insert(
            "printDouble".to_string(),
            (Type::TVoid, vec![Type::TDouble]),
        );
        function_sigs.insert(
            "printString".to_string(),
            (Type::TVoid, vec![Type::TString]),
        );
        function_sigs.insert("readInt".to_string(), (Type::TInt, vec![]));
        function_sigs.insert("readDouble".to_string(), (Type::TDouble, vec![]));
        Self {
            return_found: false,
            function_sigs,
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

fn check_program(program: Program<Expr>, env: &mut Env) -> Result<Program<TypedExpr>, TypeError> {
    program
        .into_iter()
        .map(|fun| check_function(fun, env))
        .collect()
}

fn check_function(fun: Fun<Expr>, env: &mut Env) -> Result<Fun<TypedExpr>, TypeError> {
    env.return_found = false;
    env.push_scope();
    let (ty, name, args, body) = fun;
    for (arg_ty, arg_name) in &args {
        if *arg_ty == Type::TVoid {
            return Err(TypeError::VoidArgument(name.clone(), arg_name.clone()));
        }
        env.add_variable(arg_name.clone(), *arg_ty)?;
    }
    let typed_body = body
        .into_iter()
        .map(|stm| check_stm(stm, env, ty))
        .collect::<Result<_, _>>()?;
    env.pop_scope();
    if !env.return_found && ty != Type::TVoid {
        return Err(TypeError::NoReturn(name));
    }
    Ok((ty, name, args, typed_body))
}

fn check_stm(
    stm: Box<Stm<Expr>>,
    env: &mut Env,
    ret_ty: Type,
) -> Result<Box<Stm<TypedExpr>>, TypeError> {
    //generate empty match arms for stm
    match *stm {
        Stm::SEmpty => Ok(Box::new(Stm::SEmpty)),
        Stm::SExp(expr) => {
            let typed_expr = infer_expr(expr, env)?;
            if type_of(&typed_expr) != Type::TVoid {
                return Err(TypeError::NonVoidStm(type_of(&typed_expr)));
            }
            Ok(Box::new(Stm::SExp(typed_expr)))
        }
        Stm::SBlock(block) => {
            env.push_scope();
            let typed_block: Vec<_> = block
                .into_iter()
                .map(|stm| check_stm(stm, env, ret_ty))
                .collect::<Result<_, _>>()?;
            env.pop_scope();
            Ok(Box::new(Stm::SBlock(typed_block)))
        }
        Stm::SInit(ty, decls) => {
            if ty == Type::TVoid {
                return Err(TypeError::VoidAssign("".to_string()));
            }
            let typed_decls = decls
                .into_iter()
                .map(|(ident, expr)| {
                    let typed_expr = expr
                        .map(|expr| {
                            let te = infer_expr(expr, env)?;
                            if type_of(&te) != ty {
                                return Err(TypeError::AssignTypeError(
                                    ident.clone(),
                                    ty,
                                    type_of(&te),
                                ));
                            }
                            Ok(te)
                        })
                        .transpose()?;

                    env.add_variable(ident.clone(), ty)?;
                    Ok((ident, typed_expr))
                })
                .collect::<Result<_, _>>()?;
            Ok(Box::new(Stm::SInit(ty, typed_decls)))
        }
        Stm::SAss(ident, expr) => {
            let typed_expr = infer_expr(expr, env)?;
            let var_ty = env.get_var_type(&ident)?;
            if var_ty != type_of(&typed_expr) {
                return Err(TypeError::AssignTypeError(
                    ident,
                    var_ty,
                    type_of(&typed_expr),
                ));
            }
            Ok(Box::new(Stm::SAss(ident, typed_expr)))
        }
        Stm::SIf(cond, then_stm, else_stm) => {
            let return_found_before = env.return_found;

            let typed_cond = infer_expr(cond, env)?;
            if type_of(&typed_cond) != Type::TBool {
                return Err(TypeError::ConditionError(type_of(&typed_cond)));
            }

            let typed_then = check_stm(then_stm, env, ret_ty)?;
            let return_found_fst = env.return_found;
            env.return_found = return_found_before;

            let typed_else = else_stm
                .map(|stm| check_stm(stm, env, ret_ty))
                .transpose()?;

            let return_found_snd = env.return_found;

            env.return_found = (return_found_fst && return_found_snd) || return_found_before;

            Ok(Box::new(Stm::SIf(typed_cond, typed_then, typed_else)))
        }
        Stm::SWhile(cond, body) => {
            let return_found_before = env.return_found;
            let typed_cond = infer_expr(cond, env)?;
            if type_of(&typed_cond) != Type::TBool {
                return Err(TypeError::ConditionError(type_of(&typed_cond)));
            }
            let typed_body = check_stm(body, env, ret_ty)?;
            env.return_found = return_found_before;
            Ok(Box::new(Stm::SWhile(typed_cond, typed_body)))
        }
        Stm::SIncDec(ident, incdec) => {
            let var_ty = env.get_var_type(&ident)?;
            if var_ty != Type::TInt {
                return Err(TypeError::IncDecError(ident, var_ty));
            }
            Ok(Box::new(Stm::SIncDec(ident, incdec)))
        }
        Stm::SRet(expr) => {
            env.return_found = true;
            let typed_expr = expr.map(|expr| infer_expr(expr, env)).transpose()?;
            if let Some(typed_expr) = &typed_expr {
                if type_of(typed_expr) != ret_ty {
                    return Err(TypeError::ReturnTypeError(ret_ty, type_of(typed_expr)));
                }
            } else if ret_ty != Type::TVoid {
                return Err(TypeError::ReturnTypeError(ret_ty, Type::TVoid));
            }
            Ok(Box::new(Stm::SRet(typed_expr)))
        }
    }
}

fn infer_expr(expr: Box<Expr>, env: &Env) -> Result<Box<TypedExpr>, TypeError> {
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
            use Binop::*;
            let lhs = infer_expr(lhs, env)?;
            let rhs = infer_expr(rhs, env)?;
            let tl = type_of(&lhs);
            let tr = type_of(&rhs);
            if tl != tr {
                return Err(TypeError::BinaryOpError(tl, op, tr));
            }
            let out_type: Type = match op {
                Add | Sub | Mul | Div | Mod => {
                    if tl != Type::TInt && tl != Type::TDouble {
                        return Err(TypeError::BinaryOpError(tl, op, tr));
                    }
                    Ok(tl)
                }
                Gt | Ge | Lt | Le => {
                    if tl != Type::TInt && tl != Type::TDouble {
                        return Err(TypeError::BinaryOpError(tl, op, tr));
                    }
                    Ok(Type::TBool)
                }
                And | Or => {
                    if tl != Type::TBool {
                        return Err(TypeError::BinaryOpError(tl, op, tr));
                    }
                    Ok(Type::TBool)
                }
                Eq | Ne => Ok(Type::TBool),
            }?;
            Ok(Box::new(TypedExpr::TEBinop(out_type, lhs, op, rhs)))
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
    AssignTypeError(Ident, Type, Type),
    ConditionError(Type),
    IncDecError(Ident, Type),
    ReturnTypeError(Type, Type),
    NoReturn(Ident),
    NonVoidStm(Type),
    FunctionAlreadyDefined(Ident),
    VoidAssign(Ident),
}
impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for TypeError {
    fn description(&self) -> &str {
        "type error"
    }
}
