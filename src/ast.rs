pub type Program<T> = Vec<Fun<T>>;

pub type Ident = String;
pub type Arg = (Type, Ident);
pub type Fun<T> = (Type, String, Vec<Arg>, Block<T>);
pub type Block<T> = Vec<Box<Stm<T>>>;

#[derive(Debug)]
pub enum Stm<T> {
    SEmpty,
    SExp(Box<T>),
    SBlock(Block<T>),
    SInit(Type, Vec<Init<T>>),
    SAss(Ident, Box<T>),
    SIf(Box<T>, Box<Stm<T>>, Option<Box<Stm<T>>>),
    IncDec(Ident, bool),
    SWhile(Box<T>, Box<Stm<T>>),
    SRet(Option<Box<T>>),
}

#[derive(Debug)]
pub enum TypedExpr {
    TELitInt(Type, i32),
    TELitBool(Type, bool),
    TELitString(Type, String),
    TELitDouble(Type, f64),
    TEIdent(Type, Ident),
    TEAdd(Type, Box<TypedExpr>, Addop, Box<TypedExpr>),
    TEMul(Type, Box<TypedExpr>, Mulop, Box<TypedExpr>),
    TEOr(Type, Box<TypedExpr>, Box<TypedExpr>),
    TEAnd(Type, Box<TypedExpr>, Box<TypedExpr>),
    TERel(Type, Box<TypedExpr>, Relop, Box<TypedExpr>),
    TEUnop(Type, Unop, Box<TypedExpr>),
    TEApp(Type, Ident, Vec<Box<TypedExpr>>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    ELitInt(i32),
    ELitBool(bool),
    ELitString(String),
    ELitDouble(f64),
    EIdent(Ident),
    EAdd(Box<Expr>, Addop, Box<Expr>),
    EMul(Box<Expr>, Mulop, Box<Expr>),
    EOr(Box<Expr>, Box<Expr>),
    EAnd(Box<Expr>, Box<Expr>),
    ERel(Box<Expr>, Relop, Box<Expr>),
    EUnop(Unop, Box<Expr>),
    EApp(Ident, Vec<Box<Expr>>),
}

#[derive(Debug, Clone)]
pub enum Addop {
    Plus,
    Minus,
}

#[derive(Debug, Clone)]
pub enum Mulop {
    Times,
    Div,
    Mod,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Type {
    TInt,
    TDouble,
    TString,
    TBool,
    TVoid,
}

#[derive(Debug, Clone)]
pub enum Relop {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone)]
pub enum Unop {
    Not,
    Neg,
}

#[derive(Debug)]
pub enum Init<T> {
    INoInit(Ident),
    IVarInit(Ident, Box<T>),
}
