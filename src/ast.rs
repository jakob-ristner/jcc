pub type Program<T> = Vec<Fun<T>>;

pub type Ident = String;
pub type Arg = (Type, Ident);
pub type Fun<T> = (Type, String, Vec<Arg>, Block<T>);
pub type Block<T> = Vec<Box<Stm<T>>>;
pub type Decl<T> = (Ident, Option<Box<T>>);

#[derive(Debug)]
pub enum Stm<T> {
    SEmpty,
    SExp(Box<T>),
    SBlock(Block<T>),
    SInit(Type, Vec<Decl<T>>),
    SAss(Ident, Box<T>),
    SIf(Box<T>, Box<Stm<T>>, Option<Box<Stm<T>>>),
    SWhile(Box<T>, Box<Stm<T>>),
    SIncDec(Ident, bool),
    SRet(Option<Box<T>>),
}

#[derive(Debug)]
pub enum TypedExpr {
    TELitInt(Type, i32),
    TELitBool(Type, bool),
    TELitString(Type, String),
    TELitDouble(Type, f64),
    TEIdent(Type, Ident),
    TEBinop(Type, Box<TypedExpr>, Binop, Box<TypedExpr>),
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
    EBinop(Box<Expr>, Binop, Box<Expr>),
    EUnop(Unop, Box<Expr>),
    EApp(Ident, Vec<Box<Expr>>),
}

#[derive(Debug, Clone)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
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
pub enum Unop {
    Not,
    Neg,
}
