pub type Program = Vec<Fun>;
pub type Ident = String;
pub type Arg = (Type, Ident);
pub type Fun = (Type, String, Vec<Arg>, Block);
pub type Block = Vec<Box<Stm>>;

#[derive(Debug)]
pub enum Stm {
    SExp(Box<Expr>),
    SBlock(Vec<Box<Stm>>),
    SInit(Type, Vec<Init>),
    SAss(Ident, Box<Expr>),
    SIf(Box<Expr>, Box<Stm>, Option<Box<Stm>>),
    SWhile(Box<Expr>, Box<Stm>),
    SRet(Option<Box<Expr>>),
}
#[derive(Debug)]
pub enum Expr {
    ELitInt(i32),
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
    TExpr(Type, Box<Expr>),
}

#[derive(Debug)]
pub enum Addop {
    Plus,
    Minus,
}

#[derive(Debug)]
pub enum Mulop {
    Times,
    Div,
    Mod,
}

#[derive(Debug)]
pub enum Type {
    TInt,
    TDouble,
    TString,
    TBool,
    TVoid,
}

#[derive(Debug)]
pub enum Relop {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug)]
pub enum Unop {
    Not,
    Neg,
}

#[derive(Debug)]
pub enum Init {
    INoInit(Ident),
    IVarInit(Ident, Box<Expr>),
}
