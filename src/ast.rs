use std::fmt::{Debug, Error, Formatter};

#[derive(Debug)]
pub struct Program {
    stmts: Vec<Expr>,
}

#[derive(Debug)]
pub enum Stm {
    SExp(Box<Expr>),
    SBlock(Vec<Box<Stm>>),
}

#[derive(Debug)]
pub enum Expr {
    ELitInt(i32),
    ELitString(String),
    ELitDouble(f64),
    EIdent(String),
    EAdd(Box<Expr>, Addop, Box<Expr>),
    EMul(Box<Expr>, Mulop, Box<Expr>),
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
