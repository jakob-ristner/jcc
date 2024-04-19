mod ast;
mod typechecker;
mod utils;
use lalrpop_util::lalrpop_mod;
use std::env;
lalrpop_mod!(pub ast_lalr);

const PROG: &str = include_str!("./test.jl");

fn main() {
    let program = ast_lalr::ProgramParser::new()
        .parse(PROG)
        .expect("Parse Error");

    let args: Vec<String> = env::args().collect();

    eprintln!("Parsing");
    eprintln!("Args {:?}", &args);
    eprintln!("Initial AST: {:?}", &program);
    eprintln!("Parsing Complete");

    let annotaed = typechecker::typecheck(program);
    dbg!(annotaed);
}
