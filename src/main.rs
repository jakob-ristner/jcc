mod ast;
mod typechecker;
mod utils;
use lalrpop_util::lalrpop_mod;
use std::env;
use std::fs;
use std::io::{self, BufRead, BufReader};
use std::process::exit;
lalrpop_mod!(pub ast_lalr);

fn main() {
    //TODO better arg parsing
    let args: Vec<String> = env::args().collect();

    let arg = env::args().nth(1);

    let reader: Box<dyn BufRead> = match arg {
        None => Box::new(BufReader::new(io::stdin())),
        Some(filename) => Box::new(BufReader::new(fs::File::open(filename).unwrap())),
    };

    let file = reader
        .lines()
        .map(|l| l.unwrap())
        .collect::<Vec<String>>()
        .join("\n");

    let program = ast_lalr::ProgramParser::new().parse(&file);
    if program.is_err() {
        eprintln!("{:?}", program.unwrap_err());
        eprintln!("ERROR");
        exit(1);
    }
    let program = program.unwrap();

    eprintln!("Parsing");
    eprintln!("Args {:?}", &args);
    eprintln!("Initial AST: {:?}", &program);
    eprintln!("Parsing Complete");

    let annotaed = typechecker::typecheck(program);
    if annotaed.is_err() {
        eprintln!("{:?}", annotaed.unwrap_err());
        eprintln!("ERROR");
        exit(1);
    }
    let annotaed = annotaed.unwrap();

    eprintln!("Typechecking");
    eprintln!("Annotated AST: {:?}", &annotaed);
    eprintln!("Typechecking Complete");
    eprintln!("OK");
}
