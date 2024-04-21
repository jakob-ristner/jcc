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
    //
    let arg = env::args().nth(1);

    let (quiet, reader): (bool, Box<dyn BufRead>) = match arg {
        None => (true, Box::new(BufReader::new(io::stdin()))),
        Some(filename) => (
            false,
            Box::new(BufReader::new(fs::File::open(filename).unwrap())),
        ),
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

    if !quiet {
        eprintln!("Parsed AST: {:?}\n", &program);
    }

    let annotaed = typechecker::typecheck(program);
    if annotaed.is_err() {
        eprintln!("{:?}", annotaed.unwrap_err());
        eprintln!("ERROR");
        exit(1);
    }
    let annotaed = annotaed.unwrap();

    if !quiet {
        eprintln!("Typed AST: {:?}\n", &annotaed);
    }

    eprintln!("OK");
}
