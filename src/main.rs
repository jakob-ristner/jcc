mod ast;
mod typechecker;
mod utils;
use anyhow::Result;
use ast::*;
use lalrpop_util::lalrpop_mod;
use std::env;
use std::fs;
use std::io::{self, BufRead, BufReader};
use std::process::exit;

use crate::typechecker::TypeError;
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

    let result = frontend(file);
    match result {
        Ok(prog) => {
            eprintln!("OK");
            if !quiet {
                eprintln!("{:?}", prog);
            }
            exit(0);
        }
        Err(e) => {
            eprintln!("ERROR");
            if !quiet {
                eprintln!("Error: {:?}", e);
            }
            exit(1);
        }
    }
}

fn frontend(input: String) -> Result<Program<TypedExpr>> {
    let program = ast_lalr::ProgramParser::new()
        .parse(&input)
        .map_err(|_| TypeError::ParseError)?;
    let annotated = typechecker::typecheck(program)?;
    Ok(annotated)
}
