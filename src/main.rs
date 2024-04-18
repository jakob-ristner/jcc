mod ast;
mod utils;
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub ast_lalr);

fn main() {
    let program = ast_lalr::ExprParser::new()
        .parse("5 + \"h e\\\"  \n llo\" * iö + 0.5")
        .unwrap();
    println!("{:?}", program);
}
