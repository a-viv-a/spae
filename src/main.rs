use lower::lower;
use parse::stmts;
use winnow::Parser;

mod ast;
mod ast_macros;
mod lower;
mod parse;

fn main() {
    let mut input = include_str!("../samples/river.spae"); // TODO: actually take input
    let output = stmts.parse_next(&mut input).unwrap();
    let lowered = lower(output.clone());
    println!("{input}\n------\n{output:#?}\n------\n{lowered:#?}");
}
