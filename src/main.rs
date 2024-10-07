use parse::stmts;
use winnow::Parser;

mod parse;

fn main() {
    let mut input = include_str!("../samples/river.spae"); // TODO: actually take input
    let output = stmts.parse_next(&mut input).unwrap();
    println!("{input}\n------\n{output:#?}");
}
