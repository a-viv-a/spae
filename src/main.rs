use winnow::combinator::alt;
use winnow::combinator::delimited;
use winnow::combinator::opt;
use winnow::combinator::repeat;
use winnow::combinator::terminated;
use winnow::prelude::*;
use winnow::token::take;
use winnow::token::take_until;
use winnow::token::take_while;

fn let_assignment<'s>(input: &mut &'s str) -> PResult<&'s str> {
    "let ".parse_next(input)
}

fn ident<'s>(input: &mut &'s str) -> PResult<&'s str> {
    take_while(1.., 'a'..='z').parse_next(input)
}

fn command<'s>(input: &mut &'s str) -> PResult<&'s str> {
    let open_grave = take_while(1.., '`').parse_next(input)?;
    let command = take_until(0.., open_grave).parse_next(input)?;
    take(open_grave.len()).parse_next(input)?;
    return Ok(command);
}

fn list<'s>(input: &mut &'s str) -> PResult<Vec<&'s str>> {
    delimited(
        "[",
        repeat(0.., terminated(alt((ident, command)), opt(','))),
        "]",
    )
    .parse_next(input)
}

// fn list<'s>(input: &mut &'s str) -> PResult<&'s str> {}

fn main() {
    // println!("Hello, world!");
    // let mut input = include_str!("../samples/river.spae"); // TODO: actually take input
    // let ident = preceded(let_assignment, ident)
    // .parse_next(&mut input)
    // .unwrap();
    let mut input = "[ident,``command``,ident]";
    let list = list.parse_next(&mut input).unwrap();
    println!("{input}\n------\n{list:?}");
}
