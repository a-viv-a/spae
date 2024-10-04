use winnow::combinator::alt;
use winnow::combinator::delimited;
use winnow::combinator::opt;
use winnow::combinator::preceded;
use winnow::combinator::repeat;
use winnow::combinator::terminated;
use winnow::token::take;
use winnow::token::take_until;
use winnow::token::take_while;
use winnow::PResult;
use winnow::Parser;

fn parse_let<'s>(input: &mut &'s str) -> PResult<&'s str> {
    "let ".parse_next(input)
}

fn parse_ident<'s>(input: &mut &'s str) -> PResult<&'s str> {
    take_while(1.., 'a'..='z').parse_next(input)
}

fn parse_command<'s>(input: &mut &'s str) -> PResult<&'s str> {
    let open_grave = take_while(1.., '`').parse_next(input)?;
    let command = take_until(0.., open_grave).parse_next(input)?;
    take(open_grave.len()).parse_next(input)?;
    return Ok(command);
}

fn parse_list<'s>(input: &mut &'s str) -> PResult<Vec<&'s str>> {
    delimited(
        "[",
        repeat(0.., terminated(alt((parse_ident, parse_command)), opt(','))),
        "]",
    )
    .parse_next(input)
}

// fn parse_list<'s>(input: &mut &'s str) -> PResult<&'s str> {}

fn main() {
    // println!("Hello, world!");
    // let mut input = include_str!("../samples/river.spae"); // TODO: actually take input
    // let ident = preceded(parse_let, parse_ident)
    // .parse_next(&mut input)
    // .unwrap();
    let mut input = "[ident,``command``,ident]";
    let list = parse_list.parse_next(&mut input).unwrap();
    println!("{input}\n------\n{list:?}");
}
