use winnow::ascii::multispace0;
use winnow::combinator::{alt, delimited, opt, repeat, terminated};
use winnow::error::{ParserError, StrContext};
use winnow::prelude::*;
use winnow::stream::AsChar;
use winnow::token::{one_of, take, take_until, take_while};

fn ws<'a, F, O, E: ParserError<&'a str>>(inner: F) -> impl Parser<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

fn ident<'s>(input: &mut &'s str) -> PResult<&'s str> {
    (
        one_of(|c: char| c.is_alpha() || c == '_'),
        take_while(0.., |c: char| c.is_alphanum() || c == '_'),
    )
        .take()
        .context(StrContext::Label("ident"))
        .parse_next(input)
}

fn command<'s>(input: &mut &'s str) -> PResult<&'s str> {
    let open_grave = take_while(1.., '`').parse_next(input)?;
    let command = take_until(0.., open_grave).parse_next(input)?;
    take(open_grave.len()).parse_next(input)?;
    return Ok(command);
}

fn expr<'s>(input: &mut &'s str) -> PResult<&'s str> {
    alt((ws(ident), ws(command)))
        .context(StrContext::Label("expr"))
        .parse_next(input)
}

fn list<'s>(input: &mut &'s str) -> PResult<Vec<&'s str>> {
    delimited('[', repeat(0.., terminated(expr, opt(','))), ']')
        .context(StrContext::Label("list"))
        .parse_next(input)
}

fn let_assignment<'s>(input: &mut &'s str) -> PResult<(&'s str, &'s str)> {
    (
        delimited(ws("let"), ws(ident), ws('=')),
        terminated(expr, ws(';')),
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
    // let mut input = "let list_name = [ident, ``command``, ident];";
    // let mut input = "let a = b;";
    let mut input = "let cmd = `cmd name`;";
    let assign = let_assignment.parse_next(&mut input).unwrap();
    println!("{input}\n------\n{assign:?}");
}
