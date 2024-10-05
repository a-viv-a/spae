use winnow::ascii::multispace0;
use winnow::combinator::{alt, delimited, opt, repeat, separated_pair, terminated};
use winnow::error::{ParserError, StrContext};
use winnow::prelude::*;
use winnow::stream::AsChar;
use winnow::token::{one_of, take, take_until, take_while};

type Ident<'s> = &'s str;

#[derive(Debug)]
enum InfixSymbol {
    Concat,
    SetMinus,
}

#[derive(Debug)]
enum PrefixSymbol {
    One,
    Maybe,
}

#[derive(Debug)]
enum Expr<'s> {
    Command(&'s str),
    Ident(Ident<'s>),
    List(List<'s>),
    Dependent {
        when: Box<Expr<'s>>,
        then: Box<Expr<'s>>,
    },
    Prefix(PrefixSymbol, Box<Expr<'s>>),
    Infix(Box<Expr<'s>>, InfixSymbol, Box<Expr<'s>>),
}

#[derive(Debug)]
enum Stmt<'s> {
    Let(Ident<'s>, Expr<'s>),
    Expr(Expr<'s>),
}

type List<'s> = Vec<Expr<'s>>;

fn unpack<L, R, T>(f: impl FnOnce(L, R) -> T) -> impl FnOnce((L, R)) -> T {
    |(l, r)| f(l, r)
}

fn ws<'a, F, O, E: ParserError<&'a str>>(inner: F) -> impl Parser<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

fn concat<'s>(input: &mut &'s str) -> PResult<InfixSymbol> {
    "+".parse_next(input).map(|_| InfixSymbol::Concat)
}

fn set_minus<'s>(input: &mut &'s str) -> PResult<InfixSymbol> {
    "-".parse_next(input).map(|_| InfixSymbol::SetMinus)
}

fn one<'s>(input: &mut &'s str) -> PResult<PrefixSymbol> {
    "one".parse_next(input).map(|_| PrefixSymbol::One)
}

fn maybe<'s>(input: &mut &'s str) -> PResult<PrefixSymbol> {
    "maybe".parse_next(input).map(|_| PrefixSymbol::Maybe)
}

const RESERVED: [&str; 2] = ["one", "maybe"];

fn infix<'s>(input: &mut &'s str) -> PResult<Expr<'s>> {
    (finite_expr, alt((concat, set_minus)), expr)
        .context(StrContext::Label("infix"))
        .parse_next(input)
        .map(|(lh, infix, rh)| Expr::Infix(Box::new(lh), infix, Box::new(rh)))
}

fn prefix<'s>(input: &mut &'s str) -> PResult<Expr<'s>> {
    (ws(alt((one, maybe))), expr)
        .context(StrContext::Label("prefix"))
        .parse_next(input)
        .map(|(prefix, expr)| Expr::Prefix(prefix, Box::new(expr)))
}

fn ident<'s>(input: &mut &'s str) -> PResult<Ident<'s>> {
    (
        one_of(|c: char| c.is_alpha() || c == '_'),
        take_while(0.., |c: char| c.is_alphanum() || c == '_'),
    )
        .take()
        .verify(|ident| !RESERVED.contains(ident))
        .context(StrContext::Label("ident"))
        .parse_next(input)
}

fn command<'s>(input: &mut &'s str) -> PResult<Expr<'s>> {
    let open_grave = take_while(1.., '`').parse_next(input)?;
    let command = take_until(0.., open_grave).parse_next(input)?;
    take(open_grave.len()).void().parse_next(input)?;
    return Ok(Expr::Command(command));
}

fn list<'s>(input: &mut &'s str) -> PResult<Expr<'s>> {
    delimited('[', repeat(0.., terminated(expr, ws(opt(',')))), ']')
        .context(StrContext::Label("list"))
        .parse_next(input)
        .map(Expr::List)
}

fn finite_expr<'s>(input: &mut &'s str) -> PResult<Expr<'s>> {
    ws(alt((ident.map(Expr::Ident), command, list)))
        .context(StrContext::Label("finite expr"))
        .parse_next(input)
}

fn dependent<'s>(input: &mut &'s str) -> PResult<Expr<'s>> {
    separated_pair(finite_expr, '>', expr)
        .context(StrContext::Label("dependent"))
        .parse_next(input)
        .map(|(when, then)| Expr::Dependent {
            when: Box::new(when),
            then: Box::new(then),
        })
}

fn expr<'s>(input: &mut &'s str) -> PResult<Expr<'s>> {
    alt((prefix, infix, finite_expr, dependent))
        .context(StrContext::Label("expr"))
        .parse_next(input)
}

fn let_assignment<'s>(input: &mut &'s str) -> PResult<Stmt<'s>> {
    (
        delimited(ws("let"), ws(ident), ws('=')),
        terminated(expr, ws(';')),
    )
        .parse_next(input)
        .map(unpack(Stmt::Let))
}

fn stmt<'s>(input: &mut &'s str) -> PResult<Stmt<'s>> {
    alt((let_assignment, terminated(expr.map(Stmt::Expr), ';'))).parse_next(input)
}

fn stmts<'s>(input: &mut &'s str) -> PResult<Vec<Stmt<'s>>> {
    repeat(1.., stmt).parse_next(input)
}

fn main() {
    let mut input = include_str!("../samples/river.spae"); // TODO: actually take input
    let output = stmts.parse_next(&mut input).unwrap();
    println!("{input}\n------\n{output:#?}");
}
