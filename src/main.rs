use winnow::ascii::multispace0;
use winnow::combinator::{alt, delimited, opt, preceded, repeat, separated_pair, terminated};
use winnow::error::{ParserError, StrContext};
use winnow::prelude::*;
use winnow::stream::AsChar;
use winnow::token::{one_of, take, take_until, take_while};

type Ident<'s> = &'s str;

#[derive(Debug, PartialEq, Eq, Clone)]
enum InfixSymbol {
    Dependent,
    Concat,
    SetMinus,
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum PrefixSymbol {
    Maybe,
    One,
    Some,
    All,
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Expr<'s> {
    String(&'s str),
    Directive(&'s str),
    Ident(Ident<'s>),
    List(List<'s>),
    Prefix(PrefixSymbol, Box<Expr<'s>>),
    Infix(Box<Expr<'s>>, InfixSymbol, Box<Expr<'s>>),
    Described(Box<Expr<'s>>, Box<Expr<'s>>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Stmt<'s> {
    Let(Ident<'s>, Expr<'s>),
    Expr(Expr<'s>),
}

type List<'s> = Vec<Expr<'s>>;

fn unpack<L, R, T>(f: impl FnOnce(L, R) -> T) -> impl FnOnce((L, R)) -> T {
    |(l, r)| f(l, r)
}

fn lr_ws<'a, F, O, E: ParserError<&'a str>>(inner: F) -> impl Parser<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

fn l_ws<'a, F, O, E: ParserError<&'a str>>(inner: F) -> impl Parser<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    preceded(multispace0, inner)
}

fn r_ws<'a, F, O, E: ParserError<&'a str>>(inner: F) -> impl Parser<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    terminated(inner, multispace0)
}

fn dependent<'s>(input: &mut &'s str) -> PResult<InfixSymbol> {
    ">".parse_next(input).map(|_| InfixSymbol::Dependent)
}

fn concat<'s>(input: &mut &'s str) -> PResult<InfixSymbol> {
    "+".parse_next(input).map(|_| InfixSymbol::Concat)
}

fn set_minus<'s>(input: &mut &'s str) -> PResult<InfixSymbol> {
    "-".parse_next(input).map(|_| InfixSymbol::SetMinus)
}

fn maybe<'s>(input: &mut &'s str) -> PResult<PrefixSymbol> {
    "maybe".parse_next(input).map(|_| PrefixSymbol::Maybe)
}

fn one<'s>(input: &mut &'s str) -> PResult<PrefixSymbol> {
    "one".parse_next(input).map(|_| PrefixSymbol::One)
}

fn some<'s>(input: &mut &'s str) -> PResult<PrefixSymbol> {
    "some".parse_next(input).map(|_| PrefixSymbol::Some)
}

// fn all<'s>(input: &mut &'s str) -> PResult<PrefixSymbol> {
//     "all".parse_next(input).map(|_| PrefixSymbol::All)
// }

const ILLEGAL_IDENTS: [&str; 5] = ["maybe", "one", "some", "-", "let"];

fn ltr_infix_symbol<'s>(input: &mut &'s str) -> PResult<InfixSymbol> {
    alt((concat, set_minus))
        .context(StrContext::Label("left to right infix symbol"))
        .parse_next(input)
}

fn rtl_infix_symbol<'s>(input: &mut &'s str) -> PResult<InfixSymbol> {
    dependent
        .context(StrContext::Label("right to left infix symbol"))
        .parse_next(input)
}

fn prefix<'s>(input: &mut &'s str) -> PResult<Expr<'s>> {
    (alt((maybe, one, some)), expr)
        .context(StrContext::Label("prefix"))
        .parse_next(input)
        .map(|(prefix, expr)| Expr::Prefix(prefix, Box::new(expr)))
}

fn ident<'s>(input: &mut &'s str) -> PResult<Ident<'s>> {
    take_while(1.., |c: char| c.is_alphanum() || c == '_' || c == '-')
        .verify(|ident| !ILLEGAL_IDENTS.contains(ident))
        .context(StrContext::Label("ident"))
        .parse_next(input)
}

fn ident_expr<'s>(input: &mut &'s str) -> PResult<Expr<'s>> {
    ident.map(Expr::Ident).parse_next(input)
}

fn string<'s>(input: &mut &'s str) -> PResult<Expr<'s>> {
    let open_graves = take_while(1.., '`')
        .context(StrContext::Label("opening graves"))
        .parse_next(input)?;
    let string = take_until(0.., open_graves)
        .context(StrContext::Label("string body"))
        .parse_next(input)
        .map(Expr::String)?;
    take(open_graves.len())
        .context(StrContext::Label("closing graves"))
        .void()
        .parse_next(input)?;
    return Ok(string);
}

fn directive<'s>(input: &mut &'s str) -> PResult<Expr<'s>> {
    let open_braces = take_while(1.., '{')
        .context(StrContext::Label("opening braces"))
        .parse_next(input)?;
    let close_braces = "}".repeat(open_braces.len());
    let string = take_until(0.., close_braces.as_str())
        .context(StrContext::Label("directive body"))
        .parse_next(input)
        .map(Expr::Directive)?;
    take(close_braces.len())
        .context(StrContext::Label("closing braces"))
        .void()
        .parse_next(input)?;
    return Ok(string);
}

fn list<'s>(input: &mut &'s str) -> PResult<Expr<'s>> {
    delimited('[', repeat(0.., terminated(expr, r_ws(opt(',')))), ']')
        .context(StrContext::Label("list"))
        .parse_next(input)
        .map(Expr::List)
}

fn finite_expr<'s>(input: &mut &'s str) -> PResult<Expr<'s>> {
    let expr = lr_ws(alt((ident_expr, string, list, directive)))
        .context(StrContext::Label("finite expr"))
        .parse_next(input)?;

    // check for description, which binds stronger than any other infix operator
    if let Some(description) = opt(preceded(r_ws(':'), alt((string, ident_expr))))
        .context(StrContext::Label("description"))
        .parse_next(input)?
    {
        return Ok(Expr::Described(Box::new(expr), Box::new(description)));
    }
    Ok(expr)
}

fn expr<'s>(input: &mut &'s str) -> PResult<Expr<'s>> {
    fn expr_ltr<'s>(input: &mut &'s str) -> PResult<Expr<'s>> {
        fn expr_fragment<'s>(input: &mut &'s str) -> PResult<Expr<'s>> {
            alt((prefix, finite_expr))
                .context(StrContext::Label("left to right expr"))
                .parse_next(input)
        }
        let lhs = expr_fragment.parse_next(input)?;
        fn expr_ltr_pairs<'s>(lhs: Expr<'s>) -> impl FnMut(&mut &'s str) -> PResult<Expr<'s>> {
            move |input: &mut &str| {
                if let Some((infix, rhs)) =
                    opt((ltr_infix_symbol, expr_fragment)).parse_next(input)?
                {
                    let lhs = Expr::Infix(Box::new(lhs.clone()), infix, Box::new(rhs));
                    return expr_ltr_pairs(lhs).parse_next(input);
                }
                return Ok(lhs.clone());
            }
        }
        return expr_ltr_pairs(lhs).parse_next(input);
    }
    // we need to handle right to left infix operations out here
    fn expr_rtl<'s>(input: &mut &'s str) -> PResult<Expr<'s>> {
        (expr_ltr, rtl_infix_symbol, expr)
            .context(StrContext::Label("right to left expr"))
            .map(|(lhs, infix, rhs)| Expr::Infix(Box::new(lhs), infix, Box::new(rhs)))
            .parse_next(input)
    }

    alt((expr_rtl, expr_ltr))
        .context(StrContext::Label("expr"))
        .parse_next(input)
}

fn let_assignment<'s>(input: &mut &'s str) -> PResult<Stmt<'s>> {
    (
        delimited(lr_ws("let"), ident, l_ws('=')),
        terminated(expr, r_ws(';')),
    )
        .parse_next(input)
        .map(unpack(Stmt::Let))
}

fn stmt<'s>(input: &mut &'s str) -> PResult<Stmt<'s>> {
    let_assignment.parse_next(input)
}

fn stmts<'s>(input: &mut &'s str) -> PResult<Vec<Stmt<'s>>> {
    repeat(1.., stmt).parse_next(input)
}

fn main() {
    let mut input = include_str!("../samples/min.spae"); // TODO: actually take input
    let output = stmts.parse_next(&mut input).unwrap();
    println!("{input}\n------\n{output:#?}");
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! param {
        ($transform:expr;
            $($name:ident: $input:expr => $expected:expr),* $(,)?) => {
        $(
            #[test]
            fn $name() {
                let input = $input;
                dbg!(input);
                let actual = $transform($input);
                let expected = $expected;
                dbg!(&actual, &expected);
                pretty_assertions::assert_eq!(actual, expected);
            }
        )*
        }
    }

    macro_rules! list {
        ($($item:expr),* $(,)?) => {
            Expr::List(vec![
                $($item,)*
            ])
        };
    }

    macro_rules! ident {
        ($ident:ident) => {
            Expr::Ident(stringify!($ident))
        };
    }
    macro_rules! desc {
        ($expr:expr;s $desc:expr) => {
            Expr::Described(Box::new($expr), Box::new(Expr::String($desc)))
        };
        ($expr:expr; $desc:expr) => {
            Expr::Described(Box::new($expr), Box::new($desc))
        };
    }

    macro_rules! s_let {
        ($ident:ident = $val:expr) => {
            Stmt::Let(stringify!($ident), $val)
        };
    }

    macro_rules! infix {
        ($lhs:expr, + $rhs:expr) => {
            Expr::Infix(Box::new($lhs), InfixSymbol::Concat, Box::new($rhs))
        };
        ($lhs:expr, - $rhs:expr) => {
            Expr::Infix(Box::new($lhs), InfixSymbol::SetMinus, Box::new($rhs))
        };
        ($lhs:expr, > $rhs:expr) => {
            Expr::Infix(Box::new($lhs), InfixSymbol::Dependent, Box::new($rhs))
        };
    }

    macro_rules! prefix {
        ($($prefix:ident)*: $val:expr) => {
            vec![
                $( PrefixSymbol::$prefix, )*
            ].into_iter().rfold($val, |val, symbol| Expr::Prefix(symbol, Box::new(val)))
        };
    }

    mod ident {
        use super::*;
        param! {
            |input| ident.parse(input).ok();
            basic:           "ident" => Some("ident"),
            hyphens:         "a-b"   => Some("a-b"),
            hyphen_prefix:   "-a"    => Some("-a"),
            illegal_symbol:  "try%"  => None,
            oops_all_hyphen: "-"     => None,
            reserved_ident:  "some"  => None,
            illegal_let:     "let"   => None,
        }
    }

    mod list {
        use super::*;
        param! {
            |input| list.parse(input).ok();
            basic:          "[l, n]"    => Some(list![ident!(l), ident!(n)]),
            spaces:         "[ l, n ]"  => Some(list![ident!(l), ident!(n)]),
            trailing_comma: "[ l, n, ]" => Some(list![ident!(l), ident!(n)]),
            trailing_newline: "[ l, n,\n]" => Some(list![ident!(l), ident!(n)]),
            evil:           "[\n\tl\n,\nn,\t]" => Some(list![ident!(l), ident!(n)]),
        }
    }

    mod finite_expr {
        use super::*;
        param! {
            |input| finite_expr.parse(input).ok();
            description_list:    "[a:b, c:`words`]"               => Some(list![desc!(ident!(a); ident!(b)), desc!(ident!(c);s "words")]),
            directive:           "{{{{word}}}}"                   => Some(Expr::Directive("word")),
            described_directive: "{string}: `name of the person`" => Some(desc!(Expr::Directive("string");s "name of the person")),
            // this is probably not allowed by the compiler but it will be caught later, and some compiler may allow
            described_list:      "[ a, b ]: `a and b`"            => Some(desc!(list![ident!(a), ident!(b)];s "a and b"))
        }
    }

    mod expr {
        use super::*;
        param! {
            |input| expr.parse(input).ok();
            concat:        "a + b" => Some(infix!(ident!(a), + ident!(b))),
            set_minus:     "a - b" => Some(infix!(ident!(a), - ident!(b))),
            dependent:     "a > b" => Some(infix!(ident!(a), > ident!(b))),
            left_to_right: "a + b - c"     => Some(infix!(infix!(ident!(a), + ident!(b)), - ident!(c))),
            nesting:       "a + b > b - c" => Some(infix!(infix!(ident!(a), + ident!(b)), > infix!(ident!(b), - ident!(c)))),
            description:   "a : ``an a``"  => Some(desc!(ident!(a);s "an a")),
            desc_binding:  "`a`:`b`+`c`:`d`" => Some(infix!(desc!(Expr::String("a");s "b"),+desc!(Expr::String("c");s "d"))),

            prefix: "one a" => Some(prefix!(One: ident!(a))),
            prefixes: "maybe one a" => Some(prefix!(Maybe One: ident!(a))),

            ex1: "`focus-output`:`details` > one cardinal > {string}: `name`" => Some(
                infix!(
                    desc!(Expr::String("focus-output");s "details"),
                    > infix!(
                        prefix!(One: ident!(cardinal)),
                        > desc!(Expr::Directive("string");s "name")
                    )
                )
            ),
        }
    }

    mod stmts {
        use super::*;
        param! {
            |input| stmts.parse(input).ok();
            let_assignment: "let a = b;"     => Some(vec![s_let!(a = ident!(b))]),
            let_dependence: "let a = b > c;" => Some(vec![s_let!(a = infix!(ident!(b), > ident!(c)))]),
            multilet:       "let a = b;\nlet b = c;" => Some(vec![s_let!(a = ident!(b)), s_let!(b = ident!(c))]),
            jagged_let:     "  let a = b \n;\n let b = c;\n\n" => Some(vec![s_let!(a = ident!(b)), s_let!(b = ident!(c))]),
        }
    }
}
