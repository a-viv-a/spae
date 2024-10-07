use winnow::ascii::multispace0;
use winnow::combinator::{alt, delimited, opt, repeat, separated_pair, terminated};
use winnow::error::{ParserError, StrContext};
use winnow::prelude::*;
use winnow::stream::AsChar;
use winnow::token::{one_of, take, take_until, take_while};

type Ident<'s> = &'s str;
type Str<'s> = &'s str;

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
enum Type {
    String,
    Number,
    Range,
    File,
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Expr<'s> {
    String(Str<'s>),
    Ident(Ident<'s>),
    List(List<'s>),
    Prefix(PrefixSymbol, Box<Expr<'s>>),
    Infix(Box<Expr<'s>>, InfixSymbol, Box<Expr<'s>>),
    Type(Type),
    // TODO: limit types of descriptions that are legal
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

fn ws<'a, F, O, E: ParserError<&'a str>>(inner: F) -> impl Parser<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
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
    (ws(alt((maybe, one, some))), expr)
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

fn string<'s>(input: &mut &'s str) -> PResult<Str<'s>> {
    let open_grave = take_while(1.., '`').parse_next(input)?;
    let string = take_until(0.., open_grave).parse_next(input)?;
    take(open_grave.len()).void().parse_next(input)?;
    return Ok(string);
}
fn string_expr<'s>(input: &mut &'s str) -> PResult<Expr<'s>> {
    string.parse_next(input).map(Expr::String)
}

fn described<'s>(input: &mut &'s str) -> PResult<Expr<'s>> {
    separated_pair(
        alt((ident_expr, string_expr)),
        ws(':'),
        alt((ident_expr, string_expr)),
    )
    .context(StrContext::Label("described value"))
    .parse_next(input)
    .map(|(expr, description)| Expr::Described(Box::new(expr), Box::new(description)))
}

fn list<'s>(input: &mut &'s str) -> PResult<Expr<'s>> {
    delimited('[', repeat(0.., terminated(expr, ws(opt(',')))), ']')
        .context(StrContext::Label("list"))
        .parse_next(input)
        .map(Expr::List)
}

fn finite_expr<'s>(input: &mut &'s str) -> PResult<Expr<'s>> {
    // TODO: don't waste work matching ident and string twice bc of alt
    ws(alt((described, ident_expr, string_expr, list)))
        .context(StrContext::Label("finite expr"))
        .parse_next(input)
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
        }
    }

    mod finite_expr {
        use super::*;
        param! {
            |input| finite_expr.parse(input).ok();
            description_list: "[a:b, c:`words`]" => Some(list![desc!(ident!(a); ident!(b)), desc!(ident!(c);s "words")])
        }
    }

    mod expr {
        use super::*;
        param! {
            |input| expr.parse(input).ok();
            concat: "a + b" => Some(infix!(ident!(a), + ident!(b))),
            set_minus: "a - b" => Some(infix!(ident!(a), - ident!(b))),
            dependent: "a > b" => Some(infix!(ident!(a), > ident!(b))),
            left_to_right: "a + b - c" => Some(infix!(infix!(ident!(a), + ident!(b)), - ident!(c))),
            nesting: "a + b > b - c" => Some(infix!(infix!(ident!(a), + ident!(b)), > infix!(ident!(b), - ident!(c)))),
            description: "a : ``an a``" => Some(desc!(ident!(a);s "an a")),
        }
    }

    mod stmts {
        use super::*;
        param! {
            |input| stmts.parse(input).ok();
            let_assignment: "let a = b;" => Some(vec![s_let!(a = ident!(b))]),
            let_dependence: "let a = b > c;" => Some(vec![s_let!(a = infix!(ident!(b), > ident!(c)))]),
        }
    }
}
