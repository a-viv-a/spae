use std::fmt::Debug;
use std::io;
use std::ops::Range;
use std::sync::LazyLock;

use crate::ast::*;
use ariadne::{Color, ColorGenerator, Fmt, Label, LabelAttach, Report, ReportKind, Source};
use eyre::Result;
use winnow::ascii::multispace0;
use winnow::combinator::{alt, delimited, opt, preceded, repeat, terminated};
use winnow::error::{ContextError, ParseError, ParserError, StrContext};
use winnow::prelude::*;
use winnow::stream::AsChar;
use winnow::token::{take, take_until, take_while};

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

const ILLEGAL_IDENTS: &'static [&str] = &["maybe", "require", "one", "some", "all", "-", "let"];

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
    macro_rules! prefix {
        ($string:expr => $name:ident) => {
            $string.void().map(|_| PrefixSymbol::$name)
        };
    }
    (
        r_ws(alt((
            prefix!("maybe"   => Maybe),
            prefix!("require" => Require),
            prefix!("one"     => One),
            prefix!("some"    => Some),
            prefix!("all"     => All),
        ))),
        finite_expr,
    )
        .context(StrContext::Label("prefix"))
        .parse_next(input)
        .map(|(prefix, expr)| Expr::Prefix(prefix, Box::new(expr)))
}

// there is no way this is the best solution...
static IDENT_DESCRIPTION: LazyLock<String> = LazyLock::new(|| {
    format!(
        "a valid ident, a sequence of alphanumerics, '_' and '-', not one of {} which are reserved keywords or operators.",
        ILLEGAL_IDENTS
            .iter()
            .map(|ident| format!("\"{ident}\""))
            .collect::<Vec<String>>()
            .join(", ")
    )
});
fn ident<'s>(input: &mut &'s str) -> PResult<Ident<'s>> {
    take_while(1.., |c: char| c.is_alphanum() || c == '_' || c == '-')
        .verify(|ident| !ILLEGAL_IDENTS.contains(ident))
        .context(StrContext::Label("ident"))
        .context(StrContext::Expected(
            winnow::error::StrContextValue::Description(IDENT_DESCRIPTION.as_str()),
        ))
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
    let expr = lr_ws(alt((prefix, ident_expr, string, list, directive)))
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
        let lhs = finite_expr.parse_next(input)?;
        fn expr_ltr_pairs<'s>(lhs: Expr<'s>) -> impl FnMut(&mut &'s str) -> PResult<Expr<'s>> {
            move |input: &mut &str| {
                if let Some((infix, rhs)) =
                    opt((ltr_infix_symbol, finite_expr)).parse_next(input)?
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
        (expr_ltr, lr_ws(rtl_infix_symbol), expr)
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

#[derive(Debug)]
pub struct SpaeReport<'a> {
    report: Report<'static, (&'a str, Range<usize>)>,
    source: (&'a str, Source),
}

impl SpaeReport<'_> {
    fn from_parse<'s>(error: ParseError<&'s str, ContextError>, input: &'s str) -> Self {
        let mut colors = ColorGenerator::new();

        let a = colors.next();

        let mut ctx = error.inner().context().collect::<Vec<_>>();

        let message = error
            .inner()
            .context()
            .filter(|c| matches!(c, StrContext::Label(_)))
            .map(|c| c.to_string())
            .collect::<Vec<_>>()
            .join(", ");

        let help = error
            .inner()
            .context()
            .filter(|c| matches!(c, StrContext::Expected(_)))
            .map(|c| c.to_string())
            .collect::<Vec<_>>()
            .join(", ");

        let file = "TODO.spae";
        let start = error.offset();
        let end = (start + 1..)
            .find(|e| input.is_char_boundary(*e))
            .unwrap_or(start);

        let report = Report::build(ReportKind::Error, file, 0)
            .with_code(
                8, /* TODO: build out lookup table for types of errors */
            )
            .with_message(message.clone())
            .with_label(
                Label::new((file, start..end))
                    .with_message(format!("{message} here"))
                    .with_color(a),
            )
            .with_help(help)
            .finish();
        Self {
            report,
            source: (file, Source::from(input.to_owned())),
        }
    }

    pub fn write_stderr(self) -> io::Result<()> {
        self.report.eprint(self.source)
    }
}

pub fn parse<'s>(input: &'s str) -> Result<Vec<Stmt<'s>>, SpaeReport> {
    stmts
        .parse(input)
        .map_err(|e| SpaeReport::from_parse(e, input))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast_macros::*;

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

            ex1: "a:`a` > c:`c`" => Some(
                infix!(
                    desc!(ident!(a);s "a"),
                    > desc!(ident!(c);s "c")
                )
            ),
            ex2: "a:`a` > one c:`c`" => Some(
                infix!(
                    desc!(ident!(a);s "a"),
                    > prefix!(One: desc!(ident!(c);s "c"))
                )
            ),
            ex3: "`focus-output`:`details` > one cardinal > {string}: `name`" => Some(
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
