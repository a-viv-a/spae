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
pub(crate) use param;

macro_rules! list {
    ($($item:expr),* $(,)?) => {
        Expr::List(vec![
            $($item,)*
        ])
    };
}
pub(crate) use list;

macro_rules! ident {
    ($ident:ident) => {
        Expr::Ident(stringify!($ident))
    };
}
pub(crate) use ident;

macro_rules! desc {
    ($expr:expr;s $desc:expr) => {
        Expr::Described(Box::new($expr), Box::new(Expr::String($desc)))
    };
    ($expr:expr; $desc:expr) => {
        Expr::Described(Box::new($expr), Box::new($desc))
    };
}
pub(crate) use desc;

macro_rules! s_let {
    ($ident:ident = $val:expr) => {
        Stmt::Let(stringify!($ident), $val)
    };
}
pub(crate) use s_let;

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
pub(crate) use infix;

macro_rules! prefix {
    ($($prefix:ident)*: $val:expr) => {
        vec![
            $( PrefixSymbol::$prefix, )*
        ].into_iter().rfold($val, |val, symbol| Expr::Prefix(symbol, Box::new(val)))
    };
}
pub(crate) use prefix;
