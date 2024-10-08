pub type Ident<'s> = &'s str;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum InfixSymbol {
    Dependent,
    Concat,
    SetMinus,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum PrefixSymbol {
    Maybe,
    Require,
    One,
    Some,
    All,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Expr<'s> {
    String(&'s str),
    Directive(&'s str),
    Ident(Ident<'s>),
    List(List<'s>),
    Prefix(PrefixSymbol, Box<Expr<'s>>),
    Infix(Box<Expr<'s>>, InfixSymbol, Box<Expr<'s>>),
    Described(Box<Expr<'s>>, Box<Expr<'s>>),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Stmt<'s> {
    Let(Ident<'s>, Expr<'s>),
    // Expr(Expr<'s>),
}

pub type List<'s> = Vec<Expr<'s>>;
