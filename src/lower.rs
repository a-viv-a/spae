use std::cell::RefCell;

use crate::ast::*;
use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ListAmount {
    One,
    Some,
    All,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Choice<'s> {
    // false=Maybe, true=Required
    required: bool,
    amount: ListAmount,
    from: Vec<LAST<'s>>,
}

impl<'s> Choice<'s> {
    fn apply(mut self, symbol: PrefixSymbol) -> Choice<'s> {
        match symbol {
            PrefixSymbol::Maybe => self.required = false,
            PrefixSymbol::Require => self.required = true,
            PrefixSymbol::One => self.amount = ListAmount::One,
            PrefixSymbol::Some => self.amount = ListAmount::Some,
            PrefixSymbol::All => self.amount = ListAmount::All,
        }
        self
    }

    fn default(from: Vec<LAST<'s>>) -> Choice<'s> {
        Choice {
            from,
            required: true,
            amount: ListAmount::One,
        }
    }
}

/// Lowered Abstract Syntax Tree
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum LASTNode<'s> {
    String(&'s str),
    Directive(&'s str),
    Choice(Choice<'s>),
    Dependant {
        when: Box<LAST<'s>>,
        then: Box<LAST<'s>>,
    },
}
pub type LAST<'s> = (LASTNode<'s>, Option<&'s str>);

#[derive(Debug, Clone)]
struct Ctx<'s> {
    seen: FxHashSet<&'s str>,
}

impl<'s> Ctx<'s> {
    fn new() -> Ctx<'s> {
        Ctx {
            seen: FxHashSet::default(),
        }
    }

    fn visit(mut self, ident: &'s str) -> Ctx<'s> {
        self.seen.insert(ident);
        self
    }
}

pub fn lower<'s>(stmts: Vec<Stmt<'s>>) -> LAST<'s> {
    let mut stmts = stmts;
    // TODO: add some syntax for figuring out the entrypoint
    // TODO: replace expect with proper syntax errors
    let last_assignment = stmts
        .pop()
        .expect("there should be at least one assignment");
    let idents = stmts
        .into_iter()
        .map(|s| match s {
            Stmt::Let(ident, expr) => (ident, expr),
        })
        .collect::<FxHashMap<Ident<'s>, Expr<'s>>>();

    let Stmt::Let(name, entrypoint) = last_assignment;

    let ast = lower_ast(entrypoint, &RefCell::new(idents), Ctx::new());
    return ast;
}

fn lower_ast<'s>(
    expr: Expr<'s>,
    idents: &RefCell<FxHashMap<&str, Expr<'s>>>,
    ctx: Ctx,
) -> LAST<'s> {
    match expr {
        Expr::String(s) => (LASTNode::String(s), None),
        Expr::Directive(d) => (LASTNode::Directive(d), None),
        Expr::Prefix(symbol, expr) => {
            let (choice, description) = match lower_ast(*expr, idents, ctx) {
                (LASTNode::Choice(choice), description) => (choice, description),
                (expr, description) => (
                    Choice {
                        required: true,
                        amount: ListAmount::One,
                        from: vec![(expr, description)],
                    },
                    description,
                ),
            };
            (LASTNode::Choice(choice.apply(symbol)), description)
        }
        Expr::List(list) => (
            LASTNode::Choice(Choice::default(
                list.into_iter()
                    .map(|expr| lower_ast(expr, idents, ctx.clone()))
                    .collect(),
            )),
            None,
        ),
        Expr::Described(expr, description) => {
            let l_ast = lower_ast(*expr, idents, ctx.clone()).0;
            let description = lower_ast(*description, idents, ctx);

            if let LASTNode::String(desc_str) = description.0 {
                if description.1.is_some() {
                    panic!("description can't have a description")
                }
                return (l_ast, Some(desc_str));
            } else {
                panic!("description must reduce to a string")
            }
        }
        Expr::Ident(ident) => {
            // TODO: replace expect with proper syntax errors
            // TODO: avoid clones with copy on write
            // TODO: avoid refcell? needed to pass idents map into both arms unless we switch to recursive
            lower_ast(
                idents
                    .borrow()
                    .get(ident)
                    // TODO: check for cycle! non turing complete and immutable so any recurrence is a cycle
                    // TODO: replace ident with value as optimization? need to handle differing context
                    .expect(&format!("ident {ident} is defined"))
                    .clone(),
                idents,
                ctx.visit(ident),
            )
        }
        Expr::Infix(lhs, infix, rhs) => {
            // TODO: make this not suck
            match (
                lower_ast(*lhs, idents, ctx.clone()),
                // TODO: avoid this clone
                infix.clone(),
                lower_ast(*rhs, idents, ctx),
            ) {
                (
                    (LASTNode::Choice(mut lhs), _),
                    InfixSymbol::Concat | InfixSymbol::SetMinus,
                    (LASTNode::Choice(rhs), _),
                ) => {
                    lhs.from = match infix {
                        InfixSymbol::SetMinus => {
                            let rhs = rhs.from.into_iter().collect::<FxHashSet<LAST<'_>>>();
                            lhs.from.into_iter().filter(|e| !rhs.contains(e)).collect()
                        }
                        InfixSymbol::Concat => {
                            lhs.from.extend(rhs.from);
                            lhs.from
                        }
                        InfixSymbol::Dependent => unreachable!(),
                    };

                    (LASTNode::Choice(lhs), None)
                }

                (lhs, InfixSymbol::Concat | InfixSymbol::SetMinus, rhs) => {
                    // TODO: use proper spanned error, report type ect
                    panic!(
                        "list infix operators should only be used on lists, found {lhs:?} and {rhs:?}"
                    )
                }
                (lhs, InfixSymbol::Dependent, rhs) => (
                    LASTNode::Dependant {
                        when: Box::new(lhs),
                        then: Box::new(rhs),
                    },
                    None,
                ),
            }
        }
    }
}
