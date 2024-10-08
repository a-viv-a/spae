use std::cell::RefCell;

use crate::ast::*;
use rustc_hash::{FxHashMap, FxHashSet};

pub enum ListAmount {
    One,
    Some,
    All,
}

/// Lowered Abstract Syntax Tree
pub enum LASTNode<'s> {
    String(&'s str),
    Directive(&'s str),
    Choice {
        from: Vec<LAST<'s>>,
        // false=Maybe, true=Required
        required: bool,
        amount: ListAmount,
    },
    Dependant {
        when: Box<LAST<'s>>,
        then: Box<LAST<'s>>,
    },
}
pub type LAST<'s> = (LASTNode<'s>, Option<&'s str>);

pub fn lower<'s>(stmts: Vec<Stmt<'s>>) {
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

    let ast = lower_ast(entrypoint, &RefCell::new(idents));
}

fn lower_ast<'s>(expr: Expr<'s>, idents: &RefCell<FxHashMap<&str, Expr<'s>>>) -> LAST<'s> {
    match expr {
        Expr::String(s) => (LASTNode::String(s), None),
        Expr::Directive(d) => (LASTNode::Directive(d), None),
        Expr::Described(expr, description) => {
            let l_ast = lower_ast(*expr, idents).0;
            let description = lower_ast(*description, idents);

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
                    .expect("ident is defined")
                    .clone(),
                idents,
            )
        }
        Expr::Infix(lhs, infix, rhs) => {
            match (*lhs, infix, *rhs) {
                (Expr::List(mut lhs), InfixSymbol::Concat, Expr::List(rhs)) => {
                    lhs.extend(rhs);
                    lower_ast(Expr::List(lhs), idents)
                }

                (Expr::List(lhs), InfixSymbol::SetMinus, Expr::List(rhs)) => {
                    let rhs = rhs.into_iter().collect::<FxHashSet<Expr<'_>>>();
                    lower_ast(
                        Expr::List(lhs.into_iter().filter(|e| rhs.contains(e)).collect()),
                        idents,
                    )
                }
                (_lhs, InfixSymbol::Concat | InfixSymbol::SetMinus, _rhs) => {
                    // TODO: use proper spanned error, report type ect
                    panic!("list infix operators should only be used on lists")
                }
                (lhs, infix, rhs) => (
                    LASTNode::Dependant {
                        when: Box::new(lower_ast(lhs, idents)),
                        then: Box::new(lower_ast(rhs, idents)),
                    },
                    None,
                ),
            }
        }
    }
}
