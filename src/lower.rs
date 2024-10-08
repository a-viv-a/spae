use std::cell::RefCell;

use crate::ast::*;
use rustc_hash::{FxHashMap, FxHashSet};

pub enum ListAmount {
    One,
    Some,
    All,
}

/// Lowered Abstract Syntax Tree
pub enum LAST<'s> {
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

fn lower_ast<'s>(expr: Expr<'s>, idents: &RefCell<FxHashMap<&str, Expr<'s>>>) -> Expr<'s> {
    match expr {
        Expr::Ident(ident) => {
            // TODO: replace expect with proper syntax errors
            // TODO: avoid clones with copy on write
            // TODO: avoid refcell? needed to pass idents map into both arms unless we switch to recursive
            lower_ast(
                idents
                    .borrow()
                    .get(ident)
                    .expect("ident is defined")
                    .clone(),
                idents,
            )
        }
        Expr::Infix(lhs, infix, rhs) => {
            match (lower_ast(*lhs, idents), infix, lower_ast(*rhs, idents)) {
                (Expr::List(mut lhs), InfixSymbol::Concat, Expr::List(rhs)) => {
                    lhs.extend(rhs);
                    Expr::List(lhs)
                }
                (Expr::List(lhs), InfixSymbol::SetMinus, Expr::List(rhs)) => {
                    let rhs = rhs.into_iter().collect::<FxHashSet<Expr<'_>>>();
                    Expr::List(lhs.into_iter().filter(|e| rhs.contains(e)).collect())
                }
                (_lhs, InfixSymbol::Concat | InfixSymbol::SetMinus, _rhs) => {
                    // TODO: use proper spanned error, report type ect
                    panic!("list infix operators should only be used on lists")
                }
                (lhs, infix, rhs) => Expr::Infix(Box::new(lhs), infix, Box::new(rhs)),
            }
        } // Expr::v => v,
        v => v,
    }
}
