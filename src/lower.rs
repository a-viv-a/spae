use crate::ast::*;
use rustc_hash::FxHashMap;

pub fn lower<'s>(stmts: Vec<Stmt<'s>>) {
    let mut stmts = stmts;
    // TODO: add some syntax for figuring out the entrypoint
    // TODO: replace expect with proper syntax errors
    let last_assignment = stmts
        .pop()
        .expect("there sholud be at least one assignment");
    let idents = stmts
        .into_iter()
        .map(|s| match s {
            Stmt::Let(ident, expr) => (ident, expr),
        })
        .collect::<FxHashMap<Ident<'s>, Expr<'s>>>();

    let Stmt::Let(name, entrypoint) = last_assignment;

    let ast = substitute_idents(entrypoint, idents);
}

fn substitute_idents<'s>(expr: Expr<'s>, idents: FxHashMap<&str, Expr<'s>>) -> Expr<'s> {
    match expr {
        Expr::Ident(ident) => {
            substitute_idents(idents.get(ident).expect("ident is defined").clone(), idents)
        }
        v => v,
    }
}
