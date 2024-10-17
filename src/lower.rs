use std::cell::RefCell;

use crate::ast::*;
use rustc_hash::{FxHashMap, FxHashSet};
use steel_derive::Steel;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Steel)]
pub enum ListAmount {
    One,
    Some,
    All,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Steel)]
pub struct Choice {
    // false=Maybe, true=Require
    required: bool,
    amount: ListAmount,
    from: Vec<LAST>,
}

impl Choice {
    fn apply(mut self, symbol: PrefixSymbol) -> Self {
        match symbol {
            PrefixSymbol::Maybe => self.required = false,
            PrefixSymbol::Require => self.required = true,
            PrefixSymbol::One => self.amount = ListAmount::One,
            PrefixSymbol::Some => self.amount = ListAmount::Some,
            PrefixSymbol::All => self.amount = ListAmount::All,
        }
        self
    }

    fn default(from: Vec<LAST>) -> Self {
        Choice {
            from,
            required: true,
            amount: ListAmount::One,
        }
    }
}

/// Lowered Abstract Syntax Tree
#[derive(Debug, Clone, Hash, PartialEq, Eq, Steel)]
pub enum LASTNode {
    String(String),
    Directive(String),
    Choice(Choice),
    Dependant { when: Box<LAST>, then: Box<LAST> },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Steel)]
pub struct LAST {
    node: LASTNode,
    description: Option<String>,
}

impl LAST {
    fn new(node: LASTNode) -> Self {
        LAST {
            node,
            description: None,
        }
    }

    fn described(node: LASTNode, description: String) -> Self {
        LAST {
            node,
            description: Some(description),
        }
    }

    pub fn format(&self) -> String {
        macro_rules! pad {
            ($s:expr) => {
                $s.split('\n')
                    .map(|l| format!("    {l}\n"))
                    .collect::<String>()
            };
        }
        let rep = match &self.node {
            LASTNode::String(s) => format!("`{s}`"),
            LASTNode::Directive(d) => format!("{{{d}}}"),
            LASTNode::Choice(Choice {
                required,
                amount,
                from,
            }) => {
                let required = match required {
                    true => "require ",
                    false => "maybe ",
                };

                let amount = match amount {
                    ListAmount::Some => "some ",
                    ListAmount::One => "one ",
                    ListAmount::All => "all ",
                };

                format!(
                    "{required}{amount}[\n{}]",
                    pad!(from
                        .iter()
                        .map(|e| format!("{},\n", e.format().trim()))
                        .collect::<String>()
                        .trim())
                )
            }
            LASTNode::Dependant { when, then } => {
                format!("{} >\n{}", when.format(), pad!(then.format()))
            }
        };
        match &self.description {
            Some(str) => format!("{rep}\n: `{str}`"),
            None => rep,
        }
    }
}

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

pub fn lower<'s>(stmts: Vec<Stmt<'s>>) -> LAST {
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

    let Stmt::Let(_name, entrypoint) = last_assignment;

    let ast = lower_ast(entrypoint, &RefCell::new(idents), Ctx::new());
    return ast;
}

fn lower_ast<'s>(expr: Expr<'s>, idents: &RefCell<FxHashMap<&str, Expr<'s>>>, ctx: Ctx) -> LAST {
    match expr {
        Expr::String(s) => LAST::new(LASTNode::String(s.to_string())),
        Expr::Directive(d) => LAST::new(LASTNode::Directive(d.to_string())),
        Expr::Prefix(symbol, expr) => {
            let (choice, description) = match lower_ast(*expr, idents, ctx) {
                LAST {
                    node: LASTNode::Choice(choice),
                    description,
                } => (choice, description),
                node => (
                    Choice {
                        required: true,
                        amount: ListAmount::One,
                        // peel the description off... worth reassessing
                        from: vec![node],
                    },
                    None,
                ),
            };
            LAST {
                node: LASTNode::Choice(choice.apply(symbol)),
                description,
            }
        }
        Expr::List(list) => LAST::new(LASTNode::Choice(Choice::default(
            list.into_iter()
                .map(|expr| lower_ast(expr, idents, ctx.clone()))
                .collect(),
        ))),
        Expr::Described(expr, description) => {
            let l_ast = lower_ast(*expr, idents, ctx.clone()).node;
            let description = lower_ast(*description, idents, ctx);

            if let LASTNode::String(desc_str) = description.node {
                if description.description.is_some() {
                    panic!("description can't have a description")
                }
                return LAST::described(l_ast, desc_str);
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
                    LAST {
                        node: LASTNode::Choice(mut lhs),
                        description: _,
                    },
                    InfixSymbol::Concat | InfixSymbol::SetMinus,
                    LAST {
                        node: LASTNode::Choice(rhs),
                        description: _,
                    },
                ) => {
                    lhs.from = match infix {
                        InfixSymbol::SetMinus => {
                            let rhs = rhs.from.into_iter().collect::<FxHashSet<LAST>>();
                            lhs.from.into_iter().filter(|e| !rhs.contains(e)).collect()
                        }
                        InfixSymbol::Concat => {
                            lhs.from.extend(rhs.from);
                            lhs.from
                        }
                        InfixSymbol::Dependent => unreachable!(),
                    };

                    LAST::new(LASTNode::Choice(lhs))
                }

                (lhs, InfixSymbol::Concat | InfixSymbol::SetMinus, rhs) => {
                    // TODO: use proper spanned error, report type ect
                    panic!(
                        "list infix operators should only be used on lists, found {lhs:?} and {rhs:?}"
                    )
                }
                (lhs, InfixSymbol::Dependent, rhs) => LAST::new(LASTNode::Dependant {
                    when: Box::new(lhs),
                    then: Box::new(rhs),
                }),
            }
        }
    }
}
