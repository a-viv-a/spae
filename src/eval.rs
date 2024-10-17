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
    from: Vec<Node>,
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

    fn default(from: Vec<Node>) -> Self {
        Choice {
            from,
            required: true,
            amount: ListAmount::One,
        }
    }
}

/// The types a given expression can evaluate to
#[derive(Debug, Clone, Hash, PartialEq, Eq, Steel)]
pub enum Type {
    String(String),
    Directive(String),
    Choice(Choice),
    Dependant { when: Box<Node>, then: Box<Node> },
}

/// A type + an optional description
#[derive(Debug, Clone, Hash, PartialEq, Eq, Steel)]
pub struct Node {
    ty: Type,
    description: Option<String>,
}

impl Node {
    fn new(ty: Type) -> Self {
        Node {
            ty,
            description: None,
        }
    }

    fn described(ty: Type, description: String) -> Self {
        Node {
            ty,
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
        let rep = match &self.ty {
            // TODO: handle internal braces \ graves, and put min number wrapping
            Type::String(s) => format!("`{s}`"),
            Type::Directive(d) => format!("{{{d}}}"),
            Type::Choice(Choice {
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
            Type::Dependant { when, then } => {
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

pub fn eval<'s>(stmts: Vec<Stmt<'s>>) -> Node {
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

    let ast = eval_expr(entrypoint, &RefCell::new(idents), Ctx::new());
    return ast;
}

fn eval_expr<'s>(expr: Expr<'s>, idents: &RefCell<FxHashMap<&str, Expr<'s>>>, ctx: Ctx) -> Node {
    match expr {
        Expr::String(s) => Node::new(Type::String(s.to_string())),
        Expr::Directive(d) => Node::new(Type::Directive(d.to_string())),
        Expr::Prefix(symbol, expr) => {
            let (choice, description) = match eval_expr(*expr, idents, ctx) {
                Node {
                    ty: Type::Choice(choice),
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
            Node {
                ty: Type::Choice(choice.apply(symbol)),
                description,
            }
        }
        Expr::List(list) => Node::new(Type::Choice(Choice::default(
            list.into_iter()
                .map(|expr| eval_expr(expr, idents, ctx.clone()))
                .collect(),
        ))),
        Expr::Described(expr, description) => {
            let l_ast = eval_expr(*expr, idents, ctx.clone()).ty;
            let description = eval_expr(*description, idents, ctx);

            if let Type::String(desc_str) = description.ty {
                if description.description.is_some() {
                    panic!("description can't have a description")
                }
                return Node::described(l_ast, desc_str);
            } else {
                panic!("description must reduce to a string")
            }
        }
        Expr::Ident(ident) => {
            // TODO: replace expect with proper syntax errors
            // TODO: avoid clones with copy on write
            // TODO: avoid refcell? needed to pass idents map into both arms unless we switch to recursive
            eval_expr(
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
                eval_expr(*lhs, idents, ctx.clone()),
                // TODO: avoid this clone
                infix.clone(),
                eval_expr(*rhs, idents, ctx),
            ) {
                (
                    Node {
                        ty: Type::Choice(mut lhs),
                        description: _,
                    },
                    InfixSymbol::Concat | InfixSymbol::SetMinus,
                    Node {
                        ty: Type::Choice(rhs),
                        description: _,
                    },
                ) => {
                    lhs.from = match infix {
                        InfixSymbol::SetMinus => {
                            let rhs = rhs.from.into_iter().collect::<FxHashSet<Node>>();
                            lhs.from.into_iter().filter(|e| !rhs.contains(e)).collect()
                        }
                        InfixSymbol::Concat => {
                            lhs.from.extend(rhs.from);
                            lhs.from
                        }
                        InfixSymbol::Dependent => unreachable!(),
                    };

                    Node::new(Type::Choice(lhs))
                }

                (lhs, InfixSymbol::Concat | InfixSymbol::SetMinus, rhs) => {
                    // TODO: use proper spanned error, report type ect
                    panic!(
                        "list infix operators should only be used on lists, found {lhs:?} and {rhs:?}"
                    )
                }
                (lhs, InfixSymbol::Dependent, rhs) => Node::new(Type::Dependant {
                    when: Box::new(lhs),
                    then: Box::new(rhs),
                }),
            }
        }
    }
}
