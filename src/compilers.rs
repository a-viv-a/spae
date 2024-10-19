pub mod fish;
pub mod markdown;

use crate::{eval::Node, parse::SpaeReport};

macro_rules! enum_disp {
    ($enum:ident {$($key:ident => $val:expr),* $(,)?}) => {
        impl std::fmt::Display for $enum {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", match self { $($enum::$key => $val,)* })
            }
        }
    };
}
pub(crate) use enum_disp;

#[derive(Debug, Clone, Copy, clap::ValueEnum)]
pub enum Target {
    Markdown,
    Fish,
}
enum_disp!(Target {
    Markdown => "markdown",
    Fish => "fish",
});

impl Target {
    pub fn compile<'s>(&self, node: Node<'s>) -> Result<String, SpaeReport<'s>> {
        match self {
            Target::Markdown => markdown::compile(node),
            Target::Fish => fish::compile(node),
        }
    }
}
