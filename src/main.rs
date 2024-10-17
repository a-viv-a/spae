use std::{fmt::Display, fs};

use crate::eval::eval;
use crate::parse::parse;
use camino::Utf8PathBuf;
use clap::{Parser, Subcommand};
use compile::compile;
use eyre::Result;

mod ast;
#[cfg(test)]
mod ast_macros;
mod compile;
mod eval;
mod parse;

#[derive(Debug, Parser)]
pub struct Args {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Debug the evaluation of a spae file
    Debug {
        /// The path to read from
        path: Utf8PathBuf,
        /// The detail to show
        #[clap(long, short = 's', default_value_t = DebugDetail::Ast)]
        show: DebugDetail,
    },
    /// Compile a spae file
    Compile {
        /// The path of the spae file
        path: Utf8PathBuf,
        /// The path of the scm file to use to compile it
        compiler_path: Utf8PathBuf,
    },
}

#[derive(Debug, Clone, Copy, clap::ValueEnum)]
enum DebugDetail {
    Ast,
    Eval,
}

impl Display for DebugDetail {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                DebugDetail::Ast => "ast",
                DebugDetail::Eval => "eval",
            }
        )
    }
}

fn main() -> Result<()> {
    let args = Args::parse();
    // let output = stmts.parse_next(&mut input).unwrap();
    // println!("{input}\n------\n{output:#?}\n------\n");
    // let lowered = lower(output.clone());
    // println!("{}", lowered.format());

    match args.command {
        Command::Debug { path, show } => {
            let file = fs::read_to_string(path).expect("valid path");
            match parse(file.as_str()) {
                Ok(parsed) => match show {
                    DebugDetail::Ast => {
                        println!("{parsed:#?}");
                    }
                    DebugDetail::Eval => {
                        println!("{}", eval(parsed).format());
                    }
                },
                Err(err) => err.write_stderr().expect("no io issue"),
            }
        }
        Command::Compile {
            path,
            compiler_path,
        } => {
            let spae_file = fs::read_to_string(path).expect("valid path");
            match parse(&*spae_file).map(eval) {
                Ok(l_ast) => {
                    compile(compiler_path, l_ast).expect("compiled correctly");
                }
                Err(err) => err.write_stderr().expect("no io issue"),
            }
        }
    }
    Ok(())
}
