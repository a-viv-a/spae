use std::{error::Error, fmt::Display, fs, path::PathBuf, process::ExitCode};

use crate::parse::parse;
use camino::Utf8PathBuf;
use clap::{Parser, Subcommand};
use compile::evaluate;
use eyre::{Result, WrapErr};
use lower::lower;
use winnow::{
    error::{ContextError, ParseError},
    Parser as WinnowParse,
};

mod ast;
#[cfg(test)]
mod ast_macros;
mod compile;
mod lower;
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
        compiler_path: PathBuf,
    },
}

#[derive(Debug, Clone, Copy, clap::ValueEnum)]
enum DebugDetail {
    Ast,
    Lower,
}

impl Display for DebugDetail {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                DebugDetail::Ast => "ast",
                DebugDetail::Lower => "lower",
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
                    DebugDetail::Lower => {
                        println!("{}", lower(parsed).format());
                    }
                },
                Err(err) => err.write_stderr().expect("no io issue"),
            }
        }
        Command::Compile {
            path,
            compiler_path,
        } => {
            // let spae_file = fs::read_to_string(path).expect("valid path");
            // let ast = parse(&*spae_file)?;
            // let l_ast = lower(ast);
            // evaluate(compiler_path, l_ast);
        }
    }
    Ok(())
}

fn run_command<'a>(command: Command) -> Result<(), ParseError<&'a str, ContextError>> {
    Ok(())
}
