use std::{fmt::Display, fs};

use crate::eval::eval;
use crate::parse::parse;
use camino::Utf8PathBuf;
use clap::{Parser, Subcommand};
use compilers::{enum_disp, Target};
use eyre::Result;

mod ast;
#[cfg(test)]
mod ast_macros;
mod compilers;
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
        /// The path to read the spae file from
        path: Utf8PathBuf,
        /// The detail to show
        #[clap(long, short = 's', default_value_t = DebugDetail::Ast)]
        show: DebugDetail,
    },
    /// Compile a spae file
    Compile {
        /// The path to read the spae file from
        path: Utf8PathBuf,
        /// Compilation target
        into: Target,
    },
}

#[derive(Debug, Clone, Copy, clap::ValueEnum)]
enum DebugDetail {
    Ast,
    Eval,
}
enum_disp!(DebugDetail {
    Ast  => "ast",
    Eval => "eval",
});

fn main() -> Result<()> {
    let args = Args::parse();
    // let output = stmts.parse_next(&mut input).unwrap();
    // println!("{input}\n------\n{output:#?}\n------\n");
    // let lowered = lower(output.clone());
    // println!("{}", lowered.format());

    match args.command {
        Command::Debug { path, show } => {
            let file = fs::read_to_string(path)?;
            match parse(file.as_str()) {
                Ok(parsed) => match show {
                    DebugDetail::Ast => {
                        println!("{parsed:#?}");
                    }
                    DebugDetail::Eval => {
                        println!("{}", eval(parsed).format());
                    }
                },
                Err(err) => err.write_stderr()?,
            }
        }
        Command::Compile { into, path } => {
            let spae_file = fs::read_to_string(path)?;
            match parse(&*spae_file).map(eval) {
                Ok(node) => match into.compile(node) {
                    Ok(out) => println!("{out}"),
                    Err(err) => err.write_stderr()?,
                },
                Err(err) => err.write_stderr()?,
            }
        }
    }
    Ok(())
}
