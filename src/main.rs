use std::{
    fmt::Display,
    fs,
    process::{exit, ExitCode},
};

use camino::Utf8PathBuf;
use clap::{Parser, Subcommand};
use lower::lower;
use parse::stmts;
use winnow::Parser as WinnowParse;

mod ast;
#[cfg(test)]
mod ast_macros;
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

fn main() -> ExitCode {
    let args = Args::parse();
    // let output = stmts.parse_next(&mut input).unwrap();
    // println!("{input}\n------\n{output:#?}\n------\n");
    // let lowered = lower(output.clone());
    // println!("{}", lowered.format());
    match args.command {
        Command::Debug { path, show } => {
            let file = fs::read_to_string(path).expect("valid path");
            let parsed = stmts.parse(file.as_str());
            match parsed {
                Ok(parsed) => match show {
                    DebugDetail::Ast => {
                        println!("{parsed:#?}");
                    }
                    DebugDetail::Lower => {
                        println!("{}", lower(parsed).format());
                    }
                },
                Err(error) => {
                    eprintln!("{error}");
                    return ExitCode::FAILURE;
                }
            }
        }
    }
    return ExitCode::SUCCESS;
}
