use std::path::PathBuf;

use steel::steel_vm::engine::Engine;
use steel::{SteelErr, SteelVal};

use crate::lower::LAST;

pub fn evaluate<'s>(compiler_path: PathBuf, l_ast: LAST<'s>) -> Option<SteelVal> {
    let mut engine = Engine::new();

    engine
        .compile_and_run_raw_program_with_path(l_ast.format(), compiler_path)
        .ok()
        .and_then(|vals| vals.first().cloned())
        .clone()
}
