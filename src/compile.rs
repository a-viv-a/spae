use std::path::PathBuf;

use steel::steel_vm::engine::Engine;
use steel::{SteelErr, SteelVal};

use crate::lower::{LASTNode, LAST};

pub fn evaluate<'s>(compiler_path: PathBuf, l_ast: LAST) -> Option<SteelVal> {
    let mut engine = Engine::new();

    engine.register_type::<LAST>("LAST?");
    engine.register_type::<LASTNode>("LASTNode");

    // TODO: register constructors

    engine
        .compile_and_run_raw_program_with_path(l_ast.format(), compiler_path)
        .ok();

    let result = engine.call_function_by_name_with_args("compile", vec![l_ast.into()]);

    todo!()
}
