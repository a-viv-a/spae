use std::fs;

use camino::Utf8PathBuf;
use eyre::Result;
use steel::steel_vm::engine::Engine;
use steel::{SteelErr, SteelVal};

use crate::eval::{Node, Type};

pub fn compile<'s>(compiler_path: Utf8PathBuf, node: Node) -> Result<SteelVal> {
    let compiler = fs::read_to_string(compiler_path)?;
    let mut engine = Engine::new();

    // Node struct
    engine.register_type::<Node>("Node?");
    // Node Type enum
    engine.register_type::<Type>("Type");

    // TODO: register constructors

    engine.compile_and_run_raw_program(compiler)?;

    // let result = engine.call_function_by_name_with_args("compile", vec![node]);

    todo!()
}
