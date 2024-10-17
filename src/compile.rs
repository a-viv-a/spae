use std::fs;

use camino::Utf8PathBuf;
use eyre::Result;
use steel::rvals::IntoSteelVal;
use steel::SteelVal;
use steel::{rvals::Custom, steel_vm::engine::Engine};

use crate::eval::{Node, Type};

pub fn compile<'s>(compiler_path: Utf8PathBuf, node: Node) -> Result<SteelVal> {
    let compiler = fs::read_to_string(compiler_path)?;
    let mut engine = Engine::new();

    // Node struct
    engine.register_type::<Node>("Node?");
    // Node Type enum
    engine.register_type::<Type>("Type");

    // TODO: register constructors

    if let Err(err) = engine.compile_and_run_raw_program(compiler) {
        // TODO: spanned errors? am I supposed to render them myself or is there a library fn?
        eprintln!("{err} {:?}", err.span());
        todo!()
    }

    let result = engine.call_function_by_name_with_args("compile", vec![node.into_steelval()?]);

    dbg!(result);
    todo!()
}
