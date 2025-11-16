use nilang::{
    Runtime, 
    Config,
    SymbolMap,
    InterpreterError,
    run_repl,
};

use clap::Parser as CliParser;

fn main() {
    let mut config = Config::parse();

    let runtime =
    match Runtime::init(SymbolMap::new(), config.clone()) {
        Ok(runtime) => runtime,
        Err(err) => {
            let err_msg = err.render();

            eprintln!("{}", err_msg);
            return;
        }
    };

    if config.repl_mode() {
        run_repl(runtime);
    } else if let Some(path) = config.get_source_path() {
        match run_script(runtime, path) {
            Ok(()) => {},
            Err(err) => {
                let err_msg = err.render();

                eprintln!("{}", err_msg);
            }
        }
    } else if let Some(source) = config.get_script() {
        match run_inline(runtime, source) {
            Ok(()) => {},
            Err(err) => {
                let err_msg = err.render();

                eprintln!("{}", err_msg);
            }
        }
    }
}

fn run_script(mut runtime: Runtime, path: String) -> Result<(), InterpreterError> {
    runtime.load_module(&path)?;

    runtime.run()
}

fn run_inline(mut runtime: Runtime, source: String) -> Result<(), InterpreterError> {
    runtime.load_inline(&source)?;

    runtime.run()
}
