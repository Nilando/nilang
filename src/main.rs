use nilang::{
    Runtime, 
    Config,
    SymbolMap,
    InterpreterError,
    run_repl,
};

use clap::Parser as CliParser;

fn main() {
    let config = Config::parse();
    let source_path = config.get_source_path();
    let runtime =
    match Runtime::init(SymbolMap::new(), config) {
        Ok(runtime) => runtime,
        Err(err) => {
            let err_msg = err.render();

            eprintln!("{}", err_msg);
            return;
        }
    };

    if let Some(path) = source_path {
        match run_script(runtime, path) {
            Ok(()) => {},
            Err(err) => {
                let err_msg = err.render();

                eprintln!("{}", err_msg);
            }
        }
    } else {
        run_repl(runtime);
    }
}

fn run_script(mut runtime: Runtime, path: String) -> Result<(), InterpreterError> {
    runtime.load_module(&path)?;

    runtime.run()
}
