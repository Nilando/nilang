mod driver;
mod parser;
mod symbol_map;
mod tac;

use clap::Parser as CliParser;
use driver::{execute, Config};

fn main() {
    let config = Config::parse();

    execute(config);
}
