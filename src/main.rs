mod driver;
mod generator;
mod parser;
mod symbol_map;
mod vm;

use clap::Parser as CliParser;
use driver::{execute, Config};

fn main() {
    let config = Config::parse();

    execute(config);
}
