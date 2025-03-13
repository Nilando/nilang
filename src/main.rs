mod generator;
mod parser;
mod vm;
mod symbol_map;
mod driver;

use driver::{execute, Config};
use clap::Parser as CliParser;

fn main() {
    let config = Config::parse();

    execute(config);
}
