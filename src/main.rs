mod generator;
mod parser;
mod vm;
mod symbol_map;
mod driver;

use driver::{Driver, Config};
use clap::Parser as CliParser;

fn main() {
    let config = Config::parse();
    let driver = Driver::new(config);

    driver.run();
}