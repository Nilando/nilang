use nilang::{execute, Config};

use clap::Parser as CliParser;

fn main() {
    let config = Config::parse();

    execute(config);
}
