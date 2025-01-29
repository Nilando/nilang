use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Config {
    /// File to be run (exclude for repl)
    pub file: Option<String>,
}