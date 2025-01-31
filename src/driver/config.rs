use clap::{Parser, ArgGroup};
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
#[clap(group(ArgGroup::new("input").args(&["file", "inline", "stdin"])))]
pub struct Config {
    /// Optionally output AST to stdout or to a file if provided
    #[clap(short = 'a', long = "ast_output", requires = "input", default_missing_value = "stdout")]
    pub ast_output_path: Option<PathBuf>,

    /// Optionally output bytecode to stdout or to a file if provided
    #[clap(short = 'b', long = "bytecode_output", requires = "input", default_missing_value = "stdout")]
    pub bytecode_output_path: Option<PathBuf>,

    /// Optionally read program as an arg
    #[clap(short, long, conflicts_with_all = &["file", "stdin"])]
    pub inline: Option<String>,

    /// Optionally read program from stdin
    #[clap(short, long, conflicts_with_all = &["file", "inline"])]
    pub stdin: bool,

    /// Only build AST and bytecode.
    #[clap(short, long)]
    pub dry_run: bool,

    /// File to be run (exclude for repl)
    pub file: Option<String>,
}

impl TryFrom<Vec<&str>> for Config {
    type Error = String;
    fn try_from(args: Vec<&str>) -> Result<Self, Self::Error> {
        match Config::try_parse_from(args) {
            Ok(config) => Ok(config),
            Err(e) => Err(e.to_string()),
        }
    }
}
