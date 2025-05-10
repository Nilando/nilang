use clap::{ArgGroup, Parser};

#[derive(Parser, Debug, Clone)]
#[command(version, about, long_about = None)]
#[clap(group(ArgGroup::new("input").args(&["file", "inline", "stdin"])))]
pub struct Config {
    /// Optionally output AST to stdout or to a file if provided
    #[clap(
        short = 'a',
        long = "ast_output",
        requires = "input",
    )]
    pub ast_output_path: Option<Option<String>>,

    /// Optionally output ir to stdout or to a file if provided
    #[clap(
        short = 'r',
        long = "ir_output",
        requires = "input",
    )]
    pub ir_output_path: Option<Option<String>>,

    /// Optionally output bytecode to stdout or to a file if provided
    #[clap(
        short = 'b',
        long = "bytecode_output",
        requires = "input",
    )]
    pub bytecode_output_path: Option<String>,

    /// Optionally read program as an arg
    #[clap(short, long, conflicts_with_all = &["file", "stdin"])]
    pub inline: Option<String>,

    /// Optionally read program from stdin
    #[clap(short, long, conflicts_with_all = &["file", "inline"])]
    pub stdin: bool,

    /// Don't execute the program only compile it.
    #[clap(short, long)]
    pub dry_run: bool,

    /// File to be run (exclude for repl)
    pub file: Option<String>,
}

impl Config {
    pub fn get_script(&mut self) -> Option<String> {
        if self.inline.is_some() {
            return self.inline.take();
        }

        if self.stdin {
            let mut input = String::new();

            std::io::stdin()
                .read_line(&mut input)
                .expect("Error reading from stdin");

            return Some(input);
        }

        if self.file.is_some() {
            return Some(
                std::fs::read_to_string(self.file.as_ref().unwrap()).expect("Failed to read file"),
            );
        }

        None
    }

    pub fn input_provided(&self) -> bool {
        self.inline.is_some() || self.stdin || self.file.is_some()
    }

    pub fn repl_mode(&self) -> bool {
        !self.input_provided()
    }
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
