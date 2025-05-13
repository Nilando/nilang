mod driver;
mod parser;
mod symbol_map;
mod ir;
mod codegen;

pub use driver::{execute, Config};
