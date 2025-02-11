use crate::parser::stmt::Stmt;
use serde::Serialize;

#[derive(Serialize)]
pub struct AST {
    pub stmts: Vec<Stmt>,
}
