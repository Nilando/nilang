use serde::Serialize;
use super::stmt::Stmt;

#[derive(Serialize)]
pub struct AST {
    pub stmts: Vec<Stmt>,
}
