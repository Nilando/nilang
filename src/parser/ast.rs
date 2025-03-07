use serde::Serialize;
use super::stmt::Stmt;

#[derive(Serialize)]
pub struct AST {
    pub stmts: Vec<Stmt>,
}


// STMTS
// ARE MADE OF STMTS AND EXPRS
//
// EXPRS
// ARE MODE OF EXPRS AND VALUES
//
// VALUES
