use crate::parser::AST;
use crate::parser::{Expr, Span, Stmt, ParsedValue};
use crate::bytecode::ByteCode;
use super::raw_value::RawValue;
use super::ir::IR;
use super::ir_func::IRFunc;

use std::collections::HashMap;

pub type TempID = usize;
pub type SymID = usize;
pub type FuncID = usize;
pub type LabelID = usize;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum VarID {
    Temp(TempID),
    Local(SymID),
    Global(SymID),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    pub(super) id: VarID,
    pub(super) next_use: Option<usize>,
    pub(super) live: bool,
}

impl Var {
    pub fn new(id: VarID) -> Self {
        match id {
            VarID::Temp(_) => Self {
                id,
                next_use: None,
                live: false,
            },
            _ => Self {
                id,
                next_use: None,
                live: true,
            },
        }
    }
}

struct RawFunc {
    id: FuncID,
    code: Vec<ByteCode>,
    locals: Vec<RawValue>
}

impl RawFunc {
    pub fn new(func: IRFunc) -> Self {
        let id = func.id;
        let mut locals = vec![];
        let mut code = vec![];
        let blocks = func.into_blocks();

        for block in blocks.iter() {
            block.compile(&mut code, &mut locals);
        }

        Self {
            id,
            code,
            locals
        }
    }
}

pub struct Program {
    funcs: HashMap<usize, RawFunc>,
}

pub struct Generator {
    func_stack: Vec<IRFunc>,
    func_counter: usize,
    temp_counter: usize,
    funcs: HashMap<usize, RawFunc>,
}

impl Generator {
    pub fn new() -> Self {
        Self {
            func_stack: vec![],
            funcs: HashMap::new(),
            temp_counter: 0,
            func_counter: 0,
        }
    }

    pub fn gen_program(mut self, ast: AST) -> Program {
        self.push_new_func();
        self.generate(ast.stmts);
        self.pop_func();

        Program { funcs: self.funcs }
    }

    fn generate(&mut self, stmts: Vec<Stmt>) {
        for stmt in stmts.into_iter() {
            match stmt {
                Stmt::Expr(expr) => {
                    self.generate_expr(*expr);
                    self.temp_counter = 0;
                }
                Stmt::Return(expr) => {
                    let var = self.generate_expr(*expr);

                    self.push_ir(IR::Return { src: var.val }, var.span);
                    self.temp_counter = 0;
                }
                Stmt::Log(expr) => {
                    let var = self.generate_expr(*expr);
                    let ir = IR::Log { src: var.val };

                    self.push_ir(ir, var.span);
                    self.temp_counter = 0;
                }
                Stmt::Assign { dest, src } => match dest.val {
                    Expr::Value(value) => {
                        if let RawValue::Var(dest) = self.generate_value(value, dest.span) {
                            let src = self.generate_expr(*src);
                            let ir = IR::Load { dest, src: src.val };

                            self.push_ir(ir, src.span);
                            self.temp_counter = 0;
                        }
                    }
                    Expr::Access { store, key } => {
                        let span = store.span;
                        let obj = self.generate_expr(*store).val;
                        let key = RawValue::Var(Var::new(VarID::Local(key)));
                        let val = self.generate_expr(*src).val;
                        let ir = IR::ObjStore { obj, key, val };

                        self.push_ir(ir, span);
                        self.temp_counter = 0;
                    }
                    Expr::Index { store, key } => {
                        let span = store.span;
                        let obj = self.generate_expr(*store).val;
                        let key = self.generate_expr(*key).val;
                        let val = self.generate_expr(*src).val;
                        let ir = IR::ObjStore { obj, key, val };

                        self.push_ir(ir, span);
                        self.temp_counter = 0;
                    }
                    _ => panic!("GENERATOR ERROR: assigning to non lvalue"),
                },
                Stmt::While { cond, stmts } => {
                    let label_start = self.gen_label();
                    let label_end = self.gen_label();
                    let cond = self.generate_expr(*cond);

                    self.push_ir(IR::Label { id: label_start }, (0, 0));
                    self.push_ir(
                        IR::Jnt {
                            cond: cond.val,
                            label: label_end,
                        },
                        cond.span,
                    );
                    self.temp_counter = 0;
                    self.push_label(label_start);
                    self.generate(stmts);
                    self.push_ir(IR::Jump { label: label_start }, (0, 0));
                    self.push_ir(IR::Label { id: label_end }, (0, 0));
                    self.pop_label();
                }
                Stmt::If { cond, stmts } => {
                    let cond = self.generate_expr(*cond);
                    let label = self.gen_label();

                    self.push_ir(
                        IR::Jnt {
                            cond: cond.val,
                            label,
                        },
                        cond.span,
                    );
                    self.temp_counter = 0;
                    self.generate(stmts);
                    self.push_ir(IR::Label { id: label }, (0, 0));
                }
                Stmt::IfElse {
                    cond,
                    stmts,
                    else_stmts,
                } => {
                    let cond = self.generate_expr(*cond);
                    let else_start = self.gen_label();
                    let else_end = self.gen_label();

                    self.push_ir(
                        IR::Jnt {
                            cond: cond.val,
                            label: else_start,
                        },
                        cond.span,
                    );
                    self.temp_counter = 0;
                    self.generate(stmts);
                    self.push_ir(IR::Jump { label: else_end }, (0, 0));
                    self.push_ir(IR::Label { id: else_start }, (0, 0));
                    self.generate(else_stmts);
                    self.push_ir(IR::Label { id: else_end }, (0, 0));
                }
                Stmt::Continue => {
                    let label = self.top_label();
                    let jmp = IR::Jump { label };

                    self.push_ir(jmp, (0, 0));
                }
                Stmt::Break => {
                    let label = self.top_label();
                    let jmp = IR::Jump { label };

                    self.push_ir(jmp, (0, 0));
                }
            }
        }
    }

    fn generate_expr(&mut self, spanned_expr: Span<Expr>) -> Span<RawValue> {
        let expr = spanned_expr.val;
        let span = spanned_expr.span;

        match expr {
            Expr::Value(value) => Span::new(self.generate_value(value, span), span),
            Expr::Binop { lhs, op, rhs } => {
                let lhs = self.generate_expr(*lhs).val;
                let rhs = self.generate_expr(*rhs).val;
                let dest = self.get_temp();
                let binop = IR::Binop {
                    dest,
                    lhs,
                    rhs,
                    op: op.clone(),
                };

                self.push_ir(binop, span);

                Span::new(RawValue::Var(dest), span)
            }
            Expr::Access { store, key } => {
                let obj = self.generate_expr(*store).val;
                let key = RawValue::Var(Var::new(VarID::Local(key)));
                let dest = self.get_temp();
                let obj_load = IR::ObjLoad { dest, obj, key };

                self.push_ir(obj_load, span);

                Span::new(RawValue::Var(dest), span)
            }
            Expr::Index { store, key } => {
                let obj = self.generate_expr(*store).val;
                let key = self.generate_expr(*key).val;
                let dest = self.get_temp();
                let obj_load = IR::ObjLoad { dest, obj, key };

                self.push_ir(obj_load, span);

                Span::new(RawValue::Var(dest), span)
            }
            Expr::Call { calle, input } => {
                let dest = self.get_temp();
                let calle = self.generate_expr(*calle).val;
                let input = if input.is_none() {
                    RawValue::Null
                } else {
                    self.generate_expr(*input.unwrap()).val
                };
                let ir = IR::Call { dest, calle, input };

                self.push_ir(ir, span);

                Span::new(RawValue::Var(dest), span)
            }
        }
    }

    fn generate_value(&mut self, value: ParsedValue, span: (usize, usize)) -> RawValue {
        match value {
            ParsedValue::Null => RawValue::Null,
            ParsedValue::Int(i) => RawValue::Int(i),
            ParsedValue::Float(f) => RawValue::Float(f),
            ParsedValue::Ident(id) => RawValue::Var(Var::new(VarID::Local(id))),
            ParsedValue::Global(id) => RawValue::Var(Var::new(VarID::Global(id))),
            ParsedValue::Bool(b) => RawValue::Bool(b),
            ParsedValue::String(s) => RawValue::String(s.clone()),
            ParsedValue::List(list) => {
                let dest = self.get_temp();
                let ir = IR::NewList { dest };

                self.push_ir(ir, span);

                for (i, expr) in list.into_iter().enumerate() {
                    let item = self.generate_expr(expr);
                    let ir = IR::ObjStore {
                        obj: RawValue::Var(dest),
                        key: RawValue::Int(i as isize),
                        val: item.val,
                    };
                    self.push_ir(ir, span);
                }

                RawValue::Var(dest)
            }
            ParsedValue::Map(map) => {
                let dest = self.get_temp();
                let ir = IR::NewMap { dest };

                self.push_ir(ir, span);

                for (key, val) in map.into_iter() {
                    let key = self.generate_expr(key).val;
                    let val = self.generate_expr(val).val;
                    let ir = IR::ObjStore {
                        obj: RawValue::Var(dest),
                        key,
                        val,
                    };

                    self.push_ir(ir, span);
                }

                RawValue::Var(dest)
            }
            ParsedValue::Func { stmts } => {
                let temp_counter = self.temp_counter;
                self.temp_counter = 0;

                self.push_new_func();
                self.generate(stmts);

                self.temp_counter = temp_counter;

                let func_val = self.pop_func();
                let dest = self.get_temp();
                let load = IR::Load {
                    dest,
                    src: func_val,
                };

                self.push_ir(load, span);

                RawValue::Var(dest)
            }
        }
    }

    fn pop_func(&mut self) -> RawValue {
        let mut func = self.func_stack.pop().unwrap();
        let end_return = IR::Return { src: RawValue::Null };
        let func_val = RawValue::Func(func.id);

        func.code.push(Span::new(end_return, (0, 0)));


        self.funcs.insert(func.id, RawFunc::new(func));

        func_val
    }

    fn push_new_func(&mut self) {
        let func = IRFunc::new(self.func_counter);

        self.func_counter += 1;
        self.func_stack.push(func);
    }

    fn push_label(&mut self, label: usize) {
        self.get_func().push_label(label);
    }

    fn pop_label(&mut self) {
        self.get_func().pop_label();
    }

    fn push_ir(&mut self, ir: IR, span: (usize, usize)) {
        self.get_func().code.push(Span::new(ir, span));
    }

    fn top_label(&self) -> LabelID {
        self.func_stack.last().unwrap().top_label()
    }

    fn get_func(&mut self) -> &mut IRFunc {
        self.func_stack.last_mut().unwrap()
    }

    fn gen_label(&mut self) -> usize {
        self.get_func().gen_label()
    }

    fn get_temp(&mut self) -> Var {
        let temp = Var::new(VarID::Temp(self.temp_counter));

        self.temp_counter += 1;
        temp
    }
}
