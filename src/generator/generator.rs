use crate::parser::{Stmt, Expr, Span, Value};
use crate::lexer::Op;

use std::collections::HashMap;

#[derive(Debug, Copy, Clone)]
pub enum IRVar {
    Temp(usize),
    Ident(usize),
    Global(usize)
}

#[derive(Debug)]
pub enum IRValue {
    Var(IRVar),
    String(String),
    Float(f64),
    Int(isize),
    Bool(bool),
    Func(usize),
    Null,
}

#[derive(Debug)]
pub enum IRCode {
    Binop {
        dest: IRVar,
        lhs: IRValue,
        op: Op,
        rhs: IRValue,
    },
    ObjStore {
        store: IRValue,
        key: IRValue,
        src: IRValue,
    },
    ObjLoad {
        dest: IRVar,
        store: IRValue,
        key: IRValue,
    },
    Call {
        dest: IRVar,
        src: IRValue,
        input: IRValue,
    },
    Return {
        dest: IRValue,
    },
    Log {
        dest: IRValue,
    },
    Load {
        dest: IRVar,
        src: IRValue,
    },
    Jnt {
        label: usize,
        cond: IRValue,
    },
    Jump {
        label: usize,
    },
    Label {
        id: usize,
    },
    NewList {
        dest: IRVar,
        items: Vec<IRValue>,
    },
    NewMap {
        dest: IRVar,
        items: Vec<(IRValue, IRValue)>,
    }
}

#[derive(Debug)]
pub struct IRFunc {
    pub(super) id: usize,
    pub(super) code: Vec<Span<IRCode>>,
    label_counter: usize,
    label_stack: Vec<usize>
}

#[derive(Debug)]
pub struct IRGenerator {
    func_stack: Vec<IRFunc>,
    func_counter: usize,
    temp_counter: usize,
    pub funcs: HashMap<usize, IRFunc>
}

impl IRGenerator {
    pub fn new() -> Self {
        Self {
            func_stack: vec![],
            funcs: HashMap::new(),
            temp_counter: 0,
            func_counter: 0,
        }
    }

    pub fn gen_program(&mut self, stmts: Vec<Stmt>) {
        self.push_new_func();
        self.generate(stmts);
        self.pop_func();
    }

    pub fn generate(&mut self, stmts: Vec<Stmt>) {
        for stmt in stmts.into_iter() {
            match stmt {
                Stmt::Expr(expr) => {
                    self.generate_expr(*expr);
                    self.temp_counter = 0;
                }
                Stmt::Return(expr) => {
                    let var = self.generate_expr(*expr);
                    let instr = IRCode::Return { dest: var.val };

                    self.push_instr(instr, var.span);
                    self.temp_counter = 0;
                }
                Stmt::Log(expr) => {
                    let var = self.generate_expr(*expr);
                    let instr = IRCode::Log { dest: var.val };

                    self.push_instr(instr, var.span);
                    self.temp_counter = 0;
                }
                Stmt::Assign { dest, src } => {
                    match dest.val {
                        Expr::Value(value) => {
                            if let IRValue::Var(dest) = self.generate_value(value, dest.span) {
                                let src = self.generate_expr(*src);
                                let instr = IRCode::Load { dest, src: src.val };

                                self.push_instr(instr, src.span);
                                self.temp_counter = 0;
                            }
                        }
                        Expr::Access { store, key } => {
                            let span = store.span;
                            let store = self.generate_expr(*store).val;
                            let key = IRValue::Var(IRVar::Ident(key));
                            let src = self.generate_expr(*src).val;
                            let instr = IRCode::ObjStore { store, key, src};

                            self.push_instr(instr, span);
                            self.temp_counter = 0;
                        }
                        Expr::Index { store, key } => {
                            let span = store.span;
                            let store = self.generate_expr(*store).val;
                            let key = self.generate_expr(*key).val;
                            let src = self.generate_expr(*src).val;
                            let instr = IRCode::ObjStore { store, key, src};

                            self.push_instr(instr, span);
                            self.temp_counter = 0;
                        }
                        _ => panic!("GENERATOR ERROR: assigning to non lvalue"),
                    }

                }
                Stmt::While { cond, stmts } => {
                    let label_start = self.get_new_label();
                    let label_end = self.get_new_label();
                    let cond = self.generate_expr(*cond);

                    self.push_instr(IRCode::Label { id: label_start }, (0, 0));
                    self.push_instr(IRCode::Jnt { cond: cond.val, label: label_end }, cond.span);
                    self.temp_counter = 0;
                    self.push_label(label_start);
                    self.generate(stmts);
                    self.push_instr(IRCode::Jump { label: label_start }, (0, 0));
                    self.push_instr(IRCode::Label { id: label_end }, (0, 0));
                    self.pop_label();
                }
                Stmt::If { cond, stmts } => {
                    let cond = self.generate_expr(*cond);
                    let label = self.get_new_label();

                    self.push_instr(IRCode::Jnt { cond: cond.val, label }, cond.span);
                    self.temp_counter = 0;
                    self.generate(stmts);
                    self.push_instr(IRCode::Label { id: label }, (0, 0));
                }
                Stmt::IfElse { cond, stmts, else_stmts } => {
                    let cond = self.generate_expr(*cond);
                    let else_start = self.get_new_label();
                    let else_end = self.get_new_label();

                    self.push_instr(IRCode::Jnt { cond: cond.val, label: else_start }, cond.span);
                    self.temp_counter = 0;
                    self.generate(stmts);
                    self.push_instr(IRCode::Jump { label: else_end }, (0, 0));
                    self.push_instr(IRCode::Label { id: else_start }, (0, 0));
                    self.generate(else_stmts);
                    self.push_instr(IRCode::Label { id: else_end }, (0, 0));
                }
                Stmt::Continue => {
                    let func = self.func_stack.last_mut().unwrap();
                    let label = *func.label_stack.last().unwrap();
                    let jmp = IRCode::Jump { label };

                    func.code.push(Span::new(jmp, (0, 0)));
                }
                Stmt::Break => {
                    let func = self.func_stack.last_mut().unwrap();
                    let label = 1 + *func.label_stack.last().unwrap();
                    let jmp = IRCode::Jump { label };

                    func.code.push(Span::new(jmp, (0, 0)));
                }
            }
        }
    }

    fn generate_expr(&mut self, spanned_expr: Span<Expr>) -> Span<IRValue> {
        let expr = spanned_expr.val;
        let span = spanned_expr.span;

        match expr {
            Expr::Value(value) => {
                Span::new(self.generate_value(value, span), span)
            }
            Expr::Binop { lhs, op, rhs } => {
                let lhs = self.generate_expr(*lhs).val;
                let rhs = self.generate_expr(*rhs).val;
                let dest = self.get_temp();
                let binop = IRCode::Binop { dest, lhs, rhs, op: op.clone() };

                self.push_instr(binop, span);

                Span::new(IRValue::Var(dest), span)
            }
            Expr::Access { store, key } => {
                let store = self.generate_expr(*store).val;
                let key = IRValue::Var(IRVar::Ident(key));
                let dest = self.get_temp();
                let obj_load = IRCode::ObjLoad { dest, store, key};

                self.push_instr(obj_load, span);

                Span::new(IRValue::Var(dest), span)
            }
            Expr::Index { store, key } => {
                let store = self.generate_expr(*store).val;
                let key = self.generate_expr(*key).val;
                let dest = self.get_temp();
                let obj_load = IRCode::ObjLoad { dest, store, key};

                self.push_instr(obj_load, span);

                Span::new(IRValue::Var(dest), span)
            }
            Expr::Call { calle, input } => {
                let dest = self.get_temp();
                let src = self.generate_expr(*calle).val;
                let input = if input.is_none() {
                    IRValue::Null
                } else {
                    self.generate_expr(*input.unwrap()).val
                };
                let instr = IRCode::Call { dest, src, input };

                self.push_instr(instr, span);

                Span::new(IRValue::Var(dest), span)
            }
        }
    }

    fn generate_value(&mut self, value: Value, span: (usize, usize)) -> IRValue {
        match value {
            Value::Null       => IRValue::Null,
            Value::Int(i)     => IRValue::Int(i),
            Value::Float(f)   => IRValue::Float(f),
            Value::Ident(id)  => IRValue::Var(IRVar::Ident(id)),
            Value::Global(id) => IRValue::Var(IRVar::Global(id)),
            Value::Bool(b)    => IRValue::Bool(b),
            Value::String(s)  => IRValue::String(s.clone()),
            Value::List(list) => {
                let dest = self.get_temp();
                let mut items = vec![];
                for expr in list.into_iter() {
                    let item = self.generate_expr(expr);
                    items.push(item.val);
                }
                let instr = IRCode::NewList { dest, items };

                self.push_instr(instr, span);

                IRValue::Var(dest)
            }
            Value::Map(map) => {
                let dest = self.get_temp();
                let mut items = vec![];
                for entry in map.into_iter() {
                    let (key, val) = entry;
                    let key = self.generate_expr(key).val;
                    let val = self.generate_expr(val).val;
                    items.push((key, val));
                }
                let instr = IRCode::NewMap { dest, items };

                self.push_instr(instr, span);

                IRValue::Var(dest)
            }
            Value::Func { stmts } => {
                let temp_counter = self.temp_counter;
                self.temp_counter = 0;
                self.push_new_func();
                self.generate(stmts);
                self.temp_counter = temp_counter;

                let func_val = self.pop_func();
                let dest = self.get_temp();
                let load = IRCode::Load { dest, src: func_val };

                self.push_instr(load, span);

                IRValue::Var(dest)
            }
        }
    }

    fn pop_func(&mut self) -> IRValue {
        let mut func = self.func_stack.pop().unwrap();
        let end_return = IRCode::Return { dest: IRValue::Null };
        let func_val = IRValue::Func(func.id);

        func.code.push(Span::new(end_return, (0, 0)));
        self.funcs.insert(func.id, func);

        func_val
    }

    fn push_new_func(&mut self) {
        let func = IRFunc {
            id: self.func_counter,
            code: vec![],
            label_stack: vec![],
            label_counter: 0,
        };

        self.func_counter += 1;
        self.func_stack.push(func);
    }

    fn push_label(&mut self, label: usize) {
        self.get_func().label_stack.push(label);
    }

    fn pop_label(&mut self) {
        self.get_func().label_stack.pop();
    }

    fn push_instr(&mut self, instr: IRCode, span: (usize, usize)) {
        self.get_func().code.push(Span::new(instr, span));
    }

    fn get_func(&mut self) -> &mut IRFunc {
        self.func_stack.last_mut().unwrap()
    }

    fn get_new_label(&mut self) -> usize {
        let func = self.get_func();
        let label_id = func.label_counter;

        func.label_counter += 1;
        label_id
    }

    fn get_temp(&mut self) -> IRVar {
        let temp = IRVar::Temp(self.temp_counter);

        self.temp_counter += 1;
        temp
    }
}
