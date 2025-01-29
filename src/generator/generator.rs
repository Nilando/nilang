use super::ir::{IR, IRConst, IRVar, VarID, FuncID, LabelID, IRFunc, IRProgram};
use super::compiler::FuncCompiler;

use std::collections::HashMap;

use crate::parser::{AST, Expr, ParsedValue, Span, Stmt};

struct FuncGenerator {
    id: FuncID,
    code: Vec<Span<IR>>,
    label_counter: usize,
    label_stack: Vec<LabelID>
}

impl FuncGenerator {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            code: vec![],
            label_stack: vec![],
            label_counter: 0,
        }
    }

    pub fn push_label(&mut self, label: LabelID) {
        self.label_stack.push(label)
    }

    pub fn pop_label(&mut self) {
        self.label_stack.pop();
    }

    pub fn top_label(&self) -> LabelID {
        *self.label_stack.last().unwrap()
    }

    pub fn gen_label(&mut self) -> LabelID {
        let label_id = self.label_counter;

        self.label_counter += 1;
        label_id
    }
}

// responsible for converting an AST into ByteCode
// first the generator creates IR code then internally passes IR code to a
// a compiler struct which converts IR into bytecode
pub struct Generator {
    func_stack: Vec<FuncGenerator>,
    temp_counter: u16,
    funcs: HashMap<FuncID, IRFunc>,
    func_counter: FuncID,
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

    pub fn gen_program(mut self, ast: AST) -> IRProgram {
        self.push_new_func();
        self.gen_stmts(ast.stmts);
        self.pop_func();

        IRProgram::new(self.funcs)
    }

    fn gen_stmts(&mut self, stmts: Vec<Stmt>) {
        for stmt in stmts.into_iter() {
            self.gen_stmt(stmt);
        }
    }

    fn gen_stmt(&mut self, stmt: Stmt) {
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
                    let dest = self.generate_value(value, dest.span);
                    let src = self.generate_expr(*src);
                    let ir = IR::Copy { dest, src: src.val };

                    self.push_ir(ir, src.span);
                    self.temp_counter = 0;
                }
                Expr::Access { store, key } => {
                    let span = store.span;
                    let obj = self.generate_expr(*store).val;

                    // TODO! this is wrong :(
                    // here we need to generate an instruction to load 
                    // a symbol into the key var
                    let key = IRVar::new(VarID::Local(key));
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
                self.gen_stmts(stmts);
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
                self.gen_stmts(stmts);
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
                self.gen_stmts(stmts);
                self.push_ir(IR::Jump { label: else_end }, (0, 0));
                self.push_ir(IR::Label { id: else_start }, (0, 0));
                self.gen_stmts(else_stmts);
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

    fn generate_expr(&mut self, spanned_expr: Span<Expr>) -> Span<IRVar> {
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
                    op,
                };

                self.push_ir(binop, span);

                Span::new(dest, span)
            }
            Expr::Access { store, key } => {
                let obj = self.generate_expr(*store).val;
                let key = self.generate_const(IRConst::Sym(key), span);
                // first generate a load sym
                let dest = self.get_temp();
                let obj_load = IR::ObjLoad { dest, obj, key };

                self.push_ir(obj_load, span);

                Span::new(dest, span)
            }
            Expr::Index { store, key } => {
                let obj = self.generate_expr(*store).val;
                let key = self.generate_expr(*key).val;
                let dest = self.get_temp();
                let obj_load = IR::ObjLoad { dest, obj, key };

                self.push_ir(obj_load, span);

                Span::new(dest, span)
            }
            Expr::Call { calle, input } => {
                let dest = self.get_temp();
                let calle = self.generate_expr(*calle).val;
                let input = if input.is_none() {
                    todo!()
                } else {
                    self.generate_expr(*input.unwrap()).val
                };
                let ir = IR::Call { dest, calle, input };

                self.push_ir(ir, span);

                Span::new(dest, span)
            }
        }
    }

    fn generate_const(&mut self, ir_const: IRConst, span: (usize, usize)) -> IRVar {
        let dest = self.get_temp();

        self.push_ir(IR::LoadConst { dest, src: ir_const }, span);

        dest
    }

    fn generate_value(&mut self, value: ParsedValue, span: (usize, usize)) -> IRVar {
        match value {
            ParsedValue::Ident(id) => IRVar::new(VarID::Local(id)),
            ParsedValue::Global(id) => IRVar::new(VarID::Global(id)),
            ParsedValue::Null => self.generate_const(IRConst::Null, span),
            ParsedValue::Int(i) => self.generate_const(IRConst::Int(i), span),
            ParsedValue::Float(f) => self.generate_const(IRConst::Float(f), span),
            ParsedValue::Bool(b) => self.generate_const(IRConst::Bool(b), span),
            ParsedValue::String(b) => self.generate_const(IRConst::String(b), span),
            ParsedValue::List(list) => {
                let dest = self.get_temp();
                let ir = IR::NewList { dest };

                self.push_ir(ir, span);

                for (i, expr) in list.into_iter().enumerate() {
                    let key = self.generate_const(IRConst::Int(i as isize), span);
                    let item = self.generate_expr(expr);
                    let ir = IR::ObjStore {
                        obj: dest,
                        key,
                        val: item.val,
                    };
                    self.push_ir(ir, span);
                }

                dest
            }
            ParsedValue::Map(map) => {
                let dest = self.get_temp();
                let ir = IR::NewMap { dest };

                self.push_ir(ir, span);

                for (key, val) in map.into_iter() {
                    let key = self.generate_expr(key).val;
                    let val = self.generate_expr(val).val;
                    let ir = IR::ObjStore {
                        obj: dest,
                        key,
                        val,
                    };

                    self.push_ir(ir, span);
                }

                dest
            }
            ParsedValue::Func { stmts } => {
                let temp_counter = self.temp_counter;
                self.temp_counter = 0;

                self.push_new_func();
                self.gen_stmts(stmts);

                self.temp_counter = temp_counter;

                let func_val = self.pop_func().unwrap();
                let dest = self.get_temp();
                let load = IR::Copy {
                    dest,
                    src: func_val,
                };

                self.push_ir(load, span);

                dest
            }
        }
    }

    fn pop_func(&mut self) -> Option<IRVar> {
        let null = self.generate_const(IRConst::Null, (0,0));

        let mut func = self.func_stack.pop().unwrap();
        let end_return = IR::Return { src: null, };
        let func_id = self.funcs.len();
        let compiler = FuncCompiler::new();

        func.code.push(Span::new(end_return, (0, 0)));

        let func = compiler.compile_func(func_id, func.code);

        self.funcs.insert(func_id, func);

        if self.func_stack.is_empty() {
            None
        } else {
            Some(self.generate_const(IRConst::Func(func_id), (0,0)))
        }
    }

    fn push_new_func(&mut self) {
        let id = self.get_func_id();

        self.func_stack.push(FuncGenerator::new(id));
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

    fn get_func(&mut self) -> &mut FuncGenerator {
        self.func_stack.last_mut().unwrap()
    }

    fn get_func_id(&mut self) -> usize {
        let id = self.func_counter;
        self.func_counter += 1;
        id
    }

    fn gen_label(&mut self) -> usize {
        self.get_func().gen_label()
    }

    fn get_temp(&mut self) -> IRVar {
        let temp = IRVar::new(VarID::Temp(self.temp_counter));

        self.temp_counter += 1;
        temp
    }
}
