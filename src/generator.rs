use super::parser::{Stmt, Expr, Span, Value};
use super::lexer::Op;

use colored::Colorize;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Copy, Clone)]
pub enum IRVar {
    Temp(usize),
    Ident(usize),
    Global(usize)
}

impl fmt::Display for IRVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            IRVar::Temp(i)   => format!("T{}", i).truecolor(160, 160, 160),
            IRVar::Ident(i)  => format!("ID({})", i).cyan(),
            IRVar::Global(i) => format!("G{}", i).red(),
        };

        write!(f, "{}", s)
    }
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

impl fmt::Display for IRValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Self::String(s) => format!("\"{}\"", s).bright_green(),
            Self::Int(i)    => format!("{}", i).truecolor(255, 150, 40),
            Self::Bool(b)   => format!("{}", b).truecolor(255, 150, 40),
            Self::Float(n)  => format!("{}", n).truecolor(255, 150, 40),
            Self::Var(v)    => {
                match v {
                    IRVar::Temp(i)   => format!("T{}", i).truecolor(160, 160, 160),
                    IRVar::Ident(i)  => format!("ID({})", i).cyan(),
                    IRVar::Global(i) => format!("G{}", i).red(),
                }
            }
            Self::Null      => format!("null").truecolor(255, 150, 40),
            Self::Func(i)   => format!("fn({})", i).red(),
        };

        write!(f, "{}", s)
    }
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
        args: Vec<IRValue>,
    },
    Return {
        dest: IRValue,
    },
    Load {
        dest: IRVar,
        src: IRValue,
    },
    Jnt {
        label: usize,
        cond: IRVar,
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

impl fmt::Display for IRCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s: String = match self {
            Self::Label { id }        => format!("LABEL {}\t\t", id).purple().to_string(),
            Self::Jump { label }      => format!("\tJump {}", label).purple().to_string(),
            Self::Jnt { cond, label } => {
                let jump = format!("\tJNT {}", label).purple().to_string();
                let unless = format!(" {}", cond);
                format!("{jump}{unless}")
            }
            Self::Load { dest, src }  => format!("\t{} = {}", dest, src),
            Self::Return { dest }     => format!("\t{} {}", "return".red(), dest),
            Self::ObjLoad { dest, store, key } => format!("\t{} = {}[{}]", dest, store, key),
            Self::ObjStore { store, key, src } => format!("\t{}[{}] = {}", store, key, src),
            Self::Binop { dest, lhs, op, rhs } => format!("\t{} = {} {} {}", dest, lhs, op, rhs),
            Self::Call { dest, src, args } => {
                let mut s = format!("\t{} = {}(", dest, src);
                for arg in args.iter() {
                    s.push_str(&format!(" {}, ", arg));
                }

                s.push_str(&format!(")"));
                s
            }
            Self::NewList { dest, items } => {
                let mut s = format!("\t{} = [\n", dest);
                for item in items.iter() {
                    s.push_str(&format!("\t\t\t{},\n", item));
                }
                s.push_str(&format!("\t\t]"));
                s
            }
            Self::NewMap { dest, items } => {
                let mut s = format!("\t{} = {{\n", dest);
                for item in items.iter() {
                    s.push_str(&format!("\t\t\t{}: {},\n", item.0, item.1));
                }
                s.push_str(&format!("\t\t}}"));
                s
            }
        };

        write!(f, "{}", s)
    }
}

impl fmt::Display for IRFunc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.id == 0 {
            write!(f, "{}", "======= MAIN START =======\n".red())?
        } else {
            write!(f, "{} {}", format!("======= FUNC {}", self.id).red(), "params ( ")?;
            for i in self.params.iter() {
                write!(f, "{}", format!("{}, ", i))?;
            }
            write!(f, "{} {}", ")", "=======\n".red())?;
        }

        for stmt in self.code.iter() {
            write!(f, "{:12}{}\n", format!("({}, {})", stmt.span.0, stmt.span.1).bright_black(), stmt.val)?;
        }

        if self.id == 0 {
            write!(f, "{}", "======= MAIN END =======\n".red())
        } else {
            write!(f, "{}", format!("======= FUNC {} END =======\n", self.id).red())
        }
    }
}

#[derive(Debug)]
pub struct IRFunc {
    id: usize,
    code: Vec<Span<IRCode>>,
    //code: Vec<Option<Span>, Option<Label>, IRCode>>,
    params: Vec<usize>,
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
        let func = IRFunc {
            id: self.func_counter,
            params: vec![],
            code: vec![],
            label_stack: vec![],
            label_counter: 0
        };

        self.func_counter += 1;
        self.func_stack.push(func);
        
        self.generate(stmts);

        let mut func = self.func_stack.pop().unwrap();
        let end_return = IRCode::Return { dest: IRValue::Null };
        func.code.push(Span::new(end_return, (0, 0)));
        self.funcs.insert(func.id, func);
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
                    let ret = IRCode::Return { dest: var.val };
                    let func = self.func_stack.last_mut().unwrap();

                    func.code.push(Span::new(ret, var.span));
                    self.temp_counter = 0;
                }
                Stmt::Assign { dest, src } => {
                    match dest.val {
                        Expr::Value(value) => {
                            if let IRValue::Var(dest) = self.generate_value(value) {
                                let src = self.generate_expr(*src);
                                let load = IRCode::Load { dest, src: src.val };
                                let func = self.func_stack.last_mut().unwrap();
                                func.code.push(Span::new(load, src.span));
                                self.temp_counter = 0;
                            }
                        }
                        Expr::Access { store, key } => {
                            let span = store.span;
                            let store = self.generate_expr(*store).val;
                            let key = IRValue::Var(IRVar::Ident(key));
                            let src = self.generate_expr(*src).val;
                            let store = IRCode::ObjStore { store, key, src};
                            let func = self.func_stack.last_mut().unwrap();

                            func.code.push(Span::new(store, span));
                            self.temp_counter = 0;
                        }
                        Expr::Index { store, key } => {
                            let span = store.span;
                            let store = self.generate_expr(*store).val;
                            let key = self.generate_expr(*key).val;
                            let src = self.generate_expr(*src).val;
                            let store = IRCode::ObjStore { store, key, src};
                            let func = self.func_stack.last_mut().unwrap();

                            func.code.push(Span::new(store, span));
                            self.temp_counter = 0;
                        }
                        _ => panic!("GENERATOR ERROR: assigning to non lvalue"),
                    }

                }
                Stmt::While { cond, stmts } => {
                    // TODO: check the type of condition, and generate a better type of branch instruction
                    let func = self.func_stack.last_mut().unwrap();

                    let label_start = func.label_counter;
                    func.label_counter += 1;
                    let label_end = func.label_counter;
                    func.label_counter += 1;
                    let label = IRCode::Label { id: label_start };

                    func.code.push(Span::new(label, (0, 0)));

                    let spanned_val = self.generate_expr(*cond);
                    let val = if let IRValue::Var(v) = spanned_val.val {
                        v
                    } else {
                        let temp = self.get_temp();
                        let ret = IRCode::Load { dest: temp, src: spanned_val.val };
                        let func = self.func_stack.last_mut().unwrap();

                        func.code.push(Span::new(ret, spanned_val.span));
                        temp
                    };

                    let ret = IRCode::Jnt { cond: val, label: label_end };
                    let func = self.func_stack.last_mut().unwrap();
                    func.code.push(Span::new(ret, spanned_val.span));
                    self.temp_counter = 0;

                    // push onto the loop stack
                    func.label_stack.push(label_start);

                    self.generate(stmts);

                    let func = self.func_stack.last_mut().unwrap();
                    let jmp = IRCode::Jump { label: label_start };
                    func.code.push(Span::new(jmp, (0, 0)));
                    let label = IRCode::Label { id: label_end };
                    func.code.push(Span::new(label, (0, 0)));
                    func.label_stack.pop();
                }
                Stmt::If { cond, stmts } => {
                    let spanned_val = self.generate_expr(*cond);
                    let val = if let IRValue::Var(v) = spanned_val.val {
                        v
                    } else {
                        let temp = self.get_temp();
                        let ret = IRCode::Load { dest: temp, src: spanned_val.val };
                        let func = self.func_stack.last_mut().unwrap();

                        func.code.push(Span::new(ret, spanned_val.span));
                        temp
                    };


                    let func = self.func_stack.last_mut().unwrap();
                    let ret = IRCode::Jnt { cond: val, label: func.label_counter };
                    func.code.push(Span::new(ret, spanned_val.span));
                    self.temp_counter = 0;

                    self.generate(stmts);

                    let func = self.func_stack.last_mut().unwrap();
                    let label = IRCode::Label { id: func.label_counter };
                    func.label_counter += 1;
                    func.code.push(Span::new(label, (0, 0)));
                }
                Stmt::IfElse { cond, stmts, else_stmts } => {
                    let spanned_val = self.generate_expr(*cond);
                    let val = if let IRValue::Var(v) = spanned_val.val {
                        v
                    } else {
                        let temp = self.get_temp();
                        let func = self.func_stack.last_mut().unwrap();
                        let ret = IRCode::Load { dest: temp, src: spanned_val.val };

                        func.code.push(Span::new(ret, spanned_val.span));
                        temp
                    };

                    let func = self.func_stack.last_mut().unwrap();
                    let ret = IRCode::Jnt { cond: val, label: func.label_counter };
                    func.code.push(Span::new(ret, spanned_val.span));
                    self.temp_counter = 0;

                    self.generate(stmts);

                    let func = self.func_stack.last_mut().unwrap();
                    let label = IRCode::Label { id: func.label_counter };
                    func.label_counter += 1;
                    func.code.push(Span::new(label, (0, 0)));

                    let func = self.func_stack.last_mut().unwrap();
                    let jmp = IRCode::Jump { label: func.label_counter };
                    func.code.push(Span::new(jmp, spanned_val.span));


                    self.generate(else_stmts);

                    let func = self.func_stack.last_mut().unwrap();
                    let label = IRCode::Label { id: func.label_counter };
                    func.label_counter += 1;
                    func.code.push(Span::new(label, (0, 0)));

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
                Span::new(self.generate_value(value), span)
            }
            Expr::Binop { lhs, op, rhs } => {
                let lhs = self.generate_expr(*lhs).val;
                let rhs = self.generate_expr(*rhs).val;
                let dest = self.get_temp();
                let binop = IRCode::Binop { dest, lhs, rhs, op: op.clone() };
                let func = self.func_stack.last_mut().unwrap();

                func.code.push(Span::new(binop, span));

                Span::new(IRValue::Var(dest), span)
            }
            Expr::Access { store, key } => {
                let store = self.generate_expr(*store).val;
                let key = IRValue::Var(IRVar::Ident(key));
                let dest = self.get_temp();
                let obj_load = IRCode::ObjLoad { dest, store, key};
                let func = self.func_stack.last_mut().unwrap();

                func.code.push(Span::new(obj_load, span));

                Span::new(IRValue::Var(dest), span)
            }
            Expr::Index { store, key } => {
                let store = self.generate_expr(*store).val;
                let key = self.generate_expr(*key).val;
                let dest = self.get_temp();
                let obj_load = IRCode::ObjLoad { dest, store, key};
                let func = self.func_stack.last_mut().unwrap();

                func.code.push(Span::new(obj_load, span));

                Span::new(IRValue::Var(dest), span)
            }
            Expr::Call { calle, args } => {
                let dest = self.get_temp();
                let src = self.generate_expr(*calle).val;
                let mut items = vec![];

                for expr in args.into_iter() {
                    let item = self.generate_expr(expr);
                    items.push(item.val);
                }

                let call = IRCode::Call { dest, src, args: items };
                let func = self.func_stack.last_mut().unwrap();

                func.code.push(Span::new(call, span));

                Span::new(IRValue::Var(dest), span)
            }
        }
    }

    fn generate_value(&mut self, value: Value) -> IRValue {
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

                let obj_load = IRCode::NewList { dest, items };
                let func = self.func_stack.last_mut().unwrap();

                func.code.push(Span::new(obj_load, (0, 0)));

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

                let obj_load = IRCode::NewMap { dest, items };
                let func = self.func_stack.last_mut().unwrap();

                func.code.push(Span::new(obj_load, (0, 0)));

                IRValue::Var(dest)
            }
            Value::Func { params, stmts } => {
                let temp_counter = self.temp_counter;
                self.temp_counter = 0;

                let func = IRFunc {
                    id: self.func_counter,
                    params,
                    code: vec![],
                    label_stack: vec![],
                    label_counter: 0,
                };

                self.func_counter += 1;
                self.func_stack.push(func);
                
                self.generate(stmts);

                self.temp_counter = temp_counter;

                let mut func = self.func_stack.pop().unwrap();
                let end_return = IRCode::Return { dest: IRValue::Null };
                func.code.push(Span::new(end_return, (0, 0)));
                let src = IRValue::Func(func.id);
                self.funcs.insert(func.id, func);

                let dest = self.get_temp();
                let load = IRCode::Load { dest, src };
                let func = self.func_stack.last_mut().unwrap();

                func.code.push(Span::new(load, (0, 0)));

                IRValue::Var(dest)
            }
        }
    }

    fn get_temp(&mut self) -> IRVar {
        let temp = IRVar::Temp(self.temp_counter);

        self.temp_counter += 1;
        temp
    }
}
