use crate::parser::{Span, Spanned, Stmt, Expr, Value, Op, MapKey, LhsExpr};
use crate::symbol_map::SymID;
use std::collections::HashSet;

use super::memory_ssa::MemoryAccess;
use super::tac::{
    LabelID,
    TacFunc,
    Tac,
    Var,
    TacConst,
    FuncID,
};

pub const MAIN_FUNC_ID: usize = 0;

struct LoopCtx {
    start: LabelID,
    end: LabelID,
}

struct FuncLoweringCtx {
    func: TacFunc,
    temp_counter: usize,
    label_counter: usize,
    defined_variables: HashSet<SymID>,
    loop_ctxs: Vec<LoopCtx>
    //builder: FuncBuilder
}

impl FuncLoweringCtx {
    fn new(id: FuncID, inputs: HashSet<SymID>) -> Self {
        Self {
            func: TacFunc::new(id, inputs.clone()),
            temp_counter: 0,
            label_counter: 0,
            defined_variables: inputs,
            loop_ctxs: vec![],
        }
    }
}

struct TacGenCtx<'a> {
    func_counter: usize,
    generators: Vec<FuncLoweringCtx>,
    streaming_callback: Box<dyn FnMut(TacFunc) + 'a>
}

impl<'a> TacGenCtx<'a> {
    pub fn new(callback: impl FnMut(TacFunc) + 'a) -> Self {
        let main_func = FuncLoweringCtx::new(MAIN_FUNC_ID, HashSet::new());

        Self {
            func_counter: 0,
            generators: vec![main_func],
            streaming_callback: Box::new(callback)
        }
    } 

    fn generate_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Expr(expr) => { self.generate_expr(expr); }
            Stmt::Return(expr) => self.generate_return(expr),
            Stmt::Break => self.generate_break(),
            Stmt::Continue => self.generate_continue(),
            Stmt::While { cond, stmts } => self.generate_while_block(cond, stmts),
            Stmt::If { cond, stmts } => self.generate_if_block(cond, stmts),
            Stmt::IfElse { cond, stmts, else_stmts } => self.generate_if_else_block(cond, stmts, else_stmts),
            Stmt::FuncDecl { ident, inputs, stmts } => self.generate_func_decl(ident, inputs, stmts),
            Stmt::Assign { dest, src } => self.generate_assign(dest, src),
        };
    }

    fn generate_return(&mut self, return_expr: Option<Spanned<Expr>>) {
        let var =
        if let Some(expr) = return_expr {
            self.generate_expr(expr)
        } else {
            self.new_temp()
        };

        self.emit(Tac::Return { src: var });
    }

    fn generate_assign(&mut self, lhs_expr: Spanned<LhsExpr>, src: Spanned<Expr>) {
        let src = self.generate_expr(src);
        let span = lhs_expr.get_span();

        match lhs_expr.item {
            LhsExpr::Index { store, key } => {
                let store = self.generate_expr(*store);
                let key = self.generate_expr(*key);

                self.generate_key_store(store, key, src, span);
            }
            LhsExpr::Access { store, key } => {
                let store = self.generate_expr(*store);
                let key = self.load_const(TacConst::Sym(key));

                self.generate_key_store(store, key, src, span);
            }
            LhsExpr::Local(sym_id) => {
                self.define_var(sym_id);

                if src.is_temp() && self.last_instr_has_dest() {
                  self.update_prev_dest(Var::local(sym_id));
                } else {
                    self.emit(
                        Tac::Copy {
                            dest: Var::local(sym_id),
                            src
                        }
                    )
                }
            }
            LhsExpr::Global(sym_id) => {
                self.emit(
                    Tac::Copy {
                        dest: Var::global(sym_id),
                        src
                    }
                )
            }
        }
    }

    fn generate_expr(&mut self, spanned_expr: Spanned<Expr>) -> Var {
        let span = spanned_expr.get_span();
        match spanned_expr.item {
            Expr::Value(value) => self.generate_value(value),
            Expr::Read => self.generate_read(),
            Expr::Print(expr) => {
                let var = self.generate_expr(*expr);

                self.generate_print(var)
            }
            Expr::Binop { lhs, op, rhs } => {
                match op {
                    Op::And | Op::Or => self.generate_shortcircuit(*lhs, op, *rhs),
                    _ => {
                        let v1 = self.generate_expr(*lhs);
                        let v2 = self.generate_expr(*rhs);

                        self.generate_binop(v1, op, v2, span)
                    }
                }
            }
            Expr::Access { store, key } => {
                let var = self.generate_expr(*store);
                let key = self.load_const(TacConst::Sym(key));

                self.generate_key_load(var, key, span)
            }
            Expr::Index { store, key } => {
                let store_val = self.generate_expr(*store);
                let key_val = self.generate_expr(*key);

                self.generate_key_load(store_val, key_val, span)
            }
            Expr::Call { calle, args } => {
                let arg_vals = args.into_iter().map(|a| self.generate_expr(a)).collect();
                let calle_val = self.generate_expr(*calle);

                self.generate_call(calle_val, arg_vals, span)
            }
        }
    }

    fn generate_shortcircuit(&mut self, lhs: Spanned<Expr>, op: Op, rhs: Spanned<Expr>) -> Var {
        let label = self.new_label();
        let lt = self.new_long_temp();
        let v1 = self.generate_expr(lhs);

        self.emit(
            Tac::Copy {
                dest: lt,
                src: v1
            }
        );

        match op {
            Op::And => 
                self.emit(
                    Tac::Jnt {
                        src: lt,
                        label,
                    }
                ),
            Op::Or => 
                self.emit(
                    Tac::Jit {
                        src: lt,
                        label,
                    }
                ),
            _ => panic!("tried to generated shortcircuit for wrong op"),
        }

        let v2 = self.generate_expr(rhs);

        self.emit(
            Tac::Copy {
                dest: lt,
                src: v2
            }
        );

        self.emit_label(label);

        return lt;
    }

    fn generate_value(&mut self, value: Value) -> Var {
        match value {
            Value::Int(i) => self.load_const(TacConst::Int(i)),
            Value::Null => self.load_const(TacConst::Null),
            Value::Float(f) => self.load_const(TacConst::Float(f)),
            Value::Bool(b) => self.load_const(TacConst::Bool(b)),
            Value::String(s) => self.load_const(TacConst::String(s)),
            Value::Global(sym_id) => Var::global(sym_id),
            Value::Ident(sym_id) => self.generate_ident(sym_id),
            Value::Map(map) => self.generate_map(map),
            Value::List(list) => self.generate_list(list),
            Value::InlineFunc { inputs, stmts } => self.generate_func(inputs, stmts, None),
        }
    }

    fn generate_call(&mut self, calle: Var, args: Vec<Var>, span: Span) -> Var {
        for src in args {
            self.emit(
                Tac::LoadArg {
                    src
                }
            )
        }

        let temp = self.new_temp();

        self.emit_spanned(
            Tac::Call {
                dest: temp,
                src: calle,
            },
            span
        );

        temp
    }

    fn generate_key_store(&mut self, store: Var, key: Var, src: Var, span: Span) {
        let mem = MemoryAccess::new(store, key);
        self.emit_spanned(
            Tac::MemStore {
                mem, 
                src
            },
            span
        );
    }

    fn generate_key_load(&mut self, store: Var, key: Var, span: Span) -> Var {
        let temp = self.new_temp();
        let mem = MemoryAccess::new(store, key);

        self.emit_spanned(
            Tac::MemLoad {
                dest: temp,
                mem,
            },
            span
        );

        temp
    }

    fn generate_break(&mut self) {
        let label = self.get_loop_end();

        self.emit_jump(label);
    }

    fn generate_continue(&mut self) {
        let label = self.get_loop_start();

        self.emit_jump(label);
    }

    fn generate_func_decl(&mut self, ident: SymID, inputs: Spanned<Vec<SymID>>, stmts: Vec<Stmt>) {
        self.define_var(ident);

        self.generate_func(inputs, stmts, Some(ident));
    }

    fn generate_func(&mut self, inputs: Spanned<Vec<SymID>>, stmts: Vec<Stmt>, ident: Option<SymID>) -> Var {
        let dest = if let Some(sym) = ident {
            Var::local(sym)
        } else {
            self.new_temp()
        };

        let (func_id, upvalues) = self.new_func(inputs, stmts);

        self.emit(
            Tac::LoadConst {
                dest,
                src: TacConst::Func(func_id),
            }
        );


        for upvalue in upvalues.iter() {
            self.emit(
                Tac::StoreUpvalue {
                    dest,
                    src: *upvalue
                }
            );
        }

        dest
    }

    fn new_func(&mut self, inputs: Spanned<Vec<SymID>>, stmts: Vec<Stmt>) -> (FuncID, HashSet<SymID>) {
        let func_id = self.new_func_id();
        let generator = FuncLoweringCtx::new(func_id, HashSet::from_iter(inputs.item));

        self.generators.push(generator);

        self.generate_stmts(stmts);

        let upvalues = self.generators.last().unwrap().func.upvalues.clone();

        self.stream_current_func();

        (func_id, upvalues)
    }

    fn stream_current_func(&mut self) {
        let func = self.generators.pop().unwrap().func;

        (self.streaming_callback)(func);
    }

    fn generate_if_block(&mut self, cond: Spanned<Expr>, stmts: Vec<Stmt>) {
        let label = self.new_label();
        let cond = self.generate_expr(cond);

        self.emit(
            Tac::Jnt {
                src: cond,
                label,
            }
        );
        self.generate_stmts(stmts);
        self.emit_label(label);
    }

    fn generate_if_else_block(&mut self, cond: Spanned<Expr>, stmts: Vec<Stmt>, else_stmts: Vec<Stmt>) {
        let else_end = self.new_label();
        let else_start = self.new_label();
        let cond = self.generate_expr(cond);

        self.emit(
            Tac::Jnt {
                src: cond,
                label: else_start,
            }
        );
        self.generate_stmts(stmts);
        self.emit_jump(else_end);
        self.emit_label(else_start);
        self.generate_stmts(else_stmts);
        self.emit_label(else_end);
    }

    fn generate_while_block(&mut self, cond: Spanned<Expr>, stmts: Vec<Stmt>) {
        let start = self.new_label();
        let end = self.new_label();

        self.push_loop_ctx(LoopCtx { start, end });

        self.emit_label(start);

        let cond = self.generate_expr(cond);

        self.emit(
            Tac::Jnt {
                src: cond,
                label: end,
            }
        );
        self.generate_stmts(stmts);
        self.emit_jump(start);
        self.emit_label(end);
    }

    fn generate_ident(&mut self, sym_id: SymID) -> Var {
        if self.defined_local(sym_id) {
            Var::local(sym_id)
        } else if self.defined_upvalue(sym_id) {
            self.set_upvalue(sym_id);

            Var::upvalue(sym_id)
        } else {
            // this local will be dead, probably can optimize here to keep this info
            Var::local(sym_id)
        }
    }

    fn generate_map(&mut self, pairs: Vec<(MapKey, Spanned<Expr>)>) -> Var {
        let store = self.new_temp();

        self.emit(Tac::NewMap { dest: store });

        for (k, value) in pairs.into_iter() {
            let key = 
            match k {
                MapKey::Sym(sym) => self.load_const(TacConst::Sym(sym)),
                MapKey::Expr(expr) => self.generate_expr(expr)
            };

            let src = self.generate_expr(value);
            let mem = MemoryAccess::new(store, key);

            // this doesn't need to be spanned since we are sure we are storing into a map this
            // can't fail so we don't need spanning info
            self.emit(
                Tac::MemStore {
                    mem,
                    src
                },
            );
        }

        store
    }

    fn generate_list(&mut self, exprs: Vec<Spanned<Expr>>) -> Var {
        let store = self.new_temp();

        self.emit(Tac::NewList { dest: store });

        for (i, e) in exprs.into_iter().enumerate() {
            let src = self.generate_expr(e);

            let key = self.new_temp();
            self.emit(Tac::LoadConst { dest: key, src: TacConst::Int(i as isize)});
            let mem = MemoryAccess::new(store, key);

            self.emit(
                Tac::MemStore {
                    mem,
                    src,
                }
            );
        }

        store
    }

    fn generate_binop(&mut self, lhs: Var, op: Op, rhs: Var, span: Span) -> Var {
        let temp = self.new_temp();

        self.emit_spanned(
            Tac::Binop {
                dest: temp,
                lhs,
                op,
                rhs
            },
            span
        );

        temp
    }

    fn generate_print(&mut self, src: Var) -> Var {
        self.emit(
            Tac::Print {
                src,
            }
        );

        self.new_temp()
    }

    fn generate_read(&mut self) -> Var {
        let temp = self.new_temp();

        self.emit(
            Tac::Read {
                dest: temp,
            }
        );

        temp
    }

    fn load_const(&mut self, tac_const: TacConst) -> Var {
        let temp = self.new_temp();

        self.emit(
            Tac::LoadConst {
                dest: temp,
                src: tac_const
            }
        );

        temp
    }

    fn emit_label(&mut self, label: LabelID) {
        self.emit(Tac::Label { label })
    }

    fn emit_jump(&mut self, label: LabelID) {
        self.emit(Tac::Jump { label })
    }

    fn last_instr_has_dest(&mut self) -> bool {
        self.generators.last().unwrap().func.tac.last().unwrap().dest_var().is_some()
    }

    fn update_prev_dest(&mut self, var: Var) {
        // This isn't needed but makes the printed CFG a little more readable
        self.generators.last_mut().unwrap().temp_counter -= 1; 

        let dest = self.generators.last_mut().unwrap().func.tac.last_mut().unwrap().dest_var_mut().unwrap();

        *dest = var;
        // if the last instr was a keystore
    }

    fn emit(&mut self, tac: Tac) {
        self.generators.last_mut().unwrap().func.tac.push(tac);
    }

    fn emit_spanned(&mut self, tac: Tac, span: Span) {
        let func = &mut self.generators.last_mut().unwrap().func;

        func.tac.push(tac);
        func.spans.push(span, func.tac.len() - 1);
    }

    fn push_loop_ctx(&mut self, ctx: LoopCtx) {
        self.generators.last_mut().as_mut().unwrap().loop_ctxs.push(ctx);
    }

    fn new_func_id(&mut self) -> FuncID {
        self.func_counter += 1;
        self.func_counter
    }

    fn new_temp(&mut self) -> Var {
        self.generators.last_mut().unwrap().temp_counter += 1;

        Var::temp(self.generators.last().unwrap().temp_counter)
    }

    fn new_long_temp(&mut self) -> Var {
        self.generators.last_mut().unwrap().temp_counter += 1;

        Var::long_temp(self.generators.last().unwrap().temp_counter)
    }

    fn new_label(&mut self) -> LabelID {
        self.generators.last_mut().unwrap().label_counter += 1;

        self.generators.last().unwrap().label_counter
    }

    fn get_loop_start(&mut self) -> LabelID {
        self.generators.last_mut().unwrap().loop_ctxs.last().unwrap().start
    }

    fn get_loop_end(&mut self) -> LabelID {
        self.generators.last_mut().unwrap().loop_ctxs.last().unwrap().end
    }

    fn define_var(&mut self, sym: SymID) {
        self.generators.last_mut().unwrap().defined_variables.insert(sym);
    }

    fn set_upvalue(&mut self, sym: SymID) {
        self.generators.last_mut().unwrap().func.upvalues.insert(sym);
    }

    fn defined_local(&self, sym: SymID) -> bool {
        self.generators.last().unwrap().defined_variables.contains(&sym)
    }

    fn defined_upvalue(&self, sym: SymID) -> bool {
        for i in 0..(self.generators.len() - 1) {
            if self.generators[i].defined_variables.contains(&sym) {
                return true;
            }
        }

        return false;
    }

    fn generate_stmts(&mut self, stmts: Vec<Stmt>) {
        for stmt in stmts.into_iter() {
            self.generate_stmt(stmt);
        }
    }
}

pub fn stream_tac_from_stmts(stmts: Vec<Stmt>, callback: impl FnMut(TacFunc)) {
    let mut ctx = TacGenCtx::new(callback);

    ctx.generate_stmts(stmts);

    ctx.stream_current_func();
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::parser::parse_program;
    use crate::symbol_map::SymbolMap;

    pub fn fabricate_tac_func(tac: Vec<Tac>) -> TacFunc {
        let mut func = TacFunc::new(0, HashSet::new());

        func.tac = tac;

        func
    }

    fn expect_tac(input: &str, expected_code: Vec<Vec<Tac>>) {
        let mut syms = SymbolMap::new();

        expect_tac_with_syms(input, expected_code, &mut syms);
    }

    fn expect_tac_with_syms(input: &str, mut expected_code: Vec<Vec<Tac>>, syms: &mut SymbolMap) {
        let parse_result = parse_program(input, syms);
        assert!(parse_result.errors.is_empty());

        stream_tac_from_stmts(parse_result.value.unwrap(), |tac_func| {
            assert_eq!(tac_func.tac, expected_code.pop().unwrap())
        });
    }

    #[test]
    fn load_int_value() {
        let mut ctx = TacGenCtx::new(|_| {});

        ctx.generate_value(Value::Int(69));

        assert!(ctx.generators[0].func.tac[0] == Tac::LoadConst { dest: Var::temp(1), src: TacConst::Int(69) });
        assert!(ctx.generators[0].func.spans.is_empty());
    }

    #[test]
    fn load_string_value() {
        let mut ctx = TacGenCtx::new(|_| {});

        ctx.generate_value(Value::String("test".to_string()));

        assert!(ctx.generators[0].func.tac[0] == Tac::LoadConst { dest: Var::temp(1), src: TacConst::String("test".to_string()) });
    }

    #[test]
    fn load_float_value() {
        let mut ctx = TacGenCtx::new(|_| {});

        ctx.generate_value(Value::Float(612357.234532));

        assert!(ctx.generators[0].func.tac[0] == Tac::LoadConst { dest: Var::temp(1), src: TacConst::Float(612357.234532) })
    }

    #[test]
    fn emit_read() {
        let mut ctx = TacGenCtx::new(|_| {});

        ctx.generate_expr(Spanned::new(Expr::Read, (0, 1)));

        assert!(ctx.generators[0].func.tac[0] == Tac::Read { dest: Var::temp(1) });
        assert!(ctx.generators[0].func.spans.is_empty());
    }

    #[test]
    fn expr_stmt_tac() {
        let tac = vec![
            vec![
                Tac::LoadConst { dest: Var::temp(1), src: TacConst::Int(1) }
            ]
        ];
        let input = "1;";

        expect_tac(input, tac);
    }

    #[test]
    fn generates_expr_tac() {
        let tac = vec![
            vec![
                Tac::LoadConst { dest: Var::temp(1), src: TacConst::Int(1) },
                Tac::LoadConst { dest: Var::temp(2), src: TacConst::Int(1) },
                Tac::LoadConst { dest: Var::temp(3), src: TacConst::Int(1) },
                Tac::Binop { dest: Var::temp(4), lhs: Var::temp(2), op: Op::Multiply, rhs: Var::temp(3) },
                Tac::Binop { dest: Var::temp(5), lhs: Var::temp(1), op: Op::Multiply, rhs: Var::temp(4) }
            ]
        ];
        let input = "1 * 1 * 1;";

        expect_tac(input, tac);
    }

    #[test]
    fn index_assignment_tac() {
        let tac = vec![
            vec![
                Tac::LoadConst { dest: Var::temp(1), src: TacConst::Int(1) },
                Tac::NewList { dest: Var::temp(2) },
                Tac::LoadConst { dest: Var::temp(3), src: TacConst::Int(1) },
                Tac::LoadConst { dest: Var::temp(4), src: TacConst::Int(0) },
                Tac::MemStore { src: Var::temp(3), mem: MemoryAccess::new(Var::temp(2), Var::temp(4)) }, 
                Tac::LoadConst { dest: Var::temp(5), src: TacConst::Int(0) },
                Tac::MemStore { src: Var::temp(1), mem: MemoryAccess::new(Var::temp(2), Var::temp(5)) }, 
            ]
        ];
        let input = "[1][0] = 1;";

        expect_tac(input, tac);
    }

    #[test]
    fn access_assignment_tac() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            vec![
                Tac::LoadConst { dest: Var::temp(1), src: TacConst::Int(1) },
                Tac::LoadConst { dest: Var::temp(2), src: TacConst::Sym(syms.get_id("b")) },
                Tac::MemStore { src: Var::temp(1), mem: MemoryAccess::new(Var::local(syms.get_id("a")), Var::temp(2)) }, 
            ]
        ];
        let input = "a.b = 1;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn assign_local() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            vec![
                Tac::LoadConst { dest: Var::local(syms.get_id("a")), src: TacConst::Int(1) },
            ]
        ];
        let input = "a = 1;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn assign_global() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            vec![
                Tac::LoadConst { dest: Var::temp(1), src: TacConst::Int(1) },
                Tac::Copy { dest: Var::global(syms.get_id("a")), src: Var::temp(1) },
            ]
        ];
        let input = "@a = 1;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn generates_return_with_value() {
        let tac = vec![
            vec![
                Tac::LoadConst { dest: Var::temp(1), src: TacConst::Bool(true) },
                Tac::Return { src: Var::temp(1) },
            ]
        ];
        let input = "return true;";

        expect_tac(input, tac);
    }

    #[test]
    fn generates_return_with_no_value() {
        let tac = vec![
            vec![
                Tac::Return { src: Var::temp(1) },
            ]
        ];
        let input = "return;";

        expect_tac(input, tac);
    }

    #[test]
    fn generates_print_expr() {
        let tac = vec![
            vec![
                Tac::LoadConst { dest: Var::temp(1), src: TacConst::String("Hello World".to_string()) },
                Tac::Print { src: Var::temp(1) },
            ]
        ];
        let input = "print(\"Hello World\");";

        expect_tac(input, tac);
    }

    #[test]
    fn generates_break_and_continue_stmts() {
        let tac = vec![
            vec![
                Tac::Label { label: 1 },
                Tac::LoadConst { dest: Var::temp(1), src: TacConst::Bool(true) },
                Tac::Jnt { src: Var::temp(1), label: 2 },
                Tac::Jump { label: 1 },
                Tac::Jump { label: 2 },
                Tac::Jump { label: 1 },
                Tac::Label { label: 2 },
            ]
        ];
        let input = "while true { continue; break; }";

        expect_tac(input, tac);
    }

    #[test]
    fn call_multiple_args() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            vec![
                Tac::LoadConst { dest: Var::temp(1), src: TacConst::Int(0) },
                Tac::LoadConst { dest: Var::temp(2), src: TacConst::Int(1) },
                Tac::LoadConst { dest: Var::temp(3), src: TacConst::Int(2) },
                Tac::LoadArg { src: Var::temp(1), },
                Tac::LoadArg { src: Var::temp(2), },
                Tac::LoadArg { src: Var::temp(3), },
                Tac::Call { dest: Var::temp(4), src: Var::local(syms.get_id("a")), },
            ]
        ];
        let input = "a(0, 1, 2);";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn use_defined_local() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            vec![
                Tac::LoadConst { dest: Var::local(syms.get_id("a")), src: TacConst::Int(1) },
                Tac::Copy { dest: Var::local(syms.get_id("b")), src: Var::local(syms.get_id("a")) },
            ]
        ];
        let input = "a = 1; b = a;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn simple_if_stmt() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            vec![
                Tac::LoadConst { dest: Var::temp(1), src: TacConst::Bool(true) }, 
                Tac::Jnt { label: 1, src: Var::temp(1) }, 
                Tac::LoadConst { dest: Var::temp(2), src: TacConst::String(String::from("test")) }, 
                Tac::Print { src: Var::temp(2) }, 
                Tac::Label { label: 1 }
            ]
        ];
        let input = "if true { print(\"test\"); }";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn load_null() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            vec![
                Tac::LoadConst { dest: Var::local(syms.get_id("a")), src: TacConst::Null }, 
            ]
        ];
        let input = "a = null;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn sym_key_load() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            vec![
              Tac::LoadConst { dest: Var::temp(1), src: TacConst::Sym(syms.get_id("c")) }, 
              Tac::MemLoad { dest: Var::local(syms.get_id("a")), mem: MemoryAccess::new(Var::local(syms.get_id("b")), Var::temp(1)) }, 
            ]
        ];
        let input = "a = b.c;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn val_key_load() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            vec![
              Tac::LoadConst { dest: Var::temp(1), src: TacConst::Int(0) }, 
              Tac::MemLoad { dest: Var::local(syms.get_id("a")), mem: MemoryAccess::new(Var::local(syms.get_id("b")), Var::temp(1)) }, 
            ]
        ];
        let input = "a = b[0];";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn load_global() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            vec![
              Tac::Copy { dest: Var::local(syms.get_id("a")), src: Var::global(syms.get_id("b")) }
            ]
        ];
        let input = "a = @b;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn map_assignment() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            vec![
              Tac::NewMap { dest: Var::temp(1) },
              Tac::LoadConst { dest: Var::temp(2), src: TacConst::Sym(syms.get_id("b")) },
              Tac::LoadConst { dest: Var::temp(3), src: TacConst::Int(0) },
              Tac::MemStore { mem: MemoryAccess::new(Var::temp(1), Var::temp(2)), src: Var::temp(3) },
              Tac::Copy { dest: Var::local(syms.get_id("a")), src: Var::temp(1) },
            ]
        ];
        let input = "a = { b: 0 };";

        expect_tac_with_syms(input, tac, &mut syms);
    }

}
