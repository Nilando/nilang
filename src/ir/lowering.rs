use super::func::Func;
use super::func_builder::FuncBuilder;
use super::tac::{FuncID, LabelID, Tac, TacConst};
use super::VReg;

use crate::parser::{Expr, LhsExpr, MapKey, SegmentedString, Span, Spanned, Stmt, StringSegment, Value};
use crate::op::{BinaryOp, UnaryOp};
use crate::symbol_map::{SymID, SymbolMap, ITER_SYM};

use std::collections::BTreeSet;

pub const MAIN_FUNC_ID: u32 = 0;

struct LoopCtx {
    start: LabelID,
    end: LabelID,
}

struct FuncLoweringCtx {
    label_counter: usize,
    defined_variables: BTreeSet<SymID>,
    loop_ctxs: Vec<LoopCtx>,
    builder: FuncBuilder,
}

impl FuncLoweringCtx {
    fn new(id: FuncID, inputs: Vec<SymID>, pretty_ir: bool) -> Self {
        Self {
            builder: FuncBuilder::new(id, &inputs, pretty_ir),
            label_counter: 0,
            defined_variables: BTreeSet::from_iter(inputs),
            loop_ctxs: vec![],
        }
    }
}

struct LoweringCtx {
    func_counter: u32,
    funcs: Vec<FuncLoweringCtx>,
    lowered_funcs: Vec<Func>,
    pretty_ir: bool,
}

impl LoweringCtx {
    pub fn new(pretty_ir: bool) -> Self {
        let main_func = FuncLoweringCtx::new(MAIN_FUNC_ID, vec![], pretty_ir);

        Self {
            func_counter: 0,
            funcs: vec![main_func],
            lowered_funcs: vec![],
            pretty_ir,
        }
    }

    fn lower_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Expr(expr) => { 
                self.lower_expr(expr);
            }
            Stmt::ForLoop { item, store, stmts } => self.lower_for_loop(item, store, stmts),
            Stmt::Return(expr) => self.lower_return(expr),
            Stmt::Break => self.lower_break(),
            Stmt::Continue => self.lower_continue(),
            Stmt::While { cond, stmts } => self.lower_while_block(cond, stmts),
            Stmt::If { cond, stmts } => self.lower_if_block(cond, stmts),
            Stmt::IfElse {
                cond,
                stmts,
                else_stmts,
            } => self.lower_if_else_block(cond, stmts, else_stmts),
            Stmt::FuncDecl {
                ident,
                inputs,
                stmts,
            } => self.lower_func_decl(ident, inputs, stmts),
            Stmt::Assign { dest, src } => self.lower_assign(dest, src),
            Stmt::Import { ident, path } => self.lower_import(ident, path),
        };
    }

    fn lower_import(&mut self, ident: SymID, module_path: Spanned<String>) {
        self.define_var(ident);

        let dest = self.sym_to_reg(&ident);
        let path = self.load_const(TacConst::String(module_path.item));

        self.emit(Tac::Import { dest, path });
    }

    fn lower_return(&mut self, return_expr: Option<Spanned<Expr>>) {
        let src = if let Some(expr) = return_expr {
            self.lower_expr(expr)
        } else {
            self.new_temp()
        };

        self.emit(Tac::Return { src });
    }

    fn lower_assign(&mut self, lhs_expr: Spanned<LhsExpr>, src: Spanned<Expr>) {
        let copy_flag = matches!(src.item, Expr::Value(Value::Ident(_)));
        let src = self.lower_expr(src);
        let span = lhs_expr.get_span();

        match lhs_expr.item {
            LhsExpr::Index { store, key } => {
                let store = self.lower_expr(*store);
                let key = self.lower_expr(*key);

                self.lower_key_store(store, key, src, span);
            }
            LhsExpr::Access { store, key } => {
                let store = self.lower_expr(*store);
                let key = self.load_const(TacConst::Sym(key));

                self.lower_key_store(store, key, src, span);
            }
            LhsExpr::Local(sym_id) => {
                self.define_var(sym_id);

                if !copy_flag && self.last_instr_has_dest() {
                    self.update_prev_dest(sym_id);
                } else {
                    let dest = self.sym_to_reg(&sym_id);

                    self.emit(Tac::Copy { dest, src })
                }
            }
            LhsExpr::Global(sym_id) => {
                let sym = self.load_const(TacConst::Sym(sym_id));

                self.emit(Tac::StoreGlobal { src, sym })
            }
        }
    }

    fn lower_expr(&mut self, spanned_expr: Spanned<Expr>) -> VReg {
        let span = spanned_expr.get_span();
        match spanned_expr.item {
            Expr::Value(value) => self.lower_value(value),
            Expr::Read => self.lower_read(),
            Expr::Print(expr) => {
                let var = self.lower_expr(*expr);

                self.lower_print(var)
            }
            Expr::Type(expr) => {
                let src = self.lower_expr(*expr);
                let dest = self.new_temp();

                self.emit(Tac::Type {
                    dest,
                    src
                });

                dest
            }
            Expr::Clone(expr) => {
                let src = self.lower_expr(*expr);
                let dest = self.new_temp();

                self.emit(Tac::Clone {
                    dest,
                    src
                });

                dest
            }
            Expr::Delete { store, key } => {
                let store = self.lower_expr(*store);
                let key = self.lower_expr(*key);
                let dest = self.new_temp();

                self.emit(Tac::Delete {
                    dest,
                    store,
                    key
                });

                dest
            }
            Expr::Bind { func, arg } => {
                let func = self.lower_expr(*func);
                let arg = self.lower_expr(*arg);
                let dest = self.new_temp();

                self.emit(Tac::Bind {
                    dest,
                    func,
                    arg
                });

                dest
            }
            Expr::Unaop { op, expr } => {
                match op {
                    UnaryOp::Pop => {
                        let src = self.lower_expr(*expr);
                        let dest = self.new_temp();

                        self.emit(Tac::Pop {
                            dest,
                            src
                        });

                        dest
                    }
                    UnaryOp::Negate => {
                        let v1 = self.lower_expr(*expr);
                        let v2 = self.load_const(TacConst::Int(-1));


                        self.lower_binop(v1, BinaryOp::Multiply, v2, span)
                    }
                    UnaryOp::Not => {
                        let else_end = self.new_label();
                        let else_start = self.new_label();
                        let temp = self.lower_expr(*expr);
                        let dest = self.new_temp();

                        self.emit(Tac::Jnt {
                            src: temp,
                            label: else_start,
                        });
                        self.emit(Tac::LoadConst {
                            dest,
                            src: TacConst::Bool(false),
                        });
                        self.emit_jump(else_end);
                        self.emit_label(else_start);
                        self.emit(Tac::LoadConst {
                            dest,
                            src: TacConst::Bool(true),
                        });
                        self.emit_label(else_end);

                        dest
                    }
                    _ => {
                        let v1 = self.lower_expr(*expr);

                        self.lower_unary_op(op, v1, span)
                    }
                }
            }
            Expr::Binop { lhs, op, rhs } => match op {
                BinaryOp::And | BinaryOp::Or => self.lower_shortcircuit(*lhs, op, *rhs),
                BinaryOp::Push => {
                    let v1 = self.lower_expr(*lhs);
                    let v2 = self.lower_expr(*rhs);

                    self.lower_push(v1, v2, span)
                }
                _ => {
                    let v1 = self.lower_expr(*lhs);
                    let v2 = self.lower_expr(*rhs);

                    self.lower_binop(v1, op, v2, span)
                }
            },
            Expr::Access { store, key } => {
                let var = self.lower_expr(*store);
                let key = self.load_const(TacConst::Sym(key));

                self.lower_key_load(var, key, span)
            }
            Expr::Index { store, key } => {
                let store = self.lower_expr(*store);
                let key = self.lower_expr(*key);

                self.lower_key_load(store, key, span)
            }
            Expr::Call { calle, args } => {
                let args = args.into_iter().map(|a| self.lower_expr(a)).collect();
                let calle = self.lower_expr(*calle);

                self.lower_call(calle, args, span)
            }
        }
    }

    fn lower_shortcircuit(&mut self, lhs: Spanned<Expr>, op: BinaryOp, rhs: Spanned<Expr>) -> VReg {
        let label = self.new_label();
        let dest = self.new_temp();
        let src = self.lower_expr(lhs);

        self.emit(Tac::Copy { dest, src });

        match op {
            BinaryOp::And => self.emit(Tac::Jnt { src, label }),
            BinaryOp::Or => self.emit(Tac::Jit { src, label }),
            _ => panic!("tried to generated shortcircuit for wrong op"),
        }

        let src = self.lower_expr(rhs);

        self.emit(Tac::Copy { dest, src });

        self.emit_label(label);

        dest
    }

    fn lower_value(&mut self, value: Value) -> VReg {
        match value {
            Value::Int(i) => self.load_const(TacConst::Int(i)),
            Value::Null => self.load_const(TacConst::Null),
            Value::Float(f) => self.load_const(TacConst::Float(f)),
            Value::Bool(b) => self.load_const(TacConst::Bool(b)),
            Value::String(s) => self.lower_segmented_string(s),
            Value::Symbol(s) => self.load_const(TacConst::Sym(s)),
            Value::Global(sym_id) => self.load_global(sym_id),
            Value::Ident(sym_id) => self.lower_ident(sym_id),
            Value::Map(map) => self.lower_map(map),
            Value::List(list) => self.lower_list(list),
            Value::InlineFunc { inputs, stmts } => self.lower_func(inputs, stmts, None),
        }
    }

    fn lower_segmented_string(&mut self, segmented_string: SegmentedString) -> VReg {
        let mut store = None;

        for (i, segment) in segmented_string.segments.into_iter().enumerate() {

            match segment {
                StringSegment::String(s) => {
                    let src = self.load_const(TacConst::String(s));

                    if i == 0 {
                        store = Some(src);
                    } else {
                        self.emit(Tac::Push { store: store.unwrap(), src });
                    }
                }
                StringSegment::Expr(e) => {
                    let src = self.lower_expr(e);

                    self.emit(Tac::Push { store: store.unwrap(), src });
                }
            }
        }

        store.unwrap()
    }

    fn load_global(&mut self, sym_id: SymID) -> VReg {
        let sym = self.load_const(TacConst::Sym(sym_id));
        let dest = self.new_temp();

        self.emit(Tac::LoadGlobal { dest, sym });

        dest
    }

    fn lower_call(&mut self, calle: VReg, args: Vec<VReg>, span: Span) -> VReg {
        for src in args.into_iter() {
            self.emit(Tac::StoreArg { src })
        }

        let dest = self.new_temp();

        self.emit_spanned(Tac::Call { dest, src: calle }, span);

        dest
    }

    fn lower_key_store(&mut self, store: VReg, key: VReg, src: VReg, span: Span) {
        self.emit_spanned(Tac::MemStore { store, key, src }, span);
    }

    fn lower_key_load(&mut self, store: VReg, key: VReg, span: Span) -> VReg {
        let dest = self.new_temp();

        self.emit_spanned(Tac::MemLoad { dest, store, key }, span);

        dest
    }

    fn lower_break(&mut self) {
        let label = self.get_loop_end();

        self.emit_jump(label);
    }

    fn lower_continue(&mut self) {
        let label = self.get_loop_start();

        self.emit_jump(label);
    }

    fn lower_func_decl(&mut self, ident: SymID, inputs: Spanned<Vec<SymID>>, stmts: Vec<Stmt>) {
        self.define_var(ident);

        self.lower_func(inputs, stmts, Some(ident));
    }

    // for loops are syntactic sugar!
    //
    // for value in list {
    //      print(value);
    // }
    //
    // translates into...
    //
    // iter = list.iter();
    // while true {
    //      value = iter();
    //      if value == null {
    //          break;
    //      }
    //      print(value);
    // } 
    fn lower_for_loop(
        &mut self,
        item_sym: SymID,
        store_expr: Spanned<Expr>,
        stmts: Vec<Stmt>,
    ) {
        let store_span = store_expr.get_span();
        let store_reg = self.lower_expr(store_expr);
        let iter_sym = self.load_const(TacConst::Sym(ITER_SYM));
        let loaded_iter = self.new_temp();
        self.emit(Tac::MemLoad {
            dest: loaded_iter,
            store: store_reg,
            key: iter_sym
        });
        let iter_fn_reg = self.lower_call(loaded_iter, vec![], store_span);

        let start = self.new_label();
        let end = self.new_label();

        self.push_loop_ctx(LoopCtx { start, end });

        self.emit_label(start);
        self.define_var(item_sym);
        let value_reg = self.lower_ident(item_sym);
        self.emit(Tac::Call { dest: value_reg, src: iter_fn_reg });

        let null_reg = self.load_const(TacConst::Null);
        let temp = self.new_temp();
        self.emit(Tac::Binop { dest: temp, op: BinaryOp::Equal, lhs: value_reg, rhs: null_reg });
        self.emit(Tac::Jit { src: temp, label: end });

        self.lower_stmts(stmts);

        self.emit_jump(start);
        self.emit_label(end);
    }

    fn lower_func(
        &mut self,
        inputs: Spanned<Vec<SymID>>,
        stmts: Vec<Stmt>,
        ident: Option<SymID>,
    ) -> VReg {
        let func = if let Some(sym) = ident {
            self.sym_to_reg(&sym)
        } else {
            self.new_temp()
        };

        let (func_id, upvalues) = self.new_func(inputs, stmts);

        self.emit(Tac::LoadConst {
            dest: func,
            src: TacConst::Func(func_id),
        });

        for sym_id in upvalues.iter() {
            let src = self.lower_ident(*sym_id);

            self.emit(Tac::StoreUpvalue { func, src });
        }

        func
    }

    fn new_func(&mut self, inputs: Spanned<Vec<SymID>>, stmts: Vec<Stmt>) -> (FuncID, Vec<SymID>) {
        let func_id = self.new_func_id();
        let generator = FuncLoweringCtx::new(func_id, inputs.item, self.pretty_ir);

        self.funcs.push(generator);

        self.lower_stmts(stmts);

        let func_builder = &self.funcs.last().unwrap().builder;
        let upvalues = func_builder.upvalues.clone();

        self.push_current_func();

        (func_id, upvalues)
    }

    fn push_current_func(&mut self) {
        let builder = self.funcs.pop().unwrap().builder;
        let func = builder.build();

        self.lowered_funcs.push(func);
    }

    fn lower_if_block(&mut self, cond: Spanned<Expr>, stmts: Vec<Stmt>) {
        let label = self.new_label();
        let src = self.lower_expr(cond);

        self.emit(Tac::Jnt { src, label });
        self.lower_stmts(stmts);
        self.emit_label(label);
    }

    fn lower_if_else_block(
        &mut self,
        cond: Spanned<Expr>,
        stmts: Vec<Stmt>,
        else_stmts: Vec<Stmt>,
    ) {
        let else_end = self.new_label();
        let else_start = self.new_label();
        let src = self.lower_expr(cond);

        self.emit(Tac::Jnt {
            src,
            label: else_start,
        });
        self.lower_stmts(stmts);
        self.emit_jump(else_end);
        self.emit_label(else_start);
        self.lower_stmts(else_stmts);
        self.emit_label(else_end);
    }

    fn lower_while_block(&mut self, cond: Spanned<Expr>, stmts: Vec<Stmt>) {
        let start = self.new_label();
        let end = self.new_label();

        self.push_loop_ctx(LoopCtx { start, end });

        self.emit_label(start);

        let src = self.lower_expr(cond);

        self.emit(Tac::Jnt { src, label: end });
        self.lower_stmts(stmts);
        self.emit_jump(start);
        self.emit_label(end);
    }

    fn lower_ident(&mut self, sym_id: SymID) -> VReg {
        if self.defined_local(sym_id) {
            self.sym_to_reg(&sym_id)
        } else if self.defined_upvalue(sym_id) {
            let dest = self.sym_to_reg(&sym_id);

            self.set_upvalue(sym_id);

            let upvalue = self
                .get_current_func_mut()
                .builder
                .upvalues
                .iter()
                .position(|uv| *uv == sym_id)
                .unwrap();

            let tac = Tac::LoadUpvalue {
                dest,
                id: u16::try_from(upvalue).unwrap(),
            };

            self.emit(tac);

            dest
        } else if SymbolMap::is_intrinsic(sym_id) {
            let dest = self.sym_to_reg(&sym_id);

            self.emit(Tac::LoadConst {
                dest,
                src: TacConst::Sym(sym_id),
            });

            dest
        } else {
            // this is an uninitialized variable
            let dest = self.sym_to_reg(&sym_id);

            self.emit(Tac::LoadConst {
                dest,
                src: TacConst::Null,
            });

            dest
        }
    }

    fn lower_map(&mut self, pairs: Vec<(MapKey, Spanned<Expr>)>) -> VReg {
        let store = self.new_temp();

        self.emit(Tac::NewMap { dest: store });

        for (k, value) in pairs.into_iter() {
            let key = match k {
                MapKey::Sym(sym) => self.load_const(TacConst::Sym(sym)),
                MapKey::Expr(expr) => self.lower_expr(expr),
            };
            let src = self.lower_expr(value);

            // this doesn't need to be spanned since we are sure we are storing into a map this
            // can't fail so we don't need spanning info
            self.emit(Tac::MemStore { store, key, src });
        }

        store
    }

    fn lower_list(&mut self, exprs: Vec<Spanned<Expr>>) -> VReg {
        let store = self.new_temp();

        self.emit(Tac::NewList { dest: store });

        for (i, e) in exprs.into_iter().enumerate() {
            let src = self.lower_expr(e);
            let key = self.new_temp();

            self.emit(Tac::LoadConst {
                dest: key,
                src: TacConst::Int(i64::try_from(i).unwrap()),
            });

            self.emit(Tac::Push { store, src });
        }

        store
    }

    fn lower_push(&mut self, lhs: VReg, rhs: VReg, span: Span) -> VReg {
        self.emit_spanned(Tac::Push { store: lhs, src: rhs }, span);

        self.new_temp()
    }

    fn lower_unary_op(&mut self, op: UnaryOp, src: VReg, span: Span) -> VReg {
        let dest = self.new_temp();

        self.emit_spanned(Tac::Unaop { dest, op, src }, span);

        dest
    }

    fn lower_binop(&mut self, lhs: VReg, op: BinaryOp, rhs: VReg, span: Span) -> VReg {
        let dest = self.new_temp();

        self.emit_spanned(Tac::Binop { dest, lhs, op, rhs }, span);

        dest
    }

    fn lower_print(&mut self, src: VReg) -> VReg {
        self.emit(Tac::Print { src });

        self.new_temp()
    }

    fn lower_read(&mut self) -> VReg {
        let dest = self.new_temp();

        self.emit(Tac::Read { dest });

        dest
    }

    fn load_const(&mut self, tac_const: TacConst) -> VReg {
        let dest = self.new_temp();

        self.emit(Tac::LoadConst {
            dest,
            src: tac_const,
        });

        dest
    }

    fn emit_label(&mut self, label: LabelID) {
        self.emit(Tac::Label { label })
    }

    fn emit_jump(&mut self, label: LabelID) {
        self.emit(Tac::Jump { label })
    }

    fn last_instr_has_dest(&self) -> bool {
        self.get_current_func()
            .builder
            .last_instr()
            .unwrap()
            .dest_reg()
            .is_some()
    }

    fn update_prev_dest(&mut self, sym: SymID) {
        let reg = self.sym_to_reg(&sym);
        let f = self.get_current_func_mut();
        let dest = f.builder.last_instr_mut().unwrap().dest_reg_mut().unwrap();

        *dest = reg;
    }

    fn sym_to_reg(&mut self, sym: &SymID) -> VReg {
        self.funcs.last_mut().unwrap().builder.sym_to_reg(sym)
    }

    fn emit(&mut self, instr: Tac) {
        self.funcs
            .last_mut()
            .unwrap()
            .builder
            .push_instr(instr, None);
    }

    fn emit_spanned(&mut self, instr: Tac, span: Span) {
        self.funcs
            .last_mut()
            .unwrap()
            .builder
            .push_instr(instr, Some(span));
    }

    fn push_loop_ctx(&mut self, ctx: LoopCtx) {
        self.get_current_func_mut().loop_ctxs.push(ctx);
    }

    fn new_func_id(&mut self) -> FuncID {
        self.func_counter += 1;
        self.func_counter
    }

    fn new_temp(&mut self) -> VReg {
        let f = self.get_current_func_mut();
        f.builder.new_reg()
    }

    fn new_label(&mut self) -> LabelID {
        let f = self.get_current_func_mut();

        f.label_counter += 1;
        f.label_counter
    }

    fn get_loop_start(&mut self) -> LabelID {
        self.get_current_func_mut().loop_ctxs.last().unwrap().start
    }

    fn get_loop_end(&mut self) -> LabelID {
        self.get_current_func_mut().loop_ctxs.last().unwrap().end
    }

    fn define_var(&mut self, sym: SymID) {
        self.get_current_func_mut().defined_variables.insert(sym);
    }

    fn defined_local(&self, sym: SymID) -> bool {
        self.get_current_func().defined_variables.contains(&sym)
    }

    fn defined_upvalue(&self, sym: SymID) -> bool {
        for i in 0..(self.funcs.len() - 1) {
            if self.funcs[i].defined_variables.contains(&sym) {
                return true;
            }
        }

        false
    }

    fn set_upvalue(&mut self, sym: SymID) {
        if self.get_current_func().builder.upvalues.contains(&sym) {
            return;
        }

        self.get_current_func_mut().builder.upvalues.push(sym);
    }

    fn lower_stmts(&mut self, stmts: Vec<Stmt>) {
        for stmt in stmts.into_iter() {
            self.lower_stmt(stmt);
        }
    }

    fn get_current_func_mut(&mut self) -> &mut FuncLoweringCtx {
        self.funcs.last_mut().unwrap()
    }

    fn get_current_func(&self) -> &FuncLoweringCtx {
        self.funcs.last().unwrap()
    }
}

pub fn lower_ast(stmts: Vec<Stmt>, pretty_ir: bool) -> Vec<Func> {
    let mut ctx = LoweringCtx::new(pretty_ir);

    ctx.lower_stmts(stmts);

    ctx.push_current_func();

    ctx.lowered_funcs
}

#[cfg(test)]
pub mod tests {
    use super::super::func_builder::tests::instrs_to_func;
    use super::super::func_to_string;
    use super::*;
    use crate::parser::parse_program;
    use crate::symbol_map::SymbolMap;
    use pretty_assertions::assert_eq;

    fn expect_tac(input: &str, expected_code: Vec<Tac>) {
        let mut syms = SymbolMap::new();

        expect_tac_with_syms(input, expected_code, &mut syms);
    }

    fn expect_tac_with_syms(input: &str, expected_code: Vec<Tac>, syms: &mut SymbolMap) {
        let ast = parse_program(input, syms, None).unwrap();
        let expected_func = instrs_to_func(expected_code);
        let top_level_func = lower_ast(ast, false).pop().unwrap();

        let expected_string = func_to_string(&expected_func, syms);
        let found_string = func_to_string(&top_level_func, syms);

        assert_eq!(expected_string, found_string);
    }

    #[test]
    fn expr_stmt_tac() {
        let tac = vec![
            Tac::LoadConst {
                dest: 0,
                src: TacConst::Int(1),
            },
            Tac::LoadConst {
                dest: 1,
                src: TacConst::Null,
            },
            Tac::Return { src: 1 },
        ];
        let input = "1;";

        expect_tac(input, tac);
    }

    #[test]
    fn generates_expr_tac() {
        let tac = vec![
            Tac::LoadConst {
                dest: 0,
                src: TacConst::Int(1),
            },
            Tac::LoadConst {
                dest: 1,
                src: TacConst::Int(1),
            },
            Tac::LoadConst {
                dest: 2,
                src: TacConst::Int(1),
            },
            Tac::Binop {
                dest: 3,
                lhs: 1,
                op: BinaryOp::Multiply,
                rhs: 2,
            },
            Tac::Binop {
                dest: 4,
                lhs: 0,
                op: BinaryOp::Multiply,
                rhs: 3,
            },
            Tac::LoadConst {
                dest: 5,
                src: TacConst::Null,
            },
            Tac::Return { src: 5 },
        ];
        let input = "1 * 1 * 1;";

        expect_tac(input, tac);
    }

    #[test]
    fn index_assignment_tac() {
        let tac = vec![
            Tac::LoadConst {
                dest: 0,
                src: TacConst::Int(1),
            },
            Tac::NewList { dest: 1 },
            Tac::LoadConst {
                dest: 2,
                src: TacConst::Int(1),
            },
            Tac::LoadConst {
                dest: 3,
                src: TacConst::Int(0),
            },
            Tac::Push {
                src: 2,
                store: 1,
            },
            Tac::LoadConst {
                dest: 4,
                src: TacConst::Int(0),
            },
            Tac::MemStore {
                src: 0,
                store: 1,
                key: 4,
            },
            Tac::LoadConst {
                dest: 5,
                src: TacConst::Null,
            },
            Tac::Return { src: 5 },
        ];
        let input = "[1][0] = 1;";

        expect_tac(input, tac);
    }

    #[test]
    fn access_assignment_tac() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            Tac::LoadConst {
                dest: 0,
                src: TacConst::Int(1),
            },
            Tac::LoadConst {
                dest: 1,
                src: TacConst::Null,
            },
            Tac::LoadConst {
                dest: 2,
                src: TacConst::Sym(syms.get_id("b")),
            },
            Tac::MemStore {
                src: 0,
                store: 1,
                key: 2,
            },
            Tac::LoadConst {
                dest: 3,
                src: TacConst::Null,
            },
            Tac::Return { src: 3 },
        ];
        let input = "a.b = 1;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn assign_local() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            Tac::LoadConst {
                dest: 1,
                src: TacConst::Int(1),
            },
            Tac::LoadConst {
                dest: 2,
                src: TacConst::Null,
            },
            Tac::Return { src: 2 },
        ];
        let input = "a = 1;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn assign_global() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            Tac::LoadConst {
                dest: 0,
                src: TacConst::Int(1),
            },
            Tac::LoadConst {
                dest: 1,
                src: TacConst::Sym(syms.get_id("a")),
            },
            Tac::StoreGlobal { src: 0, sym: 1 },
            Tac::LoadConst {
                dest: 2,
                src: TacConst::Null,
            },
            Tac::Return { src: 2 },
        ];
        let input = "@a = 1;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn generates_return_with_value() {
        let tac = vec![
            Tac::LoadConst {
                dest: 0,
                src: TacConst::Bool(true),
            },
            Tac::Return { src: 0 },
        ];
        let input = "return true;";

        expect_tac(input, tac);
    }

    #[test]
    fn generates_return_with_no_value() {
        let tac = vec![Tac::Return { src: 0 }];
        let input = "return;";

        expect_tac(input, tac);
    }

    #[test]
    fn generates_print_expr() {
        let tac = vec![
            Tac::LoadConst {
                dest: 0,
                src: TacConst::String("Hello World".to_string()),
            },
            Tac::Print { src: 0 },
            Tac::LoadConst {
                dest: 2,
                src: TacConst::Null,
            },
            Tac::Return { src: 2 },
        ];
        let input = "print(\"Hello World\");";

        expect_tac(input, tac);
    }

    #[test]
    fn generates_break_and_continue_stmts() {
        let tac = vec![
            Tac::Label { label: 1 },
            Tac::LoadConst {
                dest: 0,
                src: TacConst::Bool(true),
            },
            Tac::Jnt { src: 0, label: 2 },
            Tac::Jump { label: 1 },
            Tac::Jump { label: 2 },
            Tac::Jump { label: 1 },
            Tac::Label { label: 2 },
            Tac::LoadConst {
                dest: 1,
                src: TacConst::Null,
            },
            Tac::Return { src: 1 },
        ];
        let input = "while true { continue; break; }";

        expect_tac(input, tac);
    }

    #[test]
    fn call_multiple_args() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            Tac::LoadConst {
                dest: 0,
                src: TacConst::Int(0),
            },
            Tac::LoadConst {
                dest: 1,
                src: TacConst::Int(1),
            },
            Tac::LoadConst {
                dest: 2,
                src: TacConst::Int(2),
            },
            Tac::LoadConst {
                dest: 3,
                src: TacConst::Null,
            },
            Tac::StoreArg { src: 0 },
            Tac::StoreArg { src: 1 },
            Tac::StoreArg { src: 2 },
            Tac::Call { dest: 4, src: 3 },
            Tac::LoadConst {
                dest: 5,
                src: TacConst::Null,
            },
            Tac::Return { src: 5 },
        ];
        let input = "a(0, 1, 2);";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn use_defined_local() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            Tac::LoadConst {
                dest: 1,
                src: TacConst::Int(1),
            },
            Tac::Copy { dest: 2, src: 1 },
            Tac::LoadConst {
                dest: 3,
                src: TacConst::Null,
            },
            Tac::Return { src: 3 },
        ];
        let input = "a = 1; b = a;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn simple_if_stmt() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            Tac::LoadConst {
                dest: 0,
                src: TacConst::Bool(true),
            },
            Tac::Jnt { label: 1, src: 0 },
            Tac::LoadConst {
                dest: 1,
                src: TacConst::String(String::from("test")),
            },
            Tac::Print { src: 1 },
            Tac::Label { label: 1 },
            Tac::LoadConst {
                dest: 3,
                src: TacConst::Null,
            },
            Tac::Return { src: 3 },
        ];
        let input = "if true { print(\"test\"); }";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn load_null() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            Tac::LoadConst {
                dest: 1,
                src: TacConst::Null,
            },
            Tac::LoadConst {
                dest: 2,
                src: TacConst::Null,
            },
            Tac::Return { src: 2 },
        ];
        let input = "a = null;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn sym_key_load() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            Tac::LoadConst {
                dest: 0,
                src: TacConst::Null,
            },
            Tac::LoadConst {
                dest: 1,
                src: TacConst::Sym(syms.get_id("c")),
            },
            Tac::MemLoad {
                dest: 3,
                store: 0,
                key: 1,
            },
            Tac::LoadConst {
                dest: 4,
                src: TacConst::Null,
            },
            Tac::Return { src: 4 },
        ];
        let input = "a = b.c;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn val_key_load() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            Tac::LoadConst {
                dest: 0,
                src: TacConst::Null,
            },
            Tac::LoadConst {
                dest: 1,
                src: TacConst::Int(0),
            },
            Tac::MemLoad {
                dest: 3,
                store: 0,
                key: 1,
            },
            Tac::LoadConst {
                dest: 4,
                src: TacConst::Null,
            },
            Tac::Return { src: 4 },
        ];
        let input = "a = b[0];";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn load_global() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            Tac::LoadConst {
                dest: 0,
                src: TacConst::Sym(syms.get_id("b")),
            },
            Tac::LoadGlobal { dest: 2, sym: 0 },
            Tac::LoadConst {
                dest: 3,
                src: TacConst::Null,
            },
            Tac::Return { src: 3 },
        ];
        let input = "a = @b;";

        expect_tac_with_syms(input, tac, &mut syms);
    }

    #[test]
    fn map_assignment() {
        let mut syms = SymbolMap::new();
        let tac = vec![
            Tac::NewMap { dest: 0 },
            Tac::LoadConst {
                dest: 1,
                src: TacConst::Sym(syms.get_id("b")),
            },
            Tac::LoadConst {
                dest: 2,
                src: TacConst::Int(0),
            },
            Tac::MemStore {
                store: 0,
                key: 1,
                src: 2,
            },
            Tac::Copy { dest: 3, src: 0 },
            Tac::LoadConst {
                dest: 4,
                src: TacConst::Null,
            },
            Tac::Return { src: 4 },
        ];
        let input = "a = { b: 0 };";

        expect_tac_with_syms(input, tac, &mut syms);
    }
}
