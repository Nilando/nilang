use crate::lexer::Op;
use super::raw_value::RawValue;
use super::generator::{Var, LabelID};

#[derive(Debug)]
pub enum IR {
    Binop {
        dest: Var,
        op: Op,
        lhs: RawValue,
        rhs: RawValue,
    },
    ObjStore {
        obj: RawValue,
        key: RawValue,
        val: RawValue,
    },
    ObjLoad {
        dest: Var,
        obj: RawValue,
        key: RawValue,
    },
    NewList {
        dest: Var,
    },
    NewMap {
        dest: Var,
    },
    Log {
        src: RawValue,
    },
    Load {
        dest: Var,
        src: RawValue,
    },

    // Control Flow Codes Below
    Call {
        dest: Var,
        calle: RawValue,
        input: RawValue,
    },
    Jump {
        label: LabelID,
    },
    Return {
        src: RawValue,
    },
    Jnt {
        label: LabelID,
        cond: RawValue,
    },
    Label {
        id: LabelID,
    },
}
