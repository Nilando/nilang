use sandpit::Mutator;

use super::func::LoadedFunc;
use super::ByteCode;

const TIMES_BUILT_IN_ID: u32 = 0;

pub fn times<'gc>(m: &'gc Mutator<'gc>) -> LoadedFunc<'gc> {
    // fn times(x, f) {
        // list = []
        // i = 0
        // while i < x {
        //   list[i] = f(i)
        //   i += 1;
        // }
        // return list
    // }
    let code = [
        ByteCode::NewList { dest: 2 },
        ByteCode::LoadInt { dest: 3, val: 0 },
        ByteCode::LoadInt { dest: 4, val: 1 },
        ByteCode::Lt { dest: 5, lhs: 3, rhs: 0 },
        ByteCode::Jnt { src: 5, offset: 6 },
        ByteCode::StoreArg { src: 3 },
        ByteCode::Call { dest: 5, src: 1 },
        ByteCode::MemStore { store: 2, key: 3, src: 5 },
        ByteCode::Add { dest: 3, lhs: 3, rhs: 4 },
        ByteCode::Jump { offset: -6 },
        ByteCode::Return { src: 2 },
    ];

    LoadedFunc::new(
        TIMES_BUILT_IN_ID,
        2,
        6,
        m.alloc_array_from_fn(0, |_| unreachable!()),
        m.alloc_array_from_slice(&code),
        None
    )
}
