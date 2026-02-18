mod codegen;
mod ir;
mod parser;
mod runtime;
mod symbol_map;
mod compile;
mod repl;
mod operators;
mod spanned;

extern crate alloc;

pub use runtime::{Runtime, Config, InterpreterError};
pub use symbol_map::SymbolMap;
pub use repl::run_repl;
pub use compile::compile;

// Runtime types for FFI consumers
pub use runtime::{
    Value, TaggedValue, ValueTag,
    Func, NativeFunc, NativeFn, List, VMString, GcHashMap,
    ByteCode, RuntimeError, RuntimeErrorKind,
    Backtrace, BacktraceCall,
};

// Re-export sandpit types so FFI consumers don't need a separate sandpit dependency
pub use sandpit::{Gc, GcOpt, Mutator, Arena, Root, Tagged, Trace, TraceLeaf, GcVec};

// Re-export symbol types
pub use symbol_map::{
    SymID, INT_SYM, FLOAT_SYM, STR_SYM, BOOL_SYM, SYM_SYM,
    NULL_SYM, LIST_SYM, MAP_SYM, FN_SYM,
};
