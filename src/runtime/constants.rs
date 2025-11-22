/// Runtime configuration constants for the Nilang VM
///
/// This module centralizes all configuration constants used throughout the runtime.

/// Maximum depth of the call stack before stack overflow
///
/// This prevents infinite recursion from exhausting memory.
/// Value of 1000 allows for reasonable recursion depth while preventing abuse.
pub const MAX_CALL_STACK_DEPTH: usize = 1000;

/// Number of bytecode instructions to execute before yielding control
///
/// The VM executes this many instructions before checking for GC pressure
/// and handling interrupts. This value has not yet been benchmarked/tuned.
pub const DISPATCH_LOOP_LENGTH: usize = 1000;

/// Maximum load factor for hash maps before resizing
///
/// When the ratio of used buckets to total capacity exceeds this threshold,
/// the hash map will grow. A value of 0.7 balances memory usage and performance.
pub const HASH_MAP_MAX_LOAD: f64 = 0.7;

/// Initial capacity for new hash maps
///
/// Starting with 16 buckets provides reasonable initial size while avoiding
/// immediate resizes for small maps.
pub const HASH_MAP_INIT_CAPACITY: usize = 16;
