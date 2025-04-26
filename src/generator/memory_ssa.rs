use std::collections::HashMap;
use super::tac::{Var, TacConst, VerID, Tac};
use super::cfg::{CFG, BlockID};
use super::escape_dfa::MemId;
use super::gvn::ValueId;

// The purpose of this code in this file is to apply versions to Memory
// which applies versions to "MemoryLocation"s in such a way that a versioned 
// memory location has a static value. MemoryLocations are used in the 2
// TAC instructions MemStore & MemLoad. By making versioned MemoryLocation's
// have a static value, the GVN algorithm can more easily be implemented to work
// with values stored/loaded from memory locations.
//
// This algorithm is somewhat conservative, meaning it may be able to do better at scoping
// the amount of memory conflicts if more analysis were to be done. Currently
// this algorithm only relies on 2 pieces of information about a memory store/load:
//  1. is the memory location tracked or escaped, meaning is it possible the variable
//      being stored to is referenced in the heap(escaped) or not(tracked)
//  2. Is the key used to access the memory location a constant? 
//      If so what is the constant value?
// The tracking and escape information is provided by the escape dfa.
// The key constant information is provided by the GVN pass.
//
// Refresher:
//  - a memory location consists of a store and a key
//  - a memory location can have a tracking ID or be escaped
//
// Memory Conflict Rules:
//  - a memory conflict may happen during a MemStore instruction
//  - a conflict is when a store to one location may overwrite some other memory location
//  - two memory locations conflicts only if both their keys AND locations conflict
//  - escaped locations conflict with all escaped locations
//  - tracked locations only conflict with tracked locations of the same ID
//  - constant keys conflict with same value constant keys and also variable keys
//  - variable keys conflict with all keys
//
// What Happens when a Memory Conflict Occurs?
//  - the memory SSA algorithm keeps track of the versions of all the used memory locations
//  - when a store happens, the algorithm will increment the versions of all 
//      conflicting memory locations
//  - when a memory location is stored/loaded the algorithm will apply the 
//      current version for that location
//  - the algorithm propagates changes to memory location versions through the 
//      CFG until it reaches convergence

#[derive(Debug, PartialEq)]
pub struct MemoryAccess {
    pub store: Var,
    pub key: Var,
    ssa_data: Box<MemoryAccessSSAData> // Box limits size of TAC... premature optimization?
}

impl MemoryAccess {
    pub fn new(store: Var, key: Var) -> Self {
        Self {
            store,
            key,
            ssa_data: Box::new(MemoryAccessSSAData::new())
        }
    }

    pub fn set_mem_id(&mut self, mem_id: MemId) {
        self.ssa_data.canon_store = Some(CanonMemStore::MemId(mem_id));
    }
}

#[derive(Debug, PartialEq)]
struct MemoryAccessSSAData {
    canon_store: Option<CanonMemStore>,
    canon_key: Option<CanonMemKey>,
    ssa_version: Option<usize>,
}

impl MemoryAccessSSAData {
    fn new() -> Self {
        Self {
            canon_store: None,
            canon_key: None,
            ssa_version: None,
        }
    }
}

#[derive(Debug, PartialEq)]
struct CanonicalMemoryAccess {
    store: CanonMemStore,
    key: CanonMemKey,
}

impl CanonicalMemoryAccess {
    fn new(store: CanonMemStore, key: CanonMemKey) -> Self {
        Self {
            store,
            key
        }
    }
}

#[derive(Debug, PartialEq)]
enum CanonMemStore {
    Var(Var),
    Value(ValueId),
    MemId(MemId)
}

#[derive(Debug, PartialEq)]
enum CanonMemKey {
    Var(Var),
    Value {
        id: ValueId,
        is_const: bool
    },
}

struct MemorySSASnapshot {
    current_versions: HashMap<CanonicalMemoryAccess, usize>,
}

struct MemorySSADFA {
    version_counter: usize,
    resolved_entry_versions: HashMap<BlockID, HashMap<CanonicalMemoryAccess, usize>>,
}

pub fn apply_memory_ssa(cfg: &mut CFG) {
    // create a worklist of blocks
    // get block A from the worklist
    // for 
}
