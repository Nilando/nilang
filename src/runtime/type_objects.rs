use sandpit::{Gc, Mutator, Trace};

use crate::symbol_map::{SymID, BOOL_SYM, FLOAT_SYM, FN_SYM, INT_SYM, LIST_SYM, MAP_SYM, NULL_SYM, STR_SYM, SYM_SYM};

use super::hash_map::GcHashMap;

#[derive(Trace)]
pub struct TypeObjects<'gc> {
    pub null_obj: Gc<'gc, GcHashMap<'gc>>,
    pub bool_obj: Gc<'gc, GcHashMap<'gc>>,
    pub sym_obj: Gc<'gc, GcHashMap<'gc>>,
    pub str_obj: Gc<'gc, GcHashMap<'gc>>,
    pub int_obj: Gc<'gc, GcHashMap<'gc>>,
    pub float_obj: Gc<'gc, GcHashMap<'gc>>,
    pub list_obj: Gc<'gc, GcHashMap<'gc>>,
    pub map_obj: Gc<'gc, GcHashMap<'gc>>,
    pub fn_obj: Gc<'gc, GcHashMap<'gc>>,
}

impl<'gc> TypeObjects<'gc> {
    pub fn alloc(mu: &'gc Mutator) -> Self {
        Self { 
            null_obj: GcHashMap::alloc(mu), 
            bool_obj: GcHashMap::alloc(mu), 
            sym_obj: GcHashMap::alloc(mu), 
            str_obj: GcHashMap::alloc(mu), 
            int_obj: GcHashMap::alloc(mu), 
            float_obj: GcHashMap::alloc(mu), 
            list_obj: GcHashMap::alloc(mu), 
            map_obj: GcHashMap::alloc(mu), 
            fn_obj: GcHashMap::alloc(mu) 
        }
    }

    pub fn get_type_obj(&self, sym_id: SymID) -> Option<Gc<'gc, GcHashMap<'gc>>> {
        Some(
            match sym_id {
                INT_SYM => self.int_obj.clone(),
                FLOAT_SYM => self.float_obj.clone(),
                STR_SYM => self.str_obj.clone(),
                BOOL_SYM => self.bool_obj.clone(),
                SYM_SYM => self.sym_obj.clone(),
                NULL_SYM => self.null_obj.clone(),
                LIST_SYM => self.list_obj.clone(),
                MAP_SYM => self.map_obj.clone(),
                FN_SYM => self.fn_obj.clone(),
                _ => return None

            }
        )
    }
}
