use hashbrown::HashMap;
use alloc::rc::Rc;

pub type SymID = u32;

pub struct SymbolMap {
    str_to_id: HashMap<Rc<String>, SymID>,
    id_to_str: Vec<Rc<String>>,
}

// special symbols
macro_rules! generate_intrinsic_syms {
    ($($name:ident),*) => {
        // Generate consts for each intrinsic symbol with sequential SymID.
        #[allow(non_camel_case_types)]
        #[repr(u32)]
        enum Intrinsics {
            $($name,)*
        }

        $(
            pub const $name: SymID = Intrinsics::$name as u32;
        )*

        impl SymbolMap {
            // Create the is_intrinsic function to check if a symbol is intrinsic.
            pub fn is_intrinsic(sym: SymID) -> bool {
                match sym {
                    $(
                        $name => true,
                    )*
                    _ => false,
                }
            }

            // Create the init function to insert each intrinsic symbol in the correct order.
            pub fn insert_intrinsics(&mut self) {
                let symbols = vec![
                    $(
                        stringify!($name).trim_end_matches("_SYM").to_ascii_lowercase(),
                    )*
                ];

                for symbol in symbols {
                    self.insert(symbol.to_string());
                }
            }
        }
    };
}

// Use the macro to generate the necessary parts.
generate_intrinsic_syms! {
    INT_SYM,
    FLOAT_SYM,
    STR_SYM,
    BOOL_SYM,
    SYM_SYM,
    NULL_SYM,
    LIST_SYM,
    MAP_SYM,
    FN_SYM,

    // Utility intrinsics
    ARGS_SYM,
    PATCH_SYM,

    // TODO: REMOVE THESE JUST MAKE CONSTANT NOT INTRINSIC
    SELF_SYM,
    ITER_SYM
}

impl SymbolMap {
    pub fn new() -> Self {
        let mut this = Self {
            str_to_id: HashMap::new(),
            id_to_str: Vec::new(),
        };

        this.insert_intrinsics();

        this
    }


    pub fn get_id(&mut self, str: &str) -> SymID {
        let str = str.to_string();
        match self.str_to_id.get(&str) {
            Some(id) => *id,
            None => self.insert(str),
        }
    }

    pub fn get_str(&mut self, id: SymID) -> &str {
        &self.id_to_str[id as usize]
    }

    fn insert(&mut self, str: String) -> SymID {
        let id = self.id_to_str.len() as u32;
        let s = Rc::new(str);

        self.id_to_str.push(s.clone());
        self.str_to_id.insert(s.clone(), id);

        id
    }
}
