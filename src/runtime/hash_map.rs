use crate::symbol_map::SymbolMap;
use core::cell::Cell;

use super::list::List;
use super::op::equal;

use murmurhash3::murmurhash3_x64_128;
use sandpit::{field, Gc, Mutator, Trace, TraceLeaf};

use super::tagged_value::TaggedValue;
use super::value::Value;

// features of thie HashMap
// - uses power of 2 capacities
// - uses quadratic probing
// - uses murmurhash3 hashing

const MAX_LOAD: f64 = 0.7;
const INIT_CAPACITY: usize = 16;

#[derive(TraceLeaf, Copy, Clone, PartialEq)]
enum EntryStatus {
    Used,
    Free,
    Dead,
}

#[derive(Trace)]
struct Entry<'gc> {
    status: Cell<EntryStatus>,
    key: TaggedValue<'gc>,
    val: TaggedValue<'gc>,
}

impl<'gc> Entry<'gc> {
    pub fn new() -> Self {
        Self {
            status: Cell::new(EntryStatus::Free),
            key: TaggedValue::new_null(),
            val: TaggedValue::new_null(),
        }
    }

    pub fn is_free(&self) -> bool {
        matches!(self.status.get(), EntryStatus::Free)
    }

    pub fn is_used(&self) -> bool {
        matches!(self.status.get(), EntryStatus::Used)
    }
}

#[derive(Trace)]
pub struct GcHashMap<'gc> {
    buckets: Gc<'gc, [Entry<'gc>]>,
    entries: Cell<usize>,
}

// have null always map to the 0 index
// that way we can use the null key to represent a tombstone entry

impl<'gc> GcHashMap<'gc> {
    pub fn as_list(&self, mu: &'gc Mutator) -> Gc<'gc, List<'gc>> {
        let gc_list = Gc::new(mu, List::alloc(mu));

        for entry in self.buckets.iter() {
            if entry.status.get() == EntryStatus::Used {
                let inner_list = Gc::new(mu, List::alloc(mu));

                inner_list.push(entry.key.clone(), mu);
                inner_list.push(entry.val.clone(), mu);
                gc_list.push(Value::List(inner_list).as_tagged(mu), mu);
            }
        }

        gc_list
    }

    pub fn alloc(mu: &'gc Mutator) -> Gc<'gc, Self> {
        let hash_map = GcHashMap {
            buckets: mu.alloc_array_from_fn(INIT_CAPACITY, |_| Entry::new()),
            entries: Cell::new(0),
        };

        Gc::new(mu, hash_map)
    }

    pub fn insert(
        this: Gc<'gc, Self>,
        key: TaggedValue<'gc>,
        val: TaggedValue<'gc>,
        mu: &'gc Mutator,
    ) {
        if this.get_entry(&key).is_none() {
            this.entries.set(this.entries.get() + 1);

            if this.get_load() > MAX_LOAD {
                Self::grow(this.clone(), mu);
            }
        }

        loop {
            if let Some(idx) = this.get_key_index(&key) {
                this.buckets.write_barrier(mu, |barrier| {
                    let entry = barrier.at(idx);
                    let val_barrier = field!(&entry, Entry, val);
                    let key_barrier = field!(&entry, Entry, key);

                    let val_barrier = field!(&val_barrier, TaggedValue, ptr);
                    let key_barrier = field!(&key_barrier, TaggedValue, ptr);

                    entry.inner().status.set(EntryStatus::Used);
                    val_barrier.set(val.__get_ptr());
                    key_barrier.set(key.__get_ptr());
                });

                return;
            } else {
                Self::grow(this.clone(), mu);
            }
        }
    }

    pub fn get(&self, key: &TaggedValue<'gc>) -> Option<TaggedValue<'gc>> {
        self.get_entry(key).map(|entry| entry.val.clone())
    }

    fn get_entry(&self, key: &TaggedValue<'gc>) -> Option<&Entry<'gc>> {
        let idx = self.get_key_index(key)?;
        let entry = &self.buckets[idx];

        if entry.is_free() {
            return None;
        }

        Some(entry)
    }

    pub fn delete(&self, key: TaggedValue<'gc>) -> Option<TaggedValue<'gc>> {
        let idx = self.get_key_index(&key)?;
        let entry = &self.buckets[idx];

        if entry.is_free() {
            return None;
        }

        self.entries.set(self.entries.get() - 1);

        let result = Some(entry.val.clone());

        entry.key.set_null();
        entry.val.set_null();

        entry.status.set(EntryStatus::Dead);

        result
    }

    fn grow(this: Gc<'gc, Self>, mu: &'gc Mutator) {
        let new_cap = this.buckets.len() * 2;
        let new_buckets = mu.alloc_array_from_fn(new_cap, |_| Entry::new());
        let old_buckets = this.buckets.clone();

        // update to use the newly allocated array
        this.write_barrier(mu, |barrier| {
            let buckets_ptr = field!(barrier, GcHashMap, buckets);
            buckets_ptr.set(new_buckets);
        });

        // insert old used entries into the new array
        for entry in old_buckets.iter() {
            if entry.is_used() {
                Self::insert(this.clone(), entry.key.clone(), entry.val.clone(), mu);
            }
        }
    }

    fn get_capacity(&self) -> usize {
        self.buckets.len()
    }

    pub fn entries_count(&self) -> usize {
        self.entries.get()
    }

    fn get_load(&self) -> f64 {
        self.entries_count() as f64 / self.get_capacity() as f64
    }

    fn modulo_mask(&self) -> usize {
        self.get_capacity() - 1
    }

    fn get_key_index(&self, key: &TaggedValue<'gc>) -> Option<usize> {
        let hash_value = hash_value(&Value::from(key));
        let mask = self.modulo_mask();
        let mut probe_pos = hash_value & mask;
        let max_probes = self.get_capacity() / 2;
        let mut probes = 0;

        loop {
            let entry = &self.buckets[probe_pos];

            match entry.status.get() {
                EntryStatus::Free => return Some(probe_pos),
                EntryStatus::Used => {
                    let v1 = Value::from(key);
                    let v2 = Value::from(&entry.key);

                    if let Value::Bool(true) = equal(v1, v2) {
                        return Some(probe_pos);
                    }
                }
                EntryStatus::Dead => {}
            }

            probes += 1;
            probe_pos = (hash_value + (probes * probes)) & mask;
            if probes > max_probes {
                return None;
            }
        }
    }

    pub fn copy_entries_to(&self, dest: Gc<'gc, Self>, mu: &'gc Mutator) {
        // Copy all entries from self into dest
        for i in 0..self.buckets.len() {
            let entry = &self.buckets[i];
            if entry.is_used() {
                Self::insert(dest.clone(), entry.key.clone(), entry.val.clone(), mu);
            }
        }
    }

    pub fn is_structurally_equal_to(&self, other: &GcHashMap<'gc>) -> bool {
        use super::value::Value;

        if self.entries_count() != other.entries_count() {
            return false;
        }

        // Check that all entries in self exist in other with same values
        for i in 0..self.buckets.len() {
            let entry = &self.buckets[i];
            if entry.is_used() {
                let key = &entry.key;
                let val = &entry.val;

                if let Some(other_val) = other.get(key) {
                    if !Value::from(val).is_equal_to(&Value::from(&other_val)) {
                        return false;
                    }
                } else {
                    return false;
                }
            }
        }
        true
    }

    pub fn to_string(&self, syms: &mut SymbolMap) -> String {
        let mut visited = std::collections::HashSet::new();
        self.to_string_internal(syms, &mut visited)
    }

    pub(crate) fn to_string_internal(&self, syms: &mut SymbolMap, visited: &mut std::collections::HashSet<usize>) -> String {
        let mut s = String::new();

        s.push('{');

        let mut x = 0;
        for i in 0..self.buckets.len() {
            let entry = &self.buckets[i];
            let k = Value::from(&entry.key);
            let v = Value::from(&entry.val);

            if entry.is_used() {
                x += 1;
                let key_str = format!("{}: ", k.to_string_internal(syms, false, visited));

                let val_str =
                if x != self.entries_count() {
                    format!("{}, ", v.to_string_internal(syms, false, visited))
                } else {
                    v.to_string_internal(syms, false, visited)
                };

                s.push_str(&key_str);
                s.push_str(&val_str);
            }
        }

        s.push('}');
        s
    }
}

fn hash_value(v: &Value<'_>) -> usize {
    let buffer = match v {
        Value::List(l) => {
            let mut buffer = vec![5];
            for i in 0..l.len() {
                buffer.extend_from_slice(&hash_value(&l.at(i)).to_ne_bytes());
            }
            buffer
        }
        Value::Map(m) => {
            let mut buffer = vec![9];
            // Hash maps by XORing all key-value pair hashes
            // This is order-independent since XOR is commutative
            let mut combined_hash: usize = 0;
            for i in 0..m.buckets.len() {
                let entry = &m.buckets[i];
                if entry.is_used() {
                    let key_hash = hash_value(&Value::from(&entry.key));
                    let val_hash = hash_value(&Value::from(&entry.val));
                    // Combine key and value hash, then XOR with accumulated
                    combined_hash ^= key_hash.wrapping_mul(31).wrapping_add(val_hash);
                }
            }
            buffer.extend_from_slice(&combined_hash.to_ne_bytes());
            buffer
        }
        // Use the hash_bytes method for all simple types
        _ => v.hash_bytes(),
    };

    let result = murmurhash3_x64_128(&buffer, 0);

    result.0 as usize ^ result.1 as usize
}

/*
impl Debug for GcHashMap<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{{")?;
        for i in 0..self.buckets.len() {
            let entry = &self.buckets[i];
            let k = Value::from(&entry.key);
            let v = Value::from(&entry.val);

            if entry.is_used() {
                if k.is_string() {
                    write!(f, "\"{:?}\": ", k)?;
                } else {
                    write!(f, "{:?}: ", k)?;
                }
                if v.is_string() {
                    write!(f, "\"{:?}\"", v)?;
                } else {
                    write!(f, "{:?}", v)?;
                }
            }
        }
        write!(f, "}}")
    }
}
*/

#[cfg(test)]
mod tests {
    use crate::runtime::list::List;

    use super::*;
    use sandpit::{Arena, Root};

    #[test]
    fn creates_empty_map() {
        let _: Arena<Root![()]> = Arena::new(|mu| {
            GcHashMap::alloc(mu);
        });
    }

    #[test]
    fn insert_and_get_key() {
        let _: Arena<Root![()]> = Arena::new(|mu| {
            let map = GcHashMap::alloc(mu);
            let key = Value::Int(1).as_tagged(mu);
            let val = Value::Bool(true).as_tagged(mu);

            GcHashMap::insert(map.clone(), key.clone(), val, mu);

            let found = map.get(&key).unwrap();

            matches!(Value::from(&found), Value::Bool(true));
        });
    }

    #[test]
    fn insert_and_get_key_sym() {
        let _: Arena<Root![()]> = Arena::new(|mu| {
            let map = GcHashMap::alloc(mu);
            let key = Value::SymId(1).as_tagged(mu);
            let val = Value::Bool(true).as_tagged(mu);

            GcHashMap::insert(map.clone(), key.clone(), val, mu);

            let found = map.get(&key).unwrap();

            matches!(Value::from(&found), Value::Bool(true));
        });
    }

    #[test]
    fn insert_and_get_key_repeated() {
        let _: Arena<Root![()]> = Arena::new(|mu| {
            let map = GcHashMap::alloc(mu);
            for i in 0..100 {
                let key = Value::Int(i).as_tagged(mu);
                let val = Value::Int(i).as_tagged(mu);

                GcHashMap::insert(map.clone(), key.clone(), val, mu);
            }

            for i in 0..100 {
                let key = Value::Int(i).as_tagged(mu);
                let found = Value::from(&map.get(&key).unwrap());

                if let Value::Bool(true) = equal(found, Value::Int(i)) {
                } else {
                    assert!(false)
                }
            }
        });
    }

    #[test]
    fn overwrite_key() {
        let _: Arena<Root![()]> = Arena::new(|mu| {
            let map = GcHashMap::alloc(mu);
            let key = Value::Int(1).as_tagged(mu);
            let v1 = Value::Bool(true).as_tagged(mu);

            GcHashMap::insert(map.clone(), key.clone(), v1, mu);

            let found = map.get(&key).unwrap();

            matches!(Value::from(&found), Value::Bool(true));

            let v2 = TaggedValue::new_bool(false);

            GcHashMap::insert(map.clone(), key.clone(), v2, mu);

            let found = map.get(&key).unwrap();

            matches!(Value::from(&found), Value::Bool(false));
        });
    }

    #[test]
    fn use_list_as_key() {
        let _: Arena<Root![()]> = Arena::new(|mu| {
            let map = GcHashMap::alloc(mu);
            let key = Value::List(Gc::new(mu, List::alloc(mu))).as_tagged(mu);
            let val = Value::Int(333).as_tagged(mu);

            GcHashMap::insert(map.clone(), key.clone(), val, mu);

            let found = map.get(&key).unwrap();

            matches!(Value::from(&found), Value::Int(333));
        });
    }
}
