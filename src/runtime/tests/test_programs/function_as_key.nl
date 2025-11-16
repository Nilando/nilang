// Test using functions as hash map keys
map = {};

// Use a function as a key
fn my_func() {
  return 42;
}

map[my_func] = "function value";
print(map[my_func]);

// Same function reference should work
f = my_func;
print(map[f]);

// Different function should be different key
fn other_func() {
  return 42;
}

map[other_func] = "other value";
print(map[other_func]);
print(map[my_func]);
