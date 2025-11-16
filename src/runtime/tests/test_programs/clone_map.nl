// Test cloning maps
original = {a: 1, b: 2, c: 3};
cloned = clone(original);

// Verify cloned map has same values
print(cloned.a);
print(cloned.b);
print(cloned.c);

// Modify cloned map
cloned.a = 100;
cloned.d = 4;

// Original should be unchanged
print(original.a);
print(original.b);
print(original.c);
print(original.d);

// Cloned should be modified
print(cloned.a);
print(cloned.d);

// Clone nested maps
nested = {inner: {x: 10, y: 20}};
nested_clone = clone(nested);
print(nested_clone.inner.x);
print(nested_clone.inner.y);

// Test empty map clone
empty = {};
empty_clone = clone(empty);
empty_clone.test = "value";
print(empty_clone.test);
print(empty.test);
