// Test using maps as hash map keys
outer = {};

// Use a map as a key
key1 = {a: 1, b: 2};
outer[key1] = "first value";

// Retrieve using the same key
print(outer[key1]);

// Create a different map with same contents - should be equal and update the same key
key2 = {a: 1, b: 2};
outer[key2] = "second value";

// Both keys should refer to the same entry (structural equality)
print(outer[key1]);
print(outer[key2]);

// Different content should be a different key
key3 = {a: 1, b: 3};
outer[key3] = "third value";
print(outer[key3]);

// Nested maps
nested_key = {inner: {x: 10}};
outer[nested_key] = "nested";
print(outer[nested_key]);
