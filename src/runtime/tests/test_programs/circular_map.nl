// Test printing a map with circular reference
a = {};
a[$self] = a;
print(a);
