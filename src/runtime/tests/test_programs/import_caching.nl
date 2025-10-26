// Test that imported modules are cached and only executed once
// The export_example.nl file prints "Calculating Important Data!" when executed

// First import - should execute the module and print the message
data1 = import("./examples/export_example.nl");
print(data1.foo);

// Second import - should use cached value, NOT print the message again
data2 = import("./examples/export_example.nl");
print(data2.foo);

// Third import - should also use cached value
data3 = import("./examples/export_example.nl");
print(data3.foo);
