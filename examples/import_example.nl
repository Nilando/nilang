data = import("./examples/export_example.nl");

print(data);

data.foo = "updated";

// Imported values are cached, so we should see that 
// the data has updated.
data = import("./examples/export_example.nl");

print(data);
