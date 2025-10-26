// the export example we are loading checks for a global flag and returns a
// value if it is set else it returns null
value = import("./src/runtime/tests/test_programs/exporting.nl");

print(value);

@flag = true;

// export value is cached so flipping the flag does not mean file is rerun
// and value is returned
value = import("./src/runtime/tests/test_programs/exporting.nl");

print(value);
