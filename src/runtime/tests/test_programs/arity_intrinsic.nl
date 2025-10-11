print(#fn() {});
print(#fn(a) {});
print(#fn(a, b) {});
print(#fn(a, b, c) {});

// make sure it works on a closure too
x = 123;
print(#fn(a, b, c, d) { print(x); });


// make sure it works on a partial too
f = fn(a, b, c, d, e, f) {};
f = bind(f, null);
print(#f);
