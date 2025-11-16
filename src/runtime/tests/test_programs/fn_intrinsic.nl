// Test $fn() intrinsic - creates wrapper functions

// Test 1: Wrap a number
wrapped_num = $fn(42);
print(wrapped_num());

// Test 2: Wrap a string
wrapped_str = $fn("hello");
print(wrapped_str());

// Test 3: Wrap a list
wrapped_list = $fn([1, 2, 3]);
result = wrapped_list();
print(result[0]);
print(result[1]);
print(result[2]);

// Test 4: Wrap a map
wrapped_map = $fn({x: 10, y: 20});
m = wrapped_map();
print(m.x);
print(m.y);

// Test 5: Wrap null
wrapped_null = $fn(null);
print(wrapped_null());

// Test 6: Call multiple times - should return same value
constant = $fn(99);
print(constant());
print(constant());
print(constant());

// Test 7: Store in data structures
funcs = [$fn(1), $fn(2), $fn(3)];
print(funcs[0]());
print(funcs[1]());
print(funcs[2]());
