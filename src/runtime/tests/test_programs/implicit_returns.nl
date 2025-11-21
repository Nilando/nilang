// Test 1: Simple implicit return - last expression is returned
fn get_five() {
  5;
}
print(get_five());

// Test 2: Implicit return with operations
fn add_nums(a, b) {
  a + b;
}
print(add_nums(3, 7));

// Test 3: Implicit return from if/else - both branches return
fn max(a, b) {
  if a > b {
    a;
  } else {
    b;
  }
}
print(max(10, 20));
print(max(50, 30));

// Test 4: Implicit return with only if (else returns null)
fn positive_or_null(x) {
  if x > 0 {
    x;
  }
}
print(positive_or_null(5));
print(positive_or_null(-5));

// Test 5: Function with no return (implicitly returns null)
fn do_nothing() {
  x = 1;
}
print(do_nothing());

// Test 6: Nested implicit returns
fn outer(x) {
  fn inner(y) {
    x + y;
  }
  inner(10);
}
print(outer(5));

// Test 7: Implicit return after other statements
fn compute(n) {
  result = n * 2;
  temp = result + 10;
  temp - 5;
}
print(compute(7));

// Test 8: If/else with multiple statements, last is expression
fn categorize(num) {
  if num < 0 {
    category = "negative";
    category;
  } else {
    category = "non-negative";
    category;
  }
}
print(categorize(-5));
print(categorize(5));

// Test 9: Implicit return with string
fn greet(name) {
  "Hello, {name}";
}
print(greet("World"));

// Test 10: Implicit return with list
fn make_list() {
  [1, 2, 3, 4, 5];
}
print(make_list());

// Test 11: Implicit return with map
fn make_map() {
  { key: "value", num: 42 };
}
print(make_map());
