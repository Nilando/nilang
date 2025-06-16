fn new_counter() {
  val = [];
  // counter = { count: 0 };

  return fn(x) {
    push(val, x);

    return len(val);
  };
}

my_counter = new_counter();

print(my_counter(null)); // 1
print(my_counter(null)); // 2
print(my_counter(null)); // 3
