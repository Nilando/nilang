fn new_counter() {
  counter = {x:0};

  return fn(x) {
    x = counter.x;

    counter.x = x + 1;

    return x;
  };
}

my_counter = new_counter();

print(my_counter(null)); // 0
print(my_counter(null)); // 1
print(my_counter(null)); // 2
