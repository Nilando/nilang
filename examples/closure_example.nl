fn new_counter() {
  count = { val: 0 };

  return fn() {
    count.val = count.val + 1;

    return count.val;
  };
}

my_counter = new_counter();

print(my_counter()); // 1
print(my_counter()); // 2
print(my_counter()); // 3
