fn new_counter() {
  count = { val: 0 };

  return fn() {
    count.val = count.val + 1;

    return count.val;
  };
}

counter = new_counter();

i = 0;
while i < 10 {
  print(counter());
  i = i + 1;
}
