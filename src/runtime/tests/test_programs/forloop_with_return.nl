// For loop with early return
patch($list, $iter, fn(self) {
  i = [0];
  return fn() {
    n = i[0];
    if i[0] < #self {
      i[0] = i[0] + 1;
      return self[n];
    } else {
      return $iter_end;
    }
  };
});

numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

fn find_first_even(list) {
  for n in list {
    if (n % 2) == 0 {
      return n;
    }
  }
  return null;
}

result = find_first_even(numbers);
print(result);
