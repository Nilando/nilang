// For loop building a new list
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

numbers = [1, 2, 3, 4, 5];
squares = [];

for n in numbers {
  squares << (n * n);
}

print(squares);
