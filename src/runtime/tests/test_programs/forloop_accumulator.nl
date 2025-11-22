// For loop with accumulator pattern
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

numbers = [10, 20, 30, 40, 50];
sum = 0;

for n in numbers {
  sum = sum + n;
}

print(sum);
