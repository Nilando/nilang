// Basic for loop iterating over a list
// Manually patch iter function onto list type
patch($list, $iter, fn(self) {
  i = [0];
  return fn() {
    n = i[0];
    if i[0] < #self {
      i[0] = i[0] + 1;
      return self[n];
    } else {
      return null;
    }
  };
});

numbers = [1, 2, 3, 4, 5];

for num in numbers {
  print(num);
}
