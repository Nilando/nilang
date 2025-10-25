// Nested for loops
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

rows = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];

for row in rows {
  for cell in row {
    print(cell);
  }
}
