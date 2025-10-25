// For loop over empty list (should not execute body)
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

empty = [];
count = 0;

for item in empty {
  count = count + 1;
}

print(count);
