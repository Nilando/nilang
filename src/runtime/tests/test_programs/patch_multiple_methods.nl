// Patching multiple methods on list type
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

patch($list, $first, fn(self) {
  if #self > 0 {
    return self[0];
  }
  return null;
});

patch($list, $last, fn(self) {
  if #self > 0 {
    return self[#self - 1];
  }
  return null;
});

patch($list, $sum, fn(self) {
  total = 0;
  for item in self {
    total = total + item;
  }
  return total;
});

numbers = [1, 2, 3, 4, 5];
print(numbers.first());
print(numbers.last());
print(numbers.sum());

empty = [];
print(empty.first());
print(empty.last());
print(empty.sum());
