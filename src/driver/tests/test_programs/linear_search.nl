fn linear_search(list, len, target) {
  i = 0;

  while i < len {
    if list[i] == target {
      return i;
    } else {
      i = i + 1;
    }
  }

  return null;
}

my_list = [0, 0, 420, 0];


result = linear_search(my_list, 4, 420);

print(result);  // 2
